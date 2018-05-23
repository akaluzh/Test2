create or replace package PAC_CLAIM is

  -- Author  : A_KALYUZHNAYA
  -- Created : 08.10.2015 15:59:24
  -- Purpose :
  function f_get_ServType (vCode varchar2) return number;
  procedure p_Check_Availability (vClaim number, vDistrBox number, vDBcontact number, vsearch_depth number);
  procedure p_Check_Phone_Availability(vClaim number, vDistrBox number, vDBcontact number, vsearch_depth number);
  procedure p_Check_Coupled_Availability (vClaim number, vDistrBox number, vDBcontact number, vsearch_depth number);
  procedure p_Check_NS_Availability (vClaim number, 
                                     vDistrBox1 number, 
                                     vDBcontact1 number,
                                     vDistrBox2 number, 
                                     vDBcontact2 number,
                                     vsearch_depth number);
  procedure p_PreReserve_Trace(vTrace number);
  
  procedure p_Remove_Reserve(vClaim number);
  procedure p_Refine_Contacts(vTraceDetail number, 
                              vCC_from     number,
                              vCC_to       number,
                              vContact_From number,
                              vContact_To number,
                              vConnector number );

  procedure p_Reserve_LD(vClaim number, vReserveDate date);
  
  procedure p_Create_New_Workorder(vClaim number, vWorder_Code varchar2, /*vResponsible number,*/ vComments varchar2);
  procedure p_Create_Connection (vTask number);
  procedure p_Delete_Connection (vTask number);
  procedure p_Close_Workorder(vWorkorder number);
  procedure p_Cancel_Workorder(vWorkorder number, vResult number, vComment varchar2);
  procedure p_Close_Claim (vClaim number, vStatusDetail number);
      
  procedure p_Check_NS_Availability22 (vClaim number, vDistrBox1 number, vDistrBox2 number, vsearch_depth number);
  
  procedure p_put_trace22 (vClaim in number,
                         vDistrBox in number, 
                         vdepth in number,
                         vside in number,
                         vATS_between in number,
                         vDBcontact out number, 
                         vRB out number, 
                         vRBcontact out number, 
                         vRSh out number);
                         
  procedure p_Connect_Dslam(vLC number, vLC_contact number, vNum number, vServiceADSL out number);
  procedure p_Disconnect_Dslam(vService number);
  function f_trace_by_claim(vclaim NUMBER) RETURN t_traces; 
  function f_trace_by_service(vservice NUMBER) RETURN t_traces;
  function f_get_abstypes_id(vmeta_cathegory varchar2, vacronym varchar2) return number; 

  procedure p_put_trace (vClaim number,
                           vOrg   number,
                           vRSh   number,
                           vdepth number,
                           vside  number,
                           vAts_between number);    
                                           
  --procedure bulk_reconnection (vid number);
  
  procedure p_generate_testorder (vOrg number, vActType number, vConnType number);
  procedure p_generate_testorder_dslam(vOrg number);
  procedure p_build_adsl_trace (vWorkorder number);
/*  function f_find_new_service_for_claim (vClaim number) return number;
  function f_find_old_service_for_claim (vClaim number) return number;
*/  
end PAC_CLAIM;
/
create or replace package body PAC_CLAIM is
  
  function f_get_ServType (vCode varchar2) return number is
    vSTypeId  number;
  begin
    select id into vSTypeId from type_of_service tos where tos.code = vCode;
    return vSTypeId;
  end;

  -- Function and procedure implementations
  procedure p_Check_Availability (vClaim number, vDistrBox number, vDBcontact number, vsearch_depth number) is
    vActType      varchar2(50);
    vNewConnType  varchar2(20);
    vStatus       varchar2(50);
    
  begin
    select a.acronym into vStatus
      from claims cl join abs_types a on cl.status = a.id where cl.id = vClaim;
    if vStatus not in ('new', 'impossible') then
       raise_application_error (-20002, 'Перевірка можливості підключення доступна лише для нових заявок'); 
    end if;   
    
    select a.acronym into vActType
      from claims cl join abs_types a on cl.activity_type = a.id where cl.id = vClaim;

    select a.askr_id into vNewConnType
      from claims cl join type_of_service_askr a on cl.connect_type_new = a.id where cl.id = vClaim;
      
    if vActType in ('add_service', 'replace_service') and vNewConnType in ('13') then
      p_Check_Phone_Availability(vClaim, vDistrBox, vDBcontact, vsearch_depth);
    elsif vActType in ('add_service', 'replace_service') and vNewConnType in ('3') then
      p_Check_Coupled_Availability(vClaim, vDistrBox, vDBcontact, vsearch_depth);
    elsif vActType = 'ns' or (vActType = 'add_service' and vNewConnType in (20,21,22)) then
      raise_application_error (-20001, 'Для перевірки можливості підключення НС оберіть функцію "Перевірити можливість підключення НС"');
    else
      raise_application_error (-20002, 'Цей тип послуги не потребує перевірки можливості підключення');
    end if;  
     

         
  end;

-------------------------------------------------  
  procedure p_Check_Phone_Availability(vClaim number, vDistrBox number, vDBcontact number, vsearch_depth number) is
    vOrg          number;
    vCC           number;
    vCCcontact    number;
    vRSh          number;
    vCross        number;
    vSite         number;
    i             number;
    j             number;
    vCheck        number;
    vTrassa       number;
    x1            number;
    x2            number;
    y1            number;
    y2            number;
    vobj_from     number;
    vobj_to       number;
    vcc_from      number;
    vcc_to        number;
    agis          blob;
    vlength       number;
    vlength0      number;
    vAte          number;
    vtrace_num    number := 5;
  
  begin
        
    if vDistrBox is null or vDBcontact is null then
      raise_application_error (-20003, 'Вкажіть РК та контакт на РК');
    end if;
    
    delete from claim_trace_detail_gis ttg where ttg.claim = vClaim;
    delete from claim_trace_detail td where td.claim = vClaim and td.new = 1;
    delete from claim_trace t where t.claim = vClaim and t.new = 1;
    delete from temp_trace_sections t where t.claim = vClaim;
    i := 0;
  
    select c.organization into vOrg from distr_box c where c.id = vDistrBox;
    
    begin
      select a.site_id into vAte 
        from claims cl, ate_capacity ac, ate a
       where cl.num_new = ac.phone_number 
         and ac.ate = a.id
         and (ac.zone_code = cl.zone_new or '0'||ac.zone_code = (select z.code from zone_codes z where z.id = cl.zone_new))
         and cl.id = vClaim; 
    exception
      when no_data_found then vAte := null;
    end;       

   -- ищем РБ/ГП, к которому подсоединена РК
    select tt.vCC, tt.vCC_contact, tt.vRsh, tt.vCross, vSite, tt.length0
      into vCC, vCCcontact, vRSh, vCross, vSite, vlength0
      from (select ct.contact_cnt_to vCC, ct.contact_to vCC_contact, b.cabinet_id vRsh, st.cross_id vCross, st.site_id, nvl(cs.length,0) length0
               from connector ct join
                     cable_segment cs on ct.cable_segment = cs.id left join
                     box b on ct.contact_cnt_to = b.id left join
                     strafe st on ct.contact_cnt_to = st.id 
               where ct.contact_cnt_from = vDistrBox and ct.contact_from = vDBcontact
              union all 
              select ct.contact_cnt_from, ct.contact_from, b.cabinet_id, st.cross_id, st.site_id, nvl(cs.length,0)
                from connector ct join
                     cable_segment cs on ct.cable_segment = cs.id  left join
                     box b on ct.contact_cnt_from = b.id left join
                     strafe st on ct.contact_cnt_from = st.id 
               where ct.contact_cnt_to = vDistrBox and ct.contact_to = vDBcontact) tt;
                 
    if vCross is not null and vSite = vAte then
        vTrassa := seqid.nextval;
        insert into claim_trace(id, code, depth, distr_box, ats, claim, status, new)
                        values (vTrassa, 'Траса '||to_char(i), 1, vDistrBox, null, vClaim, 
                                  (select id from abs_types a where a.acronym ='possible' and a.meta_cathegory = 'TRACE_STATUS'), 1);
        insert into claim_trace_detail(id, claim, claim_trace, contact_cnt_to, obj_from, contact_cnt_from, sort_order, new)
                values (seqid.nextval, vClaim, vTrassa, vDistrBox, vCross, vCC, j, 1);
                
        ---- gis -----
        begin 
           select dbg.gis_xmin, dbg.gis_ymin into x1, y1
             from distr_box db join distr_box_gis dbg on db.id = dbg.id 
            where db.id = vDistrBox;
             
           select sg.gis_xmin, sg.gis_ymin into x2, y2
             from cross cr join 
                  site s on cr.site_id = s.id join
                  site_gis sg on s.id = sg.id 
            where cr.id = vCross;             
             
           insert into claim_trace_detail_gis(id, gis, gis_xmin, gis_xmax, gis_ymin, gis_ymax, claim, claim_trace)
                   values (seqid.currval, pac_gis.f_gisline(x1, y1, x2, y2), least(x1,x2), greatest(x1,x2), least(y1,y2), greatest(y1,y2), vClaim, vTrassa)
                   returning gis into agis;
        exception
          when others then null;
        end;  
    
    end if;    
    
    if  vRsh is not null then      
                 
    ----------  запись возможных трасс в текстовом иде во временную таблицу  -------------------------------    
        insert into temp_trace_sections (path, level_num, claim)
        select distinct SYS_CONNECT_BY_PATH(tt.pid, '/')||'/', level, vClaim
               from (select cs.id as cabsegm, c.id, oc.id as pid, oc.object_type, oc.id as objid
                       from cabinet c join
                            cable_segment cs on c.id = cs.object_cnt_to join
                            object_cnt oc on cs.object_cnt_from = oc.id
                      where cs.organization = vOrg      
                     union all
                     select cs.id as cabsegm, c.id, oc.id as pid, oc.object_type, oc.id as objid
                       from cabinet c join
                            cable_segment cs on c.id = cs.object_cnt_from join
                            object_cnt oc on cs.object_cnt_to = oc.id 
                      where cs.organization = vOrg  ) tt
              where level <= vsearch_depth and tt.object_type = 2 
                and tt.objid in (select cr.id from cross cr where cr.site_id = nvl(vAte, cr.site_id))
              start with tt.id = vRsh
              connect by nocycle prior tt.pid = tt.id;
        update temp_trace_sections tts set tts.path = substr(tts.path, 2, length(tts.path)) where tts.claim = vClaim;      
        commit;      
     
    -------------------------  разбор трасс из временной таблицы  ------------------------------   
        for c in (select * from 
                    (select * from temp_trace_sections t where t.claim = vClaim order by t.level_num) where rownum <= vTrace_num)
          loop
            SAVEPOINT complete_trace; 
            vlength := vlength0;
            i:= i+1;
            j:=1;
            vTrassa := seqid.nextval;
            insert into claim_trace(id, code, depth, distr_box, ats, claim, status, new)
                            values (vTrassa, 'Траса '||to_char(i), c.level_num, vDistrBox, null, vClaim, 
                                      (select id from abs_types a where a.acronym ='possible' and a.meta_cathegory = 'TRACE_STATUS'), 1);
            insert into claim_trace_detail(id, claim, claim_trace, contact_cnt_to, obj_from, contact_cnt_from, sort_order, new)
                    values (seqid.nextval, vClaim, vTrassa, vDistrBox, vRSh, vCC, j, 1);
                    
            ---- gis -----
            begin 
               select dbg.gis_xmin, dbg.gis_ymin into x1, y1
                 from distr_box db join distr_box_gis dbg on db.id = dbg.id 
                where db.id = vDistrBox;
                 
               select cbg.gis_xmin, cbg.gis_ymin into x2, y2
                 from cabinet cb join cabinet_gis cbg on cb.id = cbg.id 
                where cb.id = vRsh;             
                 
               insert into claim_trace_detail_gis(id, gis, gis_xmin, gis_xmax, gis_ymin, gis_ymax, claim, claim_trace)
                       values (seqid.currval, pac_gis.f_gisline(x1, y1, x2, y2), least(x1,x2), greatest(x1,x2), least(y1,y2), greatest(y1,y2), vClaim, vTrassa)
                       returning gis into agis;
            exception
              when others then null;
            end;  
            ---- gis -----

            vobj_from := vRsh;
            x1 := x2; y1 := y2;
            select to_number(substr(t.path, 1, instr(t.path, '/', 1)-1)) into vobj_to
              from temp_trace_sections t where t.claim = vClaim and t.id = c.id;        

            while length(vobj_to) > 0 
              loop
                j:=j+1;
                 select t.contact_cnt_from, t.contact_cnt_to, vlength + nvl(t.length, 0)
                   into vcc_from, vcc_to, vlength
                   from (select t.contact_cnt_from, t.contact_cnt_to, t.length
                            from cable_segment t
                           where t.object_cnt_from = vobj_from and t.object_cnt_to = vobj_to and t.organization = vOrg
                          union 
                          select t.contact_cnt_to, t.contact_cnt_from, t.length
                            from cable_segment t 
                           where t.object_cnt_from = vobj_to and t.object_cnt_to = vobj_from and t.organization = vOrg) t
                   where pac_route.f_count_free_contacts(t.contact_cnt_from,vOrg) > 0
                     and pac_route.f_count_free_contacts(t.contact_cnt_to,vOrg)> 0  
                           and rownum = 1; 
                           
                 insert into claim_trace_detail (id, claim, obj_to, contact_cnt_to, obj_from, contact_cnt_from, claim_trace, sort_order, new)
                   values (seqid.nextval, vClaim, vobj_from, vcc_from, vobj_to, vcc_to, vTrassa, j, 1);
                 
            ---- gis -----
                 begin 
                   select t.gis_xmin, t.gis_ymin into x2, y2
                     from (select cbg.gis_xmin, cbg.gis_ymin 
                             from cabinet cb join cabinet_gis cbg on cb.id = cbg.id 
                            where cb.id = vobj_to
                           union 
                           select sg.gis_xmin, sg.gis_ymin 
                             from cross cr join site s on cr.site_id = s.id join site_gis sg on s.id = sg.id 
                            where cr.id = vobj_to) t;
                    
                     insert into claim_trace_detail_gis(id, gis, gis_xmin, gis_xmax, gis_ymin, gis_ymax, claim, claim_trace)
                         values (seqid.currval, pac_gis.f_gisline(x1, y1, x2, y2), least(x1,x2), greatest(x1,x2), least(y1,y2), greatest(y1,y2), vClaim, vTrassa)
                         returning gis into agis;
                 exception
                   when others then 
                     x2 := x1;
                     y2 := y1;  
                 end;
             ---- gis -----
       
                 update temp_trace_sections t set t.path =  substr(t.path, instr(t.path, '/', 1)+1)
                  where t.id = c.id;  
                   
                 vobj_from := vobj_to;
                 x1 := x2; y1:= y2;
                 select to_number(substr(t.path, 1, instr(t.path, '/', 1)-1)) into vobj_to
                   from temp_trace_sections t where t.claim = vClaim and t.id = c.id; 
                   
                 -- проверка на "петли" в трассе       
                 select count(1) into vCheck from claim_trace_detail td 
                  where td.claim = vClaim and td.claim_trace = vTrassa and td.new = 1 and (td.obj_from = vobj_to or td.obj_to = vobj_to); 
                
                 if vCheck > 0 then
                   ROLLBACK TO complete_trace;
                   i:=i-1;
                   vobj_to := null;
                 end if; 
                   
               end loop;
             update claim_trace ct set ct.ats = vcc_to where ct.id = vTrassa;  
             update claim_trace ct set ct.length = vlength where ct.id = vTrassa;
             -- SAVEPOINT complete_trace;  
          end loop;    
     
    end if;       
   delete from temp_trace_sections t where t.claim = vClaim;    

   select count(1) into i
    from claim_trace ct where ct.claim = vClaim;
  
   if i > 0 then
     update claims cl 
        set cl.status = f_get_abstypes_id('CLAIM_STATUS', 'possible') where cl.id = vClaim;
   else
     update claims cl 
        set cl.status = f_get_abstypes_id('CLAIM_STATUS', 'impossible') where cl.id = vClaim;        
   end if;
    
  end;  


  procedure p_Check_Coupled_Availability (vClaim number, vDistrBox number, vDBcontact number, vsearch_depth number) is
    vOrg             number;                         
    vZoneCode        number;
    vNumNew          varchar2(10);
    vNumCoupled      varchar2(10);
    vCoupledService  number;
    vService         number;
    vTrace           number;
    vDBoxCoupled     number;
    vStrafeCoupled   number;
  begin
    select cl.organization, cl.zone_new, cl.num_new into vOrg, vZoneCode, vNumNew
      from claims cl where cl.id = vClaim;  
      
    select ac2.phone_number into vNumCoupled
      from ate_capacity ac1 join 
           ate_capacity ac2 on ac1.coupled_number_id = ac2.id 
     where ac1.organization = vOrg and ac1.zone_code = vZoneCode and ac1.phone_number = vNumNew;
     
    begin
      select s.id into vCoupledService
        from service s 
       where s.organization = vOrg and s.code = vNumCoupled and s.zonecode = vZoneCode; 
       
      select k.id into vDBoxCoupled
        from (select db.id
                from contact_service csv join distr_box db on csv.contact_cnt = db.id 
               where csv.service = vCoupledService order by csv.sort_order) k where rownum = 1;

      select k.id into vStrafeCoupled
        from (select s.id
                from contact_service csv join strafe s on csv.contact_cnt = s.id 
               where csv.service = vCoupledService order by csv.sort_order desc) k where rownum = 1;     
      
      insert into claim_trace (code, claim, depth, distr_box, ats, new, status)
        values ('Траса 1', vClaim, null, vDBoxCoupled, vStrafeCoupled, 1,
                (select id from abs_types a where a.meta_cathegory = 'TRACE_STATUS' and a.acronym = 'possible'))
       returning id into vTrace; 
       
      insert into claim_trace_detail(claim, claim_trace, obj_from, obj_to, contact_cnt_from, contact_cnt_to, 
                                     contact_from, contact_to, new, sort_order)
        select vClaim, vTrace, 
               decode(cs.reverse, 0, cc_from.object_cnt, cc_to.object_cnt), 
               decode(cs.reverse, 0, cc_to.object_cnt, cc_from.object_cnt), 
               decode(cs.reverse, 0, cc_from.id, cc_to.id),
               decode(cs.reverse, 0, cc_to.id, cc_from.id),
               decode(cs.reverse, 0, c_from.id, c_to.id),
               decode(cs.reverse, 0, c_to.id, c_from.id),
               1, cs.sort_order
          from connector_service cs join
               connector c on cs.connector = c.id join
               contact_cnt cc_from on c.contact_cnt_from = cc_from.id join
               contact_cnt cc_to on c.contact_cnt_to = cc_to.id join
               contact c_from on c.contact_from  = c_from.id join
               contact c_to on c.contact_to = c_to.id
         where cs.service = vCoupledService;

       update claims cl 
        set cl.status = f_get_abstypes_id('CLAIM_STATUS', 'possible') where cl.id = vClaim;
      
           -- точки питания, подключения  
    /* 
     update claims cl set cl.status = f_get_abstypes_id('CLAIM_STATUS', 'prereserved') where cl.id = vClaim;                          
*/
    exception 
      when no_data_found then 
        p_Check_Phone_Availability(vClaim, vDistrBox, vDBcontact, vsearch_depth);
    end;    
      
      
  end;  

----- построение трасс для безномерных линий с обязательным заходом на АТС 
  procedure p_Check_NS_Availability (vClaim number, 
                                       vDistrBox1 number, 
                                       vDBcontact1 number,
                                       vDistrBox2 number, 
                                       vDBcontact2 number,
                                       vsearch_depth number) is
    vOrg          number;
    vDistrBox     number;
    vDBcontact    number;
    vCC           number;
    vCCcontact    number;
    vRSh          number;
    vCross        number;
    vlength0      number;
    vContCnt1     number;
    vObjCnt1      number;
    vContCnt2     number;
    vObjCnt2      number;
    i             number;
    j             number;
    m             number;
    vcommon_obj   number;
    vobj_from     number;
    vobj_to       number;
    vcc_from      number;
    vcc_to        number;
    vTrassa       number;
    vCheck        number;
    
  begin
    
    delete from claim_trace_detail_gis ttg where ttg.claim = vClaim;
    delete from claim_trace_detail td where td.claim = vClaim;
    delete from claim_trace t where t.claim = vClaim;
    delete from temp_trace_sections t where t.claim = vClaim;
    
    select cl.organization into vOrg from distr_box cl where cl.id = vDistrBox1;
    
    for i in 1..2
      loop
          if i = 1 then
            vDistrBox := vDistrBox1; 
            vDBcontact := vDBcontact1;
          else
            vDistrBox := vDistrBox2; 
            vDBcontact := vDBcontact2;          
          end if;
          
          select tt.vCC, tt.vCC_contact, tt.vRsh, tt.vCross, tt.length0
            into vCC, vCCcontact, vRSh, vCross, vlength0
            from (select ct.contact_cnt_to vCC, ct.contact_to vCC_contact, b.cabinet_id vRsh, st.cross_id vCross, nvl(cs.length,0) length0
                     from connector ct join
                           cable_segment cs on ct.cable_segment = cs.id left join
                           box b on ct.contact_cnt_to = b.id left join
                           strafe st on ct.contact_cnt_to = st.id 
                     where ct.contact_cnt_from = vDistrBox and ct.contact_from = vDBcontact
                    union all 
                    select ct.contact_cnt_from, ct.contact_from, b.cabinet_id, st.cross_id, nvl(cs.length,0)
                      from connector ct join
                           cable_segment cs on ct.cable_segment = cs.id  left join
                           box b on ct.contact_cnt_from = b.id left join
                           strafe st on ct.contact_cnt_from = st.id 
                     where ct.contact_cnt_to = vDistrBox and ct.contact_to = vDBcontact) tt;      
                  
        if vCross is not null then
            insert into temp_trace_sections (path, level_num, claim, side, ate)
              values ('/'||vCross ||'/', 1, vClaim, i, vCross);     
            if i = 1 then
              vObjCnt1 := vCross; vContCnt1 := vCC;
            else
              vObjCnt2 := vCross; vContCnt2 := vCC;
            end if;           
        end if;  
      
        if  vRsh is not null then  
              if i = 1 then
                vObjCnt1 := vRsh; vContCnt1 := vCC;
              else
                vObjCnt2 := vRsh; vContCnt2 := vCC;
              end if;    
              p_put_trace(vClaim, vOrg, vRsh, vsearch_depth, i, 0);
              
              if i = 2 then
                 -- поиск общего кросса (АТС) в трассах РК - АТС
                select count(1) into vCheck
                  from temp_trace_sections t1 join 
                       temp_trace_sections t2 on t1.ate = t2.ate
                 where t1.claim = vClaim and t2.claim = vClaim
                   and t1.side = 1 and t2.side = 2 ;
                   
                if vCheck = 0 then
                  delete from temp_trace_sections tts where tts.side = 2 and tts.claim = vClaim;
                  p_put_trace(vClaim, vOrg, vRsh, vsearch_depth, i, 1);
                end if; 
                update temp_trace_sections tts set tts.path = substr(tts.path, 2, length(tts.path)) where tts.claim = vClaim;                    
              end if;
              
              
             -- update temp_trace_sections tts set tts.path = substr(tts.path, 2, length(tts.path)) where tts.claim = vClaim and tts.side = 2;   
        end if;
                     
      end loop;  
      commit;

    --  поиск совпадающих объектов в 2 трассах  --------------------
    i := 0;
    
    for c in (select * from 
                     (select t1.id as id1, t1.path as path1, t2.id as id2, t2.path as path2, 
                             t1.ate, t1.level_num as level1, t2.level_num as level2
                        from temp_trace_sections t1 join
                             temp_trace_sections t2 on t1.ate = t2.ate
                       where t1.claim = vClaim and t1.side = 1 --and instr(t1.path, vRsh1)=0
                         and t2.claim = vClaim and t2.side = 2 --and instr(t1.path, vRsh2)=0
                        order by t1.level_num + t2.level_num )
                where rownum =1)
       loop
         -- запись 1-й части трассы
         SAVEPOINT complete_trace;
         i:= i+1;
         j:= 1;                
         vTrassa := seqid.nextval;
         insert into claim_trace(id, code, depth, distr_box, ats, claim, status, new)
              values (vTrassa, 'Траса '||to_char(i), c.level1 + c.level2, vDistrBox1, null, vClaim, 
                                (select id from abs_types a where a.acronym ='possible' and a.meta_cathegory = 'TRACE_STATUS'), 1);
         insert into claim_trace_detail(id, claim, claim_trace, contact_cnt_to, obj_from, contact_cnt_from, sort_order, new)
              values (seqid.nextval, vClaim, vTrassa, vDistrBox1, vObjCnt1, vContCnt1, j, 1);
        
         vobj_from := vObjCnt1;        
         select to_number(substr(t.path, 1, instr(t.path, '/', 1)-1)) into vobj_to
           from temp_trace_sections t where t.id = c.id1;   
           
         while vobj_from <> c.ate  
           loop
             j:=j+1;
             select t.contact_cnt_from_id, t.contact_cnt_to_id
               into vcc_from, vcc_to
               from (select t.contact_cnt_from_id, t.contact_cnt_to_id
                       from v_cable_segment_obj t where t.object_from_id = vobj_from and t.object_to_id = vobj_to
                   union --all
                      select t.contact_cnt_to_id, t.contact_cnt_from_id
                        from v_cable_segment_obj t where t.object_from_id = vobj_to and t.object_to_id = vobj_from) t
              where pac_route.f_count_free_contacts(t.contact_cnt_from_id,vOrg) > 0
                and pac_route.f_count_free_contacts(t.contact_cnt_to_id,vOrg)> 0  
                and rownum = 1;   
                         
             insert into claim_trace_detail (id, claim, obj_to, contact_cnt_to, obj_from, contact_cnt_from, claim_trace, sort_order, new)
                    values (seqid.nextval, vClaim, vobj_from, vcc_from, vobj_to, vcc_to, vTrassa, j, 1);
                                                                             
             vobj_from := vobj_to;
      
             select substr(t.path, 
                           decode(j, 1, 1, instr(t.path, '/', 1, j-1 )+1),
                           instr(t.path, '/', 1, j) - decode(j, 1, 0, instr(t.path, '/', 1, j-1 ))- 1 )
               into vobj_to
               from temp_trace_sections t where t.claim = vClaim and t.id = c.id1; 
               
             -- проверка на "петли" в трассе       
             select count(1) into vCheck from claim_trace_detail td 
              where td.claim = vClaim and td.claim_trace = vTrassa and td.new = 1 and (td.obj_from = vobj_to or td.obj_to = vobj_to); 
            
             if vCheck > 0 then
               ROLLBACK TO complete_trace;
               i:=i-1;
               vobj_to := null;
             end if; 
           end loop;
     
         -- запись 2-й части трассы
         m := c.level1 + c.level2 + 3; 
         j := 1;    
         vobj_to := vObjCnt2;
         select to_number(substr(t.path, 1, instr(t.path, '/', 1)-1)) into vobj_from
           from temp_trace_sections t where t.id = c.id2;     

         while vobj_to <> c.ate  
           loop
             j:=j+1;
             select t.contact_cnt_from_id, t.contact_cnt_to_id
               into vcc_from, vcc_to
               from (select t.contact_cnt_from_id, t.contact_cnt_to_id
                       from v_cable_segment_obj t where t.object_from_id = vobj_from and t.object_to_id = vobj_to
                  union --all
                     select t.contact_cnt_to_id, t.contact_cnt_from_id
                      from v_cable_segment_obj t where t.object_from_id = vobj_to and t.object_to_id = vobj_from) t
              where pac_route.f_count_free_contacts(t.contact_cnt_from_id,vOrg) > 0
                and pac_route.f_count_free_contacts(t.contact_cnt_to_id,vOrg)> 0  
                and rownum = 1;        
             
             insert into claim_trace_detail (id, claim, obj_to, contact_cnt_to, obj_from, contact_cnt_from, claim_trace, sort_order, new)
                      values (seqid.nextval, vClaim, vobj_from, vcc_from, vobj_to, vcc_to, vTrassa, m-j, 1);
                        
             vobj_to := vobj_from;
             select substr(t.path, 
                           decode(j, 1, 1, instr(t.path, '/', 1, j-1 )+1),
                           instr(t.path, '/', 1, j) - decode(j, 1, 0, instr(t.path, '/', 1, j-1 ))- 1 )
              into vobj_from
              from temp_trace_sections t where t.claim = vClaim and t.id = c.id2; 
                      
            -- проверка на "петли" в трассе                             
             select count(1) into vCheck from claim_trace_detail td 
              where td.claim = vClaim and td.claim_trace = vTrassa and (td.obj_from = vobj_from or td.obj_to = vobj_from)
                and vobj_from <> vcommon_obj; 
                                     
             if vCheck > 0 then
               ROLLBACK TO complete_trace;
                vobj_from := null;
             end if; 
           
           end loop;             
  
        insert into claim_trace_detail(id, claim, claim_trace, obj_to, contact_cnt_to, contact_cnt_from, sort_order, new)
        values (seqid.nextval, vClaim, vTrassa, vObjCnt2, vContCnt2, vDistrBox2, m-1, 1);                   
                    
       end loop;   

     select count(1) into i
      from claim_trace ct where ct.claim = vClaim;
    
     if i > 0 then
       update claims cl 
          set cl.status = f_get_abstypes_id('CLAIM_STATUS', 'possible') where cl.id = vClaim;
     else
       update claims cl 
          set cl.status = f_get_abstypes_id('CLAIM_STATUS', 'impossible') where cl.id = vClaim;        
     end if;       
              
   end;  


---------------------------------------------------------  
  procedure p_PreReserve_Trace(vTrace number) is
    vOrg             number;
    vClaim           number;
    vContact_From    number;
    vContact_To      number;
    vService         number;
    vTraceStatus     varchar2(50);
    v1               number;
    vc1              number;
    vvContact_cnt    number;
    vvContact        number;
    vConnector       number;
    vSortOrder       number;
    vReverse         number;
    vServiceType     number;
    vNumCoupled      varchar2(20);
    vZoneCode        number;
    vCoupledService  number;
    i                number := 1;
  begin
    
    select a.acronym into vTraceStatus
      from claim_trace ct join abs_types a on ct.status = a.id where ct.id = vTrace;  
    if vTraceStatus <> 'possible' then
      raise_application_error (-20001, 'Траса не може бути зарезервована. Перевірте статус траси');
    end if;  
  
    select c.organization, c.id, tosa.type_of_service into vOrg, vClaim, vServiceType 
      from claims c join 
           claim_trace ct on c.id = ct.claim join
           type_of_service_askr tosa on c.connect_type_new = tosa.id
     where ct.id = vTrace;

    vService := seqid.nextval;
    insert into service (id, code, organization, status, subscriber, type_of_service, phone_number, zonecode, 
                         reserve_date, full_number)
        (select vService, 
                nvl(t.num_new, 'Заявка № '||nvl(t.askr_id, t.id)), 
                vOrg, 
                f_get_abstypes_id('SERVICE_STATUS', 'reserved'),
                t.subscriber, 
                vServiceType, 
                t.num_new, 
                t.zone_new, 
                sysdate,
                '380'||zc.code||t.num_new
           from claims t left join 
                zone_codes zc on t.zone_new = zc.id where t.id = vClaim);
    
    update claims cm set cm.service_new = vService
     where cm.id = vClaim;            
    
    begin
        select acc.phone_number, cm.zone_new into vNumCoupled, vZoneCode
          from claims cm join
               ate_capacity ac on cm.num_new = ac.phone_number and cm.zone_new = ac.zone_code join
               ate_capacity acc on ac.coupled_number = acc.id
         where cm.id = vClaim;
    
        select s.id into vCoupledService
          from service s 
         where s.organization = vOrg and s.code = vNumCoupled and s.zonecode = vZoneCode;

    exception
        when no_data_found then vCoupledService := null;
    end;       
    
    if vServiceType = 3 and vCoupledService is not null then
            for d in (select * from connector_service ct where ct.service = vCoupledService)
              loop
                insert into connector_service (connector, service, organization, sort_order, reverse)
                    select ccs.connector, vService, ccs.organization, ccs.sort_order, ccs.reverse
                      from connector_service ccs where ccs.id = d.id;
              end loop;

            for d in (select * from contact_service ct where ct.service = vCoupledService)
              loop
                insert into contact_service (service, contact_cnt, contact, organization, sort_order)
                    select vService, cts.contact_cnt, cts.contact, cts.organization, cts.sort_order
                      from contact_service cts where cts.id = d.id;
              end loop;
              
            -- точки питания, подключения  
             update service s 
              set s.contact_cnt_start = (select srv.contact_cnt_start from service srv where srv.id = vCoupledService),
                  s.contact_cnt_end =   (select srv.contact_cnt_end   from service srv where srv.id = vCoupledService),
                  s.contact_start = (select srv.contact_start from service srv where srv.id = vCoupledService),
                  s.contact_end =   (select srv.contact_end   from service srv where srv.id = vCoupledService)
            where s.id = vService;  
       
    else
            for TraceSection in (select ctd.id, ctd.contact_cnt_from, ctd.contact_cnt_to, ctd.sort_order*10 as sort_order
                                   from claim_trace_detail ctd 
                                  where ctd.claim_trace = vTrace order by ctd.sort_order)
              loop
                vSortOrder := TraceSection.Sort_Order;
                
                if vvContact_cnt is not null then      
                  select cont.id, cc.id into v1, vc1
                    from contact cont join contact_cnt cc on cont.contact_cnt_model = cc.model
                   where cc.id = TraceSection.Contact_Cnt_To
                     and not exists (select id from contact_service cs where cs.contact_cnt = cc.id and cs.contact = cont.id)
                     and cont.id in (select ct.contact_to
                                       from connector ct where ct.contact_cnt_from = TraceSection.Contact_Cnt_From and ct.contact_cnt_to = TraceSection.Contact_Cnt_To
                                     union
                                     select ct.contact_from
                                       from connector ct where ct.contact_cnt_from = TraceSection.Contact_Cnt_To and ct.contact_cnt_to = TraceSection.Contact_Cnt_From) 
                     and rownum = 1;    

                    insert into connector(contact_cnt_from, contact_from, contact_cnt_to, contact_to, modify_date, organization, type)
                       values (vc1, v1, vvContact_cnt, vvContact, sysdate, vOrg,
                               (select id from abs_types a where UPPER(a.meta_cathegory) = 'CONNECTOR_TYPE' and a.code = 'single'))
                       returning id into vConnector;
                    insert into connector_service (connector, service, organization, sort_order) 
                       values (vConnector, vService, vOrg, vSortOrder-5); 
                end if;
                
                begin
                  select t.id, t.contact_from, t.contact_to, 0 into vConnector, vContact_From, vContact_To, vReverse
                    from connector t left join
                         contact_service cs1 on (t.contact_cnt_from = cs1.contact_cnt and t.contact_from = cs1.contact) left join
                         contact_service cs2 on (t.contact_cnt_to = cs2.contact_cnt and t.contact_to = cs2.contact)
                   where t.contact_cnt_from = TraceSection.Contact_Cnt_From and t.contact_cnt_to = TraceSection.Contact_Cnt_To 
                     and t.contact_to = nvl(v1, t.contact_to)
                     and cs1.id is null and cs2.id is null
                     and rownum = 1;
                  
                exception
                  when no_data_found then
                    select t.id, t.contact_to, t.contact_from, 1 into vConnector, vContact_From, vContact_To, vReverse
                      from connector t left join
                           contact_service cs1 on (t.contact_cnt_from = cs1.contact_cnt and t.contact_from = cs1.contact) left join
                           contact_service cs2 on (t.contact_cnt_to = cs2.contact_cnt and t.contact_to = cs2.contact)
                     where t.contact_cnt_to = TraceSection.Contact_Cnt_From and t.contact_cnt_from = TraceSection.Contact_Cnt_To
                       and t.contact_from = nvl(v1, t.contact_from) 
                       and cs1.id is null and cs2.id is null
                       and rownum = 1;            
                end;       
          
                update claim_trace_detail t set t.contact_from = vContact_From, t.contact_to = vContact_To
                 where t.id = TraceSection.id;

                insert into connector_service (connector, service, organization, sort_order, reverse) 
                   values (vConnector, vService, vOrg, TraceSection.Sort_Order, vReverse); 
                insert into contact_service (service, contact_cnt, contact, organization, sort_order)
                   values (vService, TraceSection.Contact_Cnt_From, vContact_From, vOrg, i+1);
                insert into contact_service (service, contact_cnt, contact, organization, sort_order)
                   values (vService, TraceSection.Contact_Cnt_To, vContact_To, vOrg, i);
                   
                vvContact_cnt := TraceSection.Contact_Cnt_From; 
                vvContact := vContact_From;  
                i := i+2;
             end loop;
             
             -- точки питания, подключения  
             select s.contact_cnt, s.contact into vvContact_cnt, vvContact
               from contact_service s 
              where s.service = vService and s.sort_order = (select min(cs.sort_order) from contact_service cs where cs.service = vService);
             update service s 
                set s.contact_cnt_start = vvContact_cnt, s.contact_start = vvContact
              where s.id = vService;

             select s.contact_cnt, s.contact into vvContact_cnt, vvContact
               from contact_service s 
              where s.service = vService and s.sort_order = (select max(cs.sort_order) from contact_service cs where cs.service = vService);
             update service s 
                set s.contact_cnt_end = vvContact_cnt, s.contact_end = vvContact
              where s.id = vService;

     end if;     
    
     delete from claim_trace ct where ct.claim = vClaim and ct.id <> vTrace and ct.new = 1;
     update claim_trace t set t.status = f_get_abstypes_id('TRACE_STATUS', 'prereserved') where t.id = vTrace; 
     update claims cl set cl.status = f_get_abstypes_id('CLAIM_STATUS', 'prereserved') where cl.id = vClaim;                 
  end;




/*-- изменение контактов
  procedure p_Refine_Contacts__(vTraceDetail number, vContact_From_old varchar2, vContact_From number, 
                                                   vContact_To_old varchar2, vContact_To number ) is
    vCC_from     number;
    vC_from      number;
    vCC_to       number;
    vC_to        number;
  begin
     
    select ctd.contact_cnt_from, ctd.contact_from, ctd.contact_cnt_to, ctd.contact_to
      into vCC_from, vC_from, vCC_to, vC_to
      from claim_trace_detail ctd 
     where ctd.id = vTraceDetail;

    update connector cn
      set cn.contact_from = vContact_From, cn.contact_to = vContact_To
    where cn.contact_cnt_from = vCC_from and cn.contact_from = vC_from
      and cn.contact_cnt_to = vCC_to and cn.contact_to = vC_to;
    
    update contact_service cs
       set cs.contact = vContact_From
     where cs.contact_cnt = vCC_from and cs.contact = vC_from;
     
    update contact_service cs
       set cs.contact = vContact_To
     where cs.contact_cnt = vCC_to and cs.contact = vC_to;       

    update claim_trace_detail ctd
       set ctd.contact_from = vContact_From, ctd.contact_to = vContact_To
     where ctd.id = vTraceDetail;
  end;*/
  
  -- изменение контактов
  procedure p_Refine_Contacts(vTraceDetail number, 
                              vCC_from     number,
                              vCC_to       number,
                              vContact_From number,
                              vContact_To number,
                              vConnector number ) is
    vCC_from_old number;
    vC_from_old  number;
    vCC_to_old   number;
    vC_to_old    number;
    vClaim       number;
    vService     number;
    vConnector_Old number;
  begin
     
    select ctd.contact_cnt_from, ctd.contact_from, ctd.contact_cnt_to, ctd.contact_to, ctd.claim
      into vCC_from_old, vC_from_old, vCC_to_old, vC_to_old, vClaim
      from claim_trace_detail ctd 
     where ctd.id = vTraceDetail;
       
    select cm.service_new into vService
      from claims cm where cm.id = vClaim; 
      
    select id into vConnector_Old
      from (    select cn.id
                  from connector cn
                 where cn.contact_cnt_from = vCC_from_old and cn.contact_from = vC_from_old
                   and cn.contact_cnt_to = vCC_to_old and cn.contact_to = vC_to_old
                 union
                select cn.id
                  from connector cn
                 where cn.contact_cnt_to = vCC_from_old and cn.contact_to = vC_from_old
                   and cn.contact_cnt_from = vCC_to_old and cn.contact_from = vC_to_old  );
    
    insert into connector_service (connector, service, organization, sort_order, reverse)
        select vConnector, vService, cs.organization, cs.sort_order, cs.reverse
          from connector_service cs where cs.connector = vConnector_Old and cs.service = vService;
    
    delete from connector_service cs where cs.connector = vConnector_Old and cs.service = vService;      
       
    insert into contact_service(service, contact_cnt, contact, organization, sort_order)
       select vService, vCC_from, vContact_From, ccs.organization, ccs.sort_order
         from contact_service ccs
        where ccs.service = vService and ccs.contact_cnt = vCC_from_old and ccs.contact = vC_from_old;
    
    delete from contact_service ccs 
     where ccs.service = vService and ccs.contact_cnt = vCC_from_old and ccs.contact = vC_from_old;
     
    insert into contact_service(service, contact_cnt, contact, organization, sort_order)
       select vService, vCC_to, vContact_to, ccs.organization, ccs.sort_order
         from contact_service ccs
        where ccs.service = vService and ccs.contact_cnt = vCC_to_old and ccs.contact = vC_to_old;
    
    delete from contact_service ccs 
     where ccs.service = vService and ccs.contact_cnt = vCC_to_old and ccs.contact = vC_to_old;
     
    update claim_trace_detail ctd
       set ctd.contact_from = vContact_From, ctd.contact_to = vContact_To
     where ctd.id = vTraceDetail and ctd.contact_cnt_from = vCC_from and ctd.contact_cnt_to = vCC_to;
     
     
  end;
  
  
-- резервирование ЛД
  procedure p_Reserve_LD(vClaim number, vReserveDate date) is
    vService    number;
    vServiceOld number;
    vServType   number;
    vStrafe     number;
    vLD         varchar2(2000):= null;
    vActType    varchar2(50);
    vNum        varchar2(20);
    vZoneCode   number;
    vLoadType   varchar2(20);
  begin
 
    update claims cl
      set cl.status = (select id from abs_types a where a.meta_cathegory = 'CLAIM_STATUS' and a.acronym = 'reserved'),
          cl.status_details = (select id from abs_types a where a.meta_cathegory = 'CLAIM_RESULTS' and a.acronym = '5'),
          cl.success = 1,
          cl.reserve_enddate = vReserveDate
    where cl.id = vClaim;
     
    select a.acronym, cl.load_type into vActType, vLoadType 
      from claims cl join abs_types a on cl.activity_type = a.id where cl.id = vClaim; 
    
    if vActType in ('add_service', 'ns', 'replace_service') then 
        
        select cm.num_new into vNum from claims cm where cm.id = vClaim;
        update claim_trace ct set ct.status = 
              (select id from abs_types a where a.meta_cathegory = 'TRACE_STATUS' and a.acronym = 'reserved')
         where ct.claim = vClaim;        
         
        select s.id, s.contact_cnt_end, s.type_of_service into vservice, vStrafe, vServType
          from service s where s.id = (select service_new from claims where id = vClaim);
        
        for c in (select * from table(pac_claim.f_trace_by_service(vservice)))
          loop
            vLD := vLD ||c.oc1_code||': '||c.cc1_type||c.cc1||' >> '||c.oc2_code||': '||c.cc2_type||c.cc2||chr(13)||chr(10);
          end loop;
          
        update claims c set c.linear_data = vLD where c.id = vClaim;
              
        -- если в заявке не указан номер, выбираем свободный номер на АТС
        if vNum is null then
          select ac.zone_code, ac.phone_number into vZoneCode, vNum 
            from strafe s join
                 ate a on s.site_id = a.site_id join
                 ate_capacity ac on a.id = ac.ate   
           where s.id = vStrafe and ac.status = 0 and ac.service_type = vServType
             and rownum = 1;
          
          update claims cl set cl.zone_new = vZoneCode, cl.num_new = vNum
             where cl.id = vClaim;
          update service s set s.code = vNum, s.phone_number = vNum, s.zonecode = vZoneCode
           where s.id = vservice;   
        end if;
        
        select cm.num_new, cm.zone_new into vNum, vZoneCode from claims cm where cm.id = vClaim;
        update ate_capacity ac 
           set ac.status = 1
         where ac.zone_code = vZoneCode and ac.phone_number = vNum;
    end if;
    
    if vActType in ('replace_number') then 
      
      select cm.zone_new, cm.num_new into vZoneCode, vNum from claims cm where cm.id = vClaim;
      
      vService := seqid.nextval; 
      select cm.service_old into vServiceOld from claims cm where cm.id = vClaim;

      insert into service(id, code, subscriber, organization, type_of_service, contact_cnt_start, contact_cnt_end,
                        contact_start, contact_end, zonecode, phone_number, status)
       select vService, vNum, s.subscriber, s.organization, s.type_of_service, s.contact_cnt_start, s.contact_cnt_end,
                       s.contact_start, s.contact_end, vZoneCode, vNum,
                       (select id from abs_types ab where ab.meta_cathegory = 'SERVICE_STATUS' and ab.acronym = 'reserved')
         from service s where id = vServiceOld;  
      
      update claims cm set cm.service_new = vService where cm.id = vClaim;   
        
        for c in (select cs.id, cs.contact_cnt, cs.contact, cs.sort_order, cs.organization 
                    from contact_service cs where cs.service = vServiceOld)
          loop
         --   delete from contact_service t where t.id = c.id;
            insert into contact_service(service, contact_cnt, contact, organization, sort_order)
                values (vService, c.contact_cnt, c.contact, c.organization, c.sort_order);
          end loop;
          
        for c in (select cs.id, cs.connector, cs.sort_order, cs.reverse, cs.organization 
                    from connector_service cs where cs.service = vServiceOld)
          loop
         --   delete from connector_service t where t.id = c.id;
            insert into connector_service(service, connector, organization, sort_order, reverse)
                values (vService, c.connector, c.organization, c.sort_order, c.reverse);
          end loop;   
        
                    
       select cm.num_new, cm.zone_new into vNum, vZoneCode from claims cm where cm.id = vClaim;
       update ate_capacity ac 
          set ac.status = 1
        where ac.zone_code = vZoneCode and ac.phone_number = vNum;
        
        
        
    end if;
 
/*    if vActType in ('connect') then 
      select s.id into vservice
        from claims cl join 
             service s on cl.organization = s.organization and cl.zone_old = s.zonecode and cl.num_old = s.code
       where cl.id = vClaim 
         and s.status = (select id from abs_types ab where ab.meta_cathegory = 'SERVICE_STATUS' and ab.acronym = 'reserved_old'); 
      
      update service s 
             set s.status = f_get_abstypes_id ('service_status', 'reserved'),
                 s.reserve_date = vReserveDate
           where s.id = vService; 
    end if;*/
        
    if vLoadType = 'auto' then
      pac_claim_order.SetOrderStatus(vClaim);
    end if; 

  end; 

---------------------------------------------------------
  procedure p_Remove_Reserve(vClaim number) is
    vOldstatus        varchar2(50);  
    vService          number; 
  begin
    select a.acronym into vOldstatus
      from claims cl join abs_types a on cl.status = a.id where cl.id = vclaim;
    
    if vOldstatus not in ('prereserved', 'reserved') then
      raise_application_error(-20001, 'Траса має бути заброньована');
    end if;
    
    select cm.service_new into vService from claims cm where cm.id = vClaim;
    delete from contact_service cs where cs.service = vService;
    delete from connector_service cs where cs.service = vService;
    
    for c in (select conn.id, conn.cable_segment 
                from connector_service cs join connector conn on cs.connector = conn.id 
               where cs.service = vService)
      loop
        if c.cable_segment is null then 
          delete from connector conn where conn.id = c.id;
        end if;  
      end loop;
    
    delete from service s where s.id = vService;
    
     update claims cl
       set cl.status = (select id from abs_types a where a.meta_cathegory = 'CLAIM_STATUS' and a.acronym = 'impossible'),
           cl.status_details = (select id from abs_types a where a.meta_cathegory = 'CLAIM_RESULT' and a.acronym = '-12')
    where cl.id = vClaim;   

    delete from claim_trace ct where ct.claim = vClaim;  
    
  end;
  
  
-----------------------------
  procedure p_Close_Claim (vClaim number, vStatusDetail number) is
    vLoadType varchar2(10);
  begin
    select cl.load_type into vLoadType from claims cl where cl.id = vClaim;
    update claims cl 
       set cl.status = (select id from abs_types a where a.meta_cathegory = 'CLAIM_STATUS' and a.acronym = 'impossible'),
           cl.status_details = vStatusDetail,
           cl.success = 0,
           cl.end_date = trunc(sysdate)
     where cl.id = vClaim;  
     
    if vLoadType = 'auto' then
      pac_claim_order.SetOrderStatus(vClaim);
    end if; 
  
  end;    
  
-----------------------------
 -- создание нового наряда из заявки -- только для услуг телефонии   
  procedure p_Create_New_Workorder(vClaim number, vWorder_Code varchar2, /*vResponsible number,*/ vComments varchar2) is
    vStatusNew      number;
    vActivityType   varchar2(50);
    vCrossDir       number;

  begin
    select id into vStatusNew from abs_types a 
     where upper(a.meta_cathegory) = 'WORKORDER_STATUS' and a.acronym = 'new';
     
    select a.acronym into vActivityType from claims cl join abs_types a on cl.activity_type = a.id where cl.id = vClaim;
    
    if vActivityType in ('add_phone', 'add_service', 'replace_number', 'replace_service', 'connect') then
      
      select id into vCrossDir from abs_types a 
          where upper(a.meta_cathegory) = 'CROSS_DIRECTION' and a.acronym = 'cross';
   
       insert into workorder(code, organization, status, claim, date_begin, comments, cross_type, cross_direction, phone_num, service)
       select vWorder_Code, cl.organization_id, vStatusNew, cl.id, sysdate, vComments, 0, vCrossDir, 
              decode(vActivityType, 'connect', cl.num_old, cl.num_new), cl.service_new
        --      f_find_new_service_for_claim(vClaim)
         from v_claim cl where cl.id = vClaim;   
    end if;     
      
    if vActivityType in ('remove_phone', 'replace_number', 'replace_service', 'disconnect') then
      select id into vCrossDir from abs_types a 
            where upper(a.meta_cathegory) = 'CROSS_DIRECTION' and a.acronym = 'uncross';  

       insert into workorder(code, organization, status, claim, date_begin, comments, cross_type, cross_direction, phone_num, service)
       select vWorder_Code, cl.organization_id, vStatusNew, cl.id, sysdate, vComments, 0, vCrossDir, 
              cl.num_old, cl.service_old --f_find_old_service_for_claim(vClaim)
         from v_claim cl where cl.id = vClaim;             
            
    end if;  
    
    update claims cl
       set cl.status = (select id from abs_types a where upper(a.meta_cathegory) = 'CLAIM_STATUS' and a.acronym = 'task')
     where cl.id = vClaim;       
  end;
  
--------------------------------------------  
  procedure p_Close_Workorder(vWorkorder number) is
     vCrossType     number;
     vCrossDir      varchar2(20);
     vClaim         number;
     vService       number;
     vLoadType      varchar2(10);
     vActivityType  varchar2(50);
     vid            number;
  begin
      
      select count(1) into vid
        from task t 
       where t.workorder = vWorkorder
         and t.status <> f_get_abstypes_id ('workorder_status', 'done');
      
      if vid <> 0 then
        raise_application_error (-20005, 'Неможливо закрити наряд з невиконаними завданнями');
      end if;   
      
      select w.claim, w.cross_type, lower(a.acronym), w.load_type, act.acronym 
        into vClaim, vCrossType, vCrossDir, vLoadType, vActivityType
        from workorder w left join  
             claims cl on w.claim = cl.id left join  
             abs_types act on cl.activity_type = act.id join
             abs_types a on w.cross_direction = a.id where w.id = vWorkorder;
  
         -- изменяем статус сервиса
      update workorder w 
         set w.status = f_get_abstypes_id ('workorder_status', 'done'),
             w.status_details = f_get_abstypes_id ('claim_results', '0'),
             w.date_to = sysdate
       where w.id = vWorkorder;  
 
      select w.service into vService from workorder w where w.id = vWorkorder;    
     
      update service s 
             set s.status = decode(vCrossDir, 
                                   'cross', 
                                   f_get_abstypes_id ('service_status', 'enabled'), 
                                   f_get_abstypes_id ('service_status', 'reserved_old'))
           where s.id = vService;

         -- изменяем статус трассы        
      update claim_trace ct 
         set ct.status = case vCrossDir when 'cross' then f_get_abstypes_id ('trace_status', 'used')
                                        when 'uncross' then f_get_abstypes_id ('trace_status', 'reserved') end
       where ct.id = (select id from claim_trace ctt where ctt.claim = vClaim);   
      
      select count(1) into vid from workorder w 
       where w.status <> f_get_abstypes_id ('workorder_status', 'done')
         and w.claim = vClaim;
         
      if vid = 0 then
         update claims cl 
             set cl.status = f_get_abstypes_id ('claim_status', 'ready'),
                 cl.status_details = f_get_abstypes_id ('claim_results', '0'),
                 cl.end_date = trunc(sysdate)
           where cl.id = vClaim; 
           
        if vLoadType = 'auto' then
          if vCrossType = 0 then
            pac_claim_order.SetAssignmentCompleted(vWorkOrder);
          end if;
          if vCrossType = 1 and vCrossDir = 'cross' then
            pac_claim_order.CrossingResponceSend(vWorkorder, '0');
          end if;
        end if;   
                 
      end if;   
     
  end;
  
--------------------------------------------  
  procedure p_Cancel_Workorder(vWorkorder number, vResult number, vComment varchar2) is
     vCrossType     number;
     vCrossDir      varchar2(20);
     vClaim         number;
     vLoadType      varchar2(10);
  begin
     
     delete from task t where t.workorder = vWorkorder;
     
     update workorder w 
        set w.status = f_get_abstypes_id ('workorder_status', 'impossible'),
            w.status_details = vResult,
            w.date_to = sysdate,
            w.comments = vComment
      where w.id = vWorkorder;  
   
     select w.claim, w.cross_type, lower(a.acronym), w.load_type
       into vClaim, vCrossType, vCrossDir, vLoadType
       from workorder w left join  
            claims cl on w.claim = cl.id left join  
            abs_types act on cl.activity_type = act.id join
            abs_types a on w.cross_direction = a.id where w.id = vWorkorder;

     if vLoadType = 'auto' and vCrossType = 1 and vCrossDir = 'cross' then
        pac_claim_order.CrossingResponceSend(vWorkorder, '1');
     end if;
     
  end;  
  
-----------------------------------------------------------------  
  procedure p_Create_Connection (vTask number) is
    vWorder        number;
    vStatus        varchar2(50);
    vCrossType     number;
    vPhoneNum      varchar2(20);
  begin
    select a.acronym, t.workorder, w.cross_type, w.phone_num into vStatus, vWorder, vCrossType, vPhoneNum
      from task t join 
           abs_types a on t.status = a.id join
           workorder w on t.workorder = w.id where t.id = vTask;
    if vStatus <> 'active' then
      raise_application_error (-20002, 'Завдання не активне');
    end if; 
     
    update task t set t.status = f_get_abstypes_id ('workorder_status', 'done'), t.date_to = sysdate
     where t.id = vTask;  

    update task t set t.status = f_get_abstypes_id ('workorder_status', 'active' )
     where t.id = vTask+1;   
  end;

----------------------------------------------------------------
  procedure p_Delete_Connection (vTask number) is
    vWorder        number;
    vStatus        varchar2(50);
    vCrossType     number;
    vPhoneNum      varchar2(20);
    vOrg           number;
  begin

    select a.acronym, t.workorder, w.cross_type, w.phone_num, t.organization 
      into vStatus, vWorder, vCrossType, vPhoneNum, vOrg
      from task t join 
           abs_types a on t.status = a.id join
           workorder w on t.workorder = w.id where t.id = vTask;
    if vStatus <> 'active' then
      raise_application_error (-20002, 'Завдання не активне');
    end if; 
    
    update task t set t.status = 
      (select id from abs_types a where a.meta_cathegory = 'WORKORDER_STATUS' and a.acronym = 'done' ),
      t.date_to = sysdate
     where t.id = vTask;  

    update task t set t.status = 
      (select id from abs_types a where a.meta_cathegory = 'WORKORDER_STATUS' and a.acronym = 'active' )
     where t.id = vTask+1;   
      
      -- для услуг ADSL создаются удаляются записи в service, contact_service (для услуг телефонии они создавались на этапе бронирования линии)
      if vCrossType = 1 then
        delete from service s 
         where s.phone_number_prefix||s.phone_number = vPhoneNum 
           and s.type_of_service = f_get_ServType('DialupDSL');
      end if;         


  end;
  
    

-----------------------------
  procedure p_Replace_Number(vClaim number) is
    vOrg         number;
    vNumOld      number;
    vNumNew      number;
    vZoneNew     number;
    vServiceOld  number;
    vServiceNew  number;
  begin
    select cl.organization, cl.num_old, cl.num_new, cl.zone_new
      into vOrg, vNumOld, vNumNew, vZoneNew
      from claims cl where cl.id = vClaim;
      
    select id into vServiceOld
      from service s where s.organization = vOrg and s.code = vNumOld;  
    
    vServiceNew := seqid.nextval;
    insert into service(code, subscriber, organization, type_of_service, contact_cnt_start, contact_cnt_end,
                        contact_start, contact_end, zonecode, full_number)
      select vNumNew, s.subscriber, s.organization, s.type_of_service, s.contact_cnt_start, s.contact_cnt_end,
                       s.contact_start, s.contact_end, vZoneNew, (select code from zone_codes zc where zc.id = vZoneNew)||vNumNew
        from service s where id = vServiceOld;
      
    for c in (select cs.id, cs.contact_cnt, cs.contact, cs.sort_order from contact_service cs where cs.service = vServiceOld)
      loop
        delete from contact_service t where t.id = c.id;
        insert into contact_service(service, contact_cnt, contact, organization, sort_order)
            values (vServiceNew, c.contact_cnt, c.contact, vOrg, c.sort_order);
      end loop;
      
    for c in (select cs.id, cs.connector, cs.sort_order, cs.reverse from connector_service cs where cs.service = vServiceOld)
      loop
        delete from connector_service t where t.id = c.id;
        insert into connector_service(service, connector, organization, sort_order, reverse)
            values (vServiceNew, c.connector, vOrg, c.sort_order, c.reverse);
      end loop;
      
  end;  


----- построение трасс для безномерных линий с обязательным заходом на АТС 
   -- тестовый пример - vclaim = 170724; db1 = 38511871; db2 = 38511853
  procedure p_Check_NS_Availability22 (vClaim number, vDistrBox1 number, vDistrBox2 number, vsearch_depth number) is
    vOrg          number;
    vDB_contact1  number;
    vRB1          number;
    vRBcontact1   number;
    vRSh1         number;
    vDB_contact2  number;
    vRB2          number;
    vRBcontact2   number;
    vRSh2         number;
    i             number;
    j             number;
    m             number;
    vcommon_obj   number;
    vobj_from     number;
    vobj_to       number;
    vcc_from      number;
    vcc_to        number;
    vTrassa       number;
    vCheck        number;
    
  begin
    
    delete from claim_trace_detail_gis ttg where ttg.claim = vClaim;
    delete from claim_trace_detail td where td.claim = vClaim;
    delete from claim_trace t where t.claim = vClaim;
    delete from temp_trace_sections t where t.claim = vClaim;
    
    p_put_trace22(vClaim, vDistrBox1, vsearch_depth, 1, 0, vDB_contact1, vRB1, vRBcontact1, vRSh1);
    p_put_trace22(vClaim, vDistrBox2, vsearch_depth, 2, 0, vDB_contact2, vRB2, vRBcontact2, vRSh2);
    update temp_trace_sections tts set tts.path = substr(tts.path, 2, length(tts.path)) where tts.claim = vClaim;   
    commit;   
      
    -- поиск общего кросса (АТС) в трассах РК - АТС
    select count(1) into vCheck
      from temp_trace_sections t1 join 
           temp_trace_sections t2 on t1.ate = t2.ate
     where t1.claim = vClaim and t2.claim = vClaim
       and t1.side = 1 and t2.side = 2 ;
       
    if vCheck = 0 then
      delete from temp_trace_sections tts where tts.side = 2 and tts.claim = vClaim;
      p_put_trace22(vClaim, vDistrBox2, vsearch_depth, 2, 1, vDB_contact2, vRB2, vRBcontact2, vRSh2);
      update temp_trace_sections tts set tts.path = substr(tts.path, 2, length(tts.path)) where tts.claim = vClaim and tts.side = 2;   
      commit;
    end if;   
       
    --  поиск совпадающих объектов в 2 трассах  --------------------
    i := 0;
    
    for c in (select * from 
                     (select t1.id as id1, t1.path as path1, t2.id as id2, t2.path as path2, 
                             t1.ate, t1.level_num as level1, t2.level_num as level2
                        from temp_trace_sections t1 join
                             temp_trace_sections t2 on t1.ate = t2.ate
                       where t1.claim = vClaim and t1.side = 1 and instr(t1.path, vRsh1)=0
                         and t2.claim = vClaim and t2.side = 2 and instr(t1.path, vRsh2)=0
                        order by t1.level_num + t2.level_num)
                where rownum =1)
       loop
         -- запись 1-й части трассы
         SAVEPOINT complete_trace;
         i:= i+1;
         j:= 1;                
         vTrassa := seqid.nextval;
         insert into claim_trace(id, code, depth, distr_box, ats, claim, status, new)
              values (vTrassa, 'Траса '||to_char(i), c.level1 + c.level2, vDistrBox1, null, vClaim, 
                                (select id from abs_types a where a.acronym ='possible' and a.meta_cathegory = 'TRACE_STATUS'), 1);
         insert into claim_trace_detail(id, claim, claim_trace, contact_cnt_to, obj_from, contact_cnt_from, sort_order, new)
              values (seqid.nextval, vClaim, vTrassa, vDistrBox1, vRSh1, vRb1, j, 1);
        
         vobj_from := vRsh1;        
         select to_number(substr(t.path, 1, instr(t.path, '/', 1)-1)) into vobj_to
           from temp_trace_sections t where t.id = c.id1;   
           
         while vobj_from <> c.ate  
           loop
             j:=j+1;
             select t.contact_cnt_from_id, t.contact_cnt_to_id
               into vcc_from, vcc_to
               from (select t.contact_cnt_from_id, t.contact_cnt_to_id
                       from v_cable_segment_obj t where t.object_from_id = vobj_from and t.object_to_id = vobj_to
                   union --all
                      select t.contact_cnt_to_id, t.contact_cnt_from_id
                        from v_cable_segment_obj t where t.object_from_id = vobj_to and t.object_to_id = vobj_from) t
              where pac_route.f_count_free_contacts(t.contact_cnt_from_id,vOrg) > 0
                and pac_route.f_count_free_contacts(t.contact_cnt_to_id,vOrg)> 0  
                and rownum = 1;   
                         
             insert into claim_trace_detail (id, claim, obj_to, contact_cnt_to, obj_from, contact_cnt_from, claim_trace, sort_order, new)
                    values (seqid.nextval, vClaim, vobj_from, vcc_from, vobj_to, vcc_to, vTrassa, j, 1);
                                                                             
             vobj_from := vobj_to;
             select substr(t.path, decode(j, 1, 0, instr(t.path, '/', 1, j-1)) + 1, instr(t.path, '/', j)-1)
               into vobj_to
               from temp_trace_sections t where t.claim = vClaim and t.id = c.id1; 
               
             -- проверка на "петли" в трассе       
             select count(1) into vCheck from claim_trace_detail td 
              where td.claim = vClaim and td.claim_trace = vTrassa and td.new = 1 and (td.obj_from = vobj_to or td.obj_to = vobj_to); 
            
             if vCheck > 0 then
               ROLLBACK TO complete_trace;
               i:=i-1;
               vobj_to := null;
             end if; 
           end loop;
     
         -- запись 2-й части трассы
         m := c.level1 + c.level2 + 3; 
         j := 1;    
         vobj_to := vRsh2;
         select to_number(substr(t.path, 1, instr(t.path, '/', 1)-1)) into vobj_from
           from temp_trace_sections t where t.id = c.id2;     

         while vobj_to <> c.ate  
           loop
             j:=j+1;
             select t.contact_cnt_from_id, t.contact_cnt_to_id
               into vcc_from, vcc_to
               from (select t.contact_cnt_from_id, t.contact_cnt_to_id
                       from v_cable_segment_obj t where t.object_from_id = vobj_from and t.object_to_id = vobj_to
                  union --all
                     select t.contact_cnt_to_id, t.contact_cnt_from_id
                      from v_cable_segment_obj t where t.object_from_id = vobj_to and t.object_to_id = vobj_from) t
              where pac_route.f_count_free_contacts(t.contact_cnt_from_id,vOrg) > 0
                and pac_route.f_count_free_contacts(t.contact_cnt_to_id,vOrg)> 0  
                and rownum = 1;        
             
             insert into claim_trace_detail (id, claim, obj_to, contact_cnt_to, obj_from, contact_cnt_from, claim_trace, sort_order, new)
                      values (seqid.nextval, vClaim, vobj_from, vcc_from, vobj_to, vcc_to, vTrassa, m-j, 1);
                        
             vobj_to := vobj_from;
             select substr(t.path, decode(j, 1, 0, instr(t.path, '/', 1, j-1)) + 1, instr(t.path, '/', j)-1)
              into vobj_from
              from temp_trace_sections t where t.claim = vClaim and t.id = c.id2; 
                      
            -- проверка на "петли" в трассе                             
             select count(1) into vCheck from claim_trace_detail td 
              where td.claim = vClaim and td.claim_trace = vTrassa and (td.obj_from = vobj_from or td.obj_to = vobj_from)
                and vobj_from <> vcommon_obj; 
                                     
             if vCheck > 0 then
               ROLLBACK TO complete_trace;
                vobj_from := null;
             end if; 
           
           end loop;             
  
        insert into claim_trace_detail(id, claim, claim_trace, obj_to, contact_cnt_to, contact_cnt_from, sort_order, new)
        values (seqid.nextval, vClaim, vTrassa, vRSh2, vRb2, vDistrBox2, m-1, 1);                   
                    
       end loop;   

     select count(1) into i
      from claim_trace ct where ct.claim = vClaim;
    
     if i > 0 then
       update claims cl 
          set cl.status = f_get_abstypes_id('CLAIM_STATUS', 'possible') where cl.id = vClaim;
     else
       update claims cl 
          set cl.status = f_get_abstypes_id('CLAIM_STATUS', 'impossible') where cl.id = vClaim;        
     end if;       
              
   end;  


-------------------------------------------------------------  
  procedure p_put_trace22 (vClaim in number,
                         vDistrBox in number, 
                         vdepth in number,
                         vside in number,
                         vATS_between in number,
                         vDBcontact out number, 
                         vRB out number, 
                         vRBcontact out number, 
                         vRSh out number) is
      vOrg    number;   
    begin
      select c.organization into vOrg from distr_box c where c.id = vDistrBox;
      
      begin
        select c.id into vDBcontact
          from contact_cnt cc join
               contact c on cc.model = c.contact_cnt_model left join
               contact_service cs on c.id = cs.contact and cc.id = cs.contact_cnt 
         where cc.id = vDistrBox and cs.id is null and rownum = 1;
      exception      
         when no_data_found then 
              raise_application_error (-20001, 'Не знайдено вільний контакт');   
      end; 

    begin
      select ct.contact_cnt_to, ct.contact_to, b.cabinet_id
        into vRB, vRBcontact, vRSh
        from connector ct join
             cable_segment cs on ct.cable_segment = cs.id join
             box b on ct.contact_cnt_to = b.id  
       where ct.contact_cnt_from = vDistrBox and ct.contact_from = vDBcontact;
    exception
       when no_data_found then 
         select ct.contact_cnt_from, ct.contact_from, b.cabinet_id
          into vRB, vRBcontact, vRSh
          from connector ct join
               cable_segment cs on ct.cable_segment = cs.id join
               box b on ct.contact_cnt_from = b.id 
         where ct.contact_cnt_to = vDistrBox and ct.contact_to = vDBcontact;
    end;
    
      ----------  запись возможных трасс во временную таблицу  -------------------------------    
    if vATS_between = 0 then  
        insert into temp_trace_sections (path, level_num, claim, side, ate)
        select distinct SYS_CONNECT_BY_PATH(tt.pid, '/')||'/', level, vClaim, vside, tt.objid
               from (select cs.id as cabsegm, c.id, oc.id as pid, oc.object_type, oc.id as objid
                       from cabinet c join
                            cable_segment cs on c.id = cs.object_cnt_to join
                            object_cnt oc on cs.object_cnt_from = oc.id
                      where cs.organization = vOrg      
                     union 
                     select cs.id as cabsegm, c.id, oc.id as pid, oc.object_type, oc.id as objid
                       from cabinet c join
                            cable_segment cs on c.id = cs.object_cnt_from join
                            object_cnt oc on cs.object_cnt_to = oc.id 
                      where cs.organization = vOrg) tt
              where level <= nvl(vdepth,3) and tt.object_type = 2
              start with tt.id = vRsh
              connect by nocycle prior tt.pid = tt.id;
    else
        insert into temp_trace_sections (path, level_num, claim, side, ate)
        select distinct SYS_CONNECT_BY_PATH(tt.pid, '/')||'/', level, vClaim, vside, tt.objid
               from (select cs.id as cabsegm, c.id, oc.id as pid, oc.object_type, oc.id as objid
                       from cabinet c join
                            cable_segment cs on c.id = cs.object_cnt_to join
                            object_cnt oc on cs.object_cnt_from = oc.id
                      where cs.organization = vOrg      
                     union 
                     select cs.id as cabsegm, c.id, oc.id as pid, oc.object_type, oc.id as objid
                       from cabinet c join
                            cable_segment cs on c.id = cs.object_cnt_from join
                            object_cnt oc on cs.object_cnt_to = oc.id 
                      where cs.organization = vOrg
                     union 
                     select cs.id as cabsegm, c.id, oc.id as pid, 2, oc.id as objid
                       from cross c join
                            cable_segment cs on c.id = cs.object_cnt_from join
                            cross oc on cs.object_cnt_to = oc.id 
                      where cs.organization = vOrg) tt
              where level <= nvl(vdepth,3) and tt.object_type = 2
              start with tt.id = vRsh
              connect by nocycle prior tt.pid = tt.id;      
    end if;
              
  end;

  procedure p_Connect_Dslam(vLC number, vLC_contact number, vNum number, vServiceADSL out number) is
    vOrg           number;
    vid            number;
    vService       number;
    vPhoneStrafe   number;
    vPhoneContact  number;
    vSite          number;
    vIPport        number;
    vContact_cnt   number;
    vContact       number;
    vConnector     number;
    i              number :=555;
    vOrgRegion     number;
  --  vServiceADSL   number;
  begin
    
    select lc.organization, lc.site, a.region_id into vOrg, vSite, vOrgRegion
      from logic_card lc join
           abs_organization a on lc.organization = a.id where lc.id = vLC;
    select count(1) into vid
      from contact_service sc where sc.contact_cnt = vLC and sc.contact = vLC_contact;
    if vid > 0 then
      raise_application_error (-20001, 'Порт вже зайнятий іншою послугою');
    end if;   
    
   
 ---******************* модификация услуги телефонии ****************** --------------
    -- будем считать что приходит dsl порт
    begin
        select tt.id, tt.contact_cnt, tt.contact into vService, vPhoneStrafe, vPhoneContact from
               (select s.id, cs.contact_cnt, cs.contact 
                  from service s join 
                       contact_service cs on s.id = cs.service join
                       strafe s on cs.contact_cnt = s.id
                 where s.type_of_service = f_get_ServType('Indiv')
                   and s.full_number = to_char(vNum) order by cs.sort_order desc) tt where rownum = 1;
    exception
      when no_data_found then  raise_application_error (-20001, 'Не знайдено громполосу для траси телефонії');              
    end;
    
    -- ищем ip-порт
    select ct.id into vIPport
      from contact ct join 
           contact_cnt cc on ct.contact_cnt_model = cc.model
     where cc.id = vLC and ct.contact_type = 'ip';
     
    for c in (select tt.*
              from (select c.id, c.contact_cnt_from as contact_cnt1, c.contact_from as contact1, 
                           c.contact_cnt_to as contact_cnt2, c.contact_to as contact2, c.organization, 0 as reverse
                      from connector c join
                           contact_cnt cc on c.contact_cnt_from = cc.id
                     where c.organization = vOrg and cc.site = vSite and c.contact_from <> vIPport and c.contact_to <> vIPport
                     union all
                    select c.id, c.contact_cnt_to as contact_cnt1, c.contact_to as contact1, 
                           c.contact_cnt_from as contact_cnt2, c.contact_from as contact2, c.organization, 1
                      from connector c join
                           contact_cnt cc on c.contact_cnt_to = cc.id 
                     where c.organization = vOrg and cc.site = vSite and c.contact_from <> vIPport and c.contact_to <> vIPport
                   ) tt 
       where tt.contact_cnt2 in (select id from contact_cnt cct where cct.organization = vOrg and cct.site = vSite )  
      connect by tt.contact_cnt1 = prior tt.contact_cnt2 and tt.contact1 = prior tt.contact2 
                 and prior tt.id!=tt.id and tt.organization = prior tt.organization 
       start with tt.contact_cnt1 = vLC and tt.contact1 = vLC_contact         ) 
     
     loop
        select count(1) into vid 
          from contact_service sc where sc.contact_cnt = c.contact_cnt1 and sc.contact = c.contact1 and sc.service = vService and sc.organization = vOrg;
        if vid = 0 then
          insert into contact_service (contact_cnt, contact, service, organization, sort_order)
                 values (c.contact_cnt1, c.contact1, vService, vOrg, i);
          i := i+1;       
        end if;         
        
        select count(1) into vid 
          from contact_service sc where sc.contact_cnt = c.contact_cnt2 and sc.contact = c.contact2 and sc.service = vService and sc.organization = vOrg;
        if vid = 0 then
          insert into contact_service (contact_cnt, contact, service, organization, sort_order)
                 values (c.contact_cnt2, c.contact2, vService, vOrg, i);
          i := i+1;       
        end if;
                
        select count(1) into vid 
          from connector_service sc where sc.connector = c.id and sc.service = vService and sc.organization = vOrg;
        if vid = 0 then
          insert into connector_service (connector, service, organization, sort_order, reverse)
                 values (c.id, vService, vOrg, i, c.reverse);
        end if;         
     end loop;      
     
     -- соединение с ГП
    begin
      select c.contact_cnt_to, c.contact_to into vContact_cnt, vContact
        from connector_service cs join
             connector c on cs.connector = c.id join
             contact c1 on c.contact_from = c1.id 
       where cs.service =  vService and c.contact_cnt_from <> c.contact_cnt_to 
         and c1.contact_type = decode(vOrgRegion, 26, 'dsl', 'combo');
    exception
      when no_data_found then 
        select c.contact_cnt_from, c.contact_from into vContact_cnt, vContact
          from connector_service cs join
               connector c on cs.connector = c.id join
               contact c1 on c.contact_to = c1.id 
         where cs.service =  vService and c.contact_cnt_from <> c.contact_cnt_to 
           and c1.contact_type = decode(vOrgRegion, 26, 'dsl', 'combo');      
    end;   
       
    insert into connector(contact_cnt_from, contact_from, contact_cnt_to, contact_to, organization, type)
        values (vPhoneStrafe, vPhoneContact, vContact_cnt, vContact, vOrg,
                (select id from abs_types a where UPPER(a.meta_cathegory) = 'CONNECTOR_TYPE' and a.code = 'single'))
        returning id into vConnector;
        
    insert into connector_service (connector, service, organization, sort_order)
                 values (vConnector, vService, vOrg, i);
           

    -- точка питания : Phone
    begin
      select c.contact_cnt_to, c.contact_to into vContact_cnt, vContact
        from connector_service cs join
             connector c on cs.connector = c.id join
             contact c1 on c.contact_from = c1.id 
       where cs.service =  vService and c.contact_cnt_from <> c.contact_cnt_to 
         and c1.contact_type = 'phone';
    exception
      when no_data_found then 
        select c.contact_cnt_from, c.contact_from into vContact_cnt, vContact
          from connector_service cs join
               connector c on cs.connector = c.id join
               contact c1 on c.contact_to = c1.id 
         where cs.service =  vService and c.contact_cnt_from <> c.contact_cnt_to 
           and c1.contact_type = 'phone';      
    end;   
       
    update service s set s.contact_cnt_end = vContact_cnt, s.contact_end = vContact
     where s.id = vservice;
        
      -- sort_order
    
    select cs.connector into vid from connector_service cs where cs.service = vService and cs.sort_order = 2;  
      for kk in (select tt.id, 
                        tt.reverse,
                        tt.contact_cnt1,
                        tt.contact1,
                        to_number(rPAD('2', rownum, '1')) as sort_order
                   from (select c.id, c.contact_cnt_from as contact_cnt1, c.contact_from as contact1, 0 as reverse,
                                       c.contact_cnt_to as contact_cnt2, c.contact_to as contact2, c.organization, cs.sort_order,
                                       cc.object_type
                                  from connector_service cs join 
                                       connector c on cs.connector = c.id join
                                       contact_cnt cc on c.contact_cnt_from = cc.id
                                 where c.organization = vOrg and cs.service = vService
                                 union all
                         select c.id, c.contact_cnt_to as contact_cnt1, c.contact_to as contact1, 1,
                                       c.contact_cnt_from as contact_cnt2, c.contact_from as contact2, c.organization, cs.sort_order,
                                       cc.object_type
                                  from connector_service cs join 
                                       connector c on cs.connector = c.id join
                                       contact_cnt cc on c.contact_cnt_to = cc.id 
                                 where c.organization = vOrg and cs.service = vService
                               ) tt 
                    connect by tt.contact_cnt1 = prior tt.contact_cnt2 and tt.contact1 = prior tt.contact2 
                             and prior tt.id!=tt.id and tt.organization = prior tt.organization 
                      start with tt.id = vid and tt.object_type = 7)
          loop
            update connector_service cs set cs.reverse = kk.reverse, cs.sort_order = kk.sort_order 
              where cs.connector = kk.id and cs.service = vService;
            update contact_service cs set cs.sort_order = kk.sort_order 
              where cs.contact_cnt = kk.contact_cnt1 and cs.contact = kk.contact1 and cs.service = vService;
          end loop;  
          
    ---******************* создание услуги ADSL ****************** --------------
   vServiceADSL := seqid.nextval;  
   insert into service (id, code, subscriber, organization, phone_number, type_of_service,
                         contact_cnt_start, contact_start, zonecode, trace, full_number,
                         status)
       select vServiceADSL, s.code, s.subscriber, s.organization, s.phone_number, f_get_ServType('DialupDSL'),
                     s.contact_cnt_start, s.contact_start, s.zonecode, s.trace, s.full_number,
                     (select id from abs_types ab where ab.meta_cathegory = 'SERVICE_STATUS' and ab.acronym = 'reserved' )
                from service s where s.id = vService;
   
   update service s set s.contact_cnt_end = vLC, s.contact_end = vLC_contact
    where s.id = vServiceADSL;
    
   insert into contact_service (service, contact_cnt, contact, organization, sort_order)
     select vServiceADSL, cs.contact_cnt, cs.contact, cs.organization, cs.sort_order
       from contact_service cs where cs.service = vService; 
                    
   insert into connector_service (connector, service, organization, sort_order, reverse)
     select cs.connector, vServiceADSL, cs.organization, cs.sort_order, cs.reverse
       from connector_service cs where cs.service = vService;        
         
   end;
   
   
   procedure p_Disconnect_Dslam(vService number) is
     vOrg           number;
     vServCode      varchar2(10);
     vPhoneService  number;
     vConnector     number;
     vSortOrder     number;
     vCC_end        number;
     vC_end         number;
   begin
     select s.code, s.organization into vServCode, vOrg
       from service s where s.id = vService;
       
    -- delete from service s where s.id = vService;
     select s.id into vPhoneService
       from service s 
      where s.code = vServCode and s.type_of_service = f_get_ServType('Indiv');
       
     select tt.id, tt.sort_order into vConnector, vSortOrder
       from (select c.id, cs.sort_order
                    from connector c join
                         connector_service cs on c.id = cs.connector join
                         strafe cc on c.contact_cnt_from = cc.id
                   where cs.service = vPhoneService and c.cable_segment is null
                  union
                  select c.id, cs.sort_order
                    from connector c join
                         connector_service cs on c.id = cs.connector join
                         strafe cc on c.contact_cnt_to = cc.id     
                   where cs.service = vPhoneService and c.cable_segment is null) tt;
                   
    for c in (select c.contact_cnt_from, c.contact_from, c.contact_cnt_to, c.contact_to
                  from connector_service cs join
                       connector c on cs.connector = c.id
                where cs.service = vPhoneService and cs.sort_order > vSortOrder)
      loop
        delete from contact_service m 
         where m.service = vPhoneService and m.contact_cnt = c.contact_cnt_from and m.contact = c.contact_from;
        
        delete from contact_service m 
         where m.service = vPhoneService and m.contact_cnt = c.contact_cnt_to and m.contact = c.contact_to;
      end loop;
     commit;
      delete from connector_service cc where cc.service = vPhoneService and cc.sort_order >= vSortOrder;
      delete from connector c where c.id = vConnector;
     
    
      select t.contact_cnt, t.contact into vCC_end, vC_end from contact_service t where t.service = vPhoneService
        and t.sort_order = (select max(sort_order) from contact_service tt where tt.service = vPhoneService);
      
      update service s 
         set s.contact_cnt_end = vCC_end, s.contact_end = vC_end
       where s.id = vPhoneService;
                   
   end;


--------------------------------------------------------------    
  
    /*
  function f_trace_by_claim(vclaim NUMBER) RETURN t_traces IS
      vresult   t_traces;
      vservice  number;
    begin
       select s.id into vservice
         from service s join 
              claims cl on (s.phone_number = cl.num_old and nvl(s.subscriber, 0) = nvl(cl.subscriber, 0))
        where cl.id = vClaim;
              
       select t_trace(c.id, 
                      cs1.service,
                      vClaim,
                      c.contact_cnt_from, 
                      oc1.code||' : '||cc1.code,
                      c.contact_from , 
                      c1.code,
                      c.contact_cnt_to,
                      oc2.code||' : '||cc2.code,
                      c2.id,
                      c2.code)
                bulk collect
                into vresult            
           from connector c join
                contact_service cs1 on c.contact_cnt_from = cs1.contact_cnt and c.contact_from = cs1.contact join
                contact_service cs2 on c.contact_cnt_to = cs2.contact_cnt and c.contact_to = cs2.contact join
                contact_cnt cc1 on c.contact_cnt_from = cc1.id join
                contact_cnt cc2 on c.contact_cnt_to = cc2.id join
                contact c1 on c.contact_from = c1.id join
                contact c2 on c.contact_to = c2.id left join
                v_object_cnt oc1 on cc1.object_cnt = oc1.id left join
                v_object_cnt oc2 on cc2.object_cnt = oc2.id
          where (cs1.service = vService and cs2.service = vService);
    
        RETURN vresult;
    end;         
    */
----------------------------------------------------------------
  function f_trace_by_claim(vclaim NUMBER) RETURN t_traces IS
      vresult   t_traces;
      vservice  number;
      vOrg      number;
    begin
           
       select s.id, s.organization into vservice, vOrg
         from service s join 
              claims cl on (s.phone_number = cl.num_old and s.zonecode = cl.zone_old )
        where s.type_of_service in (f_get_ServType('Indiv'), f_get_ServType('Coupled'))
          and (s.status = (select a.id from abs_types a where a.meta_cathegory = 'SERVICE_STATUS' and a.acronym = 'enabled')
               or s.status is null)       
          and cl.id = vClaim ;
              
       select t_trace(cs.id, 
                      cs.organization,
                      c.id,
                      c.cable_segment,
                      cs.service,
                      vClaim,
                      cc1.id, 
                      cc2.id, 
                      oc1.id, 
                      oc2.id,
                      c1.id,
                      c2.id,
                      nvl2(s1.code,'АТС '||s1.code||': ','')||nvl2(oc1.code,decode(oc1.object_type,2,'Кросс ',3,'РШ ',14,'DSLAM '),'')||oc1.code,
                      --decode(cc1.contact_cnt_type,4,'ГП ',6,'Бокс ',7,'РК ',8,'БУ ',11,'РА ',12,'Плата DSLAM ',13,'УК '),
                      cct1.acronym,
											cc1.code,
                      nvl(c1.title,c1.code), 
                      nvl2(s2.code,'АТС '||s2.code||': ','')||nvl2(oc2.code,decode(oc2.object_type,2,'Кросс ',3,'РШ ',14,'DSLAM '),'')||oc2.code,
                      --decode(cc2.object_type,4,'ГП ',6,'Бокс ',7,'РК ',8,'БУ ',11,'РА ',12,'Плата DSLAM ',13,'УК '),
                      cct2.acronym,
											cc2.code,
                      nvl(c2.title,c2.code),
                      null,
                      cs.sort_order )
                bulk collect
                into vresult            
           from connector_service cs join
                connector c on cs.connector = c.id join
                contact_cnt cc1 on decode(cs.reverse,0,c.contact_cnt_from,c.contact_cnt_to) = cc1.id and cc1.organization = vOrg join
                contact_cnt cc2 on decode(cs.reverse,0,c.contact_cnt_to,c.contact_cnt_from) = cc2.id and cc2.organization = vOrg join
                abs_types cct1 on cc1.object_type = cct1.id join
								abs_types cct2 on cc2.object_type = cct2.id left join
								object_cnt  oc1 on cc1.object_cnt     = oc1.id and oc1.organization = vOrg left join
                object_cnt  oc2 on cc2.object_cnt     = oc2.id and oc2.organization = vOrg left join
                site s1 on oc1.site = s1.id left join
                site s2 on oc2.site = s2.id join
                contact c1 on decode(cs.reverse,0,c.contact_from,c.contact_to) = c1.id join
                contact c2 on decode(cs.reverse,0,c.contact_to,c.contact_from) = c2.id
          where cs.service = vservice
            and cs.organization = vOrg 
          order by cs.sort_order;
          /*  and cs.service in (select s.id from service s join 
                       claims cl on (s.phone_number = cl.num_old and nvl(s.subscriber, 0) = nvl(cl.subscriber, 0))
                      where cl.id = vClaim)*/
            
        RETURN vresult;
 end;  
 
 ------------------------------------------------------------
  function f_trace_by_service(vservice NUMBER) RETURN t_traces IS
      vresult   t_traces;
      vOrg      number;
    begin
      
      select s.organization into vOrg from service s where s.id = vservice;
             
      select t_trace(cs.id, 
                      cs.organization,
                      c.id,
                      c.cable_segment,
                      cs.service,
                      null,
                      oc1.id, 
                      cc1.id, 
                      c1.id,
                      oc2.id, 
                      cc2.id,
                      c2.id,
                      nvl2(s1.code,'АТС '||s1.code||': ','')||nvl2(oc1.code,decode(oc1.object_type,2,'Кросс ',3,'РШ ',14,'DSLAM '),'')||oc1.code,
                      --decode(cc1.contact_cnt_type,4,'ГП ',6,'Бокс ',7,'РК ',8,'БУ ',11,'РА ',12,'Плата DSLAM ',13,'УК '),
                      cct1.acronym,
											cc1.code,
                      nvl(c1.title,c1.code), 
                      nvl2(s2.code,'АТС '||s2.code||': ','')||nvl2(oc2.code,decode(oc2.object_type,2,'Кросс ',3,'РШ ',14,'DSLAM '),'')||oc2.code,
                      --decode(cc2.object_type,4,'ГП ',6,'Бокс ',7,'РК ',8,'БУ ',11,'РА ',12,'Плата DSLAM ',13,'УК '),
                      cct2.acronym,
											cc2.code,
                      nvl(c2.title,c2.code),
                      null,
                      cs.sort_order )
                bulk collect
                into vresult            
           from connector_service cs join
                connector c on cs.connector = c.id join
                contact_cnt cc1 on decode(cs.reverse,0,c.contact_cnt_from,c.contact_cnt_to) = cc1.id and cc1.organization = vOrg join
                contact_cnt cc2 on decode(cs.reverse,0,c.contact_cnt_to,c.contact_cnt_from) = cc2.id and cc2.organization = vOrg join
                abs_types cct1 on cc1.object_type = cct1.id join
								abs_types cct2 on cc2.object_type = cct2.id left join
								v_object_cnt  oc1 on cc1.object_cnt     = oc1.id and oc1.organization = vOrg left join
                v_object_cnt  oc2 on cc2.object_cnt     = oc2.id and oc2.organization = vOrg left join
                site s1 on oc1.site = s1.id left join
                site s2 on oc2.site = s2.id join
                contact c1 on decode(cs.reverse,0,c.contact_from,c.contact_to) = c1.id join
                contact c2 on decode(cs.reverse,0,c.contact_to,c.contact_from) = c2.id
          where cs.service = vservice
            and cs.organization = vOrg 
          order by cs.sort_order;
					            
        RETURN vresult;
 end;       
      

  function f_get_abstypes_id(vmeta_cathegory varchar2, vacronym varchar2) return number is
    vresult number;
  begin
    select id into vresult 
      from abs_types at where upper(at.meta_cathegory)=upper(vmeta_cathegory)
                          and upper(at.acronym) = upper(vacronym);
    return vresult;                      
  end;



-------------------------------------------------------------  
  procedure p_put_trace (vClaim number,
                           vOrg   number,
                           vRSh   number,
                           vdepth number,
                           vside  number,
                           vAts_between number) is
  begin

    
      ----------  запись возможных трасс во временную таблицу  -------------------------------    
    if vATS_between = 0 then  
        insert into temp_trace_sections (path, level_num, claim, side, ate)
        select distinct SYS_CONNECT_BY_PATH(tt.pid, '/')||'/', level, vClaim, vside, tt.objid
               from (select cs.id as cabsegm, c.id, oc.id as pid, oc.object_type, oc.id as objid
                       from cabinet c join
                            cable_segment cs on c.id = cs.object_cnt_to join
                            object_cnt oc on cs.object_cnt_from = oc.id
                      where cs.organization = vOrg      
                     union 
                     select cs.id as cabsegm, c.id, oc.id as pid, oc.object_type, oc.id as objid
                       from cabinet c join
                            cable_segment cs on c.id = cs.object_cnt_from join
                            object_cnt oc on cs.object_cnt_to = oc.id 
                      where cs.organization = vOrg) tt
              where level <= nvl(vdepth,3) and tt.object_type = 2
              start with tt.id = vRsh
              connect by nocycle prior tt.pid = tt.id;
    else
        insert into temp_trace_sections (path, level_num, claim, side, ate)
        select distinct SYS_CONNECT_BY_PATH(tt.pid, '/')||'/', level, vClaim, vside, tt.objid
               from (select cs.id as cabsegm, c.id, oc.id as pid, oc.object_type, oc.id as objid
                       from cabinet c join
                            cable_segment cs on c.id = cs.object_cnt_to join
                            object_cnt oc on cs.object_cnt_from = oc.id
                      where cs.organization = vOrg      
                     union 
                     select cs.id as cabsegm, c.id, oc.id as pid, oc.object_type, oc.id as objid
                       from cabinet c join
                            cable_segment cs on c.id = cs.object_cnt_from join
                            object_cnt oc on cs.object_cnt_to = oc.id 
                      where cs.organization = vOrg
                     union 
                     select cs.id as cabsegm, c.id, oc.id as pid, 2, oc.id as objid
                       from cross c join
                            cable_segment cs on c.id = cs.object_cnt_from join
                            cross oc on cs.object_cnt_to = oc.id 
                      where cs.organization = vOrg) tt
              where level <= nvl(vdepth,3) and tt.object_type = 2
              start with tt.id = vRsh
              connect by nocycle prior tt.pid = tt.id;      
    end if;
              
  end;

---------------------------------------------------------------------------
 /* procedure bulk_reconnection (vid number) is 
    vOrg              number;
    vFileContent      varchar2(5000);
    vFileName         varchar2(500);
    vNumber           varchar2(10);
    vService          number;
    vCrossDirection   varchar2(10);
  begin
    
    select to_char(b.Loadfile_Bfile), b.Loadfile_Link, b.organization into vFileContent, vFileName, vOrg
      from bulkload b where b.id = vid;
      
    if instr(vFileName, '_5160_') > 0 then
      vCrossDirection := 'cross';
    elsif instr(vFileName, '_5161_') > 0 then
      vCrossDirection := 'uncross';
    else 
      raise_application_error(-20003, 'Некоректний формат назви файлу '||vNumber);  
    end if;    
      
    while length(vFileContent) > 0
      loop
        vNumber := substr(vFileContent, 1, instr(vFileContent, ';')-1);
        
        begin
          select s.id into vService
            from service s where s.organization = vOrg and s.code = vNumber;
          
          if vCrossDirection = 'uncross' then
            update service s 
               set s.status = (select w.id from abs_types w 
                                where w.meta_cathegory = 'SERVICE_STATUS' and w.acronym = 'reserved_old')
             where s.id = vService;
          elsif vCrossDirection = 'cross' then
            update service s 
               set s.status = null
             where s.id = vService;
          end if;
          
        exception
          when no_data_found then raise_application_error(-20001, 'Не знайдено сервіс '||vNumber);
          when too_many_rows then raise_application_error(-20002, 'Знайдено декілька сервісів '||vNumber);  
        end;    
        
        vFileContent := substr(vFileContent, instr(vFileContent, chr(10))+1, length(vFileContent));
      end loop;
       
  end;*/

  procedure p_generate_testorder(vOrg number, vActType number, vConnType number) is
    vClaim                number;
    vActType_acronym      varchar2(50);
    vConnType_acronym     varchar2(50);   
    vAdres1               number;
    vAdres2               number;    
    vZoneCode             number;
    vNum                  varchar2(15);   
    vService              number;
  begin
    select a.acronym into vActType_acronym from abs_types a where a.id = vActType;
    select a.askr_id into vConnType_acronym from type_of_service_askr a where a.id = vConnType;
  
    vClaim := seqid.nextval;
    insert into claims(id, code, organization, claim_date, activity_type, load_type)
           values (vClaim, 'test-'||to_char(vClaim), vOrg, trunc(sysdate), vActType, 'manual')
         returning id into vClaim;
         
         
    if vActType_acronym = 'add_service' then
        select kk.id into vAdres2
          from (select db.address_id as id 
                  from service_area sa join
                       (select * from distr_box d where d.organization = vOrg and rownum < 1000) db on sa.contact_cnt = db.id
                order by dbms_random.value ) kk where rownum = 1;

        update claims cl
           set cl.address_new = vAdres2, cl.connect_type_new = vConnType where cl.id = vClaim;        
           
        if vConnType_acronym = '3' then
          select t.zone_code, t.phone_number into vZoneCode, vNum
            from ate_capacity t 
           where t.organization = vOrg and t.service_type = 3 and t.status = 0
             and rownum = 1;
          
          update claims cl
             set cl.zone_new = vZoneCode, cl.num_new = vNum where cl.id = vClaim;  
        end if;

        if vConnType_acronym in ('20','21','22') then
          select kk.id into vAdres1 
            from (select db.address_id as id 
                    from service_area sa join
                         (select * from distr_box d where d.organization = vOrg and rownum < 1000) db on sa.contact_cnt = db.id
                  order by dbms_random.value ) kk where rownum = 1;  
          
          update claims cl
             set cl.address_old = vAdres1 where cl.id = vClaim;  
        end if;        
        
    elsif vActType_acronym = 'ns'  then

        select kk.id into vAdres1 
          from (select db.address_id as id 
                  from service_area sa join
                       (select * from distr_box d where d.organization = vOrg and rownum < 1000) db on sa.contact_cnt = db.id
                order by dbms_random.value ) kk where rownum = 1;    

        select kk.id into vAdres2
          from (select db.address_id as id 
                  from service_area sa join
                       (select * from distr_box d where d.organization = vOrg and rownum < 1000) db on sa.contact_cnt = db.id
                order by dbms_random.value ) kk where rownum = 1;
                              
        update claims cl
           set cl.address_old = vAdres1, cl.address_new = vAdres2, cl.connect_type_new = vConnType where cl.id = vClaim;
                 
    elsif vActType_acronym in ('remove_service', 'remove_phone', 'disconnect', 'connect') then
         select kk.id, kk.zonecode, kk.phone_number 
           into vService, vZoneCode, vNum
           from (select * 
                   from (select sr.* from service sr where sr.organization = vOrg and sr.type_of_service = f_get_ServType('Indiv') and rownum < 1000) s
                  where not exists (select id from service s1 where s.full_number = s1.full_number and s1.type_of_service <> f_get_ServType('Indiv')) 
                  order by dbms_random.value) kk
          where rownum = 1;
         
         begin
           select ct.address into vAdres1
             from service s join contact_cnt ct on s.contact_cnt_start = ct.id 
            where s.id = vService;
         exception when no_data_found then vAdres1 := null;
         end;   
         
         update claims cl
            set cl.zone_old = vZoneCode, cl.num_old = vNum, cl.address_old = vAdres1, cl.connect_type_old = vConnType
        where cl.id = vClaim;    
    
    elsif vActType_acronym in ('replace_service') then

        select kk.id, kk.zonecode, kk.phone_number 
           into vService, vZoneCode, vNum
           from (select * 
                   from (select sr.* from service sr where sr.organization = vOrg and sr.type_of_service = f_get_ServType('Indiv') and rownum < 1000) s
                  where not exists (select id from service s1 where s.full_number = s1.full_number and s1.type_of_service <> f_get_ServType('Indiv')) 
                  order by dbms_random.value) kk
          where rownum = 1;        
          
         begin
           select ct.address into vAdres1
             from service s join contact_cnt ct on s.contact_cnt_start = ct.id 
            where s.id = vService;
         exception when no_data_found then vAdres1 := null;
         end;        
 
        select kk.id into vAdres2
          from (select db.address_id as id 
                  from service_area sa join
                       (select * from distr_box d where d.organization = vOrg and rownum < 1000) db on sa.contact_cnt = db.id
                order by dbms_random.value ) kk where rownum = 1;
                         
        update claims cl
           set cl.address_old = vAdres1, cl.connect_type_old = vConnType, 
               cl.address_new = vAdres2, cl.connect_type_new = vConnType,
               cl.zone_old = vZoneCode, 
               cl.num_old = vNum,
               cl.zone_new = vZoneCode,
               cl.num_new = vNum
         where cl.id = vClaim; 
    
    elsif vActType_acronym in ('replace_number') then         
    
        select kk.id into vAdres2 
          from (select db.address_id as id 
                  from service_area sa join
                       (select * from distr_box d where d.organization = vOrg and rownum < 1000) db on sa.contact_cnt = db.id
                order by dbms_random.value ) kk where rownum = 1;
        
        select kk.id, kk.zonecode, kk.phone_number 
           into vService, vZoneCode, vNum
           from (select * 
                   from (select sr.* from service sr where sr.organization = vOrg and sr.type_of_service = f_get_ServType('Indiv') and rownum < 1000) s
                  where not exists (select id from service s1 where s.full_number = s1.full_number and s1.type_of_service <> f_get_ServType('Indiv')) 
                  order by dbms_random.value) kk
          where rownum = 1;        
                
        update claims cl
           set cl.address_old = vAdres2, cl.connect_type_old = vConnType, 
               cl.address_new = vAdres2, cl.connect_type_new = vConnType,
               cl.zone_old = vZoneCode, 
               cl.num_old = vNum
         where cl.id = vClaim;    
         
        select tt.zone_code, tt.phone_number into vZoneCode, vNum
         from ate_capacity tt 
         where tt.ate in (select ac.ate
                       from ate_capacity ac where ac.zone_code = vZoneCode and ac.phone_number = vNum and ac.organization = vOrg)
          and tt.organization = vOrg and tt.status = 0  
          and tt.service_type in (select csm.type_of_service from conntype_servtype_mapping csm where csm.connection_type = vConnType)
          and rownum = 1;       
        
        update claims cl 
           set cl.zone_new = vZoneCode, cl.num_new = vNum
         where cl.id = vClaim;            
    end if;   
  end;  
  
  procedure p_generate_testorder_dslam(vOrg number) is
    vid         number;                            
    vChassis    number;
    vCard       number;
    vContact    number;
    vPhoneNum   varchar2(20);
  begin
   
   vid := seqid.nextval;
   
   select lch.id, lc.id, ct.id into vChassis, vCard, vContact
     from logic_chassis lch join
          logic_card lc on lch.id = lc.chassis_logic join
          contact ct on ct.contact_cnt_model = lc.model left join
          contact_service cs on cs.contact_cnt = lc.id and cs.contact = ct.id
    where lch.organization = vOrg
      and ct.contact_type = 'dsl'
      and cs.service is null
      and rownum = 1;
      
   select kk.full_number into vPhoneNum
     from (select s.full_number
             from (select sr.* from service sr where sr.organization = 123 and sr.type_of_service = f_get_ServType('Indiv') and rownum < 100) s
            where not exists (select id from service s1 where s.full_number = s1.full_number and s1.type_of_service <> f_get_ServType('Indiv')) 
         order by dbms_random.value) kk
    where rownum = 1;   
    
   insert into workorder 
          (id, 
           code, 
           status, 
           date_begin, 
           organization, 
           cross_type, 
           phone_num, 
           logic_chassis, 
           logic_card, 
           contact, 
           cross_direction,
           id_us,
           load_type)
        values (vid,
                'test-'||vid,
               (select id from abs_types a where a.meta_cathegory = 'WORKORDER_STATUS' and a.acronym = 'new'),
                trunc(sysdate),
                vOrg,
                1,
                vPhoneNum,
                vChassis,
                vCard,
                vContact,
               (select id from abs_types a where a.meta_cathegory = 'CROSS_DIRECTION' and a.acronym = 'cross'),
                null,
                'manual') ;   

  end; 
  
  
  procedure p_build_adsl_trace (vWorkorder number) is
    vCrossType    number;
    vCrossDir     varchar2(10);  
    vLC           number;
    vLC_contact   number;                
    vNum          varchar2(20);
    vServiceADSL  number; 
  begin

     select w.cross_type, lower(a.acronym) into vCrossType, vCrossDir
        from workorder w join    
             abs_types a on w.cross_direction = a.id where w.id = vWorkorder;
             
     if vCrossType = 1 and vCrossDir = 'cross' then
       select w.logic_card, w.contact, w.phone_num
                 into vLC, vLC_contact, vNum
                 from workorder w where w.id = vWorkorder;                
      
       if vLC is null or vLC_contact is null or vNum is null then
                     raise_application_error (-20001, 'Не задані параметри підключення ADSL');
                 else 
                     p_Connect_Dslam (vLC, vLC_contact, vNum, vServiceADSL);
                     
                     update workorder wd set wd.service = vServiceADSL 
                      where wd.id = vWorkorder;
                 end if;
     else 
       raise_application_error(-20001, 'Невірні параметри: Тип кросування '||vCrossType||', Напрямок кросування '||vCrossDir);
     end if;          
  end;
  
  
  -------------------------------------------------  


  
/*  function f_find_new_service_for_claim (vClaim number) return number is
    vActType      varchar2(50);
    vService      number;
  begin
    select ab.acronym into vActType
      from claims cl join abs_types ab on cl.activity_type = ab.id
     where cl.id = vClaim;
     
    if vActType in ('add_service', 'ns', 'replace_number', 'replace_service') then
      select s.id into vService from service s where s.claim = vClaim;
    
    elsif vActType in ('remove_phone', 'disconnect') then
      vService := null;
      
    elsif vActType in ('connect') then
      select s.id into vservice
        from service s join 
             claims cl on (s.phone_number = cl.num_old and s.zonecode = cl.zone_old )
       where s.type_of_service in (f_get_ServType('Indiv'), f_get_ServType('Coupled'))
         and s.status in (select a.id from abs_types a 
                           where a.meta_cathegory = 'SERVICE_STATUS' and a.acronym in ('disabled', 'reserved_old'))
         and cl.id = vClaim ; 
    end if; 
    
    return vService;
  end;
  
  function f_find_old_service_for_claim (vClaim number) return number is
    vActType      varchar2(50);
    vService      number;
  begin
    select ab.acronym into vActType
      from claims cl join abs_types ab on cl.activity_type = ab.id
     where cl.id = vClaim;
     
    if    vActType in ('add_service', 'ns') then
      vService := null;
      
    elsif vActType in ('replace_number', 'replace_service', 'remove_phone', 'disconnect') then
      select s.id into vservice
        from service s join 
             claims cl on (s.phone_number = cl.num_old and s.zonecode = cl.zone_old )
       where s.type_of_service in (f_get_ServType('Indiv'), f_get_ServType('Coupled'))
         and (s.status = (select a.id from abs_types a where a.meta_cathegory = 'SERVICE_STATUS' and a.acronym = 'enabled')
              or s.status is null)
         and cl.id = vClaim ;
    
    elsif vActType in ('connect') then
      vService := null;     
    end if; 
    
    return vService;
  end;  
    
*/

begin
  null;
end PAC_CLAIM;
/
