*          DATA SET RELDH7     AT LEVEL 048 AS OF 05/01/02                      
*PHASE RELDH7                                                                   
*INCLUDE PRNTBL                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
         TITLE 'RELDH7 - CONVERT MINDSHARE OFFICES'                             
* PARAMETER LIST                                                                
*                                                                               
* P1=A(RECORD)  PASS FIRST BYTE X'00'= INITIALISE                               
*                               X'01'= RECORD IN CORE                           
*                               X'FF'= END OF FILE                              
*               RETURN VALUE    X'00'= KEEP RECORD                              
*                               X'FF'= PURGE RECORD                             
*                               X'FF'/C'EOJ'=PURGE & CAUSE EOJ                  
* P2=A(TAPEOUT) PASS FIRST BYTE X'80'= TAPE INPUT                               
*                               X'40'= TAPE OUTPUT                              
*                               X'20'= RECORD IS I/S FILE RECORD                
* P3=A(PARAM CARD)                                                              
* P4=A(FILE DEFN)                                                               
* P5=A(PRINTER)                                                                 
* P6=A(CPRINT)                                                                  
*                                                                               
*******************************************************************             
*                                                                 *             
*******************************************************************             
*                                                                               
         PRINT NOGEN                                                            
DMLDEXT  CSECT                                                                  
         NMOD1 WORKX-WORKD,DMLDEXT,RR=R5                                        
         USING WORKD,RC                                                         
         EJECT                                                                  
*******************************************************************             
* CONTROL FLOW LOGIC                                              *             
*******************************************************************             
*                                                                               
DMXCTL   ST    R1,APARM            SAVE PARAMETERS                              
         MVC   PLIST,0(R1)                                                      
         L     RA,VCPRINT          SET UP FOR PRINTING                          
         USING DPRINT,RA                                                        
         SPACE 1                                                                
         L     RE,=V(PRNTBL)                                                    
         AR    RE,R5                                                            
         ST    RE,PRNTBL                                                        
*                                                                               
         SPACE 2                                                                
         CLI   PLIST,X'00'                                                      
         BE    DMXINIT             INITIALISE                                   
         SPACE 1                                                                
         CLI   PLIST,X'01'                                                      
         BE    DMXREC              PROCESS                                      
         SPACE 1                                                                
         CLI   PLIST,X'FF'                                                      
         BE    DMXEOF              END-OF-FILE                                  
         SPACE 1                                                                
         B     DMXIT                                                            
         SPACE 2                                                                
DMXKEEP  L     R1,APARM            KEEP RECORD EXIT                             
         MVI   0(R1),0                                                          
         B     DMXIT                                                            
         SPACE 2                                                                
DMXPURGE L     R1,APARM            PURGE RECORD EXIT                            
         MVI   0(R1),X'FF'                                                      
         AP    PURGE,=P'1'                                                      
         B     DMXIT                                                            
         SPACE 2                                                                
DMXPGEOF L     R1,APARM            PURGE AND CAUSE INPUT EOF EXIT               
         MVI   0(R1),X'FF'                                                      
         MVC   1(3,R1),=C'EOF'                                                  
         B     DMXIT                                                            
         SPACE 2                                                                
DMXIT    XMOD1 1                                                                
         EJECT                                                                  
*******************************************************************             
* INITIALISE LOGIC                                                *             
*******************************************************************             
         SPACE 2                                                                
DMXINIT  DS    0H                                                               
         B     DMXIT                                                            
         EJECT                                                                  
*******************************************************************             
* PROCESS RECORD LOGIC                                            *             
*******************************************************************             
         SPACE 2                                                                
DMXREC   DS    0H                                                               
         L     R5,AREC             POINT TO RECORD                              
         CLI   0(R5),X'41'         DARE                                         
         BE    DMXR300                                                          
         CLI   0(R5),X'51'         CONFIRMED DARE                               
         BE    DMXR300                                                          
         B     DMXKEEP                                                          
*                                                                               
DMXR300  DS    0H                                                               
         USING RDARREC,R5                                                       
         LA    R3,REPTABLE                                                      
DMXR310  DS    0H                                                               
         CLC   =C'H7 ',RDARKAGY                                                 
         BNE   DMXKEEP                                                          
*                                                                               
         CLC   RDARKREP,0(R3)                                                   
         BNE   DMXR320                                                          
*                                                                               
         CLC   RDARKORD,8(R3)                                                   
         BNE   DMXR320                                                          
*                                                                               
         MVC   P(9),=C'BEFORE:  '                                               
         MVC   P+12(50),0(R5)                                                   
         GOTO1 VPRINTER                                                         
*                                                                               
         MVC   RDARKAOF,2(R3)                                                   
*                                                                               
         CLC   =C'DV',2(R3)          OFFICE =? DV                               
         BNE   *+10                  NO - BRANCH                                
         MVC   RDARKAOF,=C'DN'       YES - CHANGE TO 'DN'                       
*                                                                               
         CLC   =C'MA',2(R3)          OFFICE =? MA                               
         BNE   *+10                  NO - BRANCH                                
         MVC   RDARKAOF,=C'MI'       YES - CHANGE TO 'MI'                       
*                                                                               
         CLI   RDARKRT,X'10'       AGENCY HEADER ONLY                           
         BNE   DMXR318                                                          
*                                                                               
         CLC   =C'MS',RDARSNDR                                                  
         BE    DMXR315                                                          
         MVC   P(9),=C'*** ROUTING CODE MISMATCH ***'                           
         GOTO1 VPRINTER                                                         
         MVC   P(27),0(R5)                                                      
         GOTO1 VPRINTER                                                         
*                                                                               
DMXR315  DS    0H                                                               
         MVC   RDARSNDR+2(8),SPACES                                             
         MVC   RDARSNDR+2(2),2(R3)                                              
         AP    DAREHCHG,=P'1'                                                   
*                                                                               
DMXR318  DS    0H                                                               
         MVC   P(9),=C'   AFTER:'                                               
         MVC   P+12(50),0(R5)                                                   
         GOTO1 VPRINTER                                                         
*                                                                               
         AP    DARECHG,=P'1'                                                    
         B     DMXKEEP                                                          
*                                                                               
DMXR320  DS    0H                                                               
         AHI   R3,L'REPTABLE                                                    
         CLI   0(R3),X'FF'                                                      
         BNE   DMXR310                                                          
         B     DMXKEEP             DONE                                         
         DROP  R5                                                               
         EJECT                                                                  
*******************************************************************             
* END-OF-FILE LOGIC                                               *             
*******************************************************************             
DMXEOF   DS    0H                                                               
         BAS   RE,DMCNT                                                         
         B     DMXIT               OUTPUT COUNTS                                
         EJECT                                                                  
*******************************************************************             
*              END OF FILE                                        *             
*******************************************************************             
         SPACE 1                                                                
         USING RECD,R3                                                          
DMCNT    NTR1                                                                   
         GOTO1 VPRINTER                                                         
         GOTO1 VPRINTER                                                         
         SPACE 1                                                                
         MVC   P+3(6),=C'PURGED'                                                
         EDIT  (P5,PURGE),(7,P+11)                                              
         GOTO1 VPRINTER                                                         
         MVC   P+3(7),=C'CHANGED'                                               
         EDIT  (P5,CHANGE),(7,P+12)                                             
         GOTO1 VPRINTER                                                         
         MVC   P+3(8),=C'CONTRACT'                                              
         EDIT  (P5,CONCHG),(7,P+12)                                             
         GOTO1 VPRINTER                                                         
         MVC   P+3(8),=C'MAKEGOOD'                                              
         EDIT  (P5,MKGCHG),(7,P+12)                                             
         GOTO1 VPRINTER                                                         
         MVC   P+3(8),=C'PROPOSAL'                                              
         EDIT  (P5,PROCHG),(7,P+12)                                             
         GOTO1 VPRINTER                                                         
         MVC   P+3(9),=C'DARE HEAD'                                             
         EDIT  (P5,DAREHCHG),(7,P+12)                                           
         GOTO1 VPRINTER                                                         
         MVC   P+3(4),=C'DARE ALL'                                              
         EDIT  (P5,DARECHG),(7,P+12)                                            
         GOTO1 VPRINTER                                                         
         B     DMXIT                                                            
         EJECT                                                                  
         GETEL R5,34,ELCODE                                                     
         SPACE 1                                                                
PURGE    DC    PL5'0'                                                           
CHANGE   DC    PL5'0'                                                           
CONCHG   DC    PL5'0'                                                           
MKGCHG   DC    PL5'0'                                                           
PROCHG   DC    PL5'0'                                                           
DAREHCHG DC    PL5'0'                                                           
DARECHG  DC    PL5'0'                                                           
SAVEKEY  DC    CL27' '                                                          
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
REPTABLE DS    0CL12                                                            
**                                                                              
**               REP   OFF   CONTRACT    DARE ORDER                             
**                                                                              
         DC    C'AM',C'SF',X'06922316',X'03560105'                              
         DC    C'AM',C'SF',X'06923578',X'03560106'                              
         DC    C'PV',C'SF',X'00000000',X'10020007'                              
         DC    C'PV',C'SF',X'03066913',X'03570012'                              
         DC    C'SZ',C'SF',X'06940352',X'03550233'                              
         DC    C'SZ',C'SF',X'06945265',X'03430017'                              
         DC    C'SZ',C'SF',X'06945268',X'03430018'                              
         DC    C'SZ',C'SF',X'06953863',X'03550236'                              
         DC    C'SZ',C'SF',X'06958456',X'03570011'                              
         DC    C'SZ',C'SF',X'06958851',X'10020005'                              
         DC    C'UT',C'SF',X'00137236',X'03430006'                              
         DC    C'UT',C'SF',X'00137237',X'03430009'                              
*&&DO                                                                           
         DC    C'BL',C'CH',X'05090672',X'03500072'                              
         DC    C'BL',C'CH',X'05100466',X'03500071'                              
         DC    C'CQ',C'CH',X'06922110',X'03480129'                              
         DC    C'CQ',C'CH',X'06933386',X'03570080'                              
         DC    C'PV',C'CH',X'03055265',X'03500078'                              
         DC    C'PV',C'CH',X'03055793',X'03500124'                              
         DC    C'PV',C'CH',X'03063136',X'03500125'                              
         DC    C'PV',C'CH',X'03063226',X'03500077'                              
         DC    C'PV',C'CH',X'03066134',X'03560134'                              
         DC    C'PV',C'CH',X'03066135',X'03560136'                              
         DC    C'SZ',C'CH',X'06939496',X'03500079'                              
         DC    C'SZ',C'CH',X'06939499',X'03500069'                              
         DC    C'SZ',C'CH',X'06939511',X'03550242'                              
         DC    C'SZ',C'CH',X'06939965',X'03550243'                              
         DC    C'SZ',C'CH',X'06949787',X'03500080'                              
         DC    C'SZ',C'CH',X'06949804',X'03500070'                              
         DC    C'SZ',C'CH',X'06950691',X'03480134'                              
         DC    C'SZ',C'CH',X'06950794',X'03500073'                              
         DC    C'SZ',C'CH',X'06950800',X'03500076'                              
         DC    C'SZ',C'CH',X'06957236',X'03560158'                              
         DC    C'SZ',C'CH',X'06957251',X'03560159'                              
         DC    C'SZ',C'CH',X'06957744',X'03570069'                              
         DC    C'UT',C'CH',X'00138743',X'03570023'                              
         DC    C'UT',C'CH',X'00138744',X'03570024'                              
*&&                                                                             
         DC    C'BL',C'DA',X'05104853',X'03570033'                              
         DC    C'BL',C'DA',X'05104855',X'03570034'                              
         DC    C'FN',C'DA',X'02804988',X'03480105'                              
         DC    C'FN',C'DA',X'02807283',X'03560146'                              
         DC    C'FN',C'DA',X'02807285',X'03560147'                              
         DC    C'FN',C'DA',X'02807506',X'03570067'                              
         DC    C'CQ',C'DA',X'00000000',X'03570032'                              
         DC    C'CQ',C'DA',X'06923014',X'03570035'                              
         DC    C'CQ',C'DA',X'06933718',X'03570036'                              
         DC    C'SZ',C'DA',X'06940673',X'03490080'                              
         DC    C'SZ',C'DA',X'06949926',X'03490058'                              
         DC    C'SZ',C'DA',X'06957179',X'03560144'                              
         DC    C'SZ',C'DA',X'06957183',X'03560145'                              
         DC    C'SZ',C'DA',X'06939608',X'03470060'                              
         DC    C'SZ',C'DA',X'06940071',X'03550109'                              
         DC    C'SZ',C'DA',X'06940835',X'10020016'                              
         DC    C'SZ',C'DA',X'06940854',X'10020017'                              
         DC    C'SZ',C'DA',X'06950634',X'03470061'                              
         DC    C'SZ',C'DA',X'06953972',X'03550111'                              
         DC    C'SZ',C'DA',X'06955545',X'03560029'                              
         DC    C'SZ',C'DA',X'06955550',X'03560030'                              
         DC    C'SZ',C'DA',X'06955951',X'03570018'                              
         DC    C'SZ',C'DA',X'06955958',X'03570017'                              
         DC    C'SZ',C'DA',X'06956253',X'03570016'                              
         DC    C'SZ',C'DA',X'06957971',X'03570001'                              
         DC    C'SZ',C'DA',X'06959067',X'10020020'                              
         DC    C'SZ',C'DA',X'06959320',X'10020019'                              
**                                                                              
         DC    C'BL',C'DV',X'05100387',X'03480136'                              
         DC    C'BL',C'DV',X'05105071',X'03570073'                              
         DC    C'FN',C'DV',X'02800924',X'03480137'                              
         DC    C'FN',C'DV',X'02808980',X'03570075'                              
         DC    C'PV',C'DV',X'03065224',X'03480138'                              
         DC    C'PV',C'DV',X'03068180',X'03570074'                              
         DC    C'SZ',C'DV',X'06939877',X'03550239'                              
         DC    C'SZ',C'DV',X'06941096',X'03480142'                              
         DC    C'SZ',C'DV',X'06954281',X'03550241'                              
         DC    C'SZ',C'DV',X'06957416',X'03570081'                              
**                                                                              
         DC    C'BL',C'DE',X'05105479',X'03570122'                              
         DC    C'BL',C'DE',X'05105483',X'03570123'                              
         DC    C'BL',C'DE',X'05105499',X'03570115'                              
         DC    C'BL',C'DE',X'05105516',X'03570118'                              
         DC    C'CQ',C'DE',X'06930114',X'03490325'                              
         DC    C'CQ',C'DE',X'06932014',X'03560008'                              
         DC    C'CQ',C'DE',X'06932024',X'03560005'                              
         DC    C'CQ',C'DE',X'06932584',X'03560073'                              
         DC    C'CQ',C'DE',X'06932649',X'03560044'                              
         DC    C'CQ',C'DE',X'06932657',X'03560054'                              
         DC    C'CQ',C'DE',X'06932670',X'03560045'                              
         DC    C'CQ',C'DE',X'06932676',X'03560055'                              
         DC    C'NB',C'DE',X'00088811',X'03560031'                              
         DC    C'NB',C'DE',X'00088813',X'03560032'                              
         DC    C'PV',C'DE',X'00000000',X'03430073'                              
         DC    C'PV',C'DE',X'00000000',X'03430074'                              
         DC    C'PV',C'DE',X'03063072',X'03500013'                              
         DC    C'PV',C'DE',X'03063078',X'03500012'                              
         DC    C'PV',C'DE',X'03063080',X'03500010'                              
         DC    C'PV',C'DE',X'03063095',X'03500011'                              
         DC    C'PV',C'DE',X'03066831',X'03560058'                              
         DC    C'PV',C'DE',X'03066837',X'03560059'                              
         DC    C'SZ',C'DE',X'06939640',X'03560042'                              
         DC    C'SZ',C'DE',X'06942260',X'03470053'                              
         DC    C'SZ',C'DE',X'06944102',X'03560056'                              
         DC    C'SZ',C'DE',X'06953470',X'03550041'                              
         DC    C'SZ',C'DE',X'06954916',X'03560057'                              
         DC    C'SZ',C'DE',X'06955369',X'03560043'                              
         DC    C'SZ',C'DE',X'06955377',X'03560060'                              
         DC    C'SZ',C'DE',X'06955378',X'03560062'                              
         DC    C'SZ',C'DE',X'06955985',X'03560009'                              
         DC    C'SZ',C'DE',X'06955993',X'03560006'                              
         DC    C'SZ',C'DE',X'06956319',X'03560052'                              
         DC    C'SZ',C'DE',X'06956660',X'03560053'                              
         DC    C'SZ',C'DE',X'06957926',X'03570124'                              
         DC    C'SZ',C'DE',X'06957949',X'03570125'                              
**                                                                              
         DC    C'BL',C'LA',X'05098583',X'03570028'                              
         DC    C'BL',C'LA',X'05100406',X'03480140'                              
         DC    C'BL',C'LA',X'05106862',X'03570072'                              
         DC    C'BL',C'LA',X'05107996',X'03570029'                              
         DC    C'FN',C'LA',X'02800954',X'03490140'                              
         DC    C'FN',C'LA',X'02804853',X'03490141'                              
         DC    C'PV',C'LA',X'03066147',X'03570030'                              
         DC    C'PV',C'LA',X'03066148',X'03570031'                              
         DC    C'UT',C'LA',X'00138601',X'03550231'                              
         DC    C'UT',C'LA',X'00138602',X'03550232'                              
**                                                                              
         DC    C'BL',C'MA',X'05104902',X'03550086'                              
         DC    C'BL',C'MA',X'05104908',X'03550091'                              
         DC    C'AM',C'MA',X'06916075',X'03550083'                              
         DC    C'AM',C'MA',X'06921516',X'03550089'                              
         DC    C'FN',C'MA',X'02804632',X'03480182'                              
         DC    C'FN',C'MA',X'02804636',X'03480183'                              
         DC    C'AM',C'MA',X'06919285',X'03470033'                              
         DC    C'AM',C'MA',X'06919288',X'03470034'                              
         DC    C'NB',C'MA',X'00086146',X'03480167'                              
         DC    C'NB',C'MA',X'00087612',X'03480168'                              
         DC    C'SZ',C'MA',X'06940163',X'03550088'                              
         DC    C'SZ',C'MA',X'06940672',X'03430037'                              
         DC    C'SZ',C'MA',X'06945334',X'03430038'                              
         DC    C'SZ',C'MA',X'06945345',X'03430039'                              
         DC    C'SZ',C'MA',X'06945457',X'03430036'                              
         DC    C'SZ',C'MA',X'06947258',X'03470029'                              
         DC    C'SZ',C'MA',X'06947269',X'03470030'                              
         DC    C'SZ',C'MA',X'06953443',X'03550093'                              
*&&DO                                                                           
         DC    C'BL',C'NY',X'05098194',X'03460066'                              
         DC    C'BL',C'NY',X'05098203',X'03460067'                              
         DC    C'BL',C'NY',X'05103753',X'03560000'                              
         DC    C'BL',C'NY',X'05103765',X'03560003'                              
         DC    C'BL',C'NY',X'05107402',X'03550151'                              
         DC    C'BL',C'NY',X'05107409',X'03550137'                              
         DC    C'AM',C'NY',X'06919341',X'03470097'                              
         DC    C'AM',C'NY',X'06919342',X'03470102'                              
         DC    C'FN',C'NY',X'02800156',X'03480002'                              
         DC    C'FN',C'NY',X'02804468',X'03480158'                              
         DC    C'FN',C'NY',X'02805295',X'03480049'                              
         DC    C'FN',C'NY',X'02807062',X'03560076'                              
         DC    C'CQ',C'NY',X'06926794',X'03460064'                              
         DC    C'CQ',C'NY',X'06926795',X'03460065'                              
         DC    C'CQ',C'NY',X'06932571',X'03560001'                              
         DC    C'CQ',C'NY',X'06932573',X'03560002'                              
         DC    C'CQ',C'NY',X'06932881',X'03550265'                              
         DC    C'CQ',C'NY',X'06932883',X'03550266'                              
         DC    C'CQ',C'NY',X'06933108',X'03550098'                              
         DC    C'CQ',C'NY',X'06933115',X'03550100'                              
         DC    C'NB',C'NY',X'00086143',X'03480048'                              
         DC    C'NB',C'NY',X'00089087',X'03560075'                              
         DC    C'PV',C'NY',X'00000000',X'03550174'                              
         DC    C'PV',C'NY',X'03060585',X'03430042'                              
         DC    C'PV',C'NY',X'03060589',X'03430043'                              
         DC    C'PV',C'NY',X'03063985',X'03470062'                              
         DC    C'PV',C'NY',X'03064864',X'03550143'                              
         DC    C'PV',C'NY',X'03064869',X'03550146'                              
         DC    C'PV',C'NY',X'03064872',X'03550142'                              
         DC    C'PV',C'NY',X'03064874',X'03550148'                              
         DC    C'PV',C'NY',X'03069242',X'10040025'                              
         DC    C'SZ',C'NY',X'06939609',X'03550096'                              
         DC    C'SZ',C'NY',X'06939623',X'03560040'                              
         DC    C'SZ',C'NY',X'06939828',X'03460081'                              
         DC    C'SZ',C'NY',X'06940545',X'03550260'                              
         DC    C'SZ',C'NY',X'06944171',X'03570022'                              
         DC    C'SZ',C'NY',X'06944390',X'03570092'                              
         DC    C'SZ',C'NY',X'06947246',X'03460071'                              
         DC    C'SZ',C'NY',X'06947398',X'03460072'                              
         DC    C'SZ',C'NY',X'06947437',X'03460070'                              
         DC    C'SZ',C'NY',X'06947438',X'03460069'                              
         DC    C'SZ',C'NY',X'06947998',X'03480052'                              
         DC    C'SZ',C'NY',X'06948001',X'03470079'                              
         DC    C'SZ',C'NY',X'06948009',X'03470068'                              
         DC    C'SZ',C'NY',X'06948121',X'03480053'                              
         DC    C'SZ',C'NY',X'06953476',X'03460082'                              
         DC    C'SZ',C'NY',X'06953746',X'03550185'                              
         DC    C'SZ',C'NY',X'06953768',X'03550189'                              
         DC    C'SZ',C'NY',X'06953783',X'03550183'                              
         DC    C'SZ',C'NY',X'06953796',X'03550188'                              
         DC    C'SZ',C'NY',X'06954302',X'03550264'                              
         DC    C'SZ',C'NY',X'06954692',X'03550099'                              
         DC    C'SZ',C'NY',X'06956260',X'03560049'                              
         DC    C'SZ',C'NY',X'06956560',X'03560041'                              
         DC    C'SZ',C'NY',X'06956570',X'03560050'                              
         DC    C'SZ',C'NY',X'06957364',X'03570085'                              
         DC    C'SZ',C'NY',X'06958819',X'10020021'                              
         DC    C'UT',C'NY',X'00137775',X'03470106'                              
         DC    C'UT',C'NY',X'00137776',X'03470101'                              
*&&                                                                             
         DC    C'BL',C'AT',X'05094615',X'03480008'                              
         DC    C'BL',C'AT',X'05099167',X'03480009'                              
         DC    C'BL',C'AT',X'05100593',X'03500127'                              
         DC    C'BL',C'AT',X'05103442',X'03550166'                              
         DC    C'BL',C'AT',X'05103475',X'03550168'                              
         DC    C'BL',C'AT',X'05103739',X'03550108'                              
         DC    C'BL',C'AT',X'05104045',X'03550094'                              
         DC    C'BL',C'AT',X'05104928',X'03560157'                              
         DC    C'BL',C'AT',X'05105278',X'03560035'                              
         DC    C'BL',C'AT',X'05105305',X'03560117'                              
         DC    C'FN',C'AT',X'02802601',X'03430077'                              
         DC    C'FN',C'AT',X'02806182',X'03500129'                              
         DC    C'AM',C'AT',X'06916074',X'03470003'                              
         DC    C'AM',C'AT',X'06920944',X'03500128'                              
         DC    C'AM',C'AT',X'06922146',X'03560162'                              
         DC    C'AM',C'AT',X'06922148',X'03560163'                              
         DC    C'CQ',C'AT',X'06928309',X'03480161'                              
         DC    C'CQ',C'AT',X'06928310',X'03480162'                              
         DC    C'CQ',C'AT',X'06929702',X'03470005'                              
         DC    C'CQ',C'AT',X'06929706',X'03500135'                              
         DC    C'CQ',C'AT',X'06932585',X'03550135'                              
         DC    C'CQ',C'AT',X'06932589',X'03550154'                              
         DC    C'CQ',C'AT',X'06932887',X'03550164'                              
         DC    C'CQ',C'AT',X'06932892',X'03550171'                              
         DC    C'NB',C'AT',X'00086145',X'03470067'                              
         DC    C'NB',C'AT',X'00087476',X'03470077'                              
         DC    C'NB',C'AT',X'00088595',X'03550145'                              
         DC    C'NB',C'AT',X'00088650',X'03550156'                              
         DC    C'PV',C'AT',X'03065636',X'03550170'                              
         DC    C'PV',C'AT',X'03065648',X'03550165'                              
         DC    C'PV',C'AT',X'03065904',X'03560034'                              
         DC    C'PV',C'AT',X'03065925',X'03560110'                              
         DC    C'PV',C'AT',X'03067055',X'03550110'                              
         DC    C'PV',C'AT',X'03067056',X'03550116'                              
         DC    C'PV',C'AT',X'03069640',X'03470057'                              
         DC    C'PV',C'AT',X'03069645',X'03550175'                              
         DC    C'SZ',C'AT',X'06940401',X'03490293'                              
         DC    C'SZ',C'AT',X'06941989',X'03430078'                              
         DC    C'SZ',C'AT',X'06945447',X'03430034'                              
         DC    C'SZ',C'AT',X'06945452',X'03430035'                              
         DC    C'SZ',C'AT',X'06949470',X'03490292'                              
         DC    C'SZ',C'AT',X'06949696',X'03470059'                              
         DC    C'SZ',C'AT',X'06949837',X'03500130'                              
         DC    C'SZ',C'AT',X'06953718',X'03550144'                              
         DC    C'SZ',C'AT',X'06953720',X'03550155'                              
         DC    C'SZ',C'AT',X'06954287',X'03550177'                              
         DC    C'SZ',C'AT',X'06956272',X'03550152'                              
         DC    C'SZ',C'AT',X'06956276',X'03550159'                              
         DC    C'SZ',C'AT',X'06957123',X'03560160'                              
         DC    C'SZ',C'AT',X'06957128',X'03560161'                              
         DC    X'FF'                                                            
**                                                                              
***********************************************************************         
* DSECT TO COVER MODULE WORKING STORAGE                                         
*                                                                               
WORKD    DSECT                                                                  
DUB      DS    D                                                                
DMCB     DS    6F                                                               
APARM    DS    A                                                                
PLIST    DS    0CL24                                                            
AREC     DS    A                                                                
VTAPEOUT DS    A                                                                
APARAMC  DS    A                                                                
VLDDEFN  DS    A                                                                
VPRINTER DS    A                                                                
VCPRINT  DS    A                                                                
RECUP    DS    V                                                                
         SPACE 1                                                                
PRNTBL   DS    A                                                                
HALF     DS    H                                                                
ELCODE   DS    CL1                                                              
WORK     DS    CL64                                                             
MYWORK   DS    CL64                                                             
ELEMENT  DS    XL256                                                            
WORKX    EQU   *                                                                
         EJECT                                                                  
       ++INCLUDE DMLDDEFN                                                       
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
         PRINT OFF                                                              
       ++INCLUDE REGENALLD                                                      
       ++INCLUDE REGENMKG                                                       
       ++INCLUDE REGENPRO                                                       
       ++INCLUDE REGENDAR                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'048RELDH7    05/01/02'                                      
         END                                                                    
