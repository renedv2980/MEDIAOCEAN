*          DATA SET RELDDARE   AT LEVEL 008 AS OF 01/21/05                      
*          DATA SET RELDDARE   AT LEVEL 012 AS OF 01/10/05                      
*PHASE RELDDARA                                                                 
*INCLUDE PRNTBL                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE DATCON                                                                 
         TITLE 'RELDRMR - LOAD/DUMP MODEL EXTERNAL ROUTINE'                     
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
*  DARE RECORD PURGE                                              *             
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
*        AP    10(5,R5),=P'1'                                                   
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
         MVC   P(08),=C'STARTED!'                                               
         GOTO1 VPRINTER                                                         
         B     DMXIT                                                            
         EJECT                                                                  
*******************************************************************             
* PROCESS RECORD LOGIC                                            *             
*******************************************************************             
         SPACE 2                                                                
DMXREC   DS    0H                                                               
         L     R5,AREC             POINT TO RECORD                              
         USING RDARREC,R5                                                       
         CLI   0(R5),X'0C'                                                      
         BE    DMXREC50                                                         
         CLC   =X'4100',RDARKTYP                                                
         BE    DMXREC10                                                         
         CLC   =X'5100',RDARKTYP                                                
         BE    DMXREC10                                                         
         B     DMXKEEP                                                          
*                                                                               
DMXREC10 DS    0H                                                               
*        MVC   P,0(R5)                                                          
*        GOTO1 VPRINTER                                                         
         CLC   =C'RA',RDARKREP                                                  
         BNE   DMXKEEP                                                          
         CLI   RDARKRT,X'10'       HEADER ONLY                                  
         BNE   DMXKEEP                                                          
*        DROP  R5                                                               
*                                                                               
DMXREC11 DS    0H                                                               
         CLC   =C'IM AT',RDARKAGY                                               
         BNE   DMXKEEP                                                          
         CLC   RDARESEN,=X'D221'                                                
         BL    DMXKEEP                                                          
         MVC   RDARRCVR+3(2),=C'AT'                                             
*&&DO                                                                           
*                                                                               
*CHECK DARE                                                                     
*                                                                               
*        MVC   P,0(R5)                                                          
*        GOTO1 VPRINTER                                                         
         L     R5,AREC             POINT TO RECORD                              
         USING RDARREC,R5                                                       
*                                                                               
         TM    RDARCNTL,X'80'                                                   
         BO    DMXKEEP                                                          
*                                                                               
         MVI   ELCODE,X'50'                                                     
         BAS   RE,GETEL                                                         
         BNE   DMXKEEP                                                          
*                                                                               
DMXREC13 DS    0H                                                               
         USING RDARHSEM,R5                                                      
*                                                                               
         CLC   RDARHSER,=X'038F'   ERROR 911                                    
         BNE   DMXREC14                                                         
         DROP  R5                                                               
*                                                                               
         L     R4,AREC             POINT TO RECORD                              
         MVC   P(06),=C'ORDER:'                                                 
         MVC   P+10(34),0(R4)                                                   
         GOTO1 VPRINTER                                                         
*                                                                               
DMXREC14 DS    0H                                                               
         BAS   RE,NEXTEL                                                        
         BE    DMXREC13                                                         
         B     DMXKEEP                                                          
*                                                                               
DMXREC15 DS    0H                                                               
*&&                                                                             
         L     R5,AREC             POINT TO RECORD                              
*                                                                               
         MVC   P(06),=C'ORDER:'                                                 
*        MVC   P+10(80),0(R5)                                                   
         MVC   P+20(09),=C'CONTRACT:'                                           
         GOTO1 =V(HEXOUT),DMCB,RDARKORD,P+10,4                                  
                                                                                
         ZAP   WORK+17(5),=P'0'                                                 
         MVO   WORK+17(5),RDARREP#                                              
         EDIT  (P5,WORK+17),(8,P+30),ALIGN=LEFT                                 
*                                                                               
*        MVC   P+40(4),=C'STA:'                                                 
*        MVC   P+45(5),RDARKSTA                                                 
*                                                                               
*                                                                               
*        GOTO1 =V(HEXOUT),DMCB,RDARREP#,P+30,4                                  
         CLC   =X'5100',RDARKTYP                                                
         BNE   DMXREC20                                                         
         MVC   P+40(4),=C'CONF'                                                 
*                                                                               
DMXREC20 DS    0H                                                               
         GOTO1 =V(DATCON),DMCB,(2,RDARESST),(5,P+50)                            
         GOTO1 =V(DATCON),DMCB,(2,RDARESEN),(5,P+60)                            
*                                                                               
         GOTO1 VPRINTER                                                         
         B     DMXKEEP                                                          
*                                                                               
DMXREC50 DS    0H                                                               
         L     R5,AREC             POINT TO RECORD                              
         USING RCONREC,R5                                                       
         CLC   =C'RA',RCONKREP                                                  
         BNE   DMXKEEP                                                          
         LA    R6,CONTAB                                                        
DMXREC55 CLC   RCONKCON,0(R6)                                                   
         BE    DMXREC80                                                         
         AHI   R6,L'CONTAB                                                      
         CLI   0(R6),X'FF'                                                      
         BNE   DMXREC55                                                         
         B     DMXKEEP                                                          
*                                                                               
DMXREC80 DS    0H                                                               
         MVC   RCONKAGY(6),=C'INITAT'                                           
         MVC   RCONSAL,=C'JFB'                                                  
         MVC   P(4),=C'CON:'                                                    
         MVC   P+5(100),0(R5)                                                   
         GOTO1 VPRINTER                                                         
         B     DMXKEEP                                                          
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
         MVC   P+3(6),=C'DARE  '                                                
         EDIT  (P5,DARE),(7,P+11)                                               
         GOTO1 VPRINTER                                                         
         MVC   P+3(6),=C'MKGD'                                                  
         EDIT  (P5,MKGD),(7,P+11)                                               
         GOTO1 VPRINTER                                                         
         MVC   P+3(7),=C'CHANGED'                                               
         EDIT  (P5,CHANGE),(7,P+12)                                             
         GOTO1 VPRINTER                                                         
*                                                                               
         MVC   P+3(33),=C'DETAILS OF PURGED RECORDS FOLLOWS'                    
         GOTO1 VPRINTER                                                         
*                                                                               
         B     DMXIT                                                            
         EJECT                                                                  
         GETEL R5,34,ELCODE                                                     
         SPACE 1                                                                
PURGE    DC    PL5'0'                                                           
DARE     DC    PL5'0'                                                           
MKGD     DC    PL5'0'                                                           
CHANGE   DC    PL5'0'                                                           
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
* TABLE OF REP CODES TO PURGE                                                   
*                                                                               
PRGLST   DS    0CL2                                                             
         DC    C'YH'                                                            
         DC    X'FF'                                                            
*                                                                               
CONTAB   DS    0XL4                                                             
         DC    X'00014765'                                                      
         DC    X'00014766'                                                      
         DC    X'00014802'                                                      
         DC    X'00014803'                                                      
         DC    X'00015047'                                                      
         DC    X'00016594'                                                      
         DC    X'00016595'                                                      
         DC    X'00016596'                                                      
         DC    X'00016597'                                                      
         DC    X'00016598'                                                      
         DC    X'00016599'                                                      
         DC    X'00016600'                                                      
         DC    X'00016601'                                                      
         DC    X'00016602'                                                      
         DC    X'00016603'                                                      
         DC    X'00016603'                                                      
         DC    X'00016604'                                                      
         DC    X'00016605'                                                      
         DC    X'00016606'                                                      
         DC    X'00016607'                                                      
         DC    X'00016608'                                                      
         DC    X'00016609'                                                      
         DC    X'00016610'                                                      
         DC    X'00016611'                                                      
         DC    X'00016612'                                                      
         DC    X'00016613'                                                      
         DC    X'00016614'                                                      
         DC    X'00016615'                                                      
         DC    X'00016616'                                                      
         DC    X'00016617'                                                      
         DC    X'00016618'                                                      
         DC    X'00016619'                                                      
         DC    X'00016620'                                                      
         DC    X'00016621'                                                      
         DC    X'00016622'                                                      
         DC    X'00016623'                                                      
         DC    X'00016624'                                                      
         DC    X'00016625'                                                      
         DC    X'00016626'                                                      
         DC    X'00016627'                                                      
         DC    X'00016628'                                                      
         DC    X'00016629'                                                      
         DC    X'00016630'                                                      
         DC    X'00016631'                                                      
         DC    X'00016632'                                                      
         DC    X'00016633'                                                      
         DC    X'00016634'                                                      
         DC    X'00016635'                                                      
         DC    X'00016636'                                                      
         DC    X'00016637'                                                      
         DC    X'00016638'                                                      
         DC    X'00016639'                                                      
         DC    X'00016640'                                                      
         DC    X'00016641'                                                      
         DC    X'00016642'                                                      
         DC    X'00016643'                                                      
         DC    X'00016644'                                                      
         DC    X'00016645'                                                      
         DC    X'00016646'                                                      
         DC    X'00016647'                                                      
         DC    X'00016648'                                                      
         DC    X'00016649'                                                      
         DC    X'00016650'                                                      
         DC    X'00016651'                                                      
         DC    X'00016652'                                                      
         DC    X'00016653'                                                      
         DC    X'00016654'                                                      
         DC    X'00016661'                                                      
         DC    X'00016662'                                                      
         DC    X'00016663'                                                      
         DC    X'00016664'                                                      
         DC    X'00016665'                                                      
         DC    X'00016665'                                                      
         DC    X'00016666'                                                      
         DC    X'00016667'                                                      
         DC    X'00016668'                                                      
         DC    X'00016668'                                                      
         DC    X'00016669'                                                      
         DC    X'00016670'                                                      
         DC    X'00016671'                                                      
         DC    X'00016672'                                                      
         DC    X'00016672'                                                      
         DC    X'00016673'                                                      
         DC    X'00016674'                                                      
         DC    X'00016674'                                                      
         DC    X'00016675'                                                      
         DC    X'00016676'                                                      
         DC    X'00016677'                                                      
         DC    X'00016678'                                                      
         DC    X'00016679'                                                      
         DC    X'00016680'                                                      
         DC    X'00016680'                                                      
         DC    X'00016681'                                                      
         DC    X'00016682'                                                      
         DC    X'00016693'                                                      
         DC    X'00016694'                                                      
         DC    X'00016696'                                                      
         DC    X'00016697'                                                      
         DC    X'00016698'                                                      
         DC    X'00016699'                                                      
         DC    X'00016700'                                                      
         DC    X'00016701'                                                      
         DC    X'00016702'                                                      
         DC    X'00016703'                                                      
         DC    X'00016704'                                                      
         DC    X'00017373'                                                      
         DC    X'00017374'                                                      
         DC    X'00017375'                                                      
         DC    X'00017376'                                                      
         DC    X'00017377'                                                      
         DC    X'00017378'                                                      
         DC    X'00017379'                                                      
         DC    X'00017380'                                                      
         DC    X'00017381'                                                      
         DC    X'00017383'                                                      
         DC    X'00017384'                                                      
         DC    X'00017395'                                                      
         DC    X'00017396'                                                      
         DC    X'00017397'                                                      
         DC    X'00017398'                                                      
         DC    X'00017399'                                                      
         DC    X'00017400'                                                      
         DC    X'00017401'                                                      
         DC    X'00017565'                                                      
         DC    X'00017566'                                                      
         DC    X'00017567'                                                      
         DC    X'00017568'                                                      
         DC    X'00017569'                                                      
         DC    X'00017572'                                                      
         DC    X'00017573'                                                      
         DC    X'00017574'                                                      
         DC    X'00017845'                                                      
         DC    X'00017846'                                                      
         DC    X'00017877'                                                      
         DC    X'00017883'                                                      
         DC    X'00017884'                                                      
         DC    X'00017885'                                                      
         DC    X'00017886'                                                      
         DC    X'00017887'                                                      
         DC    X'00017888'                                                      
         DC    X'00017889'                                                      
         DC    X'00017890'                                                      
         DC    X'00017891'                                                      
         DC    X'00017892'                                                      
         DC    X'00017893'                                                      
         DC    X'00017894'                                                      
         DC    X'00017895'                                                      
         DC    X'00017896'                                                      
         DC    X'00017897'                                                      
         DC    X'00017898'                                                      
         DC    X'00017899'                                                      
         DC    X'00017900'                                                      
         DC    X'00017901'                                                      
         DC    X'00017902'                                                      
         DC    X'00017903'                                                      
         DC    X'00017904'                                                      
         DC    X'00017905'                                                      
         DC    X'00017957'                                                      
         DC    X'00017958'                                                      
         DC    X'00017959'                                                      
         DC    X'00017960'                                                      
         DC    X'00017961'                                                      
         DC    X'00017962'                                                      
         DC    X'00017963'                                                      
         DC    X'00017964'                                                      
         DC    X'00017965'                                                      
         DC    X'00017966'                                                      
         DC    X'00017967'                                                      
         DC    X'00017968'                                                      
         DC    X'00017969'                                                      
         DC    X'00017970'                                                      
         DC    X'00017971'                                                      
         DC    X'00017972'                                                      
         DC    X'00017973'                                                      
         DC    X'00017974'                                                      
         DC    X'00017975'                                                      
         DC    X'00017976'                                                      
         DC    X'00017977'                                                      
         DC    X'00017978'                                                      
         DC    X'00017990'                                                      
         DC    X'00017991'                                                      
         DC    X'00017992'                                                      
         DC    X'00017993'                                                      
         DC    X'00017994'                                                      
         DC    X'00018073'                                                      
         DC    X'00018074'                                                      
         DC    X'00018076'                                                      
         DC    X'00018161'                                                      
         DC    X'00018168'                                                      
         DC    X'00018169'                                                      
         DC    X'00018170'                                                      
         DC    X'00018171'                                                      
         DC    X'00018172'                                                      
         DC    X'00018173'                                                      
         DC    X'00018174'                                                      
         DC    X'00018175'                                                      
         DC    X'00018176'                                                      
         DC    X'00018177'                                                      
         DC    X'00018178'                                                      
         DC    X'00018179'                                                      
         DC    X'00018180'                                                      
         DC    X'00018181'                                                      
         DC    X'00018182'                                                      
         DC    X'00018183'                                                      
         DC    X'00018184'                                                      
         DC    X'00018200'                                                      
         DC    X'00018201'                                                      
         DC    X'00018202'                                                      
         DC    X'00018203'                                                      
         DC    X'00018204'                                                      
         DC    X'00018205'                                                      
         DC    X'00018206'                                                      
         DC    X'00018207'                                                      
         DC    X'00018208'                                                      
         DC    X'00018209'                                                      
         DC    X'00018210'                                                      
         DC    X'00018211'                                                      
         DC    X'00018212'                                                      
         DC    X'00018213'                                                      
         DC    X'00018214'                                                      
         DC    X'00018215'                                                      
         DC    X'00018216'                                                      
         DC    X'00018217'                                                      
         DC    X'00018218'                                                      
         DC    X'00018219'                                                      
         DC    X'00018220'                                                      
         DC    X'00018221'                                                      
         DC    X'00018222'                                                      
         DC    X'00018223'                                                      
         DC    X'00018224'                                                      
         DC    X'00018225'                                                      
         DC    X'00018226'                                                      
         DC    X'00018227'                                                      
         DC    X'00018228'                                                      
         DC    X'00018229'                                                      
         DC    X'00018230'                                                      
         DC    X'00018231'                                                      
         DC    X'00018232'                                                      
         DC    X'00018233'                                                      
         DC    X'00018234'                                                      
         DC    X'00018235'                                                      
         DC    X'00018236'                                                      
         DC    X'00018237'                                                      
         DC    X'00018238'                                                      
         DC    X'00018239'                                                      
         DC    X'00018240'                                                      
         DC    X'00018241'                                                      
         DC    X'00018242'                                                      
         DC    X'00018243'                                                      
         DC    X'00018244'                                                      
         DC    X'00018245'                                                      
         DC    X'00018246'                                                      
         DC    X'00018247'                                                      
         DC    X'00018248'                                                      
         DC    X'00018249'                                                      
         DC    X'00018250'                                                      
         DC    X'00018251'                                                      
         DC    X'00018252'                                                      
         DC    X'00018253'                                                      
         DC    X'00018254'                                                      
         DC    X'00018255'                                                      
         DC    X'00018258'                                                      
         DC    X'00018259'                                                      
         DC    X'00018260'                                                      
         DC    X'00018261'                                                      
         DC    X'00018262'                                                      
         DC    X'00018263'                                                      
         DC    X'00018264'                                                      
         DC    X'00018265'                                                      
         DC    X'00018266'                                                      
         DC    X'00018267'                                                      
         DC    X'00018268'                                                      
         DC    X'00018269'                                                      
         DC    X'00018270'                                                      
         DC    X'00018271'                                                      
         DC    X'00018272'                                                      
         DC    X'00018273'                                                      
         DC    X'00018274'                                                      
         DC    X'00018275'                                                      
         DC    X'00018276'                                                      
         DC    X'00018277'                                                      
         DC    X'00018278'                                                      
         DC    X'00018279'                                                      
         DC    X'00018280'                                                      
         DC    X'00018281'                                                      
         DC    X'00018282'                                                      
         DC    X'00018283'                                                      
         DC    X'00018284'                                                      
         DC    X'00018285'                                                      
         DC    X'00018286'                                                      
         DC    X'00018287'                                                      
         DC    X'00018288'                                                      
         DC    X'00018289'                                                      
         DC    X'00018290'                                                      
         DC    X'00018291'                                                      
         DC    X'00018292'                                                      
         DC    X'00018293'                                                      
         DC    X'00018326'                                                      
         DC    X'00018327'                                                      
         DC    X'00018333'                                                      
         DC    X'00018334'                                                      
         DC    X'FFFFFFFF'                                                      
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
WORKX    EQU   *                                                                
         EJECT                                                                  
       ++INCLUDE DMLDDEFN                                                       
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
         PRINT OFF                                                              
       ++INCLUDE REGENALLD                                                      
       ++INCLUDE REGENMKG                                                       
       ++INCLUDE REGENDAR                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'008RELDDARE  01/21/05'                                      
         END                                                                    
