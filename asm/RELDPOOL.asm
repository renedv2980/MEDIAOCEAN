*          DATA SET RELDPOOL   AT LEVEL 042 AS OF 05/01/02                      
*PHASE RELDPOOL                                                                 
*INCLUDE PRNTBL                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE PRTREC                                                                 
         TITLE 'RELDCAN - SCAN FOR BAD MAKEGOOD RECORDS'                        
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
* REMOVE POOL/BRAND DARE ORDERS                                   *             
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
         L     RE,=V(PRINT)                                                     
         AR    RE,R5                                                            
         ST    RE,PRNTBL                                                        
         L     RE,=V(HEXOUT)                                                    
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
*                                                                               
*&&DO                                                                           
         USING RCONREC,R5                                                       
         CLI   RCONKTYP,X'0C'                                                   
         BNE   DMXR100                                                          
*                                                                               
         LA    R3,BLLIST                                                        
         CLC   =C'BL',RCONKREP                                                  
         BE    DMXR050                                                          
*                                                                               
         LA    R3,FNLIST                                                        
         CLC   =C'FN',RCONKREP                                                  
         BE    DMXR050                                                          
*                                                                               
         LA    R3,AMLIST                                                        
         CLC   =C'AM',RCONKREP                                                  
         BE    DMXR050                                                          
*                                                                               
         LA    R3,CQLIST                                                        
         CLC   =C'CQ',RCONKREP                                                  
         BE    DMXR050                                                          
*                                                                               
         LA    R3,NBLIST                                                        
         CLC   =C'NB',RCONKREP                                                  
         BE    DMXR050                                                          
*                                                                               
         LA    R3,PVLIST                                                        
         CLC   =C'PV',RCONKREP                                                  
         BE    DMXR050                                                          
*                                                                               
         LA    R3,UTLIST                                                        
         CLC   =C'UT',RCONKREP                                                  
         BE    DMXR050                                                          
*                                                                               
         LA    R3,SZLIST                                                        
         CLC   =C'SZ',RCONKREP                                                  
         BNE   DMXKEEP                                                          
*                                                                               
DMXR050  DS    0H                                                               
         CLC   RCONKCON,0(R3)                                                   
         BE    DMXR060                                                          
         AHI   R3,8                                                             
         CLI   0(R3),X'FF'                                                      
         BE    DMXKEEP                                                          
         B     DMXR050                                                          
*                                                                               
DMXR060  DS    0H                                                               
         GOTO1 =V(HELLO),DMCB,(C'D',=C'REPFILE'),(X'1D',RCONREC),0,0            
         CLI   DMCB+12,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
*        GOTO1 =V(PRTREC),DMCB,RCONREC,X'0022001B',PRINT,HEXOUT                 
*                                                                               
         MVC   P(2),RCONKREP                                                    
         MVC   P+4(9),=C'CONTRACT:'                                             
         GOTO1 =V(HEXOUT),DMCB,RCONKCON,P+15,4                                  
         MVC   P+26(14),=C'LINKED REMOVED'                                      
         GOTO1 VPRINTER                                                         
         B     DMXKEEP                                                          
*&&                                                                             
*                                                                               
DMXR100  DS    0H                                                               
         USING RDARREC,R5                                                       
         CLI   RDARKTYP,X'41'                                                   
         BE    DMXR110                                                          
         CLI   RDARKTYP,X'51'                                                   
         BNE   DMXPURGE                                                         
*                                                                               
DMXR110  DS    0H                                                               
         CLC   =C'OM NY',RDARKAGY                                               
         BE    DMXPURGE                                                         
*                                                                               
         LA    R3,BLLIST                                                        
         CLC   =C'BL',RDARKREP                                                  
         BE    DMXR150                                                          
*                                                                               
         LA    R3,FNLIST                                                        
         CLC   =C'FN',RDARKREP                                                  
         BE    DMXR150                                                          
*                                                                               
         LA    R3,AMLIST                                                        
         CLC   =C'AM',RDARKREP                                                  
         BE    DMXR150                                                          
*                                                                               
         LA    R3,CQLIST                                                        
         CLC   =C'CQ',RDARKREP                                                  
         BE    DMXR150                                                          
*                                                                               
         LA    R3,NBLIST                                                        
         CLC   =C'NB',RDARKREP                                                  
         BE    DMXR150                                                          
*                                                                               
         LA    R3,PVLIST                                                        
         CLC   =C'PV',RDARKREP                                                  
         BE    DMXR150                                                          
*                                                                               
         LA    R3,UTLIST                                                        
         CLC   =C'UT',RDARKREP                                                  
         BE    DMXR150                                                          
*                                                                               
         LA    R3,SZLIST                                                        
         CLC   =C'SZ',RDARKREP                                                  
         BNE   DMXPURGE                                                         
*                                                                               
DMXR150  DS    0H                                                               
         CLC   RDARKORD,4(R3)                                                   
         BE    DMXR160                                                          
         AHI   R3,8                                                             
         CLI   0(R3),X'FF'                                                      
         BE    DMXPURGE                                                         
         B     DMXR150                                                          
*                                                                               
DMXR160  DS    0H                                                               
         MVC   P(2),RDARKREP                                                    
         MVC   P+4(9),=C'DARE REC:'                                             
*                                                                               
         MVC   P+15(3),RDARKAGY                                                 
         MVI   P+18,C'-'                                                        
         MVC   P+19(2),RDARKAOF                                                 
*                                                                               
         GOTO1 =V(HEXOUT),DMCB,RDARKEY,P+25,1                                   
         GOTO1 =V(HEXOUT),DMCB,RDARKORD,P+30,4                                  
         GOTO1 =V(HEXOUT),DMCB,RDARKRT,P+40,1                                   
*                                                                               
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
         MVC   P+3(7),=C'CHANGED'                                               
         EDIT  (P5,CHANGE),(7,P+12)                                             
         GOTO1 VPRINTER                                                         
         B     DMXIT                                                            
         EJECT                                                                  
         GETEL R5,34,ELCODE                                                     
         SPACE 1                                                                
PURGE    DC    PL5'0'                                                           
CHANGE   DC    PL5'0'                                                           
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
BLLIST   DS    0XL8                                                             
         DC    X'00000000',X'01190011'                                          
         DC    X'00000000',X'01220028'                                          
         DC    X'00000000',X'01250014'                                          
         DC    X'00000000',X'01220035'                                          
         DC    X'04985096',X'01110070'                                          
         DC    X'04986145',X'01050022'                                          
         DC    X'04986700',X'01080081'                                          
         DC    X'04987181',X'01110034'                                          
         DC    X'04988126',X'01030026'                                          
         DC    X'04988654',X'01050065'                                          
         DC    X'04992544',X'01110086'                                          
         DC    X'04993099',X'01110090'                                          
         DC    X'04998163',X'01250009'                                          
         DC    X'04998171',X'01250010'                                          
         DC    X'04998327',X'01220026'                                          
         DC    X'04998328',X'01220027'                                          
         DC    X'04998335',X'01250011'                                          
         DC    X'04998336',X'01250012'                                          
         DC    X'04998337',X'01250013'                                          
         DC    X'04998339',X'01250015'                                          
         DC    X'04999849',X'01300000'                                          
         DC    X'04999883',X'01300001'                                          
         DC    X'04999886',X'01300002'                                          
         DC    X'04999897',X'01300003'                                          
         DC    X'04999899',X'01300004'                                          
         DC    X'04999904',X'01300005'                                          
         DC    X'05000248',X'01220024'                                          
         DC    X'FF'                                                            
*                                                                               
FNLIST   DS    0XL8                                                             
         DC    X'00000000',X'01220015'                                          
         DC    X'00000000',X'01220031'                                          
         DC    X'00000000',X'01110082'                                          
         DC    X'00000000',X'01050046'                                          
         DC    X'02741833',X'01110046'                                          
         DC    X'02742730',X'01030011'                                          
         DC    X'02743892',X'01050032'                                          
         DC    X'02744813',X'01100003'                                          
         DC    X'02744902',X'01110067'                                          
         DC    X'02744946',X'01110057'                                          
         DC    X'02744971',X'01110033'                                          
         DC    X'02747690',X'01230016'                                          
         DC    X'02747693',X'01230015'                                          
         DC    X'02747698',X'01230018'                                          
         DC    X'02747702',X'01230017'                                          
         DC    X'02747706',X'01230014'                                          
         DC    X'02747707',X'01230013'                                          
         DC    X'02747710',X'01230012'                                          
         DC    X'02748984',X'01260008'                                          
         DC    X'02748998',X'01260009'                                          
         DC    X'02749014',X'01260010'                                          
         DC    X'02749022',X'01260011'                                          
         DC    X'02749025',X'01260012'                                          
         DC    X'02749032',X'01260013'                                          
         DC    X'02749036',X'01260014'                                          
         DC    X'02749219',X'01260022'                                          
         DC    X'02749223',X'01260023'                                          
         DC    X'02749229',X'01260024'                                          
         DC    X'02749237',X'01260025'                                          
         DC    X'02749243',X'01260026'                                          
         DC    X'02749247',X'01260027'                                          
         DC    X'02749252',X'01260028'                                          
         DC    X'02749284',X'01260048'                                          
         DC    X'02749294',X'01260049'                                          
         DC    X'02749304',X'01260050'                                          
         DC    X'02749322',X'01260051'                                          
         DC    X'02749342',X'01260052'                                          
         DC    X'02749351',X'01260053'                                          
         DC    X'02749354',X'01260054'                                          
         DC    X'02752083',X'01300041'                                          
         DC    X'02752085',X'01300045'                                          
         DC    X'02752086',X'01300042'                                          
         DC    X'02752087',X'01300043'                                          
         DC    X'02752088',X'01300046'                                          
         DC    X'02752090',X'01300044'                                          
         DC    X'02752091',X'01300047'                                          
         DC    X'FF'                                                            
*                                                                               
AMLIST   DS    0XL8                                                             
         DC    X'00000000',X'01260007'                                          
         DC    X'06861883',X'01230028'                                          
         DC    X'00000000',X'01220017'                                          
         DC    X'00000000',X'01290043'                                          
         DC    X'00000000',X'01230004'                                          
         DC    X'00000000',X'01310014'                                          
         DC    X'00000000',X'01230005'                                          
         DC    X'00000000',X'01310011'                                          
         DC    X'00000000',X'01310015'                                          
         DC    X'06844607',X'01050031'                                          
         DC    X'06844772',X'01050030'                                          
         DC    X'06858657',X'01050019'                                          
         DC    X'06859986',X'01110048'                                          
         DC    X'06860003',X'01110036'                                          
         DC    X'06860004',X'01110055'                                          
         DC    X'06860675',X'01110042'                                          
         DC    X'06860727',X'01110029'                                          
         DC    X'06860819',X'01110032'                                          
         DC    X'06862218',X'01230006'                                          
         DC    X'06862220',X'01230000'                                          
         DC    X'06862222',X'01230003'                                          
         DC    X'06862223',X'01230001'                                          
         DC    X'06862224',X'01230002'                                          
         DC    X'06862504',X'01260002'                                          
         DC    X'06862506',X'01260003'                                          
         DC    X'06862507',X'01260004'                                          
         DC    X'06862509',X'01260005'                                          
         DC    X'06862512',X'01260006'                                          
         DC    X'06862516',X'01260000'                                          
         DC    X'06862518',X'01260001'                                          
         DC    X'06862549',X'01260041'                                          
         DC    X'06862552',X'01260042'                                          
         DC    X'06862553',X'01260043'                                          
         DC    X'06862556',X'01260044'                                          
         DC    X'06862560',X'01260045'                                          
         DC    X'06862564',X'01260046'                                          
         DC    X'06862567',X'01260047'                                          
         DC    X'06862756',X'01260058'                                          
         DC    X'06862762',X'01260059'                                          
         DC    X'06862763',X'01260060'                                          
         DC    X'06862764',X'01260061'                                          
         DC    X'06862765',X'01260062'                                          
         DC    X'06862767',X'01260063'                                          
         DC    X'06862770',X'01260064'                                          
         DC    X'06862944',X'01260065'                                          
         DC    X'06862945',X'01260066'                                          
         DC    X'06862946',X'01260067'                                          
         DC    X'06862949',X'01260068'                                          
         DC    X'06862952',X'01260069'                                          
         DC    X'06862953',X'01260070'                                          
         DC    X'06862954',X'01260071'                                          
         DC    X'06862982',X'01220016'                                          
         DC    X'06863908',X'01310012'                                          
         DC    X'06863910',X'01310013'                                          
         DC    X'06863911',X'01310016'                                          
         DC    X'FF'                                                            
*                                                                               
CQLIST   DS    0XL8                                                             
         DC    X'06816808',X'01050064'                                          
         DC    X'06816881',X'01080082'                                          
         DC    X'06817611',X'01090004'                                          
         DC    X'06827754',X'01220034'                                          
         DC    X'06828026',X'01220033'                                          
         DC    X'06828087',X'01200000'                                          
         DC    X'FF'                                                            
*                                                                               
NBLIST   DS    0XL8                                                             
         DC    X'00059159',X'01030018'                                          
         DC    X'00060014',X'01110088'                                          
         DC    X'00060387',X'01110085'                                          
         DC    X'00062247',X'01220029'                                          
         DC    X'00062362',X'01250029'                                          
         DC    X'00062365',X'01250030'                                          
         DC    X'00062366',X'01250031'                                          
         DC    X'00062368',X'01250032'                                          
         DC    X'00062369',X'01250033'                                          
         DC    X'00062371',X'01250034'                                          
         DC    X'00062372',X'01250035'                                          
         DC    X'00062598',X'01250038'                                          
         DC    X'00062599',X'01250039'                                          
         DC    X'00062600',X'01250040'                                          
         DC    X'00062601',X'01250041'                                          
         DC    X'00062602',X'01250042'                                          
         DC    X'00062603',X'01250043'                                          
         DC    X'00062604',X'01250044'                                          
         DC    X'FF'                                                            
*                                                                               
PVLIST   DS    0XL8                                                             
         DC    X'00000000',X'01200001'                                          
         DC    X'00000000',X'01220014'                                          
         DC    X'02982493',X'01050033'                                          
         DC    X'02983496',X'01050063'                                          
         DC    X'02983957',X'01110002'                                          
         DC    X'02984101',X'01110069'                                          
         DC    X'02984305',X'01080050'                                          
         DC    X'02984383',X'01110059'                                          
         DC    X'02984428',X'01110075'                                          
         DC    X'02985869',X'01190004'                                          
         DC    X'02985885',X'01190006'                                          
         DC    X'02985887',X'01190007'                                          
         DC    X'02985890',X'01190008'                                          
         DC    X'02985892',X'01190009'                                          
         DC    X'02985894',X'01190010'                                          
         DC    X'02985902',X'01190005'                                          
         DC    X'02986533',X'01240002'                                          
         DC    X'02986535',X'01240003'                                          
         DC    X'02986537',X'01240004'                                          
         DC    X'02986538',X'01240005'                                          
         DC    X'02986539',X'01240007'                                          
         DC    X'02986542',X'01240001'                                          
         DC    X'02986563',X'01240006'                                          
         DC    X'02987114',X'01250022'                                          
         DC    X'02987115',X'01250023'                                          
         DC    X'02987116',X'01250024'                                          
         DC    X'02987119',X'01250025'                                          
         DC    X'02987120',X'01250026'                                          
         DC    X'02987121',X'01250027'                                          
         DC    X'02987122',X'01250028'                                          
         DC    X'02987148',X'01260015'                                          
         DC    X'02987153',X'01260016'                                          
         DC    X'02987156',X'01260017'                                          
         DC    X'02987157',X'01260018'                                          
         DC    X'02987158',X'01260019'                                          
         DC    X'02987160',X'01260020'                                          
         DC    X'02987161',X'01260021'                                          
         DC    X'FF'                                                            
*                                                                               
UTLIST   DS    0XL8                                                             
         DC    X'00000000',X'01220022'                                          
         DC    X'00123976',X'01050024'                                          
         DC    X'00124301',X'01230033'                                          
         DC    X'00124793',X'01240000'                                          
         DC    X'FF'                                                            
*                                                                               
SZLIST   DS    0XL8                                                             
         DC    X'00000000',X'01230035'                                          
         DC    X'06811186',X'01110025'                                          
         DC    X'06811507',X'01050044'                                          
         DC    X'06815038',X'01110043'                                          
         DC    X'06816676',X'01110063'                                          
         DC    X'06820129',X'01230039'                                          
         DC    X'06820357',X'01230037'                                          
         DC    X'06820365',X'01230038'                                          
         DC    X'06820381',X'01230040'                                          
         DC    X'06820385',X'01230041'                                          
         DC    X'06820388',X'01230042'                                          
         DC    X'06820396',X'01230043'                                          
         DC    X'06820440',X'01240013'                                          
         DC    X'06820446',X'01240014'                                          
         DC    X'06820450',X'01240015'                                          
         DC    X'06820458',X'01240016'                                          
         DC    X'06820462',X'01240017'                                          
         DC    X'06820468',X'01240018'                                          
         DC    X'06820473',X'01240019'                                          
         DC    X'06820497',X'01230027'                                          
         DC    X'FF'                                                            
         EJECT                                                                  
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
PRINT    DS    A                                                                
HEXOUT   DS    A                                                                
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
       ++INCLUDE REGENSDD                                                       
       ++INCLUDE REGENDAR                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'042RELDPOOL  05/01/02'                                      
         END                                                                    
