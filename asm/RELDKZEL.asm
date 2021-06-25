*          DATA SET RELDKZEL   AT LEVEL 007 AS OF 05/01/02                      
*PHASE RELDKZEL,*                                                               
*INCLUDE PRNTBL                                                                 
*INCLUDE PRINT                                                                  
         TITLE 'RELDTEAM - LOAD/DUMP MODEL EXTERNAL ROUTINE'                    
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
* RESET STATION LETTERS FOR IMPROPER SWITCH:  KEZL-F -> KZEL-F    *             
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
         B     DMXIT                                                            
         EJECT                                                                  
*******************************************************************             
* PROCESS RECORD LOGIC                                            *             
*******************************************************************             
DMXREC   DS    0H                                                               
         L     R3,AREC             POINT TO RECORD                              
*                                                                               
         USING RCONREC,R3                                                       
         CLI   RCONKTYP,X'0C'      CONTRACT RECORD?                             
         BNE   DMXKEEP             NO  - KEEP IT                                
         CLC   RCONKREP,=C'CR'     CHRISTAL RADIO                               
         BNE   DMXKEEP             NO  - KEEP IT                                
         CLC   RCONKSTA,=C'KEZLF'  INCORRECT STATION IN KEY?                    
         BE    DMX0020             YES - CHECK CON#                             
         CLC   RCONKSTA,=C'KNRQA'  NO  - COMBO ORDER PARTICIPANT?               
         BE    DMX0020             YES                                          
         CLC   RCONKSTA,=C'KNRQF'  NO  - COMBO ORDER PARTICIPANT?               
         BE    DMX0020             YES -                                        
         B     DMXKEEP             NO  - KEEP IT                                
DMX0020  EQU   *                                                                
         LA    RF,CONTAB                                                        
DMX0030  EQU   *                                                                
         CLI   0(RF),0             END OF TABLE?                                
         BE    DMXKEEP             CONTRACT NOT IN TABLE:  KEEP IT              
         CLC   RCONKCON,0(RF)      CONTRACT IN TABLE?                           
         BE    DMX0040             YES -                                        
         LA    RF,4(RF)            NO  - BUMP TO NEXT ENTRY                     
         B     DMX0030             GO BACK FOR NEXT                             
DMX0040  EQU   *                                                                
         CLC   RCONKSTA,=C'KEZLF'  STATION REQUIRES REPLACEMENT?                
         BNE   DMX0060             NO  - JUST DO COMBO CONTROL                  
         MVC   RCONKSTA,=C'KZELF'  YES - REPLACE STATION LETTERS                
DMX0060  EQU   *                                                                
         LA    RF,RCONELEM                                                      
DMX0080  EQU   *                                                                
         CLI   0(RF),0             END OF RECORD?                               
         BE    DMXPRINT            YES - NOTHING TO CHANGE                      
         CLI   0(RF),X'17'         COMBO CONTROL ELEMENT?                       
         BE    DMX0100             YES - SCAN FOR STATION                       
         ZIC   RE,1(RF)                                                         
         AR    RF,RE                                                            
         B     DMX0080             GO BACK FOR NEXT ELEMENT                     
DMX0100  EQU   *                                                                
*                                                                               
         SR    R4,R4               CLEAR                                        
         ZIC   R5,1(RF)            GET LENGTH OF ELEMENT                        
         BCTR  R5,0                                                             
         BCTR  R5,0                BACK OFF CONTROL                             
         D     R4,=F'9'            DIV LENGTH BY ENTRY LENGTH                   
         LA    RF,2(RF)            BUMP TO 1ST ENTRY IN ELT                     
DMX0120  EQU   *                                                                
         CLC   =C'KEZLF',0(RF)                                                  
         BNE   DMX0140                                                          
*                                                                               
         MVC   0(5,RF),=C'KZELF'   YES - REPLACE STATION LETTERS                
         B     DMXPRINT            KEEP ALL RECORDS                             
DMX0140  EQU   *                                                                
         LA    RF,9(RF)            BUMP TO NEXT ENTRY                           
         BCT   R4,DMX0120          GO BACK FOR NEXT                             
         B     DMXPRINT            KEEP ALL RECORDS                             
*                                                                               
DMXPRINT EQU   *                                                                
         LA    R6,=C'KZELF='                                                    
         MVC   HALF,RCONLEN                                                     
         LH    R8,HALF                                                          
         GOTO1 PRNTBL,DMCB,(6,(R6)),(R3),C'DUMP',(R8),=C'2D'                    
         B     DMXKEEP                                                          
         DROP  R3                                                               
*                                                                               
CONTAB   EQU   *                                                                
                                                                                
*        DC   X'03283933'                                                       
*        DC   X'03232723'                                                       
*        DC   X'03237481'                                                       
*        DC   X'03259047'                                                       
*        DC   X'03263954'                                                       
*        DC   X'03256689'                                                       
*        DC   X'03234234'                                                       
*        DC   X'03256036'                                                       
*        DC   X'03239800'                                                       
*        DC   X'03266413'                                                       
*        DC   X'03287563'                                                       
*        DC   X'03235366'                                                       
*        DC   X'03271362'                                                       
*        DC   X'03271364'                                                       
*        DC   X'03283097'                                                       
*        DC   X'03272675'                                                       
*        DC   X'03283862'                                                       
*        DC   X'03283933'                                                       
*        DC   X'03283936'                                                       
*        DC   X'03232510'                                                       
*        DC   X'03255810'  COMBO                                                
*                                  THIS ORDER IS NOT! KEZL-F                    
*        DC   X'03277277'  COMBO                                                
*        DC   X'03277275'  COMBO:  KNRQ-A                                       
*        DC   X'03277276'  COMBO:  KNRQ-F                                       
*        DC   X'03233087'                                                       
*        DC   X'03234388'                                                       
*        DC   X'03260666'                                                       
*        DC   X'03232071'                                                       
*        DC   X'03232076'                                                       
*        DC   X'03296384'                                                       
*        DC   X'03264448'                                                       
*        DC   X'03290091'                                                       
*        DC   X'03291858'                                                       
*        DC   X'03294660'                                                       
*        DC   X'03295251'                                                       
*        DC   X'03297750'                                                       
*        DC   X'03297751'                                                       
*        DC   X'03297752'                                                       
*        DC   X'03266179'                                                       
*        DC   X'03231364'                                                       
*        DC   X'03231367'                                                       
*        DC   X'03232520'                                                       
*        DC   X'03243718'                                                       
*        DC   X'03255128'                                                       
*        DC   X'03270703'                                                       
*        DC   X'03270706'                                                       
*        DC   X'03280930'                                                       
*        DC   X'03281008'                                                       
*        DC   X'03289659'                                                       
*        DC   X'03292773'                                                       
*        DC   X'03294236'                                                       
*        DC   X'03284336'                                                       
*        DC   X'03256439'                                                       
*        DC   X'03259604'                                                       
*        DC   X'03271172'                                                       
*        DC   X'03278277'                                                       
*        DC   X'03235888'                                                       
*        DC   X'03249923'                                                       
*        DC   X'03260665'                                                       
*        DC   X'03265273'                                                       
*        DC   X'03265274'                                                       
*        DC   X'03266225'                                                       
*        DC   X'03281126'                                                       
*        DC   X'03263218'                                                       
*        DC   X'03263573'                                                       
*        DC   X'03263577'                                                       
*        DC   X'03249924'                                                       
*        DC   X'03228163'                                                       
*        DC   X'03194625'                                                       
*        DC   X'03212409'                                                       
*        DC   X'03229877'                                                       
*        DC   X'03230682'                                                       
*        DC   X'03236520'                                                       
*        DC   X'03236855'                                                       
*        DC   X'03237805'                                                       
*        DC   X'03244996'                                                       
*        DC   X'03244997'                                                       
*        DC   X'03249139'                                                       
*        DC   X'03249164'                                                       
*        DC   X'03249920'                                                       
*        DC   X'03249922'                                                       
*        DC   X'03254190'                                                       
*        DC   X'03256804'                                                       
*        DC   X'03257154'                                                       
*        DC   X'03257157'                                                       
*        DC   X'03257166'                                                       
*        DC   X'03257881'                                                       
*        DC   X'03258633'                                                       
*        DC   X'03260664'                                                       
*        DC   X'03265134'                                                       
*        DC   X'03265135'                                                       
*        DC   X'03265548'                                                       
*        DC   X'03266742'                                                       
*        DC   X'03272147'                                                       
*        DC   X'03283720'                                                       
*        DC   X'03293454'                                                       
*        DC   X'03235206' COMBO                                                 
*        DC   X'03235205' COMBO:   KNRQ-F                                       
*        DC   X'03232271' COMBO                                                 
*        DC   X'03232270' COMBO:   KNRQ-F                                       
*        DC   X'03233828' COMBO                                                 
*        DC   X'03233827' COMBO:   KNRQ-F                                       
*        DC   X'03233826' COMBO:   KNRQ-A                                       
*        DC   X'03235213' COMBO                                                 
*        DC   X'03235212' COMBO:   KNRQ-F                                       
*        DC   X'03238317'                                                       
*        DC   X'03251183'                                                       
*        DC   X'03251619'                                                       
*        DC   X'03256911' COMBO                                                 
*        DC   X'03256910' COMBO:   KNRQ-F                                       
*        DC   X'03256918' COMBO                                                 
*        DC   X'03256917' COMBO:   KNRQ-F                                       
*        DC   X'03256934' COMBO                                                 
*        DC   X'03256933' COMBO:   KNRQ-F                                       
*        DC   X'03259631'                                                       
*        DC   X'03272736' COMBO                                                 
*        DC   X'03272735' COMBO:   KNRQ-F                                       
*        DC   X'03273733' COMBO                                                 
*        DC   X'03273732' COMBO:   KNRQ-F                                       
*        DC   X'03273737' COMBO                                                 
*        DC   X'03273736' COMBO:   KNRQ-F                                       
*        DC   X'03238361'                                                       
*        DC   X'03238739'                                                       
*        DC   X'03239423'                                                       
*        DC   X'03233443'                                                       
*        DC   X'03249470'                                                       
*        DC   X'03291603'                                                       
*        DC   X'03294319'                                                       
*        DC   X'03238736' COMBO                                                 
*        DC   X'03238735' COMBO:   KNRQ-F                                       
*        DC   X'03251132'                                                       
*        DC   X'03298270'                                                       
*        DC   X'03298271'                                                       
*        DC   X'03298273'                                                       
*        DC   X'03298277'                                                       
*        DC   X'03299431'                                                       
*                                                                               
*   SECOND BATCH:                                                               
*                                                                               
*        DC   X'03233806'                                                       
*        DC   X'03253581' COMBO                                                 
*        DC   X'03253579' COMBO:   KNRQ-A                                       
*        DC   X'03253580' COMBO:   KNRQ-F                                       
*        DC   X'03254388'                                                       
*        DC   X'03237534'                                                       
*        DC   X'03257175'                                                       
*        DC   X'03281597'                                                       
*        DC   X'03239443'                                                       
*        DC   X'03294315'                                                       
*                                                                               
*   THIRD BATCH                                                                 
*                                                                               
         DC   X'03267070'                                                       
         DC   X'03246081'                                                       
         DC   X'03235371'                                                       
         DC   X'03256063'                                                       
         DC   X'03256067'                                                       
         DC   X'0000'                                                           
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
         MVC   P+3(33),=C'DETAILS OF PURGED RECORDS FOLLOWS'                    
         GOTO1 VPRINTER                                                         
*                                                                               
         LA    R5,CODES                                                         
DC10     MVC   P+3(8),0(R5)                                                     
         EDIT  (P5,10(R5)),(7,P+13)                                             
         GOTO1 VPRINTER                                                         
         LA    R5,L'CODES(R5)                                                   
         CLI   0(R5),X'FF'                                                      
         BNE   DC10                                                             
*                                                                               
         B     DMXIT                                                            
         EJECT                                                                  
         GETEL R5,34,ELCODE                                                     
         SPACE 1                                                                
PURGE    DC    PL5'0'                                                           
CHANGE   DC    PL5'0'                                                           
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
CODES    DS    0CL15                                                            
*        DC    CL8'REP     ',XL1'01',AL1(RREPKREP-RREPREC),PL5'0'               
         DC    CL8'STATION ',XL1'02',AL1(RSTAKREP-RSTAREC),PL5'0'               
         DC    CL8'STATION2',XL1'42',AL1(RSTAKREP-RSTAREC),PL5'0'               
         DC    CL8'REGION  ',XL1'03',AL1(RREGKREP-RREGREC),PL5'0'               
         DC    CL8'OFFICE  ',XL1'04',AL1(ROFFKREP-ROFFREC),PL5'0'               
         DC    CL8'OFFICE2 ',XL1'44',AL1(22),PL5'0'                             
         DC    CL8'DIVISION',XL1'05',AL1(RTEMKREP-RTEMREC),PL5'0'               
         DC    CL8'MAN     ',XL1'06',AL1(RSALKREP-RSALREC),PL5'0'               
         DC    CL8'GROUP   ',XL1'07',AL1(RGRPKREP-RGRPREC),PL5'0'               
         DC    CL8'ADVERTIS',XL1'08',AL1(RADVKREP-RADVREC),PL5'0'               
         DC    CL8'PRODUCT ',XL1'09',AL1(RPRDKREP-RPRDREC),PL5'0'               
         DC    CL8'AGENCY  ',XL1'0A',AL1(RAGYKREP-RAGYREC),PL5'0'               
         DC    CL8'AGENCY2 ',XL1'1A',AL1(25),PL5'0'                             
         DC    CL8'BUY     ',XL1'0B',AL1(RBUYKREP-RBUYREC),PL5'0'               
         DC    CL8'CONTRACT',XL1'0C',AL1(RCONKREP-RCONREC),PL5'0'               
         DC    CL8'CLASS   ',XL1'0D',AL1(RCLSKREP-RCLSREC),PL5'0'               
         DC    CL8'CATEGORY',XL1'0F',AL1(RCTGKREP-RCTGREC),PL5'0'               
         DC    CL8'INVENTRY',XL1'12',AL1(RINVKREP-RINVREC),PL5'0'               
         DC    CL8'BUDGET  ',XL1'13',AL1(RBUDKREP-RBUDREC),PL5'0'               
         DC    CL8'AVAIL   ',XL1'14',AL1(RAVLKREP-RAVLREC),PL5'0'               
         DC    CL8'PROPOSAL',XL1'16',AL1(RPRPKREP-RPRPREC),PL5'0'               
         DC    CL8'EOM     ',XL1'18',AL1(REOMKREP-REOMREC),PL5'0'               
         DC    CL8'OFF BUD ',XL1'19',AL1(17),PL5'0'                             
         DC    CL8'EOP ADV ',XL1'1B',AL1(15),PL5'0'                             
         DC    CL8'EOP AGY ',XL1'1C',AL1(13),PL5'0'                             
         DC    CL8'EOP OFF ',XL1'1D',AL1(17),PL5'0'                             
         DC    CL8'EOP SAL ',XL1'1E',AL1(16),PL5'0'                             
         DC    CL8'OVR UPLD',XL1'22',AL1(13),PL5'0'                             
         DC    CL8'DEMOMENU',XL1'23',AL1(RDEMKREP-RDEMREC),PL5'0'               
         DC    CL8'DAYPART ',XL1'24',AL1(24),PL5'0'                             
         DC    CL8'PRG TYPE',XL1'25',AL1(24),PL5'0'                             
         DC    CL8'SDD     ',XL1'26',AL1(20),PL5'0'                             
         DC    CL8'ATHENA  ',XL1'27',AL1(01),PL5'0'                             
         DC    CL8'CMISSION',XL1'29',AL1(11),PL5'0'                             
         DC    CL8'OWNRSHIP',XL1'2A',AL1(22),PL5'0'                             
         DC    CL8'MARKET  ',XL1'2B',AL1(21),PL5'0'                             
         DC    CL8'AUR     ',XL1'2C',AL1(04),PL5'0'                             
         DC    CL8'SBB     ',XL1'2D',AL1(12),PL5'0'                             
         DC    CL8'COMMENT ',XL1'2E',AL1(15),PL5'0'                             
         DC    CL8'TYPE    ',XL1'30',AL1(17),PL5'0'                             
         DC    CL8'PT PRSN ',XL1'31',AL1(22),PL5'0'                             
         DC    CL8'K TYPE  ',XL1'32',AL1(24),PL5'0'                             
         DC    CL8'RADAR   ',XL1'33',AL1(17),PL5'0'                             
         DC    CL8'OCM     ',XL1'34',AL1(20),PL5'0'                             
         DC    CL8'DIRESPON',XL1'35',AL1(13),PL5'0'                             
         DC    CL8'LABEL   ',XL1'36',AL1(17),PL5'0'                             
         DC    CL8'GOAL    ',XL1'37',AL1(13),PL5'0'                             
         DC    CL8'SET     ',XL1'38',AL1(19),PL5'0'                             
         DC    CL8'STRATEGY',XL1'39',AL1(13),PL5'0'                             
         DC    CL8'DEV SAL ',XL1'3A',AL1(22),PL5'0'                             
         DC    CL8'DEV K TP',XL1'3B',AL1(23),PL5'0'                             
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
HALF     DS    H                                                                
ELCODE   DS    CL1                                                              
WORK     DS    CL64                                                             
WORKX    EQU   *                                                                
         EJECT                                                                  
       ++INCLUDE DMLDDEFN                                                       
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
         PRINT OFF                                                              
       ++INCLUDE REGENALLD                                                      
       ++INCLUDE REGENSDD                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007RELDKZEL  05/01/02'                                      
         END                                                                    
