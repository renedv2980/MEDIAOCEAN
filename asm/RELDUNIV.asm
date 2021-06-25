*          DATA SET RELDUNIV   AT LEVEL 009 AS OF 05/01/02                      
*PHASE RELDUNIV,*                                                               
*INCLUDE PRNTBL                                                                 
*INCLUDE PRINT                                                                  
         TITLE 'RELDUNIV - LOAD/DUMP MODEL EXTERNAL ROUTINE'                    
***********************************************************************         
*   CONVERT CATEGORY CODES FROM OLD DDS TO NEW KEN KLEIN DDS CODES    *         
*        WITHIN ADVERTISER RECORDS                                    *         
***********************************************************************         
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
         USING RADVREC,R3                                                       
         CLC   RADVKREP,=C'UV'     UNIVISION RECORD?                            
         BNE   DMXKEEP             NO  - KEEP IT                                
         CLI   RADVKTYP,X'09'      PRODUCT RECORD?                              
         BNE   DMXKEEP             NO  - KEEP IT                                
         BAS   RE,CHEKTABL         YES - LOOK UP CATEGORY IN TABLE              
         B     DMXKEEP                                                          
         EJECT                                                                  
*                                                                               
CHEKTABL NTR1                                                                   
         LA    R2,CATTAB           SET A(CATEGORY TABLE)                        
CHTA0020 EQU   *                                                                
         CLI   0(R2),0             END OF TABLE?                                
         BE    CHTA0120            YES - ENTRY NOT IN TABLE                     
         CLC   RPRDCATG,0(R2)      PRODUCTR CATEGORY FOUND IN TABLE?            
         BE    CHTA0040            YES - DISPLAY/CHANGE/REDISPLAY               
         LA    R2,4(R2)            NO  - BUMP TO NEXT ENTRY                     
         B     CHTA0020            GO BACK FOR NEXT                             
CHTA0040 EQU   *                                                                
         MVC   P+1(06),=C'PRE:  '                                               
         MVC   P+7(96),RPRDREC                                                  
         GOTO1 VPRINTER                                                         
         MVC   RPRDCATG,2(R2)      INSERT NEW CATEGORY CODE                     
         MVC   P+1(06),=C'POST: '                                               
         MVC   P+7(96),RPRDREC                                                  
         GOTO1 VPRINTER                                                         
         GOTO1 VPRINTER                                                         
CHTA0120 EQU   *                                                                
         XIT1                                                                   
CATTAB   EQU   *                                                                
         DC    C'71AE'                                                          
         DC    C'APAP'                                                          
         DC    C'77AR'                                                          
         DC    C'79AR'                                                          
         DC    C'13BE'                                                          
         DC    C'60BF'                                                          
         DC    C'76BN'                                                          
         DC    C'83BU'                                                          
         DC    C'84BU'                                                          
         DC    C'COBU'                                                          
         DC    C'20BU'                                                          
         DC    C'91BU'                                                          
         DC    C'94BU'                                                          
         DC    C'85BU'                                                          
         DC    C'65BU'                                                          
         DC    C'97BU'                                                          
         DC    C'16BV'                                                          
         DC    C'31BV'                                                          
         DC    C'22BV'                                                          
         DC    C'28BV'                                                          
         DC    C'51CA'                                                          
         DC    C'CCCC'                                                          
         DC    C'96CL'                                                          
         DC    C'47CL'                                                          
         DC    C'50CL'                                                          
         DC    C'58CL'                                                          
         DC    C'82CT'                                                          
         DC    C'33DA' ******CHECK AGAINST MEMO:  USE DA                        
         DC    C'88DA' ******CHECK AGAINST MEMO:  USE DA                        
         DC    C'86DI'                                                          
         DC    C'02DR'                                                          
         DC    C'39DS'                                                          
         DC    C'87DV'                                                          
         DC    C'99DV'                                                          
         DC    C'64EL'                                                          
         DC    C'57EL'                                                          
         DC    C'FMFM'                                                          
         DC    C'11FO'                                                          
         DC    C'12FO'                                                          
         DC    C'14FO'                                                          
         DC    C'21FO'                                                          
         DC    C'17FO'                                                          
         DC    C'18FO'                                                          
         DC    C'19FO'                                                          
         DC    C'10FO'                                                          
         DC    C'09FO'                                                          
         DC    C'06FO'                                                          
         DC    C'24FO'                                                          
         DC    C'25FO'                                                          
         DC    C'27FO'                                                          
         DC    C'29FO'                                                          
         DC    C'53FR'                                                          
         DC    C'FPFU'                                                          
         DC    C'03GY'                                                          
         DC    C'HWHH'                                                          
         DC    C'54HH'                                                          
         DC    C'80HI'                                                          
         DC    C'81HI'                                                          
         DC    C'49HI'                                                          
         DC    C'45HI'                                                          
         DC    C'OPHI'                                                          
         DC    C'44HI'                                                          
         DC    C'HCHO'                                                          
         DC    C'73HO'                                                          
         DC    C'43HO'                                                          
         DC    C'92HO'                                                          
         DC    C'95HO'                                                          
         DC    C'38IN'                                                          
         DC    C'07JW'                                                          
         DC    C'90LT'                                                          
         DC    C'74LW'                                                          
         DC    C'72MV'                                                          
         DC    C'350G'                                                          
         DC    C'04PB'                                                          
         DC    C'CSPB'                                                          
         DC    C'ESPB'                                                          
         DC    C'GSPB'                                                          
         DC    C'ISPB'                                                          
         DC    C'67PH'                                                          
         DC    C'PCPH'                                                          
         DC    C'PTPO'                                                          
         DC    C'ADPO'                                                          
         DC    C'41RE'                                                          
         DC    C'70RF'                                                          
         DC    C'08RG'                                                          
         DC    C'RARS'                                                          
         DC    C'37RT'                                                          
         DC    C'15SC'                                                          
         DC    C'01SD'                                                          
         DC    C'40SG'                                                          
         DC    C'32SH'                                                          
         DC    C'SPSP'                                                          
         DC    C'75TA'                                                          
         DC    C'TUTA'                                                          
         DC    C'36TA'                                                          
         DC    C'63TL'                                                          
         DC    C'98TY'                                                          
         DC    C'46UT'                                                          
         DC    C'TOVA' *****VARIOUS STILL UNDER DISCUSSION                      
         DC    C'66VA'                                                          
         DC    C'05VA'                                                          
         DC    C'FCVA'                                                          
         DC    C'GCVA'                                                          
         DC    C'56VA'                                                          
         DC    C'FSVA'                                                          
         DC    C'PPVA'                                                          
         DC    C'26VA'                                                          
         DC    C'PAVA'                                                          
         DC    C'AIVA'                                                          
         DC    C'RCVA'                                                          
         DC    C'42VA'                                                          
         DC    C'ASVA'                                                          
         DC    C'TVVA'                                                          
         DC    C'TRVA'                                                          
         DC    C'TSVA'                                                          
         DC    C'ZZVA'                                                          
         DC    C'93VA'                                                          
         DC    C'20VA'                                                          
         DC    C'30VA'                                                          
         DC    X'0000'                                                          
         DS    0F                                                               
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
**PAN#1  DC    CL21'009RELDUNIV  05/01/02'                                      
         END                                                                    
