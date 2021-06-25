*          DATA SET UNLDJWNY   AT LEVEL 012 AS OF 08/10/00                      
*PHASE UNXJWNA                                                                  
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE RECUP                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PRNTBL                                                                 
         TITLE 'UNXSPT - CONVERT JWNY TO FORD'                                  
* PARAMETER LIST                                                                
*                                                                               
* P1=A(RECORD)      PASS FIRST BYTE X'00'= INITIALISE                           
*                                   X'01'= RECORD IN CORE                       
*                                   X'FF'= END OF FILE                          
*                   RETURN VALUE    X'00'= KEEP RECORD                          
*                                   X'FF'= PURGE RECORD                         
*                                   X'FF'/C'EOJ'=PURGE & CAUSE EOJ              
* P2=A(TAPEOUT)     PASS FIRST BYTE X'80'= TAPE INPUT                           
*                                   X'40'= TAPE OUTPUT                          
*                                   X'20'= RECORD IS I/S FILE RECORD            
* P3=A(PARAM CARD)  PASS FIRST BYTE C'Y' = YOU ASKED ME TO RETURN               
*                   RETURN          C'R' = RETURN BACK TO EXTERNAL              
* P4=A(FILE DEFN)                                                               
* P5=A(PRINTER)                                                                 
* P6=A(CPRINT)                                                                  
         SPACE 2                                                                
         PRINT NOGEN                                                            
UNLDXSP  CSECT                                                                  
         NMOD1 WORKX-WORKD,UNLDXSP,RR=R2                                        
         USING WORKD,RC                                                         
         EJECT                                                                  
* CONTROL FLOW LOGIC                                                            
*                                                                               
DMXCTL   ST    R1,APARM            SAVE PARAMETERS                              
         MVC   PLIST,0(R1)                                                      
         L     RA,VCPRINT          SET UP FOR PRINTING                          
         USING DPRINT,RA                                                        
         SPACE 2                                                                
         CLI   PLIST,X'00'         FIRST CALL TO INITILISE                      
         BE    DMXINIT                                                          
         CLI   PLIST,X'01'         NORMAL CALL TO PROCESS RECORD                
         BE    DMXREC                                                           
         CLI   PLIST,X'FF'         LAST CALL ON EOF                             
         BE    DMXEOF                                                           
         B     DMXIT                                                            
         SPACE 1                                                                
DMXKEEP  L     R1,APARM            KEEP RECORD EXIT                             
         MVI   0(R1),0                                                          
         MVI   8(R1),0                                                          
         GOTO1 =V(PRNTBL),DMCB,=C'BYPS',AREC,C'DUMP',30,=C'1D'                  
         B     DMXIT                                                            
         SPACE 1                                                                
DMXPURGE L     R1,APARM            PURGE RECORD EXIT                            
         MVI   0(R1),X'FF'                                                      
         MVI   8(R1),0                                                          
*        DC    H'0'                                                             
         B     DMXIT                                                            
         SPACE 1                                                                
DMXPURG2 L     R1,APARM            PURGE RECORD EXIT                            
         MVI   0(R1),X'FF'                                                      
         MVI   8(R1),0                                                          
*        DC    H'0'                                                             
         GOTO1 =V(PRNTBL),DMCB,=C'BYPS',AREC,C'DUMP',200,=C'1D'                 
         B     DMXIT                                                            
         SPACE 1                                                                
DMXPGEOF L     R1,APARM            PURGE RECORD EXIT                            
         MVI   0(R1),X'FF'                                                      
         MVC   1(3,R1),=C'EOF'                                                  
         B     DMXIT                                                            
         SPACE 1                                                                
DMXIT    XMOD1 1                                                                
         EJECT                                                                  
* INITIALISE LOGIC - FIRST CALL HERE - NO RETURN ALLOWED                        
*                                                                               
DMXINIT  DS    0H                                                               
         B     DMXIT                                                            
         EJECT                                                                  
*                                                                               
* *********************************************************************         
* THIS SI IT                                                                    
* *********************************************************************         
* PROCESS RECORD LOGIC - RECORD IN AREC                                         
DMXREC   DS    0H                                                               
         L     R6,AREC            POINT TO RECORD                               
         CLI   0(R6),X'02'                                                      
         BE    PACK                                                             
         CLI   0(R6),X'04'                                                      
         BE    UNIT                                                             
         B     DMXPURGE                                                         
*        CLI   0(R6),X'20'                                                      
*        BE    PLAN                                                             
*        CLI   0(R6),X'22'                                                      
*        BE    PROG                                                             
*        B     DMXPURGE                                                         
*                                                                               
PACK     DS    0H                                                               
         USING NPRECD,R6                                                        
         CLI   NPKAM,X'43'       AGENCY BDNY                                    
         BNE   DMXPURGE                                                         
         CLC   NPKCLT,=XL2'96FF' CLIENT FX                                      
         BNE   DMXPURGE                                                         
         B     DMXKEEP                                                          
*                                                                               
         LA    R5,STATAB                                                        
PK20     CLI   0(R5),X'FF'                                                      
         BE    DMXPURGE                                                         
         CLC   NPKEST,0(R5)                                                     
         BNE   PK30                                                             
         CLC   NPKNET,1(R5)                                                     
         BNE   PK30                                                             
         B     PK40                                                             
PK30     LA    R5,9(R5)                                                         
         B     PK20                                                             
*                                                                               
PK40     CLC   PKCOUNT,=F'50'                                                   
         BH    PK50                                                             
         GOTO1 =V(PRNTBL),DMCB,=C'BEFO',AREC,C'DUMP',150,=C'1D'                 
PK50     MVI   NPKAM,X'63'       NEW AGENCY                                     
         MVC   NPKCLT,=XL2'914D' NEW CLIENT                                     
         MVC   NPKNET,5(R5)      NEW NETWORK                                    
         CLC   PKCOUNT,=F'50'                                                   
         BH    DMXKEEP                                                          
         GOTO1 =V(PRNTBL),DMCB,=C'AFTE',AREC,C'DUMP',150,=C'1D'                 
         L     RE,PKCOUNT                                                       
         LA    RE,1(RE)                                                         
         ST    RE,PKCOUNT                                                       
         B     DMXKEEP                                                          
         DROP  R6                                                               
*                                                                               
*                                                                               
UNIT     DS    0H                                                               
         USING NURECD,R6                                                        
         CLI   NUKAM,X'43'       AGENCY BDNY                                    
         BNE   DMXPURGE                                                         
         CLC   NUKCLT,=XL2'96FF'   CLIENT FX                                    
         BNE   DMXPURGE                                                         
         B     DMXKEEP                                                          
*                                                                               
*                                                                               
         CLI   NUPRD2,0            NO PIGGYBACKS                                
         BNE   DMXPURGE                                                         
         CLI   NUPRD,X'44'         PROD APS ONLY                                
         BNE   DMXPURGE                                                         
*                                                                               
         LA    R5,STATAB                                                        
UN20     CLI   0(R5),X'FF'                                                      
         BE    DMXPURGE                                                         
         CLC   NUKEST,0(R5)                                                     
         BNE   UN30                                                             
         CLC   NUKNET,1(R5)                                                     
         BNE   UN30                                                             
         B     UN40                                                             
UN30     LA    R5,9(R5)                                                         
         B     UN20                                                             
*                                                                               
*  CHECK FOR BILLING OR PAYING                                                  
*                                                                               
UN40     GOTO1 =V(HELLO),DMCB,(C'G',UNTFILE),(X'10',AREC),0                     
         CLI   12(R1),0                                                         
         BE    DMXPURG2                                                         
         GOTO1 =V(HELLO),DMCB,(C'G',UNTFILE),(X'12',AREC),0                     
         CLI   12(R1),0                                                         
         BE    DMXPURG2                                                         
*                                                                               
         CLC   UNCOUNT,=F'50'                                                   
         BH    UN50                                                             
         GOTO1 =V(PRNTBL),DMCB,=C'BEFO',AREC,C'DUMP',150,=C'1D'                 
UN50     MVI   NUKAM,X'63'       NEW AGENCY                                     
         MVC   NUKCLT,=XL2'914D' NEW CLIENT                                     
         MVC   NUKNET,5(R5)      NEW NETWORK                                    
         MVC   NUALPHA,=CL2'OM'  ALPHA AGENCY                                   
         MVI   NUPRD,X'02'       ALCONVERT TO OM PROD APS                       
         CLC   UNCOUNT,=F'50'                                                   
         BH    DMXKEEP                                                          
         GOTO1 =V(PRNTBL),DMCB,=C'AFTE',AREC,C'DUMP',150,=C'1D'                 
         L     RE,UNCOUNT                                                       
         LA    RE,1(RE)                                                         
         ST    RE,UNCOUNT                                                       
         B     DMXKEEP                                                          
         DROP  R6                                                               
*                                                                               
         EJECT                                                                  
APRINT10 DS    0H                                                               
         L     R6,AREC            POINT TO RECORD                               
         USING RAVLREC,R6                                                       
*                                                                               
         CLI   RAVLKDET,0                                                       
         BE    DMXKEEP             NOT A DETAIL BYPASS                          
*                                                                               
         CLC   ACOUNT,=F'20'                                                    
         BH    APRINT20                                                         
         GOTO1 =V(PRNTBL),DMCB,=C'ABEF',AREC,C'DUMP',250,=C'1D'                 
*                                                                               
APRINT20 MVC   WORK2(98),RAVLDEL                                                
         MVC   RAVLDATE(92),WORK2+5                                             
*                                                                               
         EDIT  (B1,RAVLDINV),(2,HALF),FILL=0                                    
         MVC   RAVLDINV(2),HALF                                                 
         MVC   RAVLDINV+2(2),WORK2+3                                            
*                                                                               
         CLC   ACOUNT,=F'20'                                                    
         BH    APRINT30                                                         
         GOTO1 =V(PRNTBL),DMCB,=C'AVAI',AREC,C'DUMP',250,=C'1D'                 
*                                                                               
APRINT30 L     RF,ACOUNT                                                        
         LA    RF,1(RF)                                                         
         ST    RF,ACOUNT                                                        
         B     DMXKEEP                                                          
         DROP  R6                                                               
*                                                                               
         EJECT                                                                  
PPRINT10 DS    0H                                                               
         L     R6,AREC            POINT TO RECORD                               
         USING RPRPREC,R6                                                       
*                                                                               
         CLI   RPRPKDET,0                                                       
         BE    DMXKEEP             NOT A DETAIL BYPASS                          
*                                                                               
         CLC   PCOUNT,=F'20'                                                    
         BH    PPRINT20                                                         
         GOTO1 =V(PRNTBL),DMCB,=C'PBEF',AREC,C'DUMP',150,=C'1D'                 
*                                                                               
PPRINT20 MVC   WORK2(22),RPRPDEL                                                
         MVC   RPRPDATE(16),WORK2+5                                             
*                                                                               
         EDIT  (B1,RPRPDINV),(2,HALF),FILL=0                                    
         MVC   RPRPDINV(2),HALF                                                 
         MVC   RPRPDINV+2(2),WORK2+3                                            
*                                                                               
         CLC   PCOUNT,=F'20'                                                    
         BH    PPRINT30                                                         
         GOTO1 =V(PRNTBL),DMCB,=C'PRIN',AREC,C'DUMP',150,=C'1D'                 
*                                                                               
PPRINT30 L     RF,PCOUNT                                                        
         LA    RF,1(RF)                                                         
         ST    RF,PCOUNT                                                        
         B     DMXKEEP                                                          
         DROP  R6                                                               
*                                                                               
********************************************************************            
*                                                                               
         MVC   WORK(4),=X'0000E301'                                             
         GOTO1 =V(HELLO),DMCB,(C'D',=C'SPTFILE '),(X'DD',AREC),        X        
               =X'0000E301',0                                                   
         MVC   WORK(4),=X'0000C801'                                             
         GOTO1 =V(HELLO),DMCB,(C'D',=C'SPTFILE '),(X'DD',AREC),        X        
               =X'0000C801',0                                                   
         MVC   WORK(4),=X'0000E394'                                             
         GOTO1 =V(HELLO),DMCB,(C'D',=C'SPTFILE '),(X'DD',AREC),        X        
               =X'0000E394',0                                                   
         MVC   WORK(4),=X'0000C894'                                             
         GOTO1 =V(HELLO),DMCB,(C'D',=C'SPTFILE '),(X'DD',AREC),        X        
               =X'0000C894',0                                                   
***      GOTO1 =V(PRNTBL),DMCB,=C'AFT',AREC,C'DUMP',500,=C'1D'                  
         B     DMXKEEP                                                          
*                                                                               
         EJECT                                                                  
* END-OF-FILE LOGIC - LAST CALL HERE - RETURN ALLOWED                           
*                                                                               
DMXEOF   DS    0H                                                               
         MVI   SPACING+3,C'2'      PRINT A HEADLINE FOR TOTALS                  
         GOTO1 VPRINTER                                                         
         MVI   SPACING+3,C'1'                                                   
         MVC   P+10(26),=C'SUMMARY OF RECORDS CHANGED'                          
         L     R4,COUNT                                                         
         EDIT  (R4),(10,P+37)                                                   
         GOTO1 VPRINTER                                                         
         B     DMXIT                                                            
         EJECT                                                                  
* *******************************************************************           
* VARIABLE LIST                                                                 
* *******************************************************************           
UNTFILE  DC    CL8'UNTFILE'                                                     
DATADISP DC    H'0024'                                                          
PKCOUNT  DC    F'0'                                                             
UNCOUNT  DC    F'0'                                                             
PLCOUNT  DC    F'0'                                                             
PRCOUNT  DC    F'0'                                                             
ACOUNT   DC    F'0'                                                             
PCOUNT   DC    F'0'                                                             
COUNT    DC    F'0'                                                             
*                                                                               
STATAB   DC    XL1'96',CL8'ABC ABC '                                            
         DC    XL1'96',CL8'NBC NBC '                                            
         DC    XL1'96',CL8'FOX FOX '                                            
         DC    XL1'96',CL8'TPE RYS '                                            
         DC    XL1'96',CL8'IAS PAS '                                            
         DC    XL1'96',CL8'TCF TWT '                                            
         DC    XL1'97',CL8'CBS CBS '                                            
         DC    XL1'97',CL8'NBC NBC '                                            
         DC    XL1'97',CL8'BVT BVTV'                                            
         DC    XL1'98',CL8'ABC ABC '                                            
         DC    XL1'98',CL8'NBC NBC '                                            
         DC    XL1'99',CL8'LIF LIFE'                                            
         DC    XL1'99',CL8'NIK NAN '                                            
         DC    XL1'99',CL8'TDC DIS '                                            
         DC    XL1'99',CL8'TLC TLC '                                            
         DC    XL1'99',CL8'HBO CTV '                                            
         DC    XL1'99',CL8'TBS WTBS'                                            
         DC    XL1'99',CL8'NSFCSCI '                                            
         DC    X'FF'                                                            
*                                                                               
UNITTAB  DC    XL1'52',CL4'ARTS',XL1'01',XL4'C33EC39F'                          
         DC    XL1'52',CL4'CCL ',XL1'01',XL4'C33DC39F'                          
         DC    XL1'52',CL4'CMT ',XL1'01',XL4'C33DC39F'                          
         DC    XL1'52',CL4'CNN ',XL1'01',XL4'C33DC39F'                          
         DC    XL1'52',CL4'CNN ',XL1'02',XL4'C341C39F'                          
         DC    XL1'52',CL4'CNII',XL1'01',XL4'C33DC39F'                          
         DC    XL1'52',CL4'ESPN',XL1'01',XL4'C33DC39F'                          
         DC    XL1'52',CL4'ESPN',XL1'02',XL4'C33DC37E'                          
         DC    XL1'52',CL4'ESPN',XL1'03',XL4'C361C37E'                          
         DC    XL1'52',CL4'FAM ',XL1'01',XL4'C341C37E'                          
         DC    XL1'52',CL4'HGN ',XL1'01',XL4'C341C39F'                          
         DC    XL1'52',CL4'LIF ',XL1'01',XL4'C346C39F'                          
         DC    XL1'52',CL4'MTV ',XL1'01',XL4'C341C39F'                          
         DC    XL1'52',CL4'ORL ',XL1'01',XL4'C341C35F'                          
         DC    XL1'52',CL4'TBS ',XL1'01',XL4'C341C39F'                          
         DC    XL1'52',CL4'TBS ',XL1'02',XL4'C33DC39F'                          
         DC    XL1'52',CL4'TDC ',XL1'01',XL4'C33DC39F'                          
         DC    XL1'52',CL4'TNN ',XL1'01',XL4'C341C39F'                          
         DC    XL1'52',CL4'TNT ',XL1'01',XL4'C341C39F'                          
         DC    XL1'52',CL4'USA ',XL1'01',XL4'C33EC39F'                          
         DC    XL1'52',CL4'HCN ',XL1'01',XL4'0000FFFF'                          
*                                                                               
         DC    XL1'55',CL4'ABC ',XL1'01',XL4'C321C441'                          
         DC    XL1'55',CL4'ABC ',XL1'03',XL4'C321C39F'                          
         DC    XL1'55',CL4'ABC ',XL1'02',XL4'C321C441'                          
         DC    XL1'55',CL4'ABC ',XL1'05',XL4'C399C421'                          
         DC    XL1'55',CL4'ABC ',XL1'04',XL4'C321C39F'                          
         DC    XL1'55',CL4'FOX ',XL1'01',XL4'C321C42B'                          
*                                                                               
         DC    XL1'58',CL4'CBS ',XL1'01',XL4'C341C37E'                          
         DC    XL1'58',CL4'CBS ',XL1'02',XL4'C341C35E'                          
*                                                                               
         DC    XL1'51',CL4'ESN ',XL1'01',XL4'C361C422'                          
         DC    XL1'51',CL4'ESN ',XL1'04',XL4'C361C422'                          
         DC    XL1'51',CL4'ESN ',XL1'03',XL4'C361C422'                          
         DC    XL1'51',CL4'ESN ',XL1'05',XL4'C33CC35F'                          
         DC    XL1'51',CL4'ESN ',XL1'07',XL4'C381C39F'                          
         DC    XL1'51',CL4'ESN ',XL1'09',XL4'C381C39F'                          
         DC    XL1'51',CL4'ESN ',XL1'08',XL4'C381C39F'                          
         DC    XL1'51',CL4'EEP ',XL1'01',XL4'C33CC343'                          
         DC    X'FF'                                                            
         SPACE                                                                  
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
         SPACE                                                                  
         LTORG                                                                  
         SPACE 2                                                                
WORKD    DSECT                                                                  
DUB      DS    D                                                                
DMCB     DS    6F                                                               
APARM    DS    A                                                                
*                                                                               
PLIST    DS    0CL24                                                            
AREC     DS    A                                                                
VTAPEOUT DS    A                                                                
APARAMC  DS    A                                                                
VLDDEFN  DS    A                                                                
VPRINTER DS    A                                                                
VCPRINT  DS    A                                                                
*                                                                               
ELCODE   DS    CL1                                                              
BYTE     DS    CL1                                                              
WORK     DS    CL100                                                            
WORK2    DS    CL100                                                            
HALF     DS    H                                                                
INV      DS    CL3                                                              
WORKX    EQU   *                                                                
*                                                                               
         EJECT                                                                  
         PRINT OFF                                                              
*DMLDDEFN                                                                       
       ++INCLUDE DMLDDEFN                                                       
         EJECT                                                                  
*DDDPRINT                                                                       
       ++INCLUDE DDDPRINT                                                       
         EJECT                                                                  
         PRINT ON                                                               
*SPGENPRD                                                                       
       ++INCLUDE NEGENPACK                                                      
       ++INCLUDE NEGENUNIT                                                      
       ++INCLUDE NEGENPLAN                                                      
       ++INCLUDE NEGENPUA                                                       
       ++INCLUDE REGENAVLNN                                                     
       ++INCLUDE REGENPRPNN                                                     
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'012UNLDJWNY  08/10/00'                                      
         END                                                                    
