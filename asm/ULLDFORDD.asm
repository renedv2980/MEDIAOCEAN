*          DATA SET ULLDFORDD  AT LEVEL 001 AS OF 07/29/97                      
*          DATA SET UNLDFORD   AT LEVEL 024 AS OF 07/28/97                      
*          DATA SET UNLDXSP2   AT LEVEL 081 AS OF 05/08/97                      
*          DATA SET UNLDXSP    AT LEVEL 023 AS OF 11/16/93                      
*PHASE UNXFDD,+0                                                                
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
         B     DMXIT                                                            
         SPACE 1                                                                
*        GOTO1 =V(PRNTBL),DMCB,=C'BYPS',AREC,C'DUMP',30,=C'1D'                  
DMXPURGE L     R1,APARM            PURGE RECORD EXIT                            
         MVI   0(R1),X'FF'                                                      
         MVI   8(R1),0                                                          
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
*        CLI   0(R6),X'02'                                                      
*        BE    PACK                                                             
         CLI   0(R6),X'04'                                                      
         BE    UNIT                                                             
         B     DMXKEEP                                                          
*        CLI   0(R6),X'20'                                                      
*        BE    PLAN                                                             
*        CLI   0(R6),X'22'                                                      
*        BE    PROG                                                             
*        B     DMXPURGE                                                         
*                                                                               
PACK     DS    0H                                                               
         USING NPRECD,R6                                                        
         CLI   NPKAM,X'13'       AGENCY JWNY                                    
         BNE   DMXPURGE                                                         
         CLC   NPKCLT,=XL2'947F' FORD CLIENT                                    
         BNE   DMXPURGE                                                         
*                                                                               
         LA    RE,UNITTAB                                                       
PK20     CLI   0(RE),X'FF'                                                      
         BE    DMXPURGE                                                         
         CLC   NPKEST,0(RE)                                                     
         BNE   PK30                                                             
         CLC   NPKNET,1(RE)                                                     
         BNE   PK30                                                             
         CLC   NPKPACK,5(RE)                                                    
         BE    PK40                                                             
PK30     LA    RE,10(RE)                                                        
         B     PK20                                                             
*                                                                               
PK40     CLC   PKCOUNT,=F'50'                                                   
         BH    PK50                                                             
         GOTO1 =V(PRNTBL),DMCB,=C'BEFO',AREC,C'DUMP',150,=C'1D'                 
PK50     MVI   NPKAM,X'33'       NEW AGENCY                                     
         MVC   NPKCLT,=XL2'B0A3' NEW CLIENT                                     
         CLC   PKCOUNT,=F'50'                                                   
         BH    DMXKEEP                                                          
         GOTO1 =V(PRNTBL),DMCB,=C'AFTE',AREC,C'DUMP',150,=C'1D'                 
         L     RE,PKCOUNT                                                       
         LA    RE,1(RE)                                                         
         ST    RE,PKCOUNT                                                       
         B     DMXKEEP                                                          
         DROP  R6                                                               
*                                                                               
UNIT     DS    0H                                                               
         USING NURECD,R6                                                        
         CLI   NUKAM,X'33'       AGENCY FDMJW                                   
         BNE   DMXKEEP                                                          
         CLC   NUKCLT,=XL2'B0A3' FORD CLIENT                                    
         BNE   DMXKEEP                                                          
         B     UN40                                                             
*                                                                               
         LA    RE,UNITTAB                                                       
UN20     CLI   0(RE),X'FF'                                                      
         BE    DMXPURGE                                                         
         CLC   NUKEST,0(RE)                                                     
         BNE   UN30                                                             
         CLC   NUKNET,1(RE)                                                     
         BNE   UN30                                                             
         CLC   NUPACK,5(RE)                                                     
         BE    UN40                                                             
*        CLC   NUKDATE,6(RE)                                                    
*        BL    UN30                                                             
*        CLC   NUKDATE,8(RE)                                                    
*        BH    UN30                                                             
*        B     UN40                                                             
UN30     LA    RE,10(RE)                                                        
         B     UN20                                                             
*                                                                               
UN40     CLC   NUALPHA,=CL2'JW'                                                 
         BNE   DMXKEEP                                                          
         CLC   UNCOUNT,=F'50'                                                   
         BH    UN50                                                             
         GOTO1 =V(PRNTBL),DMCB,=C'BEFO',AREC,C'DUMP',150,=C'1D'                 
UN50     MVC   NUALPHA,=CL2'FR'  NEW AGENCY                                     
*        MVC   NUKCLT,=XL2'B0A3' NEW CLIENT                                     
         CLC   UNCOUNT,=F'50'                                                   
         BH    DMXKEEP                                                          
         GOTO1 =V(PRNTBL),DMCB,=C'AFTE',AREC,C'DUMP',150,=C'1D'                 
         L     RE,UNCOUNT                                                       
         LA    RE,1(RE)                                                         
         ST    RE,UNCOUNT                                                       
         B     DMXKEEP                                                          
         DROP  R6                                                               
*                                                                               
PLAN     DS    0H                                                               
         USING NPLRECD,R6                                                       
         CLI   NPLKAM,X'13'      AGENCY JWNY                                    
         BNE   DMXPURGE                                                         
         CLC   NPLKCLT,=XL2'947F' FORD CLIENT                                   
         BNE   DMXPURGE                                                         
*                                                                               
         LA    RE,PUPTAB                                                        
PL20     CLI   0(RE),X'FF'                                                      
         BE    DMXPURGE                                                         
         CLC   NPLKNET,0(RE)                                                    
         BNE   PL30                                                             
         CLC   NPLKPLAN,4(RE)                                                   
         BE    PL40                                                             
PL30     LA    RE,8(RE)                                                         
         B     PL20                                                             
*                                                                               
PL40     CLC   PLCOUNT,=F'50'                                                   
         BH    PL50                                                             
         GOTO1 =V(PRNTBL),DMCB,=C'BEFO',AREC,C'DUMP',50,=C'1D'                  
PL50     MVI   NPLKAM,X'33'      NEW AGENCY                                     
         MVC   NPLKCLT,=XL2'B0A3' NEW CLIENT                                    
         CLC   PLCOUNT,=F'50'                                                   
         BH    DMXKEEP                                                          
         GOTO1 =V(PRNTBL),DMCB,=C'AFTE',AREC,C'DUMP',50,=C'1D'                  
         L     RE,PLCOUNT                                                       
         LA    RE,1(RE)                                                         
         ST    RE,PLCOUNT                                                       
         B     DMXKEEP                                                          
         DROP  R6                                                               
*                                                                               
PROG     DS    0H                                                               
         USING NPURECD,R6                                                       
         CLI   NPUKAM,X'13'      AGENCY JWNY                                    
         BNE   DMXPURGE                                                         
         CLC   NPUKCLT,=XL2'947F' FORD CLIENT                                   
         BNE   DMXPURGE                                                         
*                                                                               
         LA    RE,PUPTAB                                                        
PR20     CLI   0(RE),X'FF'                                                      
         BE    DMXPURGE                                                         
         CLC   NPUKNET,0(RE)                                                    
         BNE   PR30                                                             
         CLC   NPUKPLAN,4(RE)                                                   
         BE    PR40                                                             
PR30     LA    RE,8(RE)                                                         
         B     PR20                                                             
*                                                                               
PR40     CLC   PRCOUNT,=F'50'                                                   
         BH    PR50                                                             
         GOTO1 =V(PRNTBL),DMCB,=C'BEFO',AREC,C'DUMP',50,=C'1D'                  
PR50     MVI   NPUKAM,X'33'       NEW AGENCY                                    
         MVC   NPUKCLT,=XL2'B0A3' NEW CLIENT                                    
         CLC   PRCOUNT,=F'50'                                                   
         BH    DMXKEEP                                                          
         GOTO1 =V(PRNTBL),DMCB,=C'AFTE',AREC,C'DUMP',50,=C'1D'                  
         L     RE,PRCOUNT                                                       
         LA    RE,1(RE)                                                         
         ST    RE,PRCOUNT                                                       
         B     DMXKEEP                                                          
         DROP  R6                                                               
                                                                                
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
DATADISP DC    H'0024'                                                          
PKCOUNT  DC    F'0'                                                             
UNCOUNT  DC    F'0'                                                             
PLCOUNT  DC    F'0'                                                             
PRCOUNT  DC    F'0'                                                             
ACOUNT   DC    F'0'                                                             
PCOUNT   DC    F'0'                                                             
COUNT    DC    F'0'                                                             
*                                                                               
PUPTAB   DC    CL8'ABC 8PRM'                                                    
         DC    CL8'ABC 8NWS'                                                    
         DC    CL8'ABC 8MAZ'                                                    
         DC    CL8'ABC 8NMZ'                                                    
         DC    CL8'ABC 8EMZ'                                                    
         DC    CL8'ABC 8LMD'                                                    
*                                                                               
         DC    CL8'CBS 8FD '                                                    
         DC    CL8'CBS LATE'                                                    
         DC    CL8'CBS 8PRM'                                                    
         DC    CL8'CBS 8MAZ'                                                    
         DC    CL8'CBS 8LAT'                                                    
         DC    CL8'CBS 8LMD'                                                    
*                                                                               
         DC    CL8'NBC 8PRM'                                                    
         DC    CL8'NBC 8LAT'                                                    
         DC    CL8'NBC 8MAZ'                                                    
         DC    CL8'NBC MAZ '                                                    
*                                                                               
         DC    CL8'FOX 8PRM'                                                    
         DC    CL8'FOX B8PR'                                                    
         DC    CL8'FOX 8MAZ'                                                    
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
**PAN#1  DC    CL21'001ULLDFORDD 07/29/97'                                      
         END                                                                    
