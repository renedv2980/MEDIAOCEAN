*          DATA SET REREP9202S AT LEVEL 054 AS OF 07/10/98                      
*PHASE RE9202A,*                                                                
*INCLUDE SORTER                                                                 
         TITLE 'REREP9202 - RE9202 - DARE SPOT COUNT'                           
*                                                                               
*********************************************************************           
*                                                                   *           
*        REREP9202 --- DARE SPOT COUNT                              *           
*                                                                   *           
* ----------------------------------------------------------------- *           
* UPDATE HISTORY:                                                   *           
*                                                                   *           
* 06OCT97 RHV -- VOILA!                                             *           
*                                                                   *           
* JAN28/98 (JRD) --- 4K CONTRACTS                                   *           
*                                                                   *           
*********************************************************************           
*                                                                   *           
* REQUEST CARD OPTION DEFINITION:                                   *           
*   OPTION 1: INPUT FROM (D)UMP TAPE OR (G)ENERATIONAL DATA TAPE    *           
*   OPTION 2: OUTPUT (S)UMMARY OR (D)ETAIL OR DO(W)NLOAD REPORT     *           
*   OPTION 3: CREATE GENERATIONAL DATA TAPE (Y)ES/(N)O              *           
*                                                                   *           
*********************************************************************           
RE9202   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 STOREX-STORED,*RE9202*,R8,RR=R5                                  
         USING STORED,RC                                                        
         ST    R5,RELO                                                          
         L     RA,0(R1)                                                         
         USING WORKD,RA                                                         
         L     R9,FILEC                                                         
         USING FILED,R9                                                         
                                                                                
*                                                                               
         CLI   MODE,REQFRST        ONLY MODE SUPPORTED                          
         BE    REQF                                                             
                                                                                
         B     EXIT                                                             
                                                                                
NZ       LTR   RB,RB               CC: NOT ZERO                                 
         B     EXIT                                                             
Z        SR    R0,R0               CC: ZERO                                     
EXIT     XMOD1                                                                  
                                                                                
**********************************************************************          
* REQFRST - PROCESS DOWNLOAD REPORT REQUEST                                     
**********************************************************************          
REQF     DS    0H                                                               
         GOTOX LOADER,DMCB,=CL8'T00AAC',0                                       
         MVC   REPFACS,4(R1)       EXTERNAL ROUTINE, MISC. SUBROUTINES          
         MVC   RFBLOCK(4),ACOMFACT                                              
*                                                                               
* VALIDATE OPTION COMBINATIONS                                                  
*                                                                               
         CLI   RCDNLOAD,C'Y'                                                    
         BNE   REQF010                                                          
         CLI   QOPTION2,C'W'                                                    
         BE    REQF020                                                          
         DC    H'0'                DOWNLOAD=YES, NOT OPT2=W                     
REQF010  DS    0H                                                               
         CLI   QOPTION2,C'W'                                                    
         BNE   *+6                                                              
         DC    H'0'                NO DOWNLOAD=YES, OPT2=W                      
REQF020  DS    0H                                                               
         CLI   QOPTION1,C'G'                                                    
         BNE   REQF030                                                          
         CLI   QOPTION3,C'Y'                                                    
         BNE   *+6                                                              
         DC    H'0'                OPT1=G, OPT3=Y                               
*                                                                               
REQF030  DS    0H                                                               
         LA    RE,HOOK                                                          
         ST    RE,HEADHOOK                                                      
*                                                                               
         BAS   RE,GETCON           READ TAPE FOR CONTRACTS                      
         BAS   RE,PROCCON          PROCESS CONTRACTS, GENERATE REPORT           
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* GETCON - READ TAPE FOR CONTRACTS & PASS TO SORTER                             
**********************************************************************          
GETCON   NTR1                                                                   
*                                                                               
* INTIALIZE                                                                     
*                                                                               
         XC    SPOTS,SPOTS                                                      
*                                                                               
         CLI   QOPTION1,C'D'       DUMP TAPE INPUT?                             
         BE    GC050               YES                                          
         CLI   QOPTION1,C'G'       GENERATIONAL TAPE IN?                        
         BE    *+6                                                              
         DC    H'0'                INVALID INPUT TYPE                           
         OPEN  FILING              OPEN GENERATIONAL TAPE                       
         B     EXIT                                                             
*                                                                               
GC050    DS    0H                                                               
         GOTOX =V(SORTER),DMCB,SORTCARD,RECCARD   INITL SORTER                  
         OPEN  FILIND                             OPEN TAPE                     
*                                                                               
         OC    QSTART(12),SPACES                                                
         CLC   QSTART,SPACES                                                    
         BNE   *+6                                                              
         DC    H'0'                MUST HAVE START DATE                         
         CLC   QEND,SPACES                                                      
         BNE   *+6                                                              
         DC    H'0'                MUST HAVE END DATE                           
         GOTOX DATCON,DMCB,(0,QSTART),(2,SDATE)   COMPRESS START DATE           
         GOTOX DATCON,DMCB,(0,QEND),(2,EDATE)     COMPRESS END DATE             
*                                                                               
* GET TO 1ST CONTRACT REC                                                       
*                                                                               
GC100    DS    0H                                                               
         LA    R7,IOAREA                                                        
         USING RCONREC,R7                                                       
         GET   FILIND,IOAREA-4                                                  
         CLI   RCONREC,X'0C'       CONTRACT REC HIT?                            
         BNE   GC100                                                            
*                                                                               
* LOOP THRU CONTRACT RECS                                                       
*                                                                               
         B     GC210                                                            
GC200    DS    0H                                                               
         GET   FILIND,IOAREA-4                                                  
         CLI   RCONREC,X'0C'       DONE W/CONTRACTS?                            
         BNE   GC500               YES                                          
GC210    DS    0H                                                               
         BAS   RE,CKREP            CHECK REP AGAINST SKIP TABLE                 
         BNZ   GC200               IN TABLE, SKIP CON                           
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1D'                                                     
         BAS   RE,GETEL            DARE 1D ELEM?                                
         BNE   GC200               NO-SKIP                                      
         USING RCONDREL,R6                                                      
         MVC   DARENUM,RCONDRLK                                                 
         DROP  R6                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1F'                                                     
         BAS   RE,GETEL            1F ELEM?                                     
         BNE   GC200               NO-SKIP                                      
         USING RCONXEL,R6                                                       
         TM    RCONCONF,X'40'+X'20'  CONFIRMED NOW OR PREVIOUSLY?               
         BZ    GC200               NO-SKIP                                      
         DROP  R6                                                               
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'2D'                                                     
         BAS   RE,GETEL            DARE 2D ELEM?                                
         BNE   GC200               NO-SKIP                                      
***>     BNE   GC250               NO-CHECK CONF DATES IN 22 ELEM               
         USING RCON2DEL,R6                                                      
         CLC   RCON2DDT,SDATE      1ST CONF VS. START DATE?                     
         BL    GC200               BEFORE START - SKIP                          
         CLC   RCON2DDT,EDATE      1ST CONF VS. END DATE?                       
         BH    GC200               AFTER END - SKIP                             
         ZICM  RE,RCON2DTS,3       SAVE OFF SPOTS AT 1ST CONF                   
         ST    RE,SPOTS                                                         
         B     GC300                                                            
         DROP  R6                                                               
*                                                                               
GC250    DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'22'                                                     
         BAS   RE,GETEL            22 MOD DATE/TIME ELEM?                       
         BNE   GC200               NO-DON'T HAVE EITHER ELEM, SKIP REC          
         USING RMODELEM,R6                                                      
         LA    R3,RMODEL3D         OLDEST CONF DATE                             
         LA    R4,RMODEL1D         NEWEST CONF DATE                             
GC260    DS    0H                                                               
         OC    0(2,R3),0(R3)       HAVE A CONF DATE?                            
         BNZ   GC270               YES - CHECK IT                               
         CR    R3,R4               AT NEWEST DATE?                              
         BNH   GC260               NO CONFIRM HIST, SKIP REC                    
         SH    R3,=H'10'           BACK UP 1 CONFIRM DATE                       
         B     GC260                                                            
GC270    DS    0H                                                               
         CLC   0(2,R3),SDATE       CONF DATE VS. START                          
         BL    GC200                                                            
         CLC   0(2,R3),EDATE       CONF DATE VS. END                            
         BH    GC200                                                            
         DROP  R6                                                               
*                                                                               
GC300    DS    0H                  BUILD SORT RECORD FOR CONTRACT               
         LA    R3,WORK                                                          
         USING SORTD,R3                                                         
         XC    SORTREC,SORTREC                                                  
         MVC   SREP,RCONKREP                                                    
         MVC   SAGY,RCONKAGY                                                    
         MVC   SADV,RCONKADV                                                    
         MVC   SSTA,RCONKSTA                                                    
         MVC   SDARE,DARENUM                                                    
         MVC   SNUM,RCONKCON                                                    
         MVC   SSPOTS,SPOTS                                                     
*                                                                               
         GOTOX =V(SORTER),DMCB,=C'PUT',SORTREC                                  
         XC    SPOTS,SPOTS                                                      
         B     GC200               BACK FOR NEXT RECORD                         
         DROP  R3                                                               
*                                                                               
* WRAP UP                                                                       
*                                                                               
GC500    DS    0H                                                               
EOFD     DS    0H                                                               
         CLOSE FILIND              DONE WITH TAPE                               
         B     EXIT                                                             
         DROP  R7                                                               
**********************************************************************          
* PROCCON - RETRIEVE SORT RECORDS & OUTPUT REPORT                               
**********************************************************************          
PROCCON  NTR1                                                                   
*                                                                               
* INITIALIZE                                                                    
*                                                                               
         CLI   QOPTION3,C'N'       CREATE TAPE?                                 
         BE    PC020               NO                                           
         CLI   QOPTION3,C'Y'                                                    
         BE    *+6                                                              
         DC    H'0'                INVALID OPTION                               
         OPEN  (FILOUT,OUTPUT)                                                  
*                                                                               
PC020    DS    0H                                                               
         XC    LASTREP,LASTREP                                                  
         XC    AGYTOT,AGYTOT                                                    
         XC    ADVTOT,ADVTOT                                                    
         XC    REPTOT,REPTOT                                                    
         XC    GTOT,GTOT                                                        
*                                                                               
* RETRIEVE CONTRACTS FROM GEN DATASET TAPE                                      
*                                                                               
PC050    DS    0H                                                               
         CLI   QOPTION1,C'D'                                                    
         BE    PC060                                                            
         CLI   QOPTION1,C'G'                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         GET   FILING,IOAREA                                                    
         LA    R3,IOAREA                                                        
         USING SORTREC,R3                                                       
         MVC   SPOTS,SSPOTS                                                     
         B     PC200                                                            
*                                                                               
* RETRIEVE CONTRACTS FROM SORTER                                                
*                                                                               
PC060    DS    0H                                                               
         GOTOX =V(SORTER),DMCB,=C'GET'                                          
         OC    DMCB+4(4),DMCB+4    HAVE RECORD?                                 
         BNZ   PC100               YES - PROCESS IT                             
*                                                                               
EOFG     DS    0H                                                               
         CLI   QOPTION1,C'G'       GEN TAPE INPUT?                              
         BNE   PC090               NO                                           
         CLOSE FILING                                                           
PC090    DS    0H                                                               
         CLI   RCDNLOAD,C'Y'       DOWNLOAD?                                    
         BE    PC550               YES - NO TOTALS, ALL DONE                    
         B     PC500               PRINT GRAND TOTAL                            
*                                                                               
* COUNT BUYS FOR CONTRACT                                                       
*                                                                               
PC100    DS    0H                                                               
         L     R3,DMCB+4                                                        
*                                                                               
***>     BAS   RE,CKREP            CHECK REP AGAINST SKIP TABLE                 
***>     BNZ   PC050               IN TABLE, SKIP CON                           
*                                                                               
         OC    SSPOTS,SSPOTS       ALREADY HAVE SPOTS?                          
         BZ    PC105               NO, COUNT BUYS                               
         MVC   SPOTS,SSPOTS                                                     
         B     PC200                                                            
PC105    DS    0H                                                               
         XC    SPOTS,SPOTS                                                      
*                                                                               
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RBUYREC,R6                                                       
         MVI   RBUYKTYP,X'0B'                                                   
         MVC   RBUYKREP,SREP                                                    
         GOTOX (RFCONNUM,REPFACS),DMCB,(1,SNUM),(3,RBUYKCON)                    
*                                                                               
         MVC   KEYSAVE(27),KEY     SAVE KEY FOR COMPARE                         
         GOTOX DATAMGR,DMCB,DMRDHI,REPDIR,KEY,KEY,0                             
         B     PC120                                                            
*                                                                               
PC110    DS    0H                                                               
         GOTOX DATAMGR,DMCB,DMRSEQ,REPDIR,KEY,KEY,0                             
PC120    DS    0H                                                               
         CLC   KEY(22),KEYSAVE     BUY FOR THIS CONTRACT?                       
         BNE   PC200               NO - DONE READING BUYS                       
         CLC   =X'FFFF',KEY+25     PLAN RECORD?                                 
         BE    PC110               YES-SKIP THIS BUY                            
         LA    R6,IOAREA                                                        
         GOTOX DATAMGR,DMCB,GETREC,REPFILE,KEY+28,RBUYREC,DMWORK                
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   HALF,RBUYTSPT                                                    
         LH    R5,HALF                                                          
         A     R5,SPOTS            ADD TO SPOT COUNT                            
         ST    R5,SPOTS                                                         
         B     PC110               NEXT BUYREC                                  
         DROP  R6                                                               
*                                                                               
PC200    DS    0H                                                               
***>     BAS   RE,CKREP            CHECK REP AGAINST SKIP TABLE                 
***>     BNZ   PC050               IN TABLE, SKIP CON                           
*                                                                               
* WRITE TO GEN DATASET TAPE                                                     
*                                                                               
         CLI   QOPTION3,C'Y'       CREATE TAPE?                                 
         BNE   PC250               NO                                           
         MVC   SSPOTS,SPOTS        PUT SPOTS IN RECORD                          
         PUT   FILOUT,SORTREC                                                   
*                                                                               
* PROCESS RECORD FOR DOWNLOADING                                                
*                                                                               
PC250    DS    0H                                                               
         CLI   RCDNLOAD,C'Y'       DOWNLOAD?                                    
         BNE   PC300               NO-USE PRINTING ROUTINE                      
*                                                                               
         LA    R4,P                                                             
         USING DLINE,R4                                                         
         MVC   DREP,SREP                                                        
         MVC   DADV,SADV                                                        
         GOTOX (RFSTAOUT,REPFACS),DMCB,SSTA,(X'01',DSTA)                        
         GOTOX (RFAGYOUT,REPFACS),DMCB,SAGY,DAGY,0,RFBLOCK                      
         GOTOX (RFCONNUM,REPFACS),DMCB,(1,SDARE),(6,DDARE)                      
         GOTOX (RFCONNUM,REPFACS),DMCB,(1,SNUM),(6,DNUM)                        
         EDIT  SPOTS,(8,DSPOT),ZERO=NOBLANK,FLOAT=-,ALIGN=LEFT                  
         BAS   RE,LOCALREP                                                      
         B     PC050               NEXT CONTRACT                                
         DROP  R4                                                               
*                                                                               
* PROCESS RECORD FOR PRINTING                                                   
*                                                                               
PC300    DS    0H                                                               
         OC    LASTREP,LASTREP                                                  
         BNZ   PC310                                                            
         MVC   LASTREP,SREP        INITIALIZE LAST REP/ADV/AGY                  
         MVC   LASTADV,SADV                                                     
         MVC   LASTAGY,SAGY                                                     
*                                                                               
PC310    DS    0H                                                               
         CLC   SREP,LASTREP        DIFFERENT REP?                               
         BE    PC320               NO - SKIP REP SUBTOTALING                    
         CLI   QOPTION2,C'S'       SUMMARY?                                     
         BE    *+8                 DON'T BREAK BY ADV                           
         BAS   RE,DOADVSUB                                                      
PC315    BAS   RE,DOAGYSUB                                                      
         BAS   RE,DOREPSUB                                                      
         MVI   FORCEHED,C'Y'                                                    
         BAS   RE,LOCALREP                                                      
         B     PC350                                                            
*                                                                               
PC320    DS    0H                                                               
         CLC   SAGY,LASTAGY        DIFFERENT AGENCY?                            
         BE    PC330               NO - SKIP AGENCY SUBTOTALING                 
         CLI   QOPTION2,C'S'       SUMMARY?                                     
         BE    *+8                 DON'T BREAK BY ADV                           
         BAS   RE,DOADVSUB                                                      
         BAS   RE,DOAGYSUB                                                      
         CLI   QOPTION2,C'S'       SUMMARY?                                     
         BE    *+8                 DON'T PAGE EJECT                             
         MVI   FORCEHED,C'Y'                                                    
         B     PC350                                                            
*                                                                               
PC330    DS    0H                                                               
         CLI   QOPTION2,C'S'       SUMMARY?                                     
         BE    PC350               DON'T BREAK BY ADV                           
         CLC   SADV,LASTADV        DIFFERENT ADVERTISER?                        
         BE    PC350               NO - SKIP ADVERTISER SUBTOTALING             
         BAS   RE,DOADVSUB                                                      
         BAS   RE,LOCALREP                                                      
         B     PC350                                                            
*                                                                               
PC350    DS    0H                                                               
         CLI   QOPTION2,C'S'       SUMMARY?                                     
         BE    PC400               SKIP DETAIL                                  
         LA    R4,P                                                             
         USING PLINE,R4                                                         
*                                                                               
         MVC   PREP,SREP                                                        
         GOTOX (RFAGYOUT,REPFACS),DMCB,SAGY,PAGY,0,RFBLOCK                      
         MVC   PADV,SADV                                                        
         GOTOX (RFSTAOUT,REPFACS),DMCB,SSTA,(X'01',PSTA)                        
         GOTOX HEXOUT,DMCB,SDARE,PDARE,4                                        
         GOTOX (RFCONNUM,REPFACS),DMCB,(1,SNUM),(5,PNUM)                        
         EDIT  SPOTS,(8,PSPOT),ZERO=NOBLANK,FLOAT=-                             
         BAS   RE,LOCALREP                                                      
*                                                                               
PC400    DS    0H                                                               
         L     R5,SPOTS            INCREMENT REP SUBTOTALS                      
         A     R5,REPTOT                                                        
         ST    R5,REPTOT                                                        
*                                                                               
         L     R5,SPOTS            INCREMENT AGY SUBTOTALS                      
         A     R5,AGYTOT                                                        
         ST    R5,AGYTOT                                                        
*                                                                               
         L     R5,SPOTS            INCREMENT ADV SUBTOTALS                      
         A     R5,ADVTOT                                                        
         ST    R5,ADVTOT                                                        
*                                                                               
         L     R5,SPOTS            INCREMENT GRAND TOTAL                        
         A     R5,GTOT                                                          
         ST    R5,GTOT                                                          
*                                                                               
         B     PC050               NEXT CONTRACT                                
*                                                                               
* GRAND TOTAL DISPLAY                                                           
*                                                                               
PC500    DS    0H                  FINAL REP SUBTOTAL                           
         LA    R3,HEAD1            JUST POINT R3 TO A VALID ADDRESS             
         CLI   QOPTION2,C'S'       SUMMARY?                                     
         BE    *+8                 SKIP ADV TOTAL                               
         BAS   RE,DOADVSUB                                                      
         BAS   RE,DOAGYSUB                                                      
         BAS   RE,DOREPSUB                                                      
         BAS   RE,LOCALREP                                                      
*                                                                               
         LA    R4,PSECOND                                                       
         MVC   P(PLEN+10),EQUALS      GRAND TOTAL                               
         MVC   PTHIRD(PLEN+10),EQUALS                                           
         EDIT  GTOT,(10,PSPOT+8),ZERO=NOBLANK,COMMAS=YES,FLOAT=-                
         MVC   0(11,R4),=C'GRAND TOTAL'                                         
         BAS   RE,LOCALREP                                                      
*                                                                               
PC550    DS    0H                                                               
         CLI   QOPTION3,C'Y'       CREATE TAPE?                                 
         BNE   EXIT                NO                                           
         CLOSE FILOUT                                                           
         B     EXIT                ALL DONE                                     
*                                                                               
* SUBTOTAL DISPLAY ROUTINES                                                     
*                                                                               
DOREPSUB NTR1                      DISPLAY REP SUBTOTAL                         
         LA    R4,PSECOND                                                       
         EDIT  REPTOT,(10,PSPOT+8),ZERO=NOBLANK,COMMAS=YES                      
         MVC   0(17,R4),=C'SUBTOTAL FOR REP '                                   
         MVC   17(2,R4),LASTREP                                                 
         BAS   RE,GOREPORT                                                      
         MVC   LASTREP,SREP                                                     
         XC    REPTOT,REPTOT                                                    
         B     EXIT                                                             
*                                                                               
DOAGYSUB NTR1                      DISPLAY AGENCY SUBTOTAL                      
         CLI   QOPTION2,C'S'       SUMMARY?                                     
         BNE   DOAGYS10                                                         
         LA    R4,P                                                             
         EDIT  AGYTOT,(10,PSPOT+8),ZERO=NOBLANK,COMMAS=YES                      
         MVC   0(15,R4),=C'REP     AGENCY '                                     
         MVC   4(2,R4),LASTREP                                                  
         MVC   RFBLOCK+4(2),LASTREP                                             
         GOTOX (RFAGYOUT,REPFACS),DMCB,LASTAGY,15(R4),24(R4),RFBLOCK            
         BAS   RE,LOCALREP                                                      
         B     DOAGYS20                                                         
DOAGYS10 LA    R4,PSECOND                                                       
         EDIT  AGYTOT,(10,PSPOT+8),ZERO=NOBLANK,COMMAS=YES                      
         MVC   0(20,R4),=C'SUBTOTAL FOR AGENCY '                                
         MVC   RFBLOCK+4(2),LASTREP                                             
         GOTOX (RFAGYOUT,REPFACS),DMCB,LASTAGY,20(R4),29(R4),RFBLOCK            
         BAS   RE,GOREPORT                                                      
DOAGYS20 MVC   LASTAGY,SAGY                                                     
         XC    AGYTOT,AGYTOT                                                    
         B     EXIT                                                             
*                                                                               
DOADVSUB NTR1                      DISPLAY ADVERTISER SUBTOTAL                  
         LA    R4,PSECOND                                                       
         EDIT  ADVTOT,(8,PSPOT),ZERO=NOBLANK,COMMAS=YES                         
         MVC   0(24,R4),=C'SUBTOTAL FOR ADVERTISER '                            
         MVC   24(4,R4),LASTADV                                                 
         MVC   P(PLEN),DASHES                                                   
         MVC   PTHIRD(PLEN),DASHES                                              
         BAS   RE,LOCALREP                                                      
         MVC   LASTADV,SADV                                                     
         XC    ADVTOT,ADVTOT                                                    
         B     EXIT                                                             
*                                                                               
GOREPORT NTR1                      FORMAT SUBTOT AREA & CALL REPORTER           
         MVC   P(PLEN+10),DASHES                                                
         MVC   PTHIRD(PLEN+10),DASHES                                           
         BAS   RE,LOCALREP                                                      
         B     EXIT                                                             
*                                                                               
         DROP  R3,R4                                                            
**********************************************************************          
* HEAD - COLUMN HEADERS                                                         
**********************************************************************          
HOOK     NTR1                                                                   
         CLI   RCDNLOAD,C'Y'       DOWNLOAD?                                    
         BE    EXIT                YES - NO HEADERS                             
*                                                                               
         MVC   HEAD1(L'RREPNAME),RREPNAME                                       
         MVC   HEAD2(L'RREPADDR),RREPADDR                                       
*                                                                               
         MVC   HEAD1+38(22),=C'DARE SPOT COUNT REPORT'                          
         GOTO1 DATCON,DMCB,(0,QSTART),(6,HEAD1+61)                              
*                                                                               
         CLI   QOPTION2,C'S'       SUMMARY?                                     
         BNE   HOOK10                                                           
         MVC   HEAD2+38(29),=C'--------- (SUMMARY) ---------'                   
         B     HOOK20                                                           
HOOK10   MVC   HEAD2+38(29),=C'-----------------------------'                   
*                                                                               
HOOK20   MVC   HEAD1+101(4),=C'PAGE'                                            
         EDIT  PAGE,(2,HEAD1+106)                                               
*                                                                               
         GOTOX DATCON,DMCB,(5,0),(11,HEAD2+91)                                  
         MVC   HEAD2+100(2),=C'AT'                                              
         GOTOX TIMCON,DMCB,REQSTART,HEAD2+103                                   
         MVI   HEAD3,0                                                          
*                                                                               
         CLI   QOPTION2,C'S'       SUMMARY?                                     
         BE    EXIT                                                             
         LA    R4,HEAD4                                                         
         USING PLINE,R4                                                         
         MVC   PREP(3),=CL3'REP'                                                
         MVC   PAGY,=CL7'AGENCY'                                                
         MVC   PADV,=CL4'ADV'                                                   
         MVC   PSTA(7),=CL7'STATION'                                            
         MVC   PDARE+2(6),=C'DARE #'                                            
         MVC   PNUM+3(5),=C'CON #'                                              
         MVC   PSPOT+3(5),=C'SPOTS'                                             
*                                                                               
         MVC   HEAD5(PLEN),DASHES                                               
         B     EXIT                                                             
         DROP  R4                                                               
**********************************************************************          
* CKREP - CHECK IF REP CODE TO BE SKIPPED                                       
**********************************************************************          
CKREP    NTR1                                                                   
         LA    R7,IOAREA                                                        
         USING RCONREC,R7                                                       
         LA    R2,CKREPTAB                                                      
CKREP10  DS    0H                                                               
         CLI   0(R2),X'FF'                                                      
         BE    Z                   EXIT CC:ZERO                                 
         CLC   RCONKREP,0(R2)                                                   
         BE    NZ                  EXIT CC:NOT ZERO                             
         LA    R2,2(R2)                                                         
         B     CKREP10                                                          
         DROP  R7                                                               
*                                                                               
CKREP2   NTR1                                                                   
         USING SORTREC,R3                                                       
         LA    R2,CKREPTAB                                                      
CKREP210 DS    0H                                                               
         CLI   0(R2),X'FF'                                                      
         BE    Z                   EXIT CC:ZERO                                 
         CLC   SREP,0(R2)                                                       
         BE    NZ                  EXIT CC:NOT ZERO                             
         LA    R2,2(R2)                                                         
         B     CKREP210                                                         
         DROP  R3                                                               
*                                                                               
CKREPTAB DC    C'NK'                                                            
         DC    C'SJ'                                                            
         DC    C'B3'                                                            
         DC    C'B4'                                                            
         DC    C'KH'                                                            
         DC    C'P9'                                                            
         DC    C'HB'                                                            
         DC    C'TV'                                                            
         DC    C'46'                                                            
         DC    X'FF'                                                            
**********************************************************************          
* LOCAL REP - DOWNLOAD HANDLING                                                 
**********************************************************************          
LOCALREP NTR1                                                                   
         CLI   RCDNLOAD,C'Y'       DOWNLOAD?                                    
         BNE   LOCALR              NO - JUST PRINT LINE                         
                                                                                
         L     R5,VXADDR                                                        
         USING VXADDRD,R5                                                       
                                                                                
         L     R2,VXDOWNDF         R2 -> COMMON DEFINITION LIST                 
         LA    R1,DEF              R1 -> DOWNOAD DEFINITION                     
         DROP  R5                                                               
                                                                                
LOCAL010 DS    0H                                                               
         MVC   0(2,R2),0(R1)       MOVE PAIRS UNTIL ZERO                        
         CLI   0(R1),0                                                          
         BE    LOCAL020                                                         
         LA    R1,2(R1)                                                         
         LA    R2,2(R2)                                                         
         B     LOCAL010                                                         
                                                                                
LOCAL020 DS    0H                                                               
         MVI   LINE,1              NEVER PAGE BREAK                             
*                                                                               
         GOTOX REPORT,DMCB,WORKC,=C'PRINT'                                      
         B     LOCALX                                                           
*                                                                               
LOCALR   GOTOX REPORT                                                           
                                                                                
LOCALX   DS    0H                                                               
         B     EXIT                                                             
                                                                                
DEF      DC    C'T',AL1(02)        REP                                          
         DC    C'T',AL1(07)        AGY                                          
         DC    C'T',AL1(04)        ADV                                          
         DC    C'T',AL1(06)        STA                                          
         DC    C'T',AL1(08)        DARE                                         
         DC    C'T',AL1(08)        CONTRACT                                     
         DC    C'N',AL1(08)        SPOTS                                        
         DC    X'0000'                                                          
*                                                                               
SORTCARD DC    CL80'SORT FIELDS=(1,25,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=29'                                    
*                                                                               
FILIND   DCB   DDNAME=FILIND,DSORG=PS,RECFM=VB,MACRF=GM,LRECL=4004,    +        
               BLKSIZE=32760,BUFNO=2,EODAD=EOFD                                 
*                                                                               
FILING   DCB   DDNAME=FILING,DSORG=PS,RECFM=FB,MACRF=GM,LRECL=29,      +        
               BLKSIZE=9280,BUFNO=2,EODAD=EOFG                                  
*                                                                               
FILOUT   DCB   DDNAME=FILOUT,DSORG=PS,RECFM=FB,MACRF=PM,LRECL=29,      +        
               BLKSIZE=9280,BUFNO=2                                             
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
DASHES   DC    80C'-'                                                           
EQUALS   DC    80C'='                                                           
         DS    CL2000              INCREASE PROGRAM SIZE!!!!                    
*                                                                               
PLINE    DSECT                     PRINT LINE DSECT                             
PREP     DS    CL2                 REP CODE                                     
         DS    CL2                                                              
PAGY     DS    CL7                 AGY CODE W/OFFICE                            
         DS    CL2                                                              
PADV     DS    CL4                 ADV CODE                                     
         DS    CL2                                                              
PSTA     DS    CL6                 STATION                                      
         DS    CL2                                                              
PDARE    DS    CL8                 DARE ORDER #                                 
         DS    CL2                                                              
PNUM     DS    CL8                 CONTRACT #                                   
         DS    CL2                                                              
PSPOT    DS    CL8                 SPOT COUNT                                   
PLEN     EQU   *-PLINE                                                          
*                                                                               
DLINE    DSECT                     DOWNLOAD LINE DSECT                          
DREP     DS    CL2                 REP CODE                                     
DAGY     DS    CL7                 AGY CODE W/OFFICE                            
DADV     DS    CL4                 ADV CODE                                     
DSTA     DS    CL6                 STATION                                      
DDARE    DS    CL8                 DARE ORDER #                                 
DNUM     DS    CL8                 CONTRACT #                                   
DSPOT    DS    CL8                 SPOT COUNT                                   
DLEN     EQU   *-DLINE                                                          
*                                                                               
SORTD    DSECT         DSECT FOR SORTER/GEN REC IN                              
SORTREC  DS    0CL29                                                            
SREP     DS    CL2                 REP CODE                                     
SAGY     DS    CL6                 AGENCY & AGY OFFICE                          
SADV     DS    CL4                 ADVERTISER                                   
SSTA     DS    CL5                 STATION                                      
SDARE    DS    CL4                 DARE ORDER #                                 
SNUM     DS    CL4                 CONTACT #                                    
SSPOTS   DS    CL4                 SPOT COUNT                                   
*                                                                               
*                                                                               
* WORKING STORAGE                                                               
*                                                                               
STORED   DSECT                                                                  
RELO     DS    A                                                                
REPFACS  DS    A                                                                
CALLOV   DS    A                                                                
REPTOT   DS    F                                                                
AGYTOT   DS    F                                                                
ADVTOT   DS    F                                                                
GTOT     DS    F                                                                
SPOTS    DS    F                                                                
ELCODE   DS    X                                                                
LASTREP  DS    CL2                                                              
LASTAGY  DS    CL6                                                              
LASTADV  DS    CL4                                                              
SDATE    DS    XL2                 START DATE COMPRESSED                        
EDATE    DS    XL2                 END DATE COMPRESSED                          
DARENUM  DS    CL4                 DARE AGENCY ORDER NUMBER                     
RFBLOCK  DS    CL6                 REPFACS PARAM BLOCK                          
         DS    0D                                                               
         DS    F                                                                
IOAREA   DS    CL4096                                                           
STOREX   EQU   *                                                                
*                                                                               
         ORG   IOAREA                                                           
       ++INCLUDE REXADDRD                                                       
       ++INCLUDE REGENALL1A                                                     
       ++INCLUDE REREPWORKD                                                     
       ++INCLUDE REREPMODES                                                     
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE REPFACSQ                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'054REREP9202S07/10/98'                                      
         END                                                                    
