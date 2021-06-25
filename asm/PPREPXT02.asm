*          DATA SET PPREPXT02  AT LEVEL 056 AS OF 07/09/14                      
*$PANAPT01S$ *******  FIRST LINE TO DELETE  *****************                   
*                                                           *                   
*  ATTN: THE LEVEL STAMPS ARE *LEGITIMATELY* OUT-OF-SYNC!   *                   
*        DELETE THIS COMMENT BLOCK ON THE NEXT PROMOTION!   *                   
*                                                           *                   
*  THIS SOURCE MODULE IS AT A HIGHER LEVEL NUMBER THAN ITS  *                   
*  CORRESPONDING LOAD OR OBJECT MODULE, BECAUSE THE MEMBER  *                   
*  WAS PROMOTED USING THE SRCE LIBCODE VIA MR# 044155.      *                   
*                                                           *                   
*  THIS COMMENT BLOCK WAS INSERTED *PROGRAMMATICALLY* BY    *                   
*  PANAPT, TO EXPLAIN WHY THE LEVEL STAMPS ARE OUT-OF-SYNC. *                   
*  BEFORE THIS MEMBER CAN BE PROMOTED AGAIN, THIS ENTIRE    *                   
*  COMMENT BLOCK *MUST* BE MANUALLY DELETED.                *                   
*                                                           *                   
*$PANAPT01X$ *******  LAST LINE TO DELETE  ******************                   
*PHASE PPXT02A                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
*  KWAN  08/00     NEW DSECT FOR PBILLREC                                       
*                                                                               
*  SMYE  12/12/95  CHANGED DTCNV TO DATCON WITH NEW PARAM'S                     
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         TITLE 'PPXT02 - STANDARD EXTRACT'                                      
*                                                                               
*QOPT1   E=SPECIAL ESTIMATE EXTRACT                                             
*        B=FILTER ON BILLED DATES                                               
*QOPT2   P=PRINT OUTPUT RECORDS (CHARACTERS)                                    
*        H=HEX DUMP                                                             
*        B=BOTH CHARACTER AND HEX DUMPS                                         
*QOPT3   IF QOPT 1 = E                                                          
*           B = FILTER ON ACTUAL BUCKET DATE                                    
*QOPT4   B = OUTPUT ONLY PIH (INVOICE HEADER) RECORDS                           
*                                                                               
PPXT02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,PPXT02                                                         
         L     RA,0(R1)                                                         
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         LA    R9,1(RC)                                                         
         LA    R9,4095(R9)                                                      
         USING PPFILED,RC,R9                                                    
         LA    R8,SPACEND                                                       
         USING PPXTWRKD,R8                                                      
         CLI   MODE,RUNFRST                                                     
         BE    INITIAL                                                          
         CLI   MODE,FBUYREQ                                                     
         BE    FIRSTB                                                           
         CLI   MODE,FBUYEST                                                     
         BE    FEST                                                             
         CLI   MODE,PROCBKT                                                     
         BE    FEST                                                             
         CLI   MODE,PROCBUY                                                     
         BE    PROCESS                                                          
         CLI   MODE,PROCBIL                                                     
         BE    BILL                                                             
         CLI   MODE,RUNLAST                                                     
         BE    TOTALS                                                           
*                                                                               
EXIT     DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
INITIAL  DS    0H                                                               
         MVI   FORCEHED,C'Y'                                                    
         LA    R2,OUTCNT           ZAP ACCUMS                                   
         LA    R3,5                                                             
INIT2    ZAP   0(4,R2),=P'0'                                                    
         LA    R2,4(R2)                                                         
         BCT   R3,INIT2                                                         
         MVI   ZEROS,C'0'                                                       
         MVC   ZEROS+1(L'ZEROS-1),ZEROS                                         
         OPEN  (OUTFILE,OUTPUT)                                                 
*                                                                               
         MVI   DMINBTS,X'08'                                                    
         MVI   DMOUTBTS,X'FD'                                                   
*                                                                               
         B     EXIT                                                             
         SPACE 2                                                                
FIRSTB   DS    0H                                                               
         CLI   QOPT4,C'B'          INVOICES ONLY                                
         BNE   FIRSTB2                                                          
         MVI   FCRDBUY,C'N'                                                     
         MVI   FCRDBKT,C'N'                                                     
         MVI   FCRDEST,C'N'                                                     
         B     FIRSTB2                                                          
*                                                                               
FIRSTB2  CLI   QOPT1,C'E'          SPECIAL ESTIMATE EXTRACT                     
         BNE   FIRSTB4                                                          
         MVI   FCRDBUY,C'N'                                                     
         MVI   FCRDBILL,C'N'                                                    
         MVI   FCRDEST,C'Y'                                                     
         MVI   FCRDBKT,C'Y'                                                     
*                                                                               
FIRSTB4  DS    0H                                                               
         MVI   FCGTREG,C'Y'                                                     
         MVI   FCGTDIST,C'Y'                                                    
         GOTO1 DATCON,DMCB,QSTART,(3,BQS)                                       
         GOTO1 (RF),(R1),QEND,(3,BQE)                                           
         MVC   SVQSTART(12),QSTART                                              
         CLI   QOPT1,C'B'                                                       
         BNE   EXIT                                                             
         MVC   QSTART(12),SPACES                                                
FIRSTBX  B     EXIT                                                             
         EJECT                                                                  
FEST     DS    0H                  OUTPUT EST HEADER                            
*                                  (FOR SPECIAL ESTIMATE EXTRACT                
*                                   DONE AT PROCBKT)                            
         LA    R7,OUTREC                                                        
         USING DDEST,R7                                                         
         XC    OUTREC(250),OUTREC                                               
         MVC   DDESYS(3),=C'PEH'                                                
         MVC   DDEAGY,QAGENCY                                                   
         MVC   DDEMED,QMEDIA                                                    
         MVC   DDECLT(6),PESTKCLT  CLT AND PRODUCT                              
         MVC   HALF,PESTKEST                                                    
         LH    R0,HALF                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  DDEEST,DUB                                                       
         MVC   DDECNAME(20),PCLTNAME                                            
         MVC   DDEPNAME(20),PPRDNAME                                            
         MVC   DDEENAME(20),PESTNAME                                            
         OC    DDECNAME(72),SPACES                                              
         MVC   DDECNUM,ZEROS                                                    
         OC    PCLTNUM,PCLTNUM                                                  
         BZ    FEST4                                                            
         MVC   DDECNUM+1(3),PCLTNUM                                             
FEST4    MVC   DDEPCOD(4),PPRDACCT                                              
         OC    DDEPCOD,SPACES                                                   
         MVC   DDESDATE(12),PESTST     MOVE START AND END DATES                 
*                                                                               
         CLI   QOPT1,C'E'          SPECIAL ESTIMATE EXTRACT                     
         BNE   FEST20              NO - DONE, WRITE RECORD                      
         CLI   QOPT3,C'B'          TEST FILTER ON BUCKET DATE                   
         BE    FEST5               YES                                          
         CLC   PESTST,QSTART       NO- FILTER ON EST START                      
         BL    FEST20                                                           
         CLC   PESTEND,QEND                                                     
         BH    FEST20                                                           
*                                                                               
FEST5    DS    0H                                                               
*                              PUT OUT A RECORD FOR EACH ACTIVE MONTH           
*                                                                               
         LA    R5,DDESDATE+12      START AT SPARE                               
         L     R2,ADBKT            A(BUCKET RECORD)                             
         LA    R2,33(R2)                                                        
         MVI   ELCODE,X'22'        USE BILLING MONTH ELEMS                      
         CLC   ELCODE,0(R2)                                                     
         BE    FEST8                                                            
*                                                                               
FEST6    DS    0H                                                               
         BAS   RE,NEXTEL                                                        
         BNE   FESTX               NO MORE ELEMS                                
*                                                                               
FEST8    DS    0H                                                               
         USING BKELEM,R2                                                        
         CLC   BKOGRS(36),=6PL6'0'                                              
         BE    FEST6                                                            
*                                                                               
         CLI   QOPT3,C'B'          TEST FILTERING OF BUCKET DATE                
         BNE   FEST7                                                            
         CLC   BKYM,BQS            DO ONLY MONTHS WITHIN PERIOD                 
         BL    FEST6                                                            
         CLC   BKYM,BQE                                                         
         BH    FEST6                                                            
*                                                                               
FEST7    DS    0H                                                               
         GOTO1 DATCON,DMCB,(3,BKYM),WORK                                        
         MVC   0(4,R5),WORK                                                     
         MVC   4(36,R5),BKOGRS                                                  
         BAS   RE,WRITE                                                         
         AP    HDRCNT,=P'1'                                                     
         B     FEST6                                                            
*                                                                               
FEST20   DS    0H                                                               
         BAS   RE,WRITE                                                         
         AP    HDRCNT,=P'1'                                                     
*                                                                               
FESTX    DS    0H                                                               
         B     EXIT                                                             
*                                                                               
         EJECT                                                                  
BILL     DS    0H                  OUTPUT INVOICE RECORDS                       
         TM    PBILLREC+27,X'80'                                                
         BNZ   EXIT                                                             
         OC    PBILKEST,PBILKEST       IGNORE BILLS WITH NO EST                 
         BZ    EXIT                                                             
         CLI   QOPT1,C'B'          BILLED DATE FILE.TER                         
         BNE   BILL2                                                            
         CLC   PBILLDAT,SVQSTART                                                
         BL    EXIT                                                             
         CLC   PBILLDAT,SVQEND                                                  
         BH    EXIT                                                             
BILL2    DS    0H                                                               
         LA    R7,OUTREC                                                        
         USING DDBILL,R7                                                        
         XC    OUTREC(250),OUTREC                                               
         MVC   DDBSYS(3),=C'PIH'                                                
         MVC   DDBAGY,QAGENCY                                                   
         MVC   DDBMED,QMEDIA                                                    
         MVC   DDBCLT(6),PBILKCLT         CLT AND PRD                           
         MVC   HALF,PBILKEST                                                    
         LH    R0,HALF                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  DDBEST,DUB                                                       
         MVC   DDBINV(2),PBILLDAT+2                                             
         MVC   HALF,PBILKBNO                                                    
         LH    R0,HALF                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  DDBINV+2(4),DUB                                                  
         MVC   WORK(2),PBILKMOS                                                 
         MVI   WORK+2,1                                                         
*        GOTO1 DTCNV,DMCB,(1,WORK),(0,WORK+6)                                   
         GOTO1 DATCON,DMCB,(3,WORK),(0,WORK+6)                                  
         MVC   DDBMOS,WORK+6                                                    
         MVC   DDBMKT(9),SPACES    NOT USED FOR PRINT                           
         ZAP   DUB,PBILLGRS                                                     
         UNPK  DDBGRS,DUB                                                       
         CP    DUB,=P'0'                                                        
         BL    *+8                                                              
         OI    DDBGRS+11,X'F0'                                                  
         ZAP   DUB,PBILLNET                                                     
         UNPK  DDBNET,DUB                                                       
         CP    DUB,=P'0'                                                        
         BL    *+8                                                              
         OI    DDBNET+11,X'F0'                                                  
         ZAP   DUB,PBILLGRS                                                     
         SP    DUB,PBILLBIL                                                     
         UNPK  DDBCD,DUB           CASH DISC                                    
         CP    DUB,=P'0'                                                        
         BL    *+8                                                              
         OI    DDBCD+11,X'F0'                                                   
         ZAP   DUB,PBILLRCV                                                     
         UNPK  DDBACT,DUB                                                       
         CP    DUB,=P'0'                                                        
         BL    *+8                                                              
         OI    DDBACT+11,X'F0'                                                  
         ZAP   DUB,PBILLRCV                                                     
         SP    DUB,PBILLNET                                                     
         UNPK  DDBINC,DUB                                                       
         CP    DUB,=P'0'                                                        
         BL    *+8                                                              
         OI    DDBINC+11,X'F0'                                                  
*        GOTO1 DTCNV,DMCB,(1,PBILINVD),(0,DDBINVD)                              
         GOTO1 DATCON,DMCB,(3,PBILINVD),(0,DDBINVD)                             
*        GOTO1 DTCNV,DMCB,(1,PBILDUED),(0,DDBDUED)                              
         GOTO1 DATCON,DMCB,(3,PBILDUED),(0,DDBDUED)                             
         MVI   DDBTYP,C'M'                                                      
         CLI   PBILLTYP,C'1'                                                    
         BE    BILL4               MANUAL                                       
         MVI   DDBTYP,C'D'                                                      
         CLI   PBILLTYP,C'4'       DETAIL                                       
         BE    BILL4                                                            
         MVI   DDBTYP,C'S'         MUST BE SUMMARY                              
*                                                                               
BILL4    DS    0H                                                               
         BAS   RE,WRITE                                                         
         AP    INVCNT,=P'1'                                                     
         B     EXIT                                                             
         EJECT                                                                  
PROCESS  DS    0H                  PROCESS BUYRECS                              
         CLI   QOPT1,C'E'          SKIP FOR SPECIAL ESTIMATE EXTRACT            
         BE    EXIT                                                             
         LA    R7,OUTREC                                                        
         USING DDINS,R7                                                         
         XC    OUTREC(250),OUTREC                                               
         MVC   DDISYS(3),=C'PIN'                                                
         MVC   DDIAGY(3),PBUYKAGY     AGY/MED                                   
         MVC   DDICLT,PCLTKCLT                                                  
         MVC   DDIPRD,PPRDKPRD                                                  
         MVC   HALF,PBUYKEST                                                    
         LH    R0,HALF                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  DDIEST,DUB                                                       
         MVC   DDIINV(6),ZEROS                                                  
         UNPK  WORK(11),PBUYKPUB(6)                                             
         MVC   DDIVEN(10),WORK         NUMBER AND ZONE                          
         MVC   DDIVENED,SPACES                                                  
         CLI   PBUYKEDT,0                                                       
         BE    PROC3                                                            
         XC    WORK(25),WORK                                                    
         MVC   WORK(5),=X'1000000000'                                           
         MVC   WORK+5(1),PBUYKEDT                                               
         GOTO1 PUBEDIT,DMCB,(C'0',WORK),(0,WORK+10)                             
         MVC   DDIVENED,WORK+19                                                 
         OC    DDIVENED,SPACES                                                  
*                                                                               
PROC3    DS    0H                                                               
*        GOTO1 DTCNV,DMCB,(1,PBUYKDAT),(0,DDIINSDT)                             
         GOTO1 DATCON,DMCB,(3,PBUYKDAT),(0,DDIINSDT)                            
         ZIC   R0,PBUYKLIN                                                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  DDIINSLN,DUB                                                     
         L     R0,GROSS                                                         
         CVD   R0,DUB                                                           
         UNPK  DDIGRS,DUB                                                       
         CP    DUB,=P'0'                                                        
         BL    *+8                                                              
         OI    DDIGRS+11,X'F0'                                                  
         L     R0,GROSS                                                         
         S     R0,AGYCOM                                                        
         S     R0,CSHDSC                                                        
         CVD   R0,DUB                                                           
         UNPK  DDINET,DUB                                                       
         CP    DUB,=P'0'                                                        
         BL    *+8                                                              
         OI    DDINET+11,X'F0'                                                  
         L     R0,CSHDSC                                                        
         CVD   R0,DUB                                                           
         UNPK  DDICD,DUB                                                        
         CP    DUB,=P'0'                                                        
         BL    *+8                                                              
         OI    DDICD+11,X'F0'                                                   
         MVC   DDIADNO,PBDJOB                                                   
*                                                                               
PUTDATES DS    0H                                                               
         MVC   WORK(3),PBDBDATE                                                 
         MVC   WORK+3(3),PBDPDATE                                               
         MVC   WORK+6(6),PBDCDATE     CLOSING AND ON-SALE DATES                 
         LA    R3,WORK                                                          
         LA    R4,DDIBLDT                                                       
         LA    R5,4                FOR BCT                                      
*PUTD2    GOTO1 DTCNV,DMCB,(1,0(R3)),(0,0(R4))                                  
PUTD2    GOTO1 DATCON,DMCB,(3,0(R3)),(0,0(R4))                                  
         LA    R3,3(R3)                                                         
         LA    R4,6(R4)                                                         
         BCT   R5,PUTD2                                                         
         CLI   QMEDIA,C'N'                                                      
         BE    PUTNEWS                                                          
         JIF   QMEDIA,EQ,C'O',AND,PBDSPACE,EQ,X'FF',PUTOUTD,JUMP=N              
*                                                                               
         GOTO1 OUTER,DMCB,PBUYREC,DDISPCE,WORK                                  
         OC    DDISPCE,SPACES                                                   
         B     PUTREG              GO PUT REGIONS                               
*                                                                               
PUTNEWS  DS    0H                  SPECIAL FOR NEWSPAPERS                       
         UNPK  DDILINES,PBDUNITS                                                
         CP    PBDUNITS,=P'0'                                                   
         BL    *+8                                                              
         OI    DDILINES+4,X'F0'                                                 
         MVC   DDILINRT,ZEROS                                                   
         CLI   PBDCOSTY,C'U'       CHK UNIT COST                                
         BNE   PUTPREM                                                          
         UNPK  DDILINRT,PBDCOS                                                  
         CP    PBDCOS,=P'0'                                                     
         BL    *+8                                                              
         OI    DDILINRT+6,X'F0'                                                 
*                                                                               
PUTPREM  DS    0H                                                               
         ZIC   R0,PBDCL                                                         
         CVD   R0,DUB                                                           
         UNPK  DDICOLOR,DUB                                                     
         OI    DDICOLOR,X'F0'                                                   
         UNPK  DDICOLCH,PBDPRCOS                                                
         CP    PBDPRCOS,=P'0'                                                   
         BL    *+8                                                              
         OI    DDICOLCH+6,X'F0'                                                 
         B     PUTREG                                                           
*                                                                               
PUTOUTD  DS    0H                  SPECIAL OUTDOOR FIELDS                       
         MVC   DDISHOW,=5C'9'                                                   
         CLC   PBDSHOW(3),=C'SPC'  SPECIAL                                      
         BE    PUTO2                                                            
         UNPK  DDISHOW,PBDSHOW                                                  
         OI    DDISHOW+4,X'F0'                                                  
*                                                                               
PUTO2    UNPK  DDIREGLR,PBDREG                                                  
         OI    DDIREGLR+4,X'F0'                                                 
         UNPK  DDIILLUM,PBDILLUM                                                
         OI    DDIILLUM+4,X'F0'                                                 
*                                                                               
         EJECT                                                                  
PUTREG   DS    0H                                                               
         MVC   DDIREG(6),ZEROS                                                  
         CLI   PREGREC,0                                                        
         BE    *+10                                                             
         MVC   DDIREG,PREGKREG                                                  
         CLI   PDSTREC,0                                                        
         BE    *+10                                                             
         MVC   DDIDIST,PDSTKDST                                                 
PUTPUBF  DS    0H                                                               
         MVC   DDISTATE,PUBSTATE                                                
         MVC   DDICITY,PUBCITY                                                  
         OC    DDISTATE(18),SPACES                                              
         MVC   DDIVENNM(20),PUBNAME                                             
         MVC   DDIZONNM,PUBZNAME                                                
         OC    DDIVENNM,SPACES                                                  
         OC    DDIZONNM,SPACES                                                  
         MVC   DDICIRC,ZEROS                                                    
         LA    R2,PUBREC+33                                                     
         MVI   ELCODE,X'30'                                                     
         BAS   RE,NEXTEL           FIND CIRC ELEM                               
         BNE   PUTSHR                                                           
         USING PPDUMD06,R2                                                      
         ZAP   DUB,PUBCIR1                                                      
         UNPK  DDICIRC,DUB                                                      
         OI    DDICIRC+8,X'F0'                                                  
         DROP  R2                                                               
*                                                                               
         EJECT                                                                  
PUTSHR   DS    0H                                                               
         MVC   DDISHRNM(6),=C'001001'                                           
         CLC   PBUYKPRD,=C'ZZZ'    CHK POL                                      
         BNE   PUTCOM                                                           
*                                                                               
         LA    R2,PBUYREC+33                                                    
         MVI   ELCODE,X'21'                                                     
PUTS2    BAS   RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                MUST FIND X'21'                              
         USING PPDUMD04,R2                                                      
         CLC   PPRDKPRD,PPRCODE                                                 
         BNE   PUTS2                                                            
         ZIC   R0,PPRCOST          SHARE                                        
         CVD   R0,DUB                                                           
         UNPK  DDISHRNM,DUB                                                     
         OI    DDISHRNM+2,X'F0'                                                 
         ZIC   R0,PBDWTSUM         WEIGHT SUM                                   
         CVD   R0,DUB                                                           
         UNPK  DDISHRNM,DUB                                                     
         OI    DDISHRDN+2,X'F0'                                                 
         DROP  R2                                                               
         EJECT                                                                  
PUTCOM   DS    0H                  SPECIAL COMMENT PROCESSING                   
         CLC   QAGENCY,=C'RH'      RUMRILL-HOYT (BACARDI)                       
         BE    PUTCOM3                                                          
         CLC   QAGENCY,=C'MC'      MC-CANN      (BACARDI)                       
         BNE   PUTINVD                                                          
*                                                                               
PUTCOM3  DS    0H                                                               
         LA    R2,PBUYREC+33                                                    
         MVI   ELCODE,X'66'                                                     
*                                                                               
PUTCOM4  DS    0H                                                               
         BAS   RE,NEXTEL                                                        
         BNE   PUTINVD                                                          
         CLC   =C'PROMO=',2(R2)                                                 
         BNE   PUTCOM4                                                          
*                                                                               
         MVC   DDISPEC(4),8(R2)    PROMO CODE AND STATE                         
*                                                                               
         EJECT                                                                  
PUTINVD  DS    0H                  OUTPUT A REC FOR EACH BILLING ELEM           
         CLI   QOPT1,C'B'          SKIP INS REC IF BILL DATE OPT                
         BE    PUTI1                                                            
         TM    PBUYKEY+27,X'80'    SKIP INS REC FOR DELETES                     
         BNZ   PUTI1                                                            
         BAS   RE,WRITE                                                         
         AP    INSCNT,=P'1'                                                     
PUTI1    DS    0H                                                               
         MVI   DDICOD+1,C'D'                                                    
         LA    R2,PBUYREC+33                                                    
         MVI   ELCODE,X'26'                                                     
PUTI2    BAS   RE,NEXTEL                                                        
         BNE   EXIT                                                             
         USING PPDUMD02,R2                                                      
         CLC   PBPRD,PPRDKPRD                                                   
         BNE   PUTI2                                                            
         OC    PBLDATE,PBLDATE     SEE IF BILLED                                
         BZ    PUTI2                                                            
         CLI   QOPT1,C'B'          BILL DATE OPT                                
         BNE   PUTI4                                                            
         CLC   PBLDATE,BQS                                                      
         BL    PUTI2                                                            
         CLC   PBLDATE,BQE                                                      
         BH    PUTI2                                                            
*                                                                               
PUTI4    DS    0H                                                               
         ZIC   R0,PBLDATE+1        BILL MONTH                                   
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  DDIINV(2),DUB                                                    
         MVC   HALF,PBINVNO        INVOICE NUMBER                               
         LH    R0,HALF                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  DDIINV+2(4),DUB                                                  
         MVC   FULL,PBGROSS                                                     
         L     R0,FULL                                                          
         CVD   R0,DUB                                                           
         UNPK  DDIGRS,DUB                                                       
         CP    DUB,=P'0'                                                        
         BL    *+8                                                              
         OI    DDIGRS+11,X'F0'                                                  
         MVC   FULL,PBGROSS                                                     
         L     R0,FULL                                                          
         MVC   FULL,PBAGYCOM                                                    
         S     R0,FULL                                                          
         MVC   FULL,PBCSHDSC                                                    
         S     R0,FULL                                                          
         CVD   R0,DUB                                                           
         UNPK  DDINET,DUB                                                       
         CP    DUB,=P'0'                                                        
         BL    *+8                                                              
         OI    DDINET+11,X'F0'                                                  
         MVC   FULL,PBCSHDSC                                                    
         L     R0,FULL                                                          
         CVD   R0,DUB                                                           
         UNPK  DDICD,DUB                                                        
         CP    DUB,=P'0'                                                        
         BL    *+8                                                              
         OI    DDICD+11,X'F0'                                                   
         BAS   RE,WRITE                                                         
         AP    INVDCNT,=P'1'                                                    
         B     PUTI2                                                            
*                                                                               
         EJECT                                                                  
TOTALS   DS    0H                                                               
         LA    R4,TITLES                                                        
         LA    R5,HDRCNT                                                        
         LA    R6,4                FOR BCT                                      
TOT2     MVC   P+7(17),0(R4)                                                    
         EDIT  (P4,0(R5)),(9,P+26),0,COMMAS=YES                                 
         GOTO1 REPORT                                                           
         LA    R4,17(R4)                                                        
         LA    R5,4(R5)                                                         
         BCT   R6,TOT2                                                          
         GOTO1 REPORT              SKIP A LINE                                  
         MVC   P+7(13),=C'TOTAL RECORDS'                                        
         EDIT  OUTCNT,(9,P+26),0,COMMAS=YES                                     
         MVI   P+35,C'*'                                                        
         GOTO1 REPORT                                                           
         CLOSE (OUTFILE,)                                                       
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
NEXTEL   DS    0H                                                               
         ZIC   R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BE    NEXTEL2                                                          
         CLC   ELCODE,0(R2)                                                     
         BER   RE                                                               
         B     NEXTEL                                                           
*                                                                               
NEXTEL2  DS    0H                                                               
         LTR   R2,R2                                                            
         BR    RE                  SET CC NOT EQ                                
         SPACE 2                                                                
         DC    F'0'                                                             
WRITE    ST    RE,WRITE-4                                                       
         CLI   QOPT2,C'P'                                                       
         BE    WRITP                                                            
         CLI   QOPT2,C'B'                                                       
         BE    WRITP                                                            
         B     WRITPX                                                           
*                                                                               
WRITP    MVC   P(125),OUTREC                                                    
         MVC   PSECOND(125),OUTREC+125                                          
         GOTO1 REPORT                                                           
*                                                                               
WRITPX   DS    0H                                                               
         CLI   QOPT2,C'H'                                                       
         BE    WRITH                                                            
         CLI   QOPT2,C'B'                                                       
         BNE   WRIT2                                                            
         GOTO1 REPORT                                                           
*                                                                               
WRITH    MVC   P(12),=C'HEX 001-050='                                           
         GOTO1 HEXOUT,DMCB,OUTREC,P+15,50,=C'N'                                 
         GOTO1 REPORT                                                           
         MVC   P+4(8),=C'051-100='                                              
         GOTO1 HEXOUT,DMCB,OUTREC+50,P+15,50,=C'N'                              
         GOTO1 REPORT                                                           
         MVC   P+4(8),=C'101-150='                                              
         GOTO1 HEXOUT,DMCB,OUTREC+100,P+15,50,=C'N'                             
         GOTO1 REPORT                                                           
         MVC   P+4(8),=C'151-200='                                              
         GOTO1 HEXOUT,DMCB,OUTREC+150,P+15,50,=C'N'                             
         GOTO1 REPORT                                                           
         MVC   P+4(8),=C'201-250='                                              
         GOTO1 HEXOUT,DMCB,OUTREC+200,P+15,50,=C'N'                             
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
*                                                                               
WRIT2    DS    0H                                                               
         LA    R1,OUTFILE                                                       
         LA    R0,OUTREC                                                        
         PUT   (1),(0)                                                          
         AP    OUTCNT,=P'1'                                                     
         L     RE,WRITE-4                                                       
         BR    RE                                                               
*                                                                               
TITLES   DS    0C                                                               
         DC    CL17'ESTIMATE HEADER'                                            
         DC    CL17'INSERTION DETAIL'                                           
         DC    CL17'INVOICE DETAIL'                                             
         DC    CL17'INVOICE HEADER'                                             
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
OUTFILE  DCB   DDNAME=OUTFILE,DSORG=PS,RECFM=FB,LRECL=250,             X        
               BLKSIZE=250,MACRF=PM                                             
         EJECT                                                                  
PPXTWRKD DSECT                                                                  
OUTCNT   DS    PL4'0'                                                           
HDRCNT   DS    PL4'0'                                                           
INSCNT   DS    PL4'0'                                                           
INVDCNT  DS    PL4'0'                                                           
INVCNT   DS    PL4'0'                                                           
*                                                                               
ELCODE   DS    CL1                                                              
SVQSTART DS    CL6                                                              
SVQEND   DS    CL6                                                              
BQS      DS    XL3                                                              
BQE      DS    XL3                                                              
ZEROS    DS    CL30                                                             
         DS    F                                                                
OUTREC   DS    CL250                                                            
         PRINT OFF                                                              
       ++INCLUDE PPMODEQU                                                       
       ++INCLUDE PPREPWORK                                                      
       ++INCLUDE PPNEWFILE         HAVE NEW PBILLREC DSECT                      
         PRINT ON                                                               
*                                                                               
       ++INCLUDE PBKREC                                                         
*                                                                               
       ++INCLUDE DDBKELEM                                                       
*                                                                               
       ++INCLUDE DDGENINTFC                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'056PPREPXT02 07/09/14'                                      
         END                                                                    
