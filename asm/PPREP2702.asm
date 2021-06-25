*          DATA SET PPREP2702  AT LEVEL 045 AS OF 07/09/14                      
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
*PHASE PP2702A                                                                  
*INCLUDE PUBFLOAT                                                               
*INCLUDE PRNTOFC                                                                
*INCLUDE PPGETCG                                                                
         TITLE 'PP2702 - CLIENT/VENDOR SUMMARY - CHANGE LOG'                    
*                                                                               
* SMYE  04-06/08 CHANGES FOR FOREIGN EXCHANGE (FX "ADD'L. CHARGE")              
*                                                                               
* SMYE  05/04/06 INCREASE PRDTAB FROM CL23000 (1000 PROD'S) TO CL34500          
*                (1500 PROD'S)                                                  
*                                                                               
* BOBY  11/05    2 CH MEDIA OFFICE CODES                                        
*                                                                               
* SMYE  01/15/03 INCREASE PRDTAB FROM CL13800 (600 PROD'S) TO CL23000           
*                (1000 PROD'S)                                                  
*                                                                               
* SMYE  02/15/02 BUG FIX -                                                      
*                INCREASE SIZE OF PAYWORK CSECT IN ACCORDANCE WITH              
*                INCREASED SIZE OF PRDTAB (SEE 04/30/01 BELOW)                  
*                PAYWORK LENGTH IS NOW SOFT CODED TO CHANGE                     
*                AS PAYWORKD (INCLUDING PRDTAB) CHANGES                         
*                                                                               
* SMYE  04/30/01 INCREASE PRDTAB FROM CL11500 (500 PROD'S) TO CL13800           
*                (600 PROD'S)                                                   
* *                                                                             
* SMYE  02/01    ADD ADDITIONAL CHARGES USING PPGETCG                           
* *                                                                             
* KWAN  03/16/00 FIX CLC BRANCHING BUG IN SV70ELM (BNL SHOULD BE BH)            
*                                                                               
* KWAN  01/04/00 USE DATACON TO CONVERT QPAY AND RCDATE INTO SAME               
*                DATE FORMAT SO THAT CLC WILL WORK PROPERLY                     
*                                                                               
* BPLA  6/99     ALTER CD/NON-CD PUBS ONLY FEATURE TO IGNORE                    
*                CD EFFECTIVE DATE IF IT 3 MONTHS OR MORE                       
*                BEFORE THE REQUEST START DATE                                  
*                                                                               
* SMYE 10/01/97  MOVE OFFOUT TO CLIF FOR ONE-TIME CALL ONLY TO                  
*                PREVENT PRREQREP PROBLEMS FOUND IN PPREPNV02 AND               
*                PPREP3602 WHEN RUNNING MULTIPLE OFFICE REQUESTS                
*                                                                               
* SMYE 11/19/96  USE OFFOUT FOR OFFICE DISPLAY                                  
*                                                                               
* SMYE 01/11/96  MODIFIED MTHTAB HANDLING FOR YEARS AFTER 1999                  
*                                                                               
* SMYE 12/12/95  CHANGED DTCNV TO DATCON WITH NEW PARAM'S                       
*                                                                               
* BPLA 8/31/94   REMOVE CODE USING GETINSA - WE CAN NOW USE GETINS              
*                                                                               
* BOBY 5/01/94   ADD PST TO GST FIELD                                           
*                                                                               
* BPLA 7/12/93   PRINT PBYOBFD WITH COMMENTS                                    
*                                                                               
* BPLA 2/26/93   CHECK PBYOCOM FOR 47 BYTES (WAS CLI C' ')                      
*                                                                               
* LWEI 02/02/93  DISPLAY X'83' (REF) ELEMENT                                    
*                                                                               
* BPLA 10/20/92  PROFILE OPTIONS TO SUPPRESS "FREE" BUYS                        
*                                                                               
* BPLA 9/17/92 "FREE" INSERTION DATE CHECK CHANGED TO SEP01/92                  
*                                                                               
* BPLA 7/20/92 IF REPORTING UNPAID ITEMS ONLY SKIP "FREE' BUYS WHOSE            
*              INSERTION DATE IS BEFORE AUG01/92                                
*                                                                               
* BPLA 6/9/92  CHANGES TO SHOW "FREE" INSERTIONS WHEN QOPT1 IS U OR P           
*             -CAN DO SINCE THESE INSERTIONS WILL NOW HAVE DATED                
*              PAY ELEMENTS WHEN THEY GET PAID                                  
*                                                                               
* BPLA 6/28/91 PRINT INSERTION TOTAL UNCLEARED OR CLEARED PORTION WHEN          
*            USING QOPT1=U OR P. FOR THOSE AGYS THAT CLEAR BY INSERTION         
*                                                                               
* BPLA 4/8/91  REP/ADDR TOTALS BUG FIXED, DUPLICATE TAGS FIXED                  
*                                                                               
* ROSA 1/2/91  ADD GST DOLLARS TO TOTALS IF REQUESTED              L01          
********************                                                            
*        PROFILE OPTIONS                                                        
*                                                                               
*    +0     I=INCH TOTALS/L=LINES                                               
*    +1     Y=SHOW LAST I/O                                                     
*    +2     Y=SHOW PORTION UNCLEARED WHEN SHOWING PREVIOUSLY CLEARED            
*             (FOR THOSE USERS THAT PAY BY INSERTION)                           
*                                                                               
         PRINT NOGEN                                                            
         TITLE 'PP2702 - CLIENT/VENDOR SUMMARY'                                 
PP2702   CSECT                                                                  
         NMOD1 0,PP2702,RR=R9                                                   
*                                                                               
*                                                                               
         L     RA,0(R1)                                                         
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         USING PPFILED,RC                                                       
*                                                                               
         L     R5,=V(PAYWORK)                                                   
         AR    R5,R9                                                            
         USING PAYWORKD,R5                                                      
         ST    R5,VPAYWORK                                                      
*                                                                               
         ST    R9,RELO                                                          
         LAY   RF,PRDTAB                                                        
         ST    RF,APRDTAB                                                       
         LAY   RF,ADCGWRK                                                       
         ST    RF,AADCGWRK                                                      
*                                                                               
         CLI   MODE,FBUYREQ                                                     
         BNE   CKMODE                                                           
         BAS   R9,INITIAL                                                       
         GOTO1 VBLDMLST              BUILD MTH LIST                             
         B     EXT                                                              
*                                                                               
CKMODE   CLI   MODE,FBUYCLI                                                     
         BNE   CKM2                                                             
         GOTO1 VCLIFRST                                                         
         B     EXT                                                              
*                                                                               
CKM2     DS    0H                                                               
*                                                                               
CKM3     CLI   MODE,FBUYPUB                                                     
         BNE   CKM4                                                             
         GOTO1 VPUBFRST                                                         
         B     EXT                                                              
*                                                                               
CKM4     CLI   MODE,PROCBUY                                                     
         BNE   CKM6                                                             
         B     PROCESS                                                          
*                                                                               
CKM6     CLI   MODE,LBUYPUB                                                     
         BNE   CKM8                                                             
         GOTO1 VPUBEND                                                          
         B     EXT                                                              
*                                                                               
CKM8     CLI   MODE,LBUYPRO                                                     
         BNE   CKM9                                                             
         GOTO1 VPRDEND                                                          
         B     EXT                                                              
*                                                                               
CKM9     CLI   MODE,LBUYCLI                                                     
         BNE   CKM11                                                            
         GOTO1 VCLTEND                                                          
         B     EXT                                                              
*                                                                               
CKM11    CLC   QSORT,=C'08'                                                     
         BE    CKM11A                                                           
         CLC   QSORT,=C'09'                                                     
         BNE   CKM12                                                            
*                                                                               
CKM11A   CLI   MODE,LBUYREP                                                     
         BNE   CKM12                                                            
         GOTO1 VREPEND                                                          
         B     EXT                                                              
*                                                                               
CKM12    CLI   MODE,FBUYPRO                                                     
         BNE   CKM14                                                            
         CLI   PRDSW,1                                                          
         BNE   EXT                                                              
         MVI   FORCEHED,C'Y'                                                    
         B     EXT                                                              
*                                                                               
CKM14    CLI   MODE,RUNFRST                                                     
         BNE   EXT                                                              
         XC    SVMEDCLI,SVMEDCLI                                                
         B     EXT                                                              
*                                                                               
         EJECT                                                                  
PROCESS  EQU   *                                                                
         OC    KEY+21(3),KEY+21       IGNORE PASSIVE POINTERS                   
         BNZ   EXT                                                              
*                                                                               
PROC10   CLI   PUBSW,0             SEE IF DOING THIS PUB                        
         BNE   EXT                 NO                                           
PROC12   DS    0H                                                               
         CLI   QBPDATE,C'B'        SEE IF REPORTING BILLABLE MONTHS             
         BNE   PROC12D                                                          
         GOTO1 VMTHEND             MUST ALWAYS GOT TO MTHEND                    
         B     PROC12X                                                          
*                                                                               
PROC12D  CLC   LASTYM,PBUYKDAT     CHECK FOR CHANGE OF MONTH                    
         BE    *+10                                                             
         GOTO1 VMTHEND                                                          
         MVC   LASTYM,PBUYKDAT                                                  
*                                                                               
PROC12X  XC    THISGST,THISGST                                     L01          
         XC    PAIDGST,PAIDGST                                     L01          
         XC    UPAIDGST,UPAIDGST                                   L01          
         XC    GSTCOMM,GSTCOMM                                                  
*                                                                               
         CLI   QOPT1,C' '            AM I DOING PAID OR UNPAID                  
         BE    PROC13X5              NO                                         
         CLI   ASOFDTE,0             SEE IF I HAVE AN AS OF DATE                
         BE    PROC13X5                                                         
*                                                                               
         LA    R3,PBDELEM                                                       
         MVI   ELCODE,X'25'                                                     
PROC13   BAS   RE,NEXTEL                                                        
         BNE   PROC13X                                                          
         USING PPDUMD03,R3                                                      
         OC    PPDDATE,PPDDATE                                                  
         BZ    PROC13                                                           
         CLC   PPDDATE(3),ASOFDTE                                               
         BNH   PROC13                                                           
         XC    PPDDATE(3),PPDDATE    CLEAR DATE  FROM PAYELEM                   
*                                    SO GETINS WON'T FIND THEM                  
         B     PROC13                CHECK FOR MORE                             
*                                                                               
PROC13X  DS    0H                    RECALL GETINS                              
         DROP  R3                                                               
*                                                                               
         GOTO1 GETINS,DMCB,PBUYKEY,GROSS,(FCOAPRD,PBUYKPRD)                     
*                                                                               
PROC13X5 LAY   R2,TSTPAID          AREA TO SAVE GETINS VALUES FROM PPG          
*                                   THAT WOULD INCLUDE FGN. EXCH. (FX)          
         XC    0(80,R2),0(R2)      CLEAR                                        
         MVC   0(64,R2),GROSS      SAVE PPG GETINS VALUES IN TSTPAID            
*                                                                               
         LAY   R2,FWORK                                                         
         XC    0(80,R2),0(R2)      CLEAR FOR "POSSIBLE" USE                     
*                                                                               
         CLI   PAGYNAT,C'C'        CANADIAN ?                                   
         BNE   CKQOPT1             NO                                           
*                                                                               
         MVC   0(5,R2),=C'CRATE'                                                
         LA    R1,PBUYREC                                                       
         ST    R1,DMCB                                                          
         CLI   ASOFDTE,0                                                        
         BE    DOGOTO                                                           
*        *                                                                      
         XC    GSTDTS,GSTDTS                                                    
         MVC   GSTDTS+3(3),ASOFDTE                                              
         LA    R1,GSTDTS                                                        
         ST    R1,DMCB+12                                                       
         MVI   DMCB,C'P' PASS BACK PAID DATA BETWEEN BOTH DATES                 
DOGOTO   LA    RF,KEY+7                                                         
         CLI   KEY+3,X'20'                                                      
         BE    DOGOTOO                                                          
         LA    RF,KEY+13                                                        
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
DOGOTOO  DS    0H                R2 STILL POINTING TO FWORK                     
*NOF*    CLI   QOPT7,C'F'          SEPARATE FOREIGN EXCHANGE ?                  
*NOF*    BNE   DOGOTOF             NO                                           
*                                                                               
         MVI   DMCB+12,C'F'      EXCLUDE FX (FOREIGN EXCHANGE) ADDL CHG         
DOGOTOF  DS    0H                                                               
         GOTO1 GETINS,DMCB,,(R2),(RF),,=C'BOTH',0                               
*                                                                               
*NOF*    CLI   QOPT7,C'F'          SEPARATE FOREIGN EXCHANGE ?                  
*NOF*    BNE   DOGOTOX             NO                                           
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
         MVC   GROSS(64),0(R2)     REPLACE PPG GETINS VALUES WITH               
*                                    VALUES EXCLUDING FX ADDL CHG               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
DOGOTOX  DS    0H                                                               
         XC    THISGST,THISGST     INIT GST FIELDS                              
         XC    GSTCOMM,GSTCOMM                                                  
         XC    PAIDGST,PAIDGST                                                  
         XC    UPAIDGST,UPAIDGST                                                
*                                                                               
         CLI   PBDCOSIN,C'C'       COMMISSION ONLY BUY                          
*SMY*    BE    CKQOPT1             NO GST                                       
         BE    GSTDELXX            NO GST                                       
*                                                                               
         USING GVALUESD,R1                                         L01          
         L     R1,16(R1)                                           L01          
*                                                                               
         TM    PBUYCNTL,X'80'        IF DELETED                                 
         BNO   GSTDELX                                                          
*                                                                               
         XC    GSTTAX,GSTTAX            CLEAR ORDERED GST                       
*                                                                               
         LA    R0,10                    TEN PROVINCES                           
         LA    RF,PSTAREA               PST SAVEAREA                            
         USING PSTAREA,RF               ESTABLISH PSTAREA                       
*                                                                               
         XC    PSTTAX,PSTTAX            CLEAR GROSS PST                         
         LA    RF,PSTAREAL(RF)          BUMP POINTER                            
         BCT   R0,*-10                                                          
*                                                                               
         DROP  RF                                                               
*                                                                               
GSTDELX  DS    0H                                                               
*                                                                               
         MVC   THISGST,GSTTAX      COPY GST AND PAID GST                        
         MVC   PAIDGST,GSTTAXPD                                                 
*                                                                               
*        ADD CORRESPONDING PST AMOUNTS TO GST                                   
*                                                                               
         LA    R0,10                    TEN PROVINCES                           
         LA    RF,PSTAREA               PST SAVEAREA                            
         USING PSTAREA,RF               ESTABLISH PSTAREA                       
*                                                                               
         ICM   RE,15,THISGST                                                    
         ICM   R2,15,PAIDGST                                                    
*                                                                               
         A     RE,PSTTAX                ADD PST TO GST BUCKET                   
         A     R2,PSTTAXPD              ADD PST TO GST PAID BUCKET              
         LA    RF,PSTAREAL(RF)          BUMP POINTER                            
         BCT   R0,*-12                                                          
*                                                                               
         STCM  RE,15,THISGST                                                    
         STCM  R2,15,PAIDGST                                                    
*                                                                               
         DROP  RF                                                               
*                                                                               
         MVC   GSTCOMM,THISGST        PREPARE FOR GST ACCUMULATION              
*                                                                               
         L     RF,THISGST                                                       
         L     RE,PAIDGST                                                       
         SR    RF,RE               UNPAID GST                                   
         ST    RF,UPAIDGST                                                      
*                                                                               
GSTDELXX DS    0H                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*      NOW SET ANY FOREIGN EXCHANGE (FX) VALUES INTO FWORK                      
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
*NOF*    CLI   QOPT7,C'F'          SEPARATE FOREIGN EXCHANGE ?                  
*NOF*    BNE   CKQOPT1             NO                                           
*                                                                               
         LAY   R2,FWORK                                                         
         XC    0(80,R2),0(R2)                                                   
*                                  PUT "ONLY FX" GETINS VALUES IN FWORK         
         GOTO1 GETINS,DMCB,PBUYREC,0(R2),PBUYKPRD,(C'A',0),,=C'FX'              
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         DROP  R1                                                               
         B     CKQOPT1                                                          
*                                                                               
GSTDTS   DS    CL6                                                              
*                                                                               
*                                                                               
CKQOPT1  DS    0H                                                               
         CLI   QOPT1,C' '      SEE IF DOING PAID OR UNPAID REQ                  
         BNE   CKOPT1A                                                          
         CLI   PROGPROF+5,C'Y' SUPPRESSING FREE BUYS ON ALL ITEM REQ            
         BNE   CKOPT1A                                                          
*NEW                                                                            
         LAY   RF,TSTPAID          HAS GETINS O/P INCLUDING FX                  
         OC    0(12,RF),0(RF)      TESTING GROSS(12)                            
         BZ    NEXTBUY                                                          
*NEW                                                                            
*                                                                               
CKOPT1A  MVI   PAIDSW,0       WILL BE SET TO X'01'                              
*                             IF DATED PAY ELEM IS FOUND                        
         LA    R3,PBDELEM                                                       
         MVI   ELCODE,X'25'                                                     
CKQOPT1A BAS   RE,NEXTEL                                                        
         BNE   CKQOPT1D                                                         
         USING PPDUMD03,R3                                                      
         OC    PPDDATE,PPDDATE                                                  
         BZ    CKQOPT1A                                                         
         CLI   ASOFDTE,0             SEE IF I HAVE AN AS OF DATE                
         BE    CKQOPT1C                                                         
         CLC   PPDDATE(3),ASOFDTE                                               
         BH    CKQOPT1A                                                         
*                                                                               
CKQOPT1C MVI   PAIDSW,X'01'       SET DATE PAY ELEM FOUND                       
         CLI   ASOFDTE,0                                                        
         BE    CKQOPT1X         NOT USING AS OF DATE                            
         XC    PGROSS(16),PGROSS                                                
         B     CKQOPT1E         GO PROCESS THIS ELEM                            
*                                                                               
CKQOPT1D DS    0H        I GET HERE IF NO VALID PAY ELEMS                       
         CLI   ASOFDTE,0                                                        
         BE    CKQOPT1X         NOT USING AS OF DATE                            
         XC    PGROSS(16),PGROSS                                                
         B     CKQOPT1X                                                         
*                                                                               
CKQOPT1E DS    0H                                                               
         CLC   PPDDATE(3),ASOFDTE                                               
         BH    CKQOPT1F                                                         
         LM    R7,R9,PGROSS                                                     
         A     R7,PPGROSS                                                       
         A     R8,PPAGYCOM                                                      
         A     R9,PPCSHDSC                                                      
         STM   R7,R9,PGROSS                                                     
         LM    R7,R9,PPGROSS                                                    
         SR    R7,R8                                                            
         SR    R7,R9                                                            
         L     R6,PAID                                                          
         AR    R6,R7                                                            
         ST    R6,PAID                                                          
CKQOPT1F BAS   RE,NEXTEL                                                        
         BE    CKQOPT1E                                                         
         B     CKQOPT1X                                                         
*                                                                               
*                                                                               
NEXTEL   DS    0H       GET  NEXT ELEMENT                                       
         CLI   0(R3),0                                                          
         BE    NEXTELX                                                          
         ZIC   R0,1(R3)                                                         
         AR    R3,R0                                                            
         CLC   0(1,R3),ELCODE                                                   
         BER   RE                                                               
         B     NEXTEL                                                           
NEXTELX  LTR   RE,RE                                                            
         BR    RE                                                               
*                                                                               
         DROP  R3                                                               
*                                                                               
CKQOPT1X TM    PBUYCNTL,X'80'        SEE IF DELETED                             
         BZ    CKPD                                                             
*NEW                                                                            
         LAY   RF,TSTPAID          HAS GETINS O/P INCLUDING FX                  
         OC    28(12,RF),28(RF)    TESTING PGROSS INCLUDING FX                  
*NEW                                                                            
****     OC    PGROSS(12),PGROSS                                                
         BZ    NEXTBUY              NOT PAID SO IGNORE                          
         XC    GROSS(20),GROSS                                                  
*NEW                                                                            
         XC    0(20,RF),0(RF)       SEE ABOVE                                   
*NEW                                                                            
CKPD     CLI   QOPT1,C'P'                                                       
         BNE   CKUNPD                                                           
         LAY   RF,TSTPAID          HAS GETINS O/P INCLUDING FX                  
*NEW                                                                            
         OC    28(12,RF),28(RF)    TESTING PGROSS INCLUDING FX                  
*NEW                                                                            
****     OC    PGROSS(12),PGROSS                                                
         BNZ   CKPD5                                                            
         CLI   PAIDSW,X'01'       SEE IF I FOUND A DATED PAY ELEM               
         BNE   NEXTBUY                                                          
*                                                                               
         OC    0(12,RF),0(RF)     SEE IF FREE BUY INCLUDING FX                  
****     OC    GROSS(12),GROSS    SEE IF FREE BUY                               
         BNZ   CKPD5                                                            
*                                                                               
         CLI   PROGPROF+4,C'Y'  SUPPRESSING "FREE" BUYS ON PAID REQ             
         BE    NEXTBUY                                                          
*                                                                               
CKPD5    L     RF,GSTCOMM                                                       
         S     RF,UPAIDGST                                                      
         ST    RF,GSTCOMM                                                       
         MVC   BUYGST,UPAIDGST IF PAID REQUESTED WANT TO SHOW UNPD L01X         
         B     BUYOKK                                                           
*                                                                               
CKUNPD   CLI   QOPT1,C'U'                                                       
         BNE   BUYOK        DOING ALL ITEMS                                     
*NEW                                                                            
         LAY   RF,TSTPAID          HAS GETINS O/P INCLUDING FX                  
         CLC   0(12,RF),28(RF)     COMPARING GROSS(12) TO PGROSS                
****     CLC   GROSS(12),PGROSS                                                 
         BNE   CKUNPD5                                                          
         OC    0(12,RF),0(RF)      SEE IF "FREE" BUY INCLUDING FX               
****     OC    GROSS(12),GROSS     SEE IF "FREE" BUY                            
*NEW                                                                            
         BNE   NEXTBUY                                                          
*                                                                               
         CLI   PAIDSW,X'01'        SEE IF I HAVE A DATED PAY ELEM               
         BE    NEXTBUY             YES THEN SKIP                                
         CLC   PBUYKDAT,=X'5C0901'  SEE IF BEFORE SEP01/92                      
         BL    NEXTBUY                                                          
         CLI   PROGPROF+3,C'Y'  SEE IF SKIPPING "FREE" ON UNPAID REQ            
         BE    NEXTBUY                                                          
*                                                                               
CKUNPD5  MVC   BUYGST,PAIDGST      PAID GST                        L01X         
         L     RF,GSTCOMM                                                       
         S     RF,PAIDGST                                                       
         ST    RF,GSTCOMM                                                       
         B     BUYOKK                                                           
*                                                                               
BUYOK    DS    0H                                                               
         MVC   BUYGST,THISGST                                      L01X         
BUYOKK   DS    0H                                                               
         CLC   SAVEPUB,PBUYKPUB                                                 
         BE    BUYOK5                                                           
         GOTO1 VPRNTPUB                                                         
BUYOK5   CLC   SAVEPRD,PBUYKPRD                                                 
         BE    BUYOK6                                                           
         GOTO1 =A(PROFRST)                                                      
BUYOK6   DS    0H                                                               
*                                                                               
BUYOK10  MVC   MTHACT(5),=5C'Y'    SET ACTIVITY SWITCHES                        
*                                                                               
         LA    R2,BUYOUTA                                                       
         USING PPBYOUTD,R2                                                      
         GOTO1 PPBYOUT,DMCB,BUYOUTA                                             
         MVC   P+4(8),PBYOINS                                                   
*                                                                               
*DIE      DC    H'0'                                                            
*                                                                               
         CLI   PBYOINS2,C' '                                                    
         BE    PPBY00                                                           
         MVI   PSECOND+3,C'+'                                                   
         MVC   PSECOND+4(8),PBYOINS2                                            
         B     PPBYX                                                            
PPBY00   DS    0H                                                               
*                                                                               
         CLC   PBYOISNM,SPACES                                                  
         BE    PPBYX                                                            
         MVC   PSECOND+5(11),PBYOISNM                                           
         B     PPBYX                                                            
PPBYX    DS    0H                                                               
         TM    PBUYCNTL,X'80'           SEE IF DELETED                          
         BZ    PRTN1                                                            
         MVI   P+12,C'D'                                                        
         MVC   P+53(15),=C'CLEARED+DELETED'                                     
         B     PRTN2                                                            
PRTN1    MVC   P+53(14),PBYOGRS                                                 
PRTN2    DS    0H                                                               
         CLI   QMEDIA,C'N'                                                      
         BNE   PRTMAG                                                           
         MVC   P+25(8),PBYOUR                                                   
         CLI   PBYOSPC,C' '                                                     
         BE    PRTN5                                                            
         MVC   P+34(10),PBYOSPC                                                 
         MVC   PSECOND+34(11),PBYOPRM                                           
         B     PRTN10                                                           
*                                                                               
PRTN5    MVC   P+34(11),PBYOPRM                                                 
*                                                                               
PRTN10   MVC   P+45(7),PBYOUNTS                                                 
         LA    R3,5                                                             
         LA    R4,P+45                                                          
PRTN20   CLI   P+51,C' '                                                        
         BNE   PRTEST                                                           
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   1(0,R4),PBYOUNTS                                                 
         MVI   0(R4),C' '                                                       
         LA    R4,1(R4)                                                         
         BCT   R3,PRTN20                                                        
         B     PRTEST                                                           
*                                                                               
PRTMAG   DS    0H                                                               
         MVC   P+28(20),PBYOSPC                                                 
         MVC   PSECOND+28(20),PBYOSPC2                                          
*                                                                               
PRTEST   CLI   QOPT5,C'Y'      SEE IF FLAGGING BILLED/TRAFFICED ITEMS           
         BNE   *+8                                                              
         BAS   RE,TSTBLTR                                                       
         SR    R0,R0                                                            
         IC    R0,PBUYKEST                                                      
         SLL   R0,8                                                             
         IC    R0,PBUYKEST+1                                                    
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+20(3),DUB+6(2)                                                 
*        GOTO1 DTCNV,DMCB,(1,PBDPDATE),(4,P+14)                                 
         GOTO1 DATCON,DMCB,(3,PBDPDATE),(7,P+14)                                
         LM    R6,R9,GROSS                                                      
         STM   R6,R8,BUYGO                                         L01          
         A     R9,THISGST                                          L01X         
         ST    R9,BUYNP                                            L01          
         SR    R6,R7                                                            
         ST    R6,BUYGLAC                                                       
         EDIT  BUYGLAC,(14,P+68),2,COMMAS=YES,MINUS=YES                         
         EDIT  BUYCD,(12,P+82),2,COMMAS=YES,MINUS=YES                           
         LA    R3,P+95                                             L01          
         CLI   PAGYNAT,C'C'                                        L01          
         BNE   NTCAND1                                             L01          
         EDIT  THISGST,(12,(R3)),2,COMMAS=YES,MINUS=YES            L01X         
         LA    R3,14(R3)                                           L01          
*                                                                               
NTCAND1  DS    0H                                                  L01          
*                                                                               
         EDIT  BUYNP,(14,(R3)),2,COMMAS=YES,MINUS=YES              L01          
         CLC   PBUYKPRD,=C'ZZZ'                                                 
         BNE   PRTE20                                                           
         CLI   PBYOZZZ,C' '                                                     
         BE    PRTE20                                                           
         CLC   PSECOND+28(10),SPACES        SEE IF PSECOND IS USED              
         BNE   PRTE10              YES                                          
         MVC   PSECOND+25(50),PBYOZZZ                                           
         CLI   QMEDIA,C'N'                                                      
         BE    PRTE5                                                            
         MVC   PSECOND+28(50),PBYOZZZ                                           
         MVC   PSECOND+25(3),SPACES                                             
PRTE5    GOTO1 VPRINTIT                                                         
         B     PRTEX                                                            
*                                                                               
PRTE10   GOTO1 VPRINTIT                                                         
         MVC   P+25(50),PBYOZZZ                                                 
         CLI   QMEDIA,C'N'                                                      
         BE    PRTE15                                                           
         MVC   P+28(50),PBYOZZZ                                                 
         MVC   P+25(3),SPACES                                                   
PRTE15   GOTO1 VPRINTIT                                                         
         B     PRTEX                                                            
*                                                                               
PRTE20   GOTO1 VPRINTIT                                                         
PRTEX    OC    PGROSS(12),PGROSS                                                
         BZ    LTSTIO                   ADD TO MTH TOTALS                       
         CLI   QOPT1,C'P'                                                       
         BNE   PRTPPD                                                           
*                                                                               
         CLC   GROSS(12),PGROSS                                                 
         BE    LTSTIO              TOTALLY PAID SO ADD TO MTH ACCUMS            
*                                                                               
         MVC   P+35(17),=C'UNCLEARED PORTION'                                   
*                                                                               
*              SUBTRACT PGROSS FROM GROSS AND PUT IN PGROSS SO THAT             
*              PGROSS,PAGYCOM,PCSHDSC,PAID WILL REFLECT UNPAID AMTS             
*                                                                               
         L     R3,GROSS                                                         
         L     R4,PGROSS                                                        
         SR    R3,R4                                                            
         ST    R3,PGROSS                                                        
         L     R3,AGYCOM                                                        
         L     R4,PAGYCOM                                                       
         SR    R3,R4                                                            
         ST    R3,PAGYCOM                                                       
         L     R3,CSHDSC                                                        
         L     R4,PCSHDSC                                                       
         SR    R3,R4                                                            
         ST    R3,PCSHDSC                                                       
         L     R3,PYABLE                                                        
         L     R4,PAID                                                          
         SR    R3,R4                                                            
         ST    R3,PAID                                                          
******   L     R3,THISGST                                          L01          
******   L     R4,PAIDGST                                          L01          
******   SR    R3,R4                                               L01          
******   ST    R3,THISGST        PAYABLE                           L01          
ADDINGST L     R3,BUYGST                                           L01X         
         A     R3,PAID           INCLUDE GST IN PAYABLE           L01           
         ST    R3,PAID                                                          
         B     PRTPPD1                                                          
PRTPPD   CLI   QOPT1,C'U'                                                       
         BNE   LTSTIO       IF DOING ALL ITEMS - NO ADJUSTMENT                  
         MVC   P+35(18),=C'PREVIOUSLY CLEARED'                                  
         B     ADDINGST                                            L01X         
PRTPPD1  EDIT  PGROSS,(14,P+53),2,COMMAS=YES,MINUS=YES,FLOAT=$                  
         MVI   0(R1),C'('                                                       
         MVC   P+53(14),WORK+3                                                  
         MVI   P+66,C')'                                                        
         C     R0,=F'0'                                                         
         BNL   *+10                                                             
         MVC   P+66(2),=C'-)'                                                   
         LM    R3,R4,PGROSS                                                     
         SR    R3,R4                                                            
         ST    R3,PAGYCOM                                                       
         EDIT  (R3),(14,P+68),2,COMMAS=YES,MINUS=YES,FLOAT=$                    
         MVI   0(R1),C'('                                                       
         MVC   P+68(14),WORK+3                                                  
         MVI   P+81,C')'                                                        
         C     R0,=F'0'                                                         
         BNL   *+10                                                             
         MVC   P+81(2),=C'-)'                                                   
         EDIT  PCSHDSC,(11,P+83),2,COMMAS=YES,MINUS=YES,FLOAT=$                 
         MVI   0(R1),C'('                                                       
         MVC   P+83(11),WORK+6                                                  
         MVI   P+93,C')'                                                        
         C     R0,=F'0'                                                         
         BNL   *+10                                                             
         MVC   P+93(2),=C'-)'                                                   
         LA    R3,P+95                                             L01          
         CLI   PAGYNAT,C'C'                                        L01          
         BNE   NTCAND21                                            L01          
         EDIT  BUYGST,(12,(R3)),2,COMMAS=YES,MINUS=YES,FLOAT=$   L01X           
         MVI   0(R1),C'('                                                       
         MVC   1(11,R3),WORK+6                                     L01          
         MVI   11(R3),C')'                                         L01          
         C     R0,=F'0'                                                         
         BNL   *+10                                                             
         MVC   11(2,R3),=C'-)'                                     L01          
         LA    R3,13(R3)                                           L01X         
         L     RF,THISGST   ORDERED GST                                         
         S     RF,BUYGST                                                        
         ST    RF,BUYGST    GET NET WHEN PAID/UNPAID REQ                        
NTCAND21 DS    0H                                                  L01          
*                                                                               
         EDIT  PAID,(13,(R3)),2,COMMAS=YES,MINUS=YES,FLOAT=$    L01X            
         MVI   0(R1),C'('                                                       
*        MVC   P+94(14),WORK+3                                                  
*        MVI   P+107,C')'                                                       
         MVC   1(14,R3),WORK+3                                     L01          
         MVI   14(R3),C')'                                         L01          
         C     R0,=F'0'                                                         
         BNL   *+10                                                             
         MVC   14(2,R3),=C'-)'                                     L01          
         GOTO1 VPRINTIT                                                         
         L     R3,BUYGO                                                         
         L     R4,PGROSS                                                        
         SR    R3,R4                                                            
         ST    R3,BUYGO                                                         
         L     R3,BUYGLAC                                                       
         L     R4,PAGYCOM                                                       
         SR    R3,R4                                                            
         ST    R3,BUYGLAC                                                       
         L     R3,BUYCD                                                         
         L     R4,PCSHDSC                                                       
         SR    R3,R4                                                            
         ST    R3,BUYCD                                                         
******** MVC   BUYGST,PAIDGST                                     ***1          
         L     R3,BUYNP                                                         
         L     R4,PAID                                                          
         SR    R3,R4                                                            
         ST    R3,BUYNP                                                         
*                                                                               
         CLI   PROGPROF+2,C'Y'                                                  
         BNE   LTSTIO                                                           
*                                                                               
*        NOW PRINT PORTION UNCLEARED                                            
*                                                                               
         MVC   P+35(17),=C'UNCLEARED PORTION'                                   
         CLI   QOPT1,C'U'                                                       
         BE    *+10                                                             
         MVC   P+35(2),SPACES     CHANGE TO "CLEARED"                           
*                                                                               
         EDIT  BUYGO,(14,P+53),2,COMMAS=YES,MINUS=YES                           
         EDIT  BUYGLAC,(14,P+68),2,COMMAS=YES,MINUS=YES                         
         EDIT  BUYCD,(12,P+82),2,COMMAS=YES,MINUS=YES                           
         LA    R3,P+95                                             L01          
         CLI   PAGYNAT,C'C'                                        L01          
         BNE   NTCAND3                                             L01          
         EDIT  BUYGST,(12,(R3)),2,COMMAS=YES,MINUS=YES            L01X          
         LA    R3,14(R3)                                           L01          
*                                                                               
NTCAND3  DS    0H                                                  L01          
*                                                                               
         EDIT  BUYNP,(14,(R3)),2,COMMAS=YES,MINUS=YES              L01          
         GOTO1 VPRINTIT                                                         
*                                                                               
LTSTIO   DS    0H                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*     SEE IF HAVE FOREIGN EXCHANGE AND SEE IF IT SHOULD BE INCLUDED             
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         LAY   R6,FWORK                                                         
         OC    0(80,R6),0(R6)      ANY FOREIGN EXCHANGE ?                       
         BZ    LTSTIOX             NO                                           
*                                                                               
         BRAS  RE,FXRTN            PROCESS FOREIGN EXCHANGE FOR BUY             
*                                                                               
LTSTIOX  DS    0H                                                               
*                                                                               
         LA    R6,PBUYREC+33     CHECK FOR REF ELEM                             
         MVI   ELCODE,X'83'                                                     
         BAS   RE,LNEXTEL                                                       
         BNE   LTST2                                                            
         MVC   P+28(4),=C'REF='                                                 
         ZIC   R1,1(R6)                                                         
         SH    R1,=H'3'            ELCODE + LEN + 1 FOR EX                      
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   P+32(0),2(R6)                                                    
         GOTO1 VPRINTIT                                                         
*                                                                               
LTST2    LA    R6,PBUYREC+33     CHECK FOR SPECIAL REP ELEM                     
         MVI   ELCODE,X'80'                                                     
         BAS   RE,LNEXTEL                                                       
         BNE   LTST4                                                            
         MVC   P+28(5),=C'SREP='                                                
         MVC   P+34(4),2(R6)                                                    
         GOTO1 VPRINTIT                                                         
*                                                                               
LTST4    CLI   PROGPROF+1,C'Y'                                                  
         BNE   LTST31                                                           
         XC    SV70ELM,SV70ELM                                                  
         LA    R6,PBUYREC+33                                                    
         MVI   ELCODE,X'70'                                                     
LTST5    BAS   RE,LNEXTEL                                                       
         BNE   LTST15                                                           
         OC    2(3,R6),2(R6)                                                    
         BZ    LTST5                                                            
         CLC   SV70ELM+2(3),2(R6)                                               
         BH    LTST5                                                            
         MVC   SV70ELM(11),0(R6)                                                
         B     LTST5                                                            
*                                                                               
*                                                                               
*              SAVE FIRST 11 BYTES OF 70 ELEM                                   
SV70ELM  DS    CL11                                                             
ELCODE   DS    CL1                                                              
*                                                                               
LNEXTEL  DS    0H       GET  NEXT ELEMENT                                       
         CLI   0(R6),0                                                          
         BE    LNEXTELX                                                         
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLC   0(1,R6),ELCODE                                                   
         BER   RE                                                               
         B     LNEXTEL                                                          
LNEXTELX LTR   RE,RE                                                            
         BR    RE                                                               
*                                                                               
*                                                                               
*                                                                               
LTST15   OC    SV70ELM,SV70ELM                                                  
         BZ    LTST31                                                           
*        GOTO1 DTCNV,DMCB,(1,SV70ELM+2),(0,P+38)                                
         GOTO1 DATCON,DMCB,(3,SV70ELM+2),(0,P+38)                               
         MVC   P+28(9),=C'LAST I/O='                                            
         MVC   P+37(1),PBUYREC+2                                                
         MVI   P+38,C'-'                                                        
         MVI   P+44,C'-'                                                        
         MVC   HALF,SV70ELM+5                                                   
         LH    R0,HALF                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+45(4),DUB                                                      
*                                                                               
         CLI   SV70ELM+10,C'N'                                                  
         BNE   LTST20                                                           
         MVC   P+50(3),=C'NEW'                                                  
         B     LTST30                                                           
*                                                                               
LTST20   CLI   SV70ELM+10,C'C'                                                  
         BNE   LTST25                                                           
         MVC   P+50(3),=C'CHA'                                                  
         B     LTST30                                                           
*                                                                               
LTST25   CLI   SV70ELM+10,C'D'                                                  
         BNE   LTST30                                                           
         MVC   P+50(3),=C'CAN'                                                  
*                                                                               
*LTST30   GOTO1 DTCNV,DMCB,(1,SV70ELM+2),(3,P+54)                               
LTST30   GOTO1 DATCON,DMCB,(3,SV70ELM+2),(5,P+54)                               
         GOTO1 VPRINTIT                                                         
*                                                                               
LTST31   LA    R6,PBUYREC+33     CHECK INTERNET SITE LOCATION ELEM              
         MVI   ELCODE,X'98'                                                     
         BAS   RE,LNEXTEL                                                       
         BNE   RLMTH                                                            
         MVC   P+28(5),=C'SITE='                                                
         MVC   P+34(20),2(R6)                                                   
         GOTO1 VPRINTIT                                                         
*                                                                               
RLMTH    DS    0H          PRINT UP TO 5 COMMENTS                               
         CLC   PBYOBFD(L'PBYOBFD),SPACES                                        
         BE    RL0                                                              
         MVC   P+28(L'PBYOBFD),PBYOBFD                                          
         GOTO1 VPRINTIT                                                         
*                                                                               
RL0      LA    R3,PBYOCOMS                                                      
         LA    R4,5                                                             
RL1      CLC   0(47,R3),SPACES                                                  
         BE    RL2X                                                             
         MVC   P+28(47),0(R3)                                                   
         GOTO1 VPRINTIT                                                         
         LA    R3,47(R3)                                                        
         BCT   R4,RL1                                                           
*                                                                               
         DROP  R2                                                               
*                                                                               
RL2X     DS    0H                                                               
         OC    PBDJOB,PBDJOB                                                    
         BZ    RLM2                                                             
         CLI   QOPT4,C' '                                                       
         BE    RLM2                                                             
         CLI   QOPT4,C'A'         AD NO ONLY                                    
         BE    RLM1A          NO NEED TO READ JOB REC                           
*                                                                               
*                                                                               
RLM1     DS    0H                                                               
         TM    QOPT4,X'30'                                                      
         BZ    RLM1A                                                            
         MVC   P+28(6),=C'COPY ='                                               
         MVC   P+28+7(17),PJOBCPY                                               
         B     RLM1B                                                            
*                                                                               
RLM1A    DS    0H                                                               
         MVC   P+28(8),=C'AD NO. ='                                             
         MVC   P+28+9(6),PBDJOB                                                 
*                                                                               
RLM1B    GOTO1 VPRINTIT                                                         
         TM    QOPT4,X'02'                                                      
         BZ    RLM2                                                             
         MVC   P+28(25),PJOBCAP1                                                
         MVC   PSECOND+28(25),PJOBCAP2                                          
*                                                                               
         GOTO1 VPRINTIT                                                         
*                                                                               
RLM2     DS    0H                                                               
*                                                                               
         CLI   PBDSPACE,C'*'      SEE IF REAL INSERTION                         
         BE    RLMTH1              NO - BYPASS INS TOTALS                       
         AP    MTHINS,=P'1'                                                     
RLMTH1   CLI   QMEDIA,C'N'         IF MAG                                       
         BNE   RLMTH2              BYPASS LINES                                 
RLM2A    CLI   PROGPROF,C'I'                                                    
         BNE   RLM2C                                                            
         CLI   PBDUIND,X'89'                                                    
         BE    RLMTH1B                                                          
         CLI   PBDUIND,C'I'                                                     
         BE    RLM2B                                                            
         L     R6,UNITS                                                         
         CVD   R6,DUB                                                           
         MP    DUB,=P'1000'                                                     
         DP    DUB,=P'14'                                                       
         SRP   DUB(6),63,5                                                      
         ZAP   DUB,DUB(6)                                                       
         CVB   R6,DUB                                                           
         ST    R6,UNITS                                                         
         B     RLMTH1B                                                          
RLM2B    L     R6,UNITS                                                         
         CVD   R6,DUB                                                           
         MP    DUB,=P'100'                                                      
         CVB   R6,DUB                                                           
         ST    R6,UNITS                                                         
         B     RLMTH1B                                                          
RLM2C    CLI   PBDUIND,C'I'        CONVERT INCHES TO LINES FOR ACCUMS           
         BE    RLMTH1A                                                          
         CLI   PBDUIND,X'89'       CONVERT INCHES TO LINES FOR ACCUMS           
         BNE   RLMTH1B             LOWER CASE I INCHES TO 2 DECIMALS            
RLMTH1A  SR    R6,R6                                                            
         L     R7,UNITS                                                         
         M     R6,=F'14'                                                        
         ST    R7,UNITS                                                         
         CLI   PBDUIND,X'89'       SEE IF LOWER CASE I                          
         BNE   RLMTH1B             NO                                           
         CVD   R7,DUB                                                           
         AP    DUB,=P'50'          MUST ROUND TO NEAREST LINE                   
         DP    DUB,=P'100'                                                      
         ZAP   DUB,DUB(6)                                                       
         CVB   R7,DUB                                                           
         ST    R7,UNITS                                                         
*                                                                               
RLMTH1B  EQU   *                                                                
         L     R3,UNITS                                                         
         CVD   R3,DUB                                                           
         AP    MTHLINES,DUB                                                     
RLMTH2   L     R3,BUYGO                                                         
         CVD   R3,DUB                                                           
         AP    MTHGO,DUB                                                        
         L     R3,BUYGLAC                                                       
         CVD   R3,DUB                                                           
         AP    MTHGLAC,DUB                                                      
         L     R3,BUYCD                                                         
         CVD   R3,DUB                                                           
         AP    MTHCD,DUB                                                        
         L     R3,GSTCOMM                                          L01          
         CVD   R3,DUB                                              L01          
         AP    MTHGST,DUB                                          L01          
         L     R3,BUYNP                                                         
         CVD   R3,DUB                                                           
         AP    MTHNP,DUB                                                        
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
         BRAS  RE,ACGRTN           CHECK FOR AND OUTPUT ADDL CHG DATA           
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         MVC   SAVEYMD,BLBLDT                                                   
         CLI   QBPDATE,C'B'                                                     
         BE    NEXTBUY                                                          
         MVC   SAVEYMD,PBUYKDAT                                                 
         B     NEXTBUY                                                          
*                                                                               
*                                                                               
NEXTBUY  DS    0H                                                               
         B     EXT                                                              
         EJECT                                                                  
INITIAL  GOTO1 =V(IINITIAL)                                                     
         BR    R9                                                               
         EJECT                                                                  
*              ROUTINE TO FLAG BILLED/TRAFFICED ITEMS                           
TSTBLTR  NTR                                                                    
         LA    R3,PBDELEM                                                       
TSTB1    CLI   0(R3),X'26'                                                      
         BNE   TSTB4                                                            
         OC    5(3,R3),5(R3)       CK FOR ANY DATE                              
         BZ    TSTB4                                                            
         CLI   ASOFDTE,0                                                        
         BNE   TSTB2                                                            
         MVI   P+1,C'B'                                                         
         B     NEXTBEL                                                          
*                                                                               
TSTB2    CLC   5(3,R3),ASOFDTE     CHK VS. AS OF DATE                           
         BH    NEXTBEL                                                          
         MVI   P+1,C'B'                                                         
         B     NEXTBEL                                                          
*                                                                               
TSTB4    CLI   0(R3),X'70'                                                      
         BNE   NEXTBEL                                                          
         OC    2(3,R3),2(R3)                                                    
         BZ    NEXTBEL                                                          
         CLI   ASOFDTE,0                                                        
         BNE   TSTB6                                                            
         MVI   P+2,C'T'                                                         
         B     NEXTBEL                                                          
*                                                                               
TSTB6    CLC   2(3,R3),ASOFDTE                                                  
         BH    NEXTBEL                                                          
         MVI   P+2,C'T'                                                         
*                                                                               
NEXTBEL  SR    R0,R0                                                            
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         CLI   0(R3),0         END OF REC                                       
         BNE   TSTB1                                                            
*                                                                               
TSTBX    XIT                                                                    
EXT      XMOD1 1                                                                
THISGST  DS    F ORDERED GST       L01X                                         
PAIDGST  DS    F PAID GST          L01X                                         
UPAIDGST DS    F UNPAID GST        L01X                                         
GSTCOMM  DS    F DEPENDING ON OPTION COULD BE UNPAID /PAID         L01X         
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
PROFRST  NMOD1 0,PROFRST                                                        
*                                                                               
         L     RC,PPFILEC                                                       
         USING PPFILED,RC                                                       
*                                                                               
         CLI   PRDSW,1             SEE IF DOING PRDS SEPARATELY                 
         BE    PRDFX               SKIP PRD PRINT                               
         CLC   QCLIENT,=C'ALL'                                                  
         BE    PRDF                                                             
         CLI   QCLIENT,C'*'        OFFICE REQS                                  
         BE    PRDF                                                             
         CLI   QCLIENT,C'$'        OFFICE LIST                                  
         BE    PRDF                                                             
         CLI   QCLIENT,C'&&'       GROUP REQS                                   
         BE    PRDF                                                             
         B     PRDF0               ALL PRDS - PRD WILL BE IN TABLE              
*                                                                               
PRDF     DS    0H                                                               
         CLI   QPUB,C'0'           SEE IF DOING ONE PUB                         
         BNL   PRDF3               YES- MUST READ PRODUCT                       
PRDF0    L     R6,APRDTAB                                                       
PRDF1    CLC   0(3,R6),=X'FFFFFF'                                               
         BNE   *+6                                                              
         DC    H'0'                PRD NOT FOUND                                
         CLC   0(3,R6),PBUYKPRD                                                 
         BE    PRDF4                                                            
         LA    R6,23(R6)                                                        
         B     PRDF1                                                            
*                                                                               
*                                                                               
PRDF3    DS    0H                                                               
         MVC   PPGKEY,KEY          SAVE PPG KEY                                 
         XC    KEY,KEY                                                          
         MVC   KEY(2),QAGENCY                                                   
         MVC   KEY+2(1),QMEDIA                                                  
         MVI   KEY+3,X'06'                                                      
         MVC   KEY+4(3),PCLTKCLT                                                
         MVC   KEY+7(3),PBUYKPRD                                                
         LA    R4,DMRDHI                                                        
         MVC   KEYSAVE,KEY                                                      
         BAS   RE,PDIRRD                                                        
         CLC   KEYSAVE(10),KEY                                                  
         BE    *+6                                                              
         DC    H'0'                PRD NOT FOUND                                
         TM    KEY+25,X'80'                                                     
         BZ    *+6                                                              
         DC    H'0'                FATAL ERROR PRD IS DELETED                   
         LA    R4,GETREC                                                        
         LA    R3,PPRDREC                                                       
         BAS   RE,PFILERD                                                       
PRDF3C   DS    0H                                                               
         MVC   KEY,PPGKEY                                                       
         LA    R4,DMRDHI                                                        
         BAS   RE,PDIRRD                                                        
         B     PRDF5                                                            
*                                                                               
PRDF4    MVC   PPRDNAME(20),3(R6)                                               
         MVC   PPRDKPRD(3),PBUYKPRD                                             
*                                                                               
PRDF5    DS    0H                                                               
         MVC   P+1(7),=C'PRODUCT'                                               
         MVC   P+9(3),PBUYKPRD                                                  
         MVC   P+14(20),PPRDNAME                                                
         IC    R0,LINE                                                          
         AH    R0,=H'3'                                                         
         STC   R0,SAVELINE                                                      
         CLC   SAVELINE,MAXLINES                                                
         BNH   *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
         GOTO1 VPRINTIT                                                         
         GOTO1 VPRINTIT                                                         
PRDF8    MVC   SAVEPRD,PBUYKPRD                                                 
*                                                                               
PRDFX    DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
PDIRRD   NTR                                                                    
         GOTO1 DATAMGR,DMCB,(DMINBTS,(R4)),=C'PRTDIR',KEY,KEY,(0,0)             
         CLI   DMCB+8,0                                                         
         BE    PDIRX                                                            
         CLI   DMCB+8,X'02'          PASS DELETES                               
         BE    PDIRX                                                            
         DC    H'0'                                                             
PDIRX    XIT                                                                    
*                                                                               
*                                                                               
PFILERD  NTR                                                                    
         GOTO1 DATAMGR,DMCB,(DMINBTS,(R4)),=C'PRTFILE',KEY+27,         X        
               (R3),(0,DMWORK)                                                  
         CLI   DMCB+8,0                                                         
         BE    PFILX                                                            
         DC    H'0'                                                             
PFILX    XIT                                                                    
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'ACGRTN - ADDITIONAL CHARGE PRINT OUTPUT'                        
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*        CHECK FOR AND OUTPUT ADDITIONAL CHARGE CODES AND VALUES                
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
ACGRTN   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING PPWORKD,RA                                                       
         USING PPFILED,RC                                                       
         USING PAYWORKD,R5                                                      
*                                                                               
         MVC   SAVEKEY,KEY         SAVE CURRENT KEY                             
*                                                                               
*NOF*    CLI   QOPT7,C'F'          SEPARATE FX (FOREIGN EXCHANGE) ?             
*NOF*    BE    ACGR10              YES                                          
*                               GET ADDITIONAL CHARGE DATA                      
*NOF*    GOTO1 VPPGETCG,DMCB,(C'T',PBUYREC),DATAMGR,GETINS,PBUYKPRD             
*NOF*    B     ACGR20                                                           
*                                                                               
ACGR10   DS    0H                                                               
*                 GET ADDITIONAL CHARGE DATA EXCLUDING FX (X IN PARAM4)         
         GOTO1 VPPGETCG,DMCB,(C'T',PBUYREC),DATAMGR,GETINS,            X        
               (C'X',PBUYKPRD)                                                  
*                                                                               
ACGR20   DS    0H                                                               
         CLI   0(R1),X'FF'         ERROR IN CALL ?                              
         BNE   *+6                 NO                                           
         DC    H'0'                                                             
*                                                                               
         CLI   0(R1),X'00'         ANY ADD'L. CHARGES FOUND ?                   
         BE    ACGRTNX             NO                                           
*                                                                               
         ZIC   R3,0(R1)         FOR LUP COUNTER - NO OF LINES IN BLOCK          
*                                                                               
         ICM   RE,15,0(R1)         RE POINTING TO DATA BLOCK                    
         BNZ   *+6                                                              
         DC    H'0'                ADDRESS IS NULLS                             
*                                                                               
         L     RF,AADCGWRK         ADDRESS OF AREA FOR ADD'L. CHGS.             
         LA    R1,ADCDLNTH         LENGTH OF BLOCK                              
         MOVE  ((RF),(R1)),(RE)    MOVE BLOCK TO ADCGWRK AREA IN W/S            
*                                                                               
         L     R4,AADCGWRK         WORK AREA FOR ADD'L. CHGS.                   
         USING ADDCHGD,R4                                                       
*                                                                               
         MVC   P+21(24),=C'** ADDITIONAL CHARGES **'                            
         GOTO1 VPRINTIT                                                         
*                                                                               
ACGLUP   MVC   P+21(ADCLNLEN),ADCCOD1                                           
         GOTO1 VPRINTIT                                                         
         LA    R4,ADCLNLEN(R4)     BUMP TO NEXT BLOCK "LINE"                    
         BCT   R3,ACGLUP                                                        
*                                                                               
         DROP  R4                                                               
*                                                                               
         MVC   KEY,SAVEKEY         RESTORE CURRENT KEY                          
         LA    R4,DMRDHI                                                        
         GOTO1 DATAMGR,DMCB,(DMINBTS,(R4)),=C'PRTDIR',KEY,KEY,(0,0)             
         CLI   DMCB+8,0                                                         
         BE    ACGLUPP                                                          
         CLI   DMCB+8,X'02'          PASS DELETES                               
         BE    *+6                                                              
         DC    H'0'                                                             
ACGLUPP  CLC   KEY(25),SAVEKEY                                                  
         BE    *+6                                                              
         DC    H'0'                MUST BE FOUND                                
*                                                                               
ACGRTNX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         DROP  R5                                                               
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'FXRTN - FOREIGN EXCHANGE PRINT OUTPUT PLUS'                     
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*        PROCESS AND PRINT FOREIGN EXCHANGE FOR BUY                             
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
FXRTN    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
*NOF*    CLI   QOPT7,C'F'          SEPARATE FX FROM OTHER ADD'L CHG'S.?         
*NOF*    BNE   FXRTNX              NO                                           
*                                                                               
         USING PPWORKD,RA                                                       
         USING PPFILED,RC                                                       
         USING PAYWORKD,R5                                                      
*                                                                               
*****    GETINS VALUES FOR FX (FOREIGN EXCHANGE) ARE STORED AT FWORK            
*                                                                               
         LAY   R2,FWORK            FWORK HAS STORED GETINS VALUES FOR           
         USING GROSS,R2              FX (FOREIGN EXCHANGE)                      
*                                                                               
         OC    GROSS(12),GROSS     ZERO VALUE RETURNED ?                        
         BZ    FXRTNX              YES - DO NOT PRINT                           
*                                                                               
         CLI   PBDSPACE,C'*'      SEE IF REAL INSERTION                         
         BE    FXRTN05             NO - BYPASS INS TOTALS                       
         AP    MTHINSF,=P'1'                                                    
*                                                                               
FXRTN05  DS    0H                                                               
         TM    PBUYCNTL,X'80'      DELETED ?                                    
         BNO   FXRTN10             NO                                           
*                                                                               
         OC    PGROSS(12),PGROSS                                                
         BZ    FXRTNX               NOT PAID SO IGNORE                          
*                                                                               
         XC    GROSS(20),GROSS                                                  
         B     FXRTN15                                                          
*                                                                               
FXRTN10  DS    0H                                                               
*                                                                               
         EDIT  GROSS,(14,P+53),2,COMMAS=YES,MINUS=YES                           
*                                                                               
FXRTN15  DS    0H                                                               
*                                                                               
         MVC   P+10(20),=C'* FOREIGN EXCHANGE *'                                
*                                                                               
         LM    R6,R9,GROSS                                                      
         STM   R6,R8,BUYGOF                                                     
*****    A     R9,THISGST                                                       
         ST    R9,BUYNPF                                                        
         SR    R6,R7                                                            
         ST    R6,BUYGLACF                                                      
         EDIT  BUYGLACF,(14,P+68),2,COMMAS=YES,MINUS=YES                        
         EDIT  BUYCDF,(12,P+82),2,COMMAS=YES,MINUS=YES                          
         LA    R3,P+95                                                          
         CLI   PAGYNAT,C'C'                                                     
         BNE   FXRTN20                                                          
*****    EDIT  THISGST,(12,(R3)),2,COMMAS=YES,MINUS=YES                         
         LA    R3,14(R3)                                                        
*                                                                               
FXRTN20  DS    0H                                                               
*                                                                               
         EDIT  BUYNPF,(14,(R3)),2,COMMAS=YES,MINUS=YES                          
         GOTO1 VPRINTIT                                                         
*                                                                               
         LAY   R2,TSTPAID         POINT TO GETINS VALUES FROM PPG               
         USING GROSS,R2            THAT WOULD INCLUDE FGN EXCH (FX)             
*                                                                               
         OC    PGROSS(12),PGROSS                                                
         BZ    FXRTN90                  ADD TO MTH TOTALS                       
         CLI   QOPT1,C'P'                                                       
         BNE   FXRTN40                                                          
*                                                                               
         CLC   GROSS(12),PGROSS    TEST TOTAL VALUES INCLUDING FX               
         BE    FXRTN90             TOTALLY PAID SO ADD TO MTH ACCUMS            
         DROP R2                                                                
*                                                                               
         LAY   R2,FWORK            FWORK HAS STORED GETINS VALUES FOR           
         USING GROSS,R2              FX (FOREIGN EXCHANGE)                      
*                                                                               
         MVC   P+35(17),=C'UNCLEARED PORTION'                                   
*                                                                               
*              SUBTRACT PGROSS FROM GROSS AND PUT IN PGROSS SO THAT             
*              PGROSS,PAGYCOM,PCSHDSC,PAID WILL REFLECT UNPAID AMTS             
*                                                                               
         L     R3,GROSS                                                         
         L     R4,PGROSS                                                        
         SR    R3,R4                                                            
         ST    R3,PGROSS                                                        
         L     R3,AGYCOM                                                        
         L     R4,PAGYCOM                                                       
         SR    R3,R4                                                            
         ST    R3,PAGYCOM                                                       
         L     R3,CSHDSC                                                        
         L     R4,PCSHDSC                                                       
         SR    R3,R4                                                            
         ST    R3,PCSHDSC                                                       
         L     R3,PYABLE                                                        
         L     R4,PAID                                                          
         SR    R3,R4                                                            
         ST    R3,PAID                                                          
*****    L     R3,THISGST                                          L01          
*****    L     R4,PAIDGST                                          L01          
*****    SR    R3,R4                                               L01          
*****    ST    R3,THISGST        PAYABLE                           L01          
*****ADDINGST L     R3,BUYGST                                                   
*****    A     R3,PAID           INCLUDE GST IN PAYABLE           L01           
*****    ST    R3,PAID                                                          
*****    B     PRTPPD1                                                          
*                                                                               
FXRTN40  DS    0H                                                               
         CLI   QOPT1,C'P'                                                       
         BE    FXRTN50                                                          
         CLI   QOPT1,C'U'                                                       
         BNE   FXRTN90      IF DOING ALL ITEMS - NO ADJUSTMENT                  
         MVC   P+35(18),=C'PREVIOUSLY CLEARED'                                  
*****    B     ADDINGST                                            L01X         
*****PRTPPD1  DS    0H                                                          
*                                                                               
FXRTN50  DS    0H                                                               
         DROP  R2                                                               
         LAY   R2,FWORK            FWORK HAS STORED GETINS VALUES FOR           
         USING GROSS,R2              FX (FOREIGN EXCHANGE)                      
*                                                                               
         EDIT  PGROSS,(14,P+53),2,COMMAS=YES,MINUS=YES,FLOAT=$                  
         MVI   0(R1),C'('                                                       
         MVC   P+53(14),WORK+3                                                  
         MVI   P+66,C')'                                                        
         C     R0,=F'0'                                                         
         BNL   *+10                                                             
         MVC   P+66(2),=C'-)'                                                   
         LM    R3,R4,PGROSS                                                     
         SR    R3,R4                                                            
         ST    R3,PAGYCOM                                                       
         EDIT  (R3),(14,P+68),2,COMMAS=YES,MINUS=YES,FLOAT=$                    
         MVI   0(R1),C'('                                                       
         MVC   P+68(14),WORK+3                                                  
         MVI   P+81,C')'                                                        
         C     R0,=F'0'                                                         
         BNL   *+10                                                             
         MVC   P+81(2),=C'-)'                                                   
         EDIT  PCSHDSC,(11,P+83),2,COMMAS=YES,MINUS=YES,FLOAT=$                 
         MVI   0(R1),C'('                                                       
         MVC   P+83(11),WORK+6                                                  
         MVI   P+93,C')'                                                        
         C     R0,=F'0'                                                         
         BNL   *+10                                                             
         MVC   P+93(2),=C'-)'                                                   
         LA    R3,P+95                                             L01          
         CLI   PAGYNAT,C'C'                                        L01          
         BNE   FXCAND21                                            L01          
*****    EDIT  BUYGST,(12,(R3)),2,COMMAS=YES,MINUS=YES,FLOAT=$   L01X           
*****    MVI   0(R1),C'('                                                       
*****    MVC   1(11,R3),WORK+6                                     L01          
*****    MVI   11(R3),C')'                                         L01          
*****    C     R0,=F'0'                                                         
*****    BNL   *+10                                                             
*****    MVC   11(2,R3),=C'-)'                                     L01          
         LA    R3,13(R3)                                           L01X         
*****    L     RF,THISGST   ORDERED GST                                         
*****    S     RF,BUYGST                                                        
*****    ST    RF,BUYGST    GET NET WHEN PAID/UNPAID REQ                        
FXCAND21 DS    0H                                                  L01          
*                                                                               
         EDIT  PAID,(13,(R3)),2,COMMAS=YES,MINUS=YES,FLOAT=$    L01X            
         MVI   0(R1),C'('                                                       
*        MVC   P+94(14),WORK+3                                                  
*        MVI   P+107,C')'                                                       
         MVC   1(14,R3),WORK+3                                     L01          
         MVI   14(R3),C')'                                         L01          
         C     R0,=F'0'                                                         
         BNL   *+10                                                             
         MVC   14(2,R3),=C'-)'                                     L01          
         GOTO1 VPRINTIT                                                         
*                                                                               
         L     R3,BUYGOF                                                        
         L     R4,PGROSS                                                        
         SR    R3,R4                                                            
         ST    R3,BUYGOF                                                        
         L     R3,BUYGLACF                                                      
         L     R4,PAGYCOM                                                       
         SR    R3,R4                                                            
         ST    R3,BUYGLACF                                                      
         L     R3,BUYCDF                                                        
         L     R4,PCSHDSC                                                       
         SR    R3,R4                                                            
         ST    R3,BUYCDF                                                        
******** MVC   BUYGST,PAIDGST                                     ***1          
         L     R3,BUYNPF                                                        
         L     R4,PAID                                                          
         SR    R3,R4                                                            
         ST    R3,BUYNPF                                                        
*                                                                               
         CLI   PROGPROF+2,C'Y'                                                  
         BNE   FXRTN90                                                          
*                                                                               
*        NOW PRINT PORTION UNCLEARED                                            
*                                                                               
         MVC   P+35(17),=C'UNCLEARED PORTION'                                   
         CLI   QOPT1,C'U'                                                       
         BE    *+10                                                             
         MVC   P+35(2),SPACES     CHANGE TO "CLEARED"                           
*                                                                               
         EDIT  BUYGOF,(14,P+53),2,COMMAS=YES,MINUS=YES                          
         EDIT  BUYGLACF,(14,P+68),2,COMMAS=YES,MINUS=YES                        
         EDIT  BUYCDF,(12,P+82),2,COMMAS=YES,MINUS=YES                          
         LA    R3,P+95                                                          
         CLI   PAGYNAT,C'C'                                                     
         BNE   FXCAND3                                                          
*****    EDIT  BUYGST,(12,(R3)),2,COMMAS=YES,MINUS=YES                          
         LA    R3,14(R3)                                                        
*                                                                               
FXCAND3  DS    0H                                                               
*                                                                               
         EDIT  BUYNPF,(14,(R3)),2,COMMAS=YES,MINUS=YES                          
         GOTO1 VPRINTIT                                                         
*                                                                               
FXRTN90  DS    0H          NOW ADD BUY FX VALUES TO "MONTH" FX TOTALS           
*                                                                               
         DROP  R2                                                               
         LAY   R2,FWORK            FWORK HAS STORED GETINS VALUES FOR           
         USING GROSS,R2              FX (FOREIGN EXCHANGE)                      
*                                                                               
         L     R3,BUYGOF                                                        
         CVD   R3,DUB                                                           
         AP    MTHGOF,DUB                                                       
         L     R3,BUYGLACF                                                      
         CVD   R3,DUB                                                           
         AP    MTHGLACF,DUB                                                     
         L     R3,BUYCDF                                                        
         CVD   R3,DUB                                                           
         AP    MTHCDF,DUB                                                       
*****    L     R3,GSTCOMM                                                       
*****    CVD   R3,DUB                                                           
*****    AP    MTHGST,DUB                                                       
         L     R3,BUYNPF                                                        
         CVD   R3,DUB                                                           
         AP    MTHNPF,DUB                                                       
*                                                                               
FXRTNX   DS    0H                                                               
         XIT1                                                                   
*                                                                               
         DROP  R2,R5                                                            
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
PRNTPUB  CSECT                                                                  
         NMOD1 0,PRNTPUB                                                        
         USING PPWORKD,RA                                                       
         USING PAYWORKD,R5         WAS R5,R9                                    
         USING PPFILED,RC                                                       
         L     RC,PPFILEC                                                       
         L     R5,VPAYWORK                                                      
         CLI   QOPT2,C'Y'     YES= SKIP ON NEW PUB                              
         BNE   *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
         SR    R0,R0                                                            
         IC    R0,LINE                                                          
         AH    R0,=H'6'                                                         
         STC   R0,SAVELINE                                                      
         CLC   SAVELINE,MAXLINES                                                
         BNH   *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
         LAY   R7,PUBREC                                                        
         USING PUBREC,R7                                                        
         MVC   P+1(17),SPACES                                                   
         MVI   PUBPSW,0                                                         
         MVC   SAVPLIN1,SPACES                                                  
         MVC   SAVPLIN2,SPACES                                                  
         IC    R3,PAGYPROF+12                                                   
         GOTO1 PUBEDIT,DMCB,((R3),PBUYKPUB),(0,SAVPLIN1+1)                      
         CLI   FORCEHED,C'Y'                                                    
         BE    SETR3               DONT SKIP A LINE                             
         GOTO1 VPRINTIT                                                         
SETR3    LA    R3,SAVPLIN1+1                                                    
         LA    R4,SAVPLIN2+1                                                    
         SR    R9,R9                                                            
SETR3A   CLI   0(R3),C' '                                                       
         BE    FLOAT                                                            
         MVI   0(R4),C'-'                                                       
         LA    R9,1(R9)                                                         
         LA    R4,1(R4)                                                         
         LA    R3,1(R3)                                                         
         B     SETR3A                                                           
*                                                                               
FLOAT    LA    R3,1(R3)                                                         
         LA    R4,1(R4)                                                         
         LA    R9,1(R9)            SAVE DISPLACEMENT                            
         GOTO1 VPUBFLOT,DMCB,PUBREC,(R3)                                        
         MVC   P,SAVPLIN1                                                       
         MVC   PSECOND,SAVPLIN2                                                 
         MVC   0(11,R4),=C'(CONTINUED)'                                         
*                                                                               
         LA    R3,P+1                                                           
         AR    R3,R9                                                            
         LA    R4,PSECOND+1                                                     
         AR    R4,R9                                                            
*                                                                               
         MVC   SAVEPUB,PBUYKPUB                                                 
         CLI   QOPT6,C'Y'          SEE IF PRINTING PAYING ADDR                  
         BE    PRNTP1                                                           
         CLC   QSORT,=C'08'         OR REP SORT                                 
         BE    PRNTP1                                                           
         CLC   QSORT,=C'09'         OR PAY ADDR NAME                            
         BE    PRNTP1                                                           
         GOTO1 VPRINTIT                                                         
         B     PRNTPX                                                           
*                                                                               
PRNTP1   DS    0H                                                               
         CLI   FORCEHED,C'Y'                                                    
         BE    PRNTP2                                                           
         IC    R0,SAVELINE                                                      
         CLC   QSORT,=C'08'        SEE IF SORTING BY REP                        
         BNE   *+8                                                              
         AH    R0,=H'1'            FOR REP NUMBER LINE                          
         CLC   QSORT,=C'09'        OR PAY ADDR NAME                             
         BNE   *+8                                                              
         AH    R0,=H'1'            FOR REP NUMBER LINE                          
         CLI   QOPT6,C'Y'          SEE IF SHOWING ADDRS                         
         BNE   PRNTP1C                                                          
         AH    R0,=H'4'                                                         
         CLI   PREPNAME,C' '       SEE IF ADDR FOUND                            
         BH    *+8                                                              
         AH    R0,=H'1'                                                         
PRNTP1C  STC   R0,SAVELINE                                                      
         CLC   SAVELINE,MAXLINES                                                
         BNH   *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
PRNTP2   DS    0H                                                               
         CLI   PREPNAME,C' '       SEE IF ADDR FOUND                            
         BH    PRNTP2C             YES                                          
         MVC   0(20,R4),=C'** PAY PUB DIRECT **'                                
         GOTO1 VPRINTIT                                                         
         CLI   QOPT6,C'Y'          SEE IF SHOWING ADDR                          
         BNE   PRNTP7                                                           
         MVC   0(30,R3),PUBLINE1   P                                            
         MVC   0(30,R4),PUBLINE2   PSECOND                                      
         GOTO1 VPRINTIT                                                         
         LA    R6,PUBREC+33                                                     
PRNTP2A  CLI   0(R6),X'11'                                                      
         BE    PRNTP2B                                                          
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0             END OF REC                                   
         BE    PRNTP7              NO ATTN OR TELE                              
         B     PRNTP2A                                                          
*                                                                               
PRNTP2B  DS    0H                                                               
         USING PUBSADEL,R6                                                      
         LR    R4,R3                                                            
         CLI   PUBATTN,C' '                                                     
         BNH   PRNTP2B2                                                         
         MVC   0(5,R4),=C'ATTN='                                                
         MVC   6(24,R4),PUBATTN                                                 
         LA    R4,31(R4)                                                        
*                                                                               
PRNTP2B2 CLI   PUBTEL,C' '                                                      
         BNH   PRNTP6              NO TELE                                      
         MVC   0(6,R4),=C'PHONE='                                               
         MVC   7(12,R4),PUBTEL                                                  
         B     PRNTP6                                                           
*                                                                               
         DROP  R6                                                               
         GOTO1 VPRINTIT                                                         
         B     PRNTP7                                                           
*                                                                               
PRNTP2C  DS    0H                                                               
         CLC   QSORT,=C'08'        SEE IF DOING REP SORT                        
         BE    PRNTP2C5                                                         
         CLC   QSORT,=C'09'        OR PAY ADDR NAME                             
         BE    PRNTP2C5                                                         
         B     PRNTP2F                                                          
*                                                                               
PRNTP2C5 CLI   PREPKEY+3,X'11'     SEE IF I FOUND A REP                         
         BNE   PRNTP2F             NO WAS PAY ADDR                              
         MVC   0(4,R4),=C'REP='                                                 
         MVC   5(4,R4),PREPKREP                                                 
         CLI   PREPKREP+4,0                                                     
         BE    PRNTP2D                                                          
         MVI   9(R4),C'.'                                                       
         MVC   10(1,R4),PREPKREP+4    SUFFIX                                    
PRNTP2D  GOTO1 VPRINTIT                                                         
         LR    R4,R3                                                            
*                                                                               
PRNTP2F  MVC   0(30,R4),PREPNAME                                                
         GOTO1 VPRINTIT                                                         
         CLI   QOPT6,C'Y'          SEE IF SHOWING ADDR                          
         BNE   PRNTP7              NO THEN DONE                                 
         CLI   PREPLIN1,C' '                                                    
         BNH   PRNTP3                                                           
         MVC   0(30,R3),PREPLIN1                                                
         GOTO1 VPRINTIT                                                         
*                                                                               
PRNTP3   CLI   PREPLIN2,C' '                                                    
         BNH   PRNTP4                                                           
         MVC   0(30,R3),PREPLIN2                                                
         GOTO1 VPRINTIT                                                         
*                                                                               
PRNTP4   LR    R4,R3                                                            
         CLI   PREPATTN,C' '                                                    
         BNH   PRNTP5                                                           
         MVC   0(5,R4),=C'ATTN='                                                
         MVC   6(20,R4),PREPATTN                                                
         LA    R4,27(R4)                                                        
*                                                                               
PRNTP5   CLI   PREPTEL,C' '                                                     
         BNH   PRNTP6              NO TELEPHONE                                 
         MVC   0(6,R4),=C'PHONE='                                               
         MVC   7(12,R4),PREPTEL                                                 
*                                                                               
PRNTP6   CLC   P,SPACES                                                         
         BE    PRNTP7                                                           
         GOTO1 VPRINTIT                                                         
*                                                                               
PRNTP7   DS    0H                                                               
         GOTO1 VPRINTIT                                                         
PRNTPX   DS    0H                                                               
         MVI   PUBPSW,1            SET PUB PRINTED                              
         XIT1                                                                   
*                                                                               
         DROP  R7,R5                                                            
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
IINITIAL CSECT                                                                  
         NMOD1 0,INITL                                                          
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         USING PPFILED,RC                                                       
         USING PAYWORKD,R5              WAS R5,R6 (3/24/08)                     
*                                                                               
         LAY   RF,MTHTAB                                                        
         USING MTHTAB,RF                                                        
         XC    MTHTAB(250),MTHTAB       LINKED VIA R9.                          
         XC    MTHTAB+250(L'MTHTAB-250),MTHTAB+250                              
         DROP  RF                                                               
*                                                                               
         OI    DMINBTS,X'08'       SET TO PASS DELETES                          
         L     RF,=V(MTHEND)           RELOCATE ROUTINES                        
         A     RF,RELO                                                          
         ST    RF,VMTHEND                                                       
         L     RF,=V(PRDEND)                                                    
         A     RF,RELO                                                          
         ST    RF,VPRDEND                                                       
         L     RF,=V(PUBEND)                                                    
         A     RF,RELO                                                          
         ST    RF,VPUBEND                                                       
         L     RF,=V(CLTEND)                                                    
         A     RF,RELO                                                          
         ST    RF,VCLTEND                                                       
         L     RF,=V(PRINTIT)                                                   
         A     RF,RELO                                                          
         ST    RF,VPRINTIT                                                      
         L     RF,=V(BLDMLST)                                                   
         A     RF,RELO                                                          
         ST    RF,VBLDMLST                                                      
         L     RF,=V(PAYWORK)                                                   
         A     RF,RELO                                                          
         ST    RF,VPAYWORK                                                      
         L     RF,=V(CLIF)                                                      
         A     RF,RELO                                                          
         ST    RF,VCLIFRST                                                      
         L     RF,=V(PUBF)                                                      
         A     RF,RELO                                                          
         ST    RF,VPUBFRST                                                      
         L     RF,=V(PUBFLOAT)                                                  
         A     RF,RELO                                                          
         ST    RF,VPUBFLOT                                                      
         L     RF,=V(REPEND)                                                    
         A     RF,RELO                                                          
         ST    RF,VREPEND                                                       
         L     RF,=V(REPCSCT)                                                   
         A     RF,RELO                                                          
         ST    RF,VREPCSCT                                                      
         L     RF,=V(CLTCSCT)                                                   
         A     RF,RELO                                                          
         ST    RF,VCLTCSCT                                                      
         L     RF,=V(PROCSCT)                                                   
         A     RF,RELO                                                          
         ST    RF,VPROCSCT                                                      
         L     RF,=V(REPCSCTF)                                                  
         A     RF,RELO                                                          
         ST    RF,VREPCSCF                                                      
         L     RF,=V(CLTCSCTF)                                                  
         A     RF,RELO                                                          
         ST    RF,VCLTCSCF                                                      
         L     RF,=V(PROCSCTF)                                                  
         A     RF,RELO                                                          
         ST    RF,VPROCSCF                                                      
         L     RF,=V(PRNTPUB)                                                   
         A     RF,RELO                                                          
         ST    RF,VPRNTPUB                                                      
         L     RF,=V(PPGETCG)                                                   
         A     RF,RELO                                                          
         ST    RF,VPPGETCG                                                      
         LA    R2,BUYOUTA                                                       
         USING PPBYOUTD,R2                                                      
         XC    PBYOINPT(24),PBYOINPT                                            
         LA    R6,PBUYREC                                                       
         L     R7,DATCON                                                        
         LA    R8,GROSS                                                         
         STM   R6,R8,0(R2)                                                      
         MVI   PBYOCTL,X'28'                                                    
*                                                                               
*        GET A(OFFICER)                                                         
*                                                                               
         XC    DMCB(12),DMCB                                                    
*                                                                               
         MVC   DUB,SPACES          GET OFFICER                                  
         MVC   DUB(6),=C'T00A38'                                                
         GOTO1 LOADER,DMCB,DUB,0                                                
*                                                                               
         MVC   VOFFICER,4(R1)      SAVE ADDRESS                                 
*                                                                               
         L     RF,=V(PRNTOFC)                                                   
         A     RF,RELO                                                          
         ST    RF,VPRNTOFC         STORE PRNTOFC ADDRESS                        
*                                                                               
         DROP  R2                                                               
*                                                                               
         MVI   FCRDBUY,C'Y'        RESET TO READ 20 POINTERS                    
         MVI   FCRDACTV,C'Y'       SET TO READ ACTIVE ONLY                      
         MVI   FCGTJOB,C'N'        RESET JOB RECORD READ                        
         MVI   PRDSW,1                                                          
         CLC   QPRODUCT(3),=C'   '                                              
         BNE   *+12                                                             
         MVI   FCRDBUY,X'21'       USE 21 POINTERS                              
         MVI   PRDSW,0                                                          
         JIF   QOPT4,EQ,C' ',OR,QOPT4,EQ,C'A',INIT1,JUMP=N                      
         MVI   FCGTJOB,C'Y'        SO PPG WILL GET JOBRECS                      
*                                                                               
INIT1    DS    0H                                                               
         MVI   FCGTREP,C'N'                                                     
         CLI   QOPT6,C'Y'          SEE IF SHOWING PAYING ADDRESS                
         BE    INIT2                                                            
         CLC   QSORT,=C'08'        OR SORTING BY REP                            
         BE    INIT2                                                            
         CLC   QSORT,=C'09'        OR SORTING BY PAY ADDR NAME                  
         BE    INIT2                                                            
         B     INIT2B                                                           
INIT2    MVI   FCGTREP,C'Y'                                                     
INIT2B   CLI   QPAY,C' '           SEE IF USING AS OF DATE                      
         BE    INIT3                                                            
         MVC   DUB(2),RCDATE+6                                                  
         MVC   DUB+2(2),RCDATE+0                                                
         MVC   DUB+4(2),RCDATE+3                                                
*                                                                               
         GOTO1 DATCON,DMCB,(0,QPAY),(0,QPAY)                                    
         GOTO1 DATCON,DMCB,(0,DUB),(0,DUB)                                      
*                                                                               
         CLC   QPAY(6),DUB         SEE IF AS OF DATE IS TODAY                   
         BNL   INIT3               OR FUTURE - YES CAN USE FCPDFILT             
         B     INIT4                                                            
*                                                                               
INIT3    MVC   FCPDFILT,QOPT1      P=PAID,U=UNPAID                              
         CLI   QOPT1,C' '                                                       
         BNE   *+8                                                              
INIT4    MVI   FCPDFILT,C'N'       RESET TO N                                   
         MVC   PAGE,=H'1'                                                       
         MVI   REQERR,0                                                         
         CLC   SVMEDCLI,QMEDIA     CHK FOR SAME MEDIA/CLIENT                    
         BNE   FRCLI                                                            
INOUTX   XIT                                                                    
FRCLI    DS    0H                  FIRST REQUEST FOR CLIENT                     
         L     R6,APRDTAB                                                       
         CLC   QCLIENT,=C'ALL'                                                  
         BE    FRCLI1                                                           
         CLI   QCLIENT,C'*'                                                     
         BE    FRCLI1                                                           
         CLI   QCLIENT,C'$'            OFFICE LIST                              
         BE    FRCLI1                                                           
         CLI   QCLIENT,C'&&'           GROUP                                    
         BE    FRCLI1                                                           
         CLC   QPRODUCT,=C'ALL'        ONLY BUILD TABLE FOR ALL PRDS            
         BE    FRCLI1C                                                          
         CLC   QPRODUCT,=C'   '                                                 
         BE    FRCLI1C                                                          
FRCLI1   MVC   0(3,R6),=X'FFFFFF'                                               
         XC    SVMEDCLI,SVMEDCLI                                                
         B     FRCLIX              PRD TABLE WILL BE BUILT AT CLTFRST           
*                                  IF NECESSARY                                 
*                                                                               
FRCLI1C  DS    0H                                                               
         MVC   PPGKEY,KEY          SAVE PPG'S KEY                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),QAGENCY                                                   
         MVC   KEY+2(1),QMEDIA                                                  
         MVI   KEY+3,X'06'                                                      
         MVC   KEY+4(3),QCLIENT                                                 
         LA    R4,DMRDHI                                                        
         MVC   KEYSAVE,KEY                                                      
         B     FRCLI5                                                           
*                                                                               
FRCLI2   LA    R4,DMRSEQ                                                        
FRCLI5   DS    0H                                                               
******   BAS   RE,PDIRRD                                                        
         GOTO1 DATAMGR,DMCB,(DMINBTS,(R4)),=C'PRTDIR',KEY,KEY,(0,0)             
         CLI   DMCB+8,0                                                         
         BE    PDIRXX1                                                          
         CLI   DMCB+8,X'02'          PASS DELETES                               
         BE    PDIRXX1                                                          
         DC    H'0'                                                             
PDIRXX1  CLC   KEY(7),KEYSAVE                                                   
         BNE   FRCLI20                                                          
         TM    KEY+25,X'80'                                                     
         BO    FRCLI2              BYPASS DELETED PRDS                          
         LA    R4,GETREC                                                        
         LA    R3,PPRDREC                                                       
*****    BAS   RE,PFILERD                                                       
         GOTO1 DATAMGR,DMCB,(DMINBTS,(R4)),=C'PRTFILE',KEY+27,         X        
               (R3),(0,DMWORK)                                                  
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   0(3,R6),PPRDKPRD                                                 
         MVC   3(20,R6),PPRDNAME                                                
         LA    R6,23(R6)                                                        
         B     FRCLI2                                                           
*                                                                               
FRCLI20  MVC   0(3,R6),=X'FFFFFF'                                               
         MVC   KEY,PPGKEY                                                       
         LA    R4,DMRDHI                                                        
*******  BAS   RE,PDIRRD                                                        
         GOTO1 DATAMGR,DMCB,(DMINBTS,(R4)),=C'PRTDIR',KEY,KEY,(0,0)             
         CLI   DMCB+8,0                                                         
         BE    PDIRXX                                                           
         CLI   DMCB+8,X'02'          PASS DELETES                               
         BE    PDIRXX                                                           
         DC    H'0'                                                             
PDIRXX   MVC   SVMEDCLI,QMEDIA     SAVE MEDIA/CLIENT                            
FRCLIX   B     INOUTX                                                           
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
BLDMLST  CSECT                                                                  
         NMOD1 0,BLDMLST                                                        
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         USING PPFILED,RC                                                       
         L     R5,VPAYWORK                                                      
         USING PAYWORKD,R5                                                      
SPRDOK   MVC   WORK(12),QSTART       SAVE STRT AND END                          
         CLI   QBPDATE,C'B'                                                     
         BE    BILLPAY                                                          
         CLI   QBPDATE,C'P'                                                     
         BE    BILLPAY                                                          
BLDLIST  LA    R6,ACCNUM                SET FOR BCT                             
         LAY   R4,MTHTAB                                                        
         MVC   0(4,R4),QSTART                                                   
PUTMTH   PACK  DUB,2(2,R4)                                                      
         AP    DUB,=P'1'                                                        
         CP    DUB,=P'13'                                                       
         BL    SAMEYR                                                           
         SP    DUB,=P'12'                                                       
         OI    DUB+7,X'0F'                                                      
         UNPK  10(2,R4),DUB+6(2)                                                
         PACK  DUB,0(2,R4)                                                      
         BAS   RE,BLDMUPYR         ADD 1 TO YR                                  
         UNPK  8(2,R4),DUB+6(2)                                                 
         CLC   8(4,R4),QEND                                                     
         BL    NEXTMTH                                                          
         BE    MTHDONE             EXIT                                         
         XC    8(8,R4),8(R4)       CLEAR LAST MTH                               
         B     MTHDONE             EXIT                                         
*                                                                               
SAMEYR   OI    DUB+7,X'0F'                                                      
         UNPK  10(2,R4),DUB+6(2)                                                
         MVC   8(2,R4),0(R4)                                                    
         CLC   8(4,R4),QEND                                                     
         BL    NEXTMTH                                                          
         BE    MTHDONE                                                          
         XC    8(8,R4),8(R4)                                                    
         B     MTHDONE             EXIT                                         
*                                                                               
NEXTMTH  LA    R4,8(R4)                                                         
         BCT   R6,PUTMTH           ACCNUM MTHS MAX                              
         B     MTHDONE             EXIT                                         
*                                                                               
*                                                                               
*                                                                               
BILLPAY  PACK  DUB,QSTART+2(2)     SET START DATE BACK 3 MONTHS                 
         SP    DUB,=P'3'                                                        
         CP    DUB,=P'0'                                                        
         BNH   CHGSYR                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  QSTART+2(2),DUB+6(2)                                             
         B     CHGEND                                                           
*                                                                               
CHGSYR   AP    DUB,=P'12'                                                       
         OI    DUB+7,X'0F'                                                      
         UNPK  QSTART+2(2),DUB+6(2)                                             
         PACK  DUB,QSTART(2)                                                    
         BAS   RE,BLDMDNYR         SUBTRACT 1 FROM YR                           
         UNPK  QSTART(2),DUB+6(2)                                               
*                                                                               
CHGEND   PACK  DUB,QEND+2(2)                                                    
         AP    DUB,=P'6'      ADVANCE END DATE 6 MONTHS                         
         CP    DUB,=P'12'                                                       
         BH    CHGEYR                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  QEND+2(2),DUB+6(2)                                               
         B     BLDLIST                                                          
*                                                                               
CHGEYR   SP    DUB,=P'12'                                                       
         OI    DUB+7,X'0F'                                                      
         UNPK  QEND+2(2),DUB+6(2)                                               
         PACK  DUB,QEND(2)                                                      
         BAS   RE,BLDMUPYR         ADD 1 TO YR                                  
*        AP    DUB,=P'1'                                                        
*        OI    DUB+7,X'0F'                                                      
         UNPK  QEND(2),DUB+6(2)                                                 
         B     BLDLIST                                                          
*                                                                               
MTHDONE  MVC   QSTART(12),WORK    RESTORE DATES                                 
*                                                                               
         MVC   WORK(6),QSTART                                                   
         MVC   WORK+4(1),=C'01'    DAY TO 01                                    
         GOTO1 ADDAY,DMCB,WORK,WORK+6,F'-75'                                    
*        SUBTRACTING 75 DAYS WILL ALWAYS GET ME BACK 3 MONTHS                   
         MVC   WORK+10(2),=C'01'    SET DAY TO THE 1ST                          
*                                                                               
         GOTO1 DATCON,DMCB,(0,WORK+6),(3,CDPDATE)                               
*                                                                               
*        CDPDATE SHOULD NOW BE SET TO THE FIRST DAY OF                          
*        OF THE OF THE 3RD MONTHT PRIOR TO THE REQUEST                          
*        THIS DATE WILL BE USED TO DETERMINE THE CD STATUS OF THE PUB           
*        WHEN USING QOPT3  (CD OR NON CD PUBS ONLY)                             
*                                                                               
         XC    ASOFDTE,ASOFDTE                                                  
         CLI   QPAY,C' '           AS OF DATE IN QPAY YYMMDD                    
         BE    MTHD1                                                            
*        GOTO1 DTCNV,DMCB,(0,QPAY),(1,ASOFDTE)                                  
         GOTO1 DATCON,DMCB,(0,QPAY),(3,ASOFDTE)                                 
*        GOTO1 DTCNV,DMCB,(1,ASOFDTE),(3,ASDATE)                                
         GOTO1 DATCON,DMCB,(3,ASOFDTE),(5,ASDATE)                               
MTHD1    DS    0H                  NOW CLEAR ALL ACCUMS                         
         LA    R6,7                WAS 6                           L01          
         LA    R4,MTHTOTS                                                       
CLRMTH   ZAP   0(8,R4),=P'0'                                                    
         LA    R4,8(R4)                                                         
         BCT   R6,CLRMTH                                                        
         LA    R6,7                WAS 6                           L01          
         LA    R4,PRDTOTS                                                       
CLRPRDA  ZAP   0(8,R4),=P'0'                                                    
         LA    R4,8(R4)                                                         
         BCT   R6,CLRPRDA                                                       
         LA    R6,ACCNUM*7         WAS 6                           L01          
         LA    R4,PUBTOTS                                                       
CLRPUB   ZAP   0(8,R4),=P'0'                                                    
         LA    R4,8(R4)                                                         
         BCT   R6,CLRPUB                                                        
*                                                                               
         L     R9,VREPCSCT                                                      
         USING REPDSCT,R9                                                       
         LA    R6,ACCNUM*8         WAS 7                           L01          
         LA    R4,REPTOTS                                                       
CLRREP   ZAP   0(8,R4),=P'0'                                                    
         LA    R4,8(R4)                                                         
         BCT   R6,CLRREP                                                        
         XC    RTOTPUBS,RTOTPUBS                                                
         DROP  R9                                                               
*                                                                               
         L     R9,VPROCSCT                                                      
         USING PRODSCT,R9                                                       
         LA    R6,ACCNUM*8         WAS 7                           L01          
         LA    R4,PROTOTS                                                       
CLRPRO   ZAP   0(8,R4),=P'0'                                                    
         LA    R4,8(R4)                                                         
         BCT   R6,CLRPRO                                                        
         XC    PTOTPUBS,PTOTPUBS                                                
         DROP  R9                                                               
*                                                                               
         L     R9,VCLTCSCT                                                      
         USING CLTDSCT,R9                                                       
         LA    R6,ACCNUM*8         WAS 7                           L01          
         LA    R4,CLTTOTS                                                       
CLRCLT   ZAP   0(8,R4),=P'0'                                                    
         LA    R4,8(R4)                                                         
         BCT   R6,CLRCLT                                                        
         XC    CTOTPUBS,CTOTPUBS                                                
         DROP  R9                                                               
*                             NOW CLEAR ALL FOREIGN EXCHANGE ACCUMS             
         LA    R6,7                                                             
         LA    R4,MTHTOTSF                                                      
CLRMTHF  ZAP   0(8,R4),=P'0'                                                    
         LA    R4,8(R4)                                                         
         BCT   R6,CLRMTHF                                                       
         LA    R6,7                                                             
         LA    R4,PRDTOTSF                                                      
CLRPRDAF ZAP   0(8,R4),=P'0'                                                    
         LA    R4,8(R4)                                                         
         BCT   R6,CLRPRDAF                                                      
         LA    R6,ACCNUM*7                                                      
         LAY   R4,PUBTOTSF                                                      
CLRPUBF  ZAP   0(8,R4),=P'0'                                                    
         LA    R4,8(R4)                                                         
         BCT   R6,CLRPUBF                                                       
*                                                                               
         L     R9,VREPCSCF                                                      
         USING REPDSCTF,R9                                                      
         LA    R6,ACCNUM*8                                                      
         LA    R4,REPTOTSF                                                      
CLRREPF  ZAP   0(8,R4),=P'0'                                                    
         LA    R4,8(R4)                                                         
         BCT   R6,CLRREPF                                                       
         XC    RTOTPUBF,RTOTPUBF                                                
         DROP  R9                                                               
*                                                                               
         L     R9,VPROCSCF                                                      
         USING PRODSCTF,R9                                                      
         LA    R6,ACCNUM*8                                                      
         LA    R4,PROTOTSF                                                      
CLRPROF  ZAP   0(8,R4),=P'0'                                                    
         LA    R4,8(R4)                                                         
         BCT   R6,CLRPROF                                                       
         XC    PTOTPUBF,PTOTPUBF                                                
         DROP  R9                                                               
*                                                                               
         L     R9,VCLTCSCF                                                      
         USING CLTDSCTF,R9                                                      
         LA    R6,ACCNUM*8         WAS 7                           L01          
         LA    R4,CLTTOTSF                                                      
CLRCLTF  ZAP   0(8,R4),=P'0'                                                    
         LA    R4,8(R4)                                                         
         BCT   R6,CLRCLTF                                                       
         XC    CTOTPUBF,CTOTPUBF                                                
         DROP  R9                                                               
*                                                                               
         MVI   MTHACT,0            CLEAR ACTIVITY INDICATORS                    
         MVI   PRDACT,0                                                         
         MVI   PUBACT,0                                                         
         MVI   REPACT,0                                                         
         MVI   CLTACT,0                                                         
         XC    PRDMTHS,PRDMTHS                                                  
         XC    PRDMTHSF,PRDMTHSF                                                
         XC    PUBPRDS,PUBPRDS                                                  
         XC    PUBPRDSF,PUBPRDSF                                                
         GOTO1 DATCON,DMCB,(0,QSTART),(3,REQST)                                 
         GOTO1 DATCON,DMCB,(0,QEND),(3,REQEND)                                  
*                                                                               
BLMEXT   XIT1                                                                   
*                                                                               
****  INCREMENT YEARS BY 1 FOR "FA = 2000, FB=2010, ETC." FORMATS               
*                                                                               
BLDMUPYR DS    0H                                                               
         NTR1                                                                   
         MVC   STRHIYR,DUB+6       SAVE HI-ORDER OF YR                          
         TM    DUB+7,X'90'         YR ENDING IN 9 ??                            
         BO    BMCYUP              YES                                          
         MVI   DUB+6,X'00'         "CLEAR" HI-ORDER OF YR                       
         AP    DUB,=P'1'           INCREMENT YR                                 
         OI    DUB+7,X'0F'                                                      
         MVC   DUB+6(1),STRHIYR    RESTORE HI-ORDER OF YR                       
         B     BLDMUPX             DONE WITH YR NOT ENDING IN 9                 
*                                                                               
BMCYUP   DS    0H                                                               
         MVI   DUB+7,X'0F'         LO-ORDER OF YR MUST BE 0 HERE                
         ZIC   R0,STRHIYR                                                       
         AH    R0,=H'1'            INCREMENT HI-ORDER OF YR                     
         STC   R0,DUB+6            RESTORE HI-ORDER OF YR                       
*                                                                               
BLDMUPX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
****  DECREMENT YEARS BY 1 FOR "FA = 2000, FB=2010, ETC." FORMATS               
*                                                                               
BLDMDNYR DS    0H                                                               
         NTR1                                                                   
         MVC   STRHIYR,DUB+6       SAVE HI-ORDER OF YR                          
         TM    DUB+7,X'F0'         YR ENDING IN 0 ??                            
         BZ    BMCYDN              YES                                          
         MVI   DUB+6,X'00'         "CLEAR" HI-ORDER OF YR                       
         SP    DUB,=P'1'           DECREMENT YR                                 
         OI    DUB+7,X'0F'                                                      
         MVC   DUB+6(1),STRHIYR    RESTORE HI-ORDER OF YR                       
         B     BLDMDNX             DONE WITH YR NOT ENDING IN 0                 
*                                                                               
BMCYDN   DS    0H                                                               
         MVI   DUB+7,X'9F'         LO-ORDER OF YR MUST BE 9 HERE                
         ZIC   R0,STRHIYR                                                       
         SH    R0,=H'1'            DECREMENT HI-ORDER OF YR                     
         STC   R0,DUB+6            RESTORE HI-ORDER OF YR                       
*                                                                               
BLDMDNX  DS    0H                                                               
         XIT1                                                                   
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
PRINTIT  CSECT                                                                  
         NMOD1 0,PRINTIT                                                        
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         USING PPFILED,RC                                                       
         L     R5,VPAYWORK                                                      
         USING PAYWORKD,R5                                                      
         MVI   RCSUBPRG,4                                                       
         CLI   PROGPROF,C'I'                                                    
         BNE   PRINT1                                                           
         MVI   RCSUBPRG,6                                                       
*                                                                               
PRINT1   DS    0H                                                               
         CLI   QCLIENT,C'$'        OFFICE LIST                                  
         BNE   PRINT1B                                                          
         MVC   HEAD3(11),=C'OFFICE LIST'                                        
         MVC   HEAD3+12(1),QCLIENT+1                                            
         B     PRINT1C                                                          
*                                                                               
PRINT1B  CLI   QCLIENT,C'*'        SEE IF OFFICE REQUEST                        
         BNE   PRINT1D                                                          
PRINT1C  MVC   HEAD4(6),=C'OFFICE'                                              
         MVC   HEAD4+9(24),SVPTOFC SVPTOFC IS OUTPUT FROM PRNTOFC CALL          
         B     PRINT1X                                                          
*                                                                               
PRINT1D  CLI   QCLIENT,C'&&'       GROUP                                        
         BNE   PRINT1X                                                          
         MVC   HEAD4(5),=C'GROUP'                                               
         MVC   HEAD4+6(1),PCLTBLGP                                              
         B     PRINT1X                                                          
*                                                                               
PRINT1X  CLI   PRDSW,0             SEE IF DOING PRDS SEPARATELY                 
         BE    CKMED                                                            
         CLC   P+1(20),=C' ** CLIENT TOTALS **'                                 
         BE    CKMED                                                            
         MVC   HEAD6(7),=C'PRODUCT'                                             
         MVC   HEAD6+9(3),PPRDKPRD                                              
         MVC   HEAD6+13(20),PPRDNAME                                            
CKMED    CLI   QMEDIA,C'N'                                                      
         BE    *+8                                                              
         MVI   RCSUBPRG,2                                                       
CKEST    CLC   QEST,=C'ALL'                                                     
         BE    CKESTX                                                           
         CLC   QEST,=C'   '                                                     
         BE    CKEST2                                                           
*                                                                               
CKEST1   ZIC   R2,RCSUBPRG                                                      
         LA    R2,1(R2)                                                         
         STC   R2,RCSUBPRG         SET SPROG - TO PRINT EST FROM PPG            
         B     CKESTX                                                           
*                                                                               
CKEST2   CLC   QESTEND,=C'   '                                                  
         BE    CKESTX                                                           
         B     CKEST1              PRINT FILTERS                                
*                                                                               
CKESTX   DS    0H                                                               
         CLC   P+2(9),=C'** CLIENT'        SEE IF DOING CLT TOTS                
         BE    PRINT2                                                           
         CLC   P+1(10),=C'** PRODUCT'        DOING PRD TOTAL LINE               
         BNE   PRINT4                        FOR PRODUCTS SEPARATELY            
PRINT2   MVI   RCSUBPRG,0                                                       
         MVC   HEAD10+5(7),=C'BILLING'                                          
         CLI   QBPDATE,C'B'                                                     
         BE    PRINT4                                                           
         MVC   HEAD10+4(9),=C'INSERTION'                                        
*                                                                               
PRINT4   MVC   HEAD8(26),=C'** CASH DISC. PUBS ONLY **'                         
         CLI   QOPT3,C'C'                                                       
         BE    CKPUP                                                            
         MVC   HEAD8(30),=C'** NON CASH DISC. PUBS ONLY **'                     
         CLI   QOPT3,C'N'                                                       
         BE    CKPUP                                                            
         MVC   HEAD8(33),SPACES                                                 
CKPUP    MVC   HEAD8+75(26),=C'** UNCLEARED ITEMS ONLY **'                      
         CLI   QOPT1,C'U'                                                       
         BE    CKBP                                                             
         MVC   HEAD8+75(26),=C' ** CLEARED ITEMS ONLY ** '                      
         CLI   QOPT1,C'P'                                                       
         BE    CKBP                                                             
         MVC   HEAD8+75(26),SPACES                                              
CKBP     CLI   QBPDATE,C'B'                                                     
         BE    BILMSG                                                           
         CLI   QBPDATE,C'P'                                                     
         BNE   CKASOF                                                           
         MVC   HEAD4+43(19),=C'** PAYING PERIOD **'                             
         B     CKASOF                                                           
*                                                                               
BILMSG   MVC   HEAD4+43(20),=C'** BILLING PERIOD **'                            
CKASOF   CLI   ASOFDTE,0                                                        
         BE    PRINTX         AS OF DATE NOT USED                               
         MVC   HEAD5+45(5),=C'AS OF'                                            
         MVC   HEAD5+51(8),ASDATE                                               
PRINTX   CLI   PAGYNAT,C'C'                                        L01          
         BNE   PRINTX1                                             L01          
         ZIC   RF,RCSUBPRG                                         L01          
         LA    RF,8(RF)                                            L01          
         STC   RF,RCSUBPRG                                         L01          
PRINTX1  CLC   LINE,MAXLINES                                                    
         BL    PRINTX5                                                          
         CLI   PUBPSW,1            SEE IF PUB NAME ALREADY PRINTED              
         BNE   PRINTX5             NO - DONT PRINT CONTINUED MESSAGE            
         MVC   SAVEP,P                                                          
         MVC   SAVPSEC,PSECOND                                                  
         MVC   P,SAVPLIN1                                                       
         MVC   PSECOND,SAVPLIN2                                                 
         GOTO1 REPORT                                                           
         MVC   P,SAVEP                                                          
         MVC   PSECOND,SAVPSEC                                                  
PRINTX5  GOTO1 REPORT                                                           
         XIT1                                                                   
         LTORG                                                                  
SAVEP    DS    CL132                                                            
SAVPSEC  DS    CL132                                                            
         EJECT                                                                  
MTHEND   CSECT                                                                  
         NMOD1 0,MTHEND                                                         
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         USING PPFILED,RC                                                       
         L     R5,VPAYWORK                                                      
         USING PAYWORKD,R5                                                      
         CLI   MTHACT,C'Y'                                                      
         BNE   MTHENDX             NO ACTIVITY                                  
         CLI   QBPDATE,C'B'            THEN NO MTH BREAKS                       
         BE    MTHEND2C                                                         
*                                                                               
*        GOTO1 DTCNV,DMCB,(1,SAVEYMD),(5,P+5)                                   
         GOTO1 DATCON,DMCB,(3,SAVEYMD),(9,P+5)                                  
         MVC   P+11(6),=C'-TOTAL'                                               
*                                                                               
         CP    MTHINS,=P'0'                                                     
         BNH   MTHEND1                                                          
         EDIT  MTHINS,(3,P+28),0                                                
         MVC   P+32(10),=C'INSERTIONS'                                          
         CP    MTHINS,=P'1'                                                     
         BNE   MTHEND1                                                          
         MVI   P+41,C' '                                                        
MTHEND1  CLI   QMEDIA,C'N'                                                      
         BNE   MTHEND2             OMIT LINES FOR MAGS + SUPS                   
         CP    MTHLINES,=P'0'                                                   
         BZ    MTHEND2             NO LINES                                     
         CLI   PROGPROF,C'I'                                                    
         BNE   MTHEND1A                                                         
         EDIT  MTHLINES,(9,P+42),2                                              
         MVI   P+51,C'I'                                                        
         MVI   P+52,C'*'                                                        
         B     MTHEND2                                                          
MTHEND1A EDIT  MTHLINES,(6,P+45),0                                              
         MVI   P+51,C'L'                                                        
         MVI   P+52,C'*'                                                        
MTHEND2  DS    0H                                                               
         EDIT  MTHGO,(14,P+53),2,COMMAS=YES,MINUS=YES                           
         CP    MTHGO,=P'0'                                                      
         BL    *+8                                                              
         MVI   P+66,C'*'                                                        
         EDIT  MTHGLAC,(14,P+68),2,COMMAS=YES,MINUS=YES                         
         CP    MTHGLAC,=P'0'                                                    
         BL    *+8                                                              
         MVI   P+81,C'*'                                                        
         EDIT  MTHCD,(12,P+82),2,COMMAS=YES,MINUS=YES                           
         CP    MTHCD,=P'0'                                                      
         BL    *+8                                                              
         MVI   P+93,C'*'                                                        
         LA    R3,P+95                                             L01          
         CLI   PAGYNAT,C'C'                                        L01          
         BNE   NTCAND4                                             L01          
         EDIT  MTHGST,(12,(R3)),2,COMMAS=YES,MINUS=YES            L01           
         MVI   12(R3),C'*'                                                      
         CP    MTHGST,=P'0'                                        L01          
         BL    *+10                                                L01          
         MVC   11(2,R3),=C'* '                                     L01          
         LA    R3,14(R3)                                           L01          
NTCAND4  DS    0H                                                  L01          
*                                                                               
         EDIT  MTHNP,(14,(R3)),2,COMMAS=YES,MINUS=YES              L01          
         MVI   14(R3),C'*'                                                      
         CP    MTHNP,=P'0'                                                      
         BL    *+10                                                             
*        MVI   P+107,C'*'                                                       
         MVC   13(2,R3),=C'* '                                    L01           
         ZIC   R3,MAXLINES                                                      
         MVI   MAXLINES,99                                                      
         GOTO1 VPRINTIT                                                         
         STC   R3,MAXLINES                                                      
******   CLC   LINE,MAXLINES                                                    
******   BNL   MTHEND2B                                                         
******   GOTO1 VPRINTIT                                                         
*                                                                               
MTHEND2B CLI   PRDSW,1                SEE IF DOING PRDS SEPERATELY              
         BE    MTHEND3C                                                         
*                                                                               
*    ROLL TO PRD TOTALS                                                         
*                                                                               
MTHEND2C L     R3,PRDMTHS                                                       
         A     R3,=F'1'                                                         
         ST    R3,PRDMTHS                                                       
         LA    R3,PRDINS                                                        
         LA    R4,MTHINS                                                        
         LA    R6,7                WAS 6                           L01          
MTHEND3  AP    0(8,R3),0(8,R4)                                                  
         LA    R3,8(R3)                                                         
         LA    R4,8(R4)                                                         
         BCT   R6,MTHEND3                                                       
*                                                                               
*     ROLL TO PUB ACCUMS                                                        
MTHEND3C L     R3,PUBMTHS                                                       
         AH    R3,=H'1'                                                         
         ST    R3,PUBMTHS                                                       
*                                                                               
*        GOTO1 DTCNV,DMCB,(1,SAVEYMD),(0,WORK)                                  
         GOTO1 DATCON,DMCB,(3,SAVEYMD),(0,WORK)                                 
         LAY   R3,MTHTAB                                                        
         LA    R4,0                                                             
COMPDAT  CLC   WORK(4),0(R3)                                                    
         BE    MTHEND4                                                          
         LA    R3,8(R3)                                                         
         LA    R4,8(R4)                                                         
         CLC   0(4,R3),=4X'00'                                                  
         BNE   COMPDAT                                                          
         DC    H'0'                MTH NOT FOUND IN LIST                        
*                                                                               
MTHEND4  LA    R3,7                WAS 6                           L01          
         LA    R6,PUBINS                                                        
         LA    R7,MTHINS                                                        
         LA    R6,0(R4,R6)                                                      
MTHEND5  AP    0(8,R6),0(8,R7)                                                  
         LA    R6,ACCNUM*8(R6)                                                  
         LA    R7,8(R7)                                                         
         BCT   R3,MTHEND5                                                       
         LA    R3,7                WAS 6                           L01          
         LA    R6,MTHTOTS               CLEAR MTH ACCUMS                        
MTHEND8  ZAP   0(8,R6),=P'0'                                                    
         LA    R6,8(R6)                                                         
         BCT   R3,MTHEND8                                                       
******   XC    SAVEYMD,SAVEYMD                                                  
******   MVI   MTHACT,0                                                         
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                           NOW CHECK FOR AND PROCESS FOREIGN EXCHANGE          
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
*NOF*    CLI   QOPT7,C'F'          SEPARATE FOREIGN EXCHANGE ?                  
*NOF*    BNE   MTHENDFX            NO - DONE WITH MONTH END                     
*                                                                               
         CP    MTHGOF,=P'0'        ANY FOREIGN EXCHANGE ?                       
         BE    MTHENDFX            NO - DONE WITH MONTH END                     
*                                                                               
         CLI   QBPDATE,C'B'                                                     
         BE    MTHENF2C            NO MTH BREAKS                                
*                                                                               
         MVC   P+11(26),=C'* FOREIGN EXCHANGE TOTAL *'                          
*                                                                               
         EDIT  MTHGOF,(14,P+53),2,COMMAS=YES,MINUS=YES                          
         LA    R3,P+66                                                          
         CP    MTHGOF,=P'0'                                                     
         BNL   *+8                                                              
         LA    R3,1(R3)                                                         
         MVI   0(R3),C'*'                                                       
         EDIT  MTHGLACF,(14,P+68),2,COMMAS=YES,MINUS=YES                        
         LA    R3,P+81                                                          
         CP    MTHGLACF,=P'0'                                                   
         BNL   *+8                                                              
         LA    R3,1(R3)                                                         
         MVI   0(R3),C'*'                                                       
         EDIT  MTHCDF,(11,P+83),2,COMMAS=YES,MINUS=YES                          
         LA    R3,P+93                                                          
         CP    MTHCDF,=P'0'                                                     
         BNL   *+8                                                              
         LA    R3,1(R3)                                                         
         MVI   0(R3),C'*'                                                       
         LA    R3,P+95                                                          
         CLI   PAGYNAT,C'C'                                                     
         BNE   NTCAND4F                                                         
*****    EDIT  MTHGSTF,(12,(R3)),2,COMMAS=YES,MINUS=YES                         
*****    MVI   12(R3),C'*'                                                      
*****    CP    MTHGSTF,=P'0'                                                    
*****    BL    *+10                                                             
*****    MVC   11(2,R3),=C'* '                                                  
         LA    R3,14(R3)                                                        
NTCAND4F DS    0H                                                               
*                                                                               
         EDIT  MTHNPF,(14,(R3)),2,COMMAS=YES,MINUS=YES                          
         MVI   14(R3),C'*'                                                      
         CP    MTHNPF,=P'0'                                                     
         BL    *+10                                                             
         MVC   13(2,R3),=C'* '                                                  
         ZIC   R3,MAXLINES                                                      
         MVI   MAXLINES,99                                                      
         GOTO1 VPRINTIT                                                         
         STC   R3,MAXLINES                                                      
*****    CLC   LINE,MAXLINES                                                    
*****    BNL   MTHENF2B                                                         
*****    GOTO1 VPRINTIT                                                         
*                                                                               
MTHENF2B CLI   PRDSW,1                SEE IF DOING PRDS SEPERATELY              
         BE    MTHENF3C                                                         
*                                                                               
*    ROLL TO PRD TOTALS                                                         
*                                                                               
MTHENF2C L     R3,PRDMTHSF                                                      
         A     R3,=F'1'                                                         
         ST    R3,PRDMTHSF                                                      
         LA    R3,PRDINSF                                                       
         LA    R4,MTHINSF                                                       
         LA    R6,7                WAS 6                                        
MTHENF3  AP    0(8,R3),0(8,R4)                                                  
         LA    R3,8(R3)                                                         
         LA    R4,8(R4)                                                         
         BCT   R6,MTHENF3                                                       
*                                                                               
*     ROLL TO PUB ACCUMS                                                        
MTHENF3C L     R3,PUBMTHSF                                                      
         AH    R3,=H'1'                                                         
         ST    R3,PUBMTHSF                                                      
*                                                                               
*        GOTO1 DTCNV,DMCB,(1,SAVEYMD),(0,WORK)                                  
         GOTO1 DATCON,DMCB,(3,SAVEYMD),(0,WORK)                                 
         LAY   R3,MTHTAB                                                        
         LA    R4,0                                                             
COMPDATF CLC   WORK(4),0(R3)                                                    
         BE    MTHENF4                                                          
         LA    R3,8(R3)                                                         
         LA    R4,8(R4)                                                         
         CLC   0(4,R3),=4X'00'                                                  
         BNE   COMPDATF                                                         
         DC    H'0'                MTH NOT FOUND IN LIST                        
*                                                                               
MTHENF4  LA    R3,7                WAS 6                                        
         LAY   R6,PUBINSF                                                       
         LA    R7,MTHINSF                                                       
         LA    R6,0(R4,R6)                                                      
MTHENF5  AP    0(8,R6),0(8,R7)                                                  
         LA    R6,ACCNUM*8(R6)                                                  
         LA    R7,8(R7)                                                         
         BCT   R3,MTHENF5                                                       
         LA    R3,7                WAS 6                                        
         LA    R6,MTHTOTSF              CLEAR MTH ACCUMS                        
MTHENF8  ZAP   0(8,R6),=P'0'                                                    
         LA    R6,8(R6)                                                         
         BCT   R3,MTHENF8                                                       
MTHENDFX DS    0H                                                               
         XC    SAVEYMD,SAVEYMD                                                  
         MVI   MTHACT,0                                                         
         CLC   LINE,MAXLINES                                                    
         BNL   MTHENDX                                                          
         GOTO1 VPRINTIT                                                         
*                                                                               
*     END OF MONTH END AND MONTH END FX (FOREIGN EXCHANGE) PROCESSING           
*                                                                               
MTHENDX  XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
PRDEND   CSECT                                                                  
         NMOD1 0,PRDEND                                                         
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         USING PPFILED,RC                                                       
         L     R5,VPAYWORK                                                      
         USING PAYWORKD,R5                                                      
         CLI   PRDACT,C'Y'                                                      
         BNE   PRDENDX                                                          
         GOTO1 VMTHEND                                                          
*                                                                               
         CLI   PRDSW,1              SEE  IF DOING PRDS SEPERATELY               
         BE    PRDEND10                                                         
*                                                                               
         L     R3,PUBPRDS                                                       
         A     R3,=F'1'                                                         
         ST    R3,PUBPRDS                                                       
         CLI   QBPDATE,C'B'        SEE IF DOING BILLABLE DATES                  
         BE    PRDEND0                                                          
         CLC   PRDMTHS,=F'1'                                                    
         BNH   CLRPRD                                                           
*                                                                               
PRDEND0  MVC   P+2(20),=C'** PRODUCT TOTALS **'                                 
         CP    PRDINS,=P'0'                                                     
         BE    NOINS               NO INSERTIONS                                
         EDIT  PRDINS,(3,P+28),0                                                
         MVC   P+32(10),=C'INSERTIONS'                                          
         CP    PRDINS,=P'1'                                                     
         BNE   *+8                                                              
         MVI   P+41,C' '                                                        
NOINS    CLI   QMEDIA,C'N'                                                      
         BNE   PRDEND1                  SKIP LINES                              
         CP    PRDLINES,=P'0'                                                   
         BE    PRDEND1                                                          
         CLI   PROGPROF,C'I'                                                    
         BNE   PRDENDA                                                          
         EDIT  PRDLINES,(9,P+42),2                                              
         MVI   P+51,C'I'                                                        
         MVI   P+52,C'*'                                                        
         B     PRDEND1                                                          
PRDENDA  EDIT  PRDLINES,(6,P+45),0                                              
         MVI   P+51,C'L'                                                        
         MVI   P+52,C'*'                                                        
PRDEND1  DS    0H                                                               
         EDIT  PRDGO,(14,P+53),2,COMMAS=YES,MINUS=YES                           
         CP    PRDGO,=P'0'                                                      
         BL    *+8                                                              
         MVI   P+66,C'*'                                                        
         EDIT  PRDGLAC,(14,P+68),2,COMMAS=YES,MINUS=YES                         
         CP    PRDGLAC,=P'0'                                                    
         BL    *+8                                                              
         MVI   P+81,C'*'                                                        
         EDIT  PRDCD,(12,P+82),2,COMMAS=YES,MINUS=YES                           
         CP    PRDCD,=P'0'                                                      
         BL    *+8                                                              
         MVI   P+93,C'*'                                                        
         LA    R3,P+95                                             L01          
         CLI   PAGYNAT,C'C'                                        L01          
         BNE   NTCAND5                                             L01          
         EDIT  PRDGST,(12,(R3)),2,COMMAS=YES,MINUS=YES            L01           
         MVI   P+106,C'*'                                          L01          
         CP    PRDGST,=P'0'                                                     
         BL    *+10                                                             
         MVC   P+105(2),=C'* '                                                  
         LA    R3,14(R3)                                           L01          
NTCAND5  DS    0H                                                  L01          
*                                                                               
         EDIT  PRDNP,(14,(R3)),2,COMMAS=YES,MINUS=YES                           
         MVI   14(R3),C'*'                                         L01          
         CP    PRDNP,=P'0'                                                      
         BL    *+10                                                             
         MVC   13(2,R3),=C'* '                                                  
         SR    R3,R3                                                            
         IC    R3,MAXLINES                                                      
         MVI   MAXLINES,99                                                      
         GOTO1 VPRINTIT                                                         
         STC   R3,MAXLINES                                                      
*                                                                               
         CP    PRDGOF,=P'0'        ANY FOREIGN EXCHANGE ?                       
         BE    PRDEND8             NO - DONE WITH TOTALS                        
*                                                                               
*NOF*    CLI   QOPT7,C'F'          SEPARATE FOREIGN EXCHANGE ?                  
*NOF*    BE    PRDENFX             YES                                          
*                                                                               
         B     PRDENFX             HAVE FOREIGN EXCHANGE                        
*                                                                               
PRDEND8  DS    0H                                                               
         CLC   LINE,MAXLINES                                                    
         BNL   CLRPRD                                                           
         GOTO1 VPRINTIT                                                         
         B     CLRPRD              DONE WITH PRODUCT TOTALS                     
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                             NOW CHECK FOR FOREIGN EXCHANGE                    
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
PRDENFX  DS    0H                                                               
         L     R3,PUBPRDSF                                                      
         A     R3,=F'1'                                                         
         ST    R3,PUBPRDSF                                                      
         CLI   QBPDATE,C'B'        SEE IF DOING BILLABLE DATES                  
         BE    PRDENF0                                                          
         CLC   PRDMTHS,=F'1'        NOTE - TESTING PRDMTHS NOT PRDMTHSF         
         BNH   CLRPRD                                                           
*                                                                               
PRDENF0  DS    0H                                                               
         MVC   P+02(21),=C'* PRODUCT FX TOTALS *'                               
         CP    PRDINSF,=P'0'                                                    
         BE    NOINSF               NO INSERTIONS                               
         EDIT  PRDINSF,(3,P+28),0                                               
         MVC   P+32(10),=C'INSERTIONS'                                          
         CP    PRDINSF,=P'1'                                                    
         BNE   *+8                                                              
         MVI   P+41,C' '                                                        
NOINSF   DS    0H                                                               
         CLI   QMEDIA,C'N'                                                      
         BNE   PRDENF1                  SKIP LINES                              
         CP    PRDLINEF,=P'0'                                                   
         BE    PRDENF1                                                          
         CLI   PROGPROF,C'I'                                                    
         BNE   PRDENFA                                                          
         EDIT  PRDLINEF,(9,P+42),2                                              
         MVI   P+51,C'I'                                                        
         MVI   P+52,C'*'                                                        
         B     PRDENF1                                                          
PRDENFA  DS    0H                                                               
         EDIT  PRDLINEF,(6,P+45),0                                              
         MVI   P+51,C'L'                                                        
         MVI   P+52,C'*'                                                        
PRDENF1  DS    0H                                                               
         EDIT  PRDGOF,(14,P+53),2,COMMAS=YES,MINUS=YES                          
         CP    PRDGOF,=P'0'                                                     
         BL    *+8                                                              
         MVI   P+66,C'*'                                                        
         EDIT  PRDGLACF,(14,P+68),2,COMMAS=YES,MINUS=YES                        
         CP    PRDGLACF,=P'0'                                                   
         BL    *+8                                                              
         MVI   P+81,C'*'                                                        
         EDIT  PRDCDF,(12,P+82),2,COMMAS=YES,MINUS=YES                          
         CP    PRDCDF,=P'0'                                                     
         BL    *+8                                                              
         MVI   P+93,C'*'                                                        
         LA    R3,P+95                                                          
         CLI   PAGYNAT,C'C'                                                     
         BNE   NTCAND5F                                                         
*****    EDIT  PRDGST,(12,(R3)),2,COMMAS=YES,MINUS=YES                          
*****    MVI   P+106,C'*'                                                       
*****    CP    PRDGST,=P'0'                                                     
*****    BL    *+10                                                             
*****    MVC   P+105(2),=C'* '                                                  
         LA    R3,14(R3)                                                        
NTCAND5F DS    0H                                                               
*                                                                               
         EDIT  PRDNPF,(14,(R3)),2,COMMAS=YES,MINUS=YES                          
         MVI   14(R3),C'*'                                                      
         CP    PRDNPF,=P'0'                                                     
         BL    *+10                                                             
         MVC   13(2,R3),=C'* '                                                  
*        MVI   P+107,C'*'                                                       
         SR    R3,R3                                                            
         IC    R3,MAXLINES                                                      
         MVI   MAXLINES,99                                                      
         GOTO1 VPRINTIT                                                         
         STC   R3,MAXLINES                                                      
         CLC   LINE,MAXLINES                                                    
         BNL   PRDENF8                                                          
         GOTO1 VPRINTIT                                                         
PRDENF8  DS    0H                                                               
*                                                                               
         B     CLRPRD              DONE WITH PRDEND                             
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
PRDEND10 DS    0H                  PRODUCTS SEPARATELY                          
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
         L     R9,VPROCSCT                                                      
         USING PRODSCT,R9                                                       
         GOTO1 VPUBEND                                                          
         MVI   PUBPSW,0            SO I WON'T PRINT PUB CONTINUED MSG           
         MVI   FORCEHED,C'Y'                                                    
         MVC   P+1(20),=C'** PRODUCT TOTALS **'                                 
         GOTO1 VPRINTIT                                                         
         LA    R8,ACCNUM                                                        
         LA    R4,0                                                             
PRDEND15 LA    R2,PROINS                                                        
         LA    R2,0(R4,R2)                                                      
         LA    R7,7                WAS 6                           L01          
PRDEND17 CP    0(8,R2),=P'0'                                                    
         BNE   PACTV                                                            
         LA    R2,ACCNUM*8(R2)                                                  
         BCT   R7,PRDEND17                                                      
PRDEND18 LA    R4,8(R4)                                                         
         BCT   R8,PRDEND15                                                      
         B     PRDEND20                                                         
*                                                                               
PACTV    LAY   R1,MTHTAB                                                        
         LA    R1,0(R4,R1)                                                      
         MVC   WORK(4),0(R1)                                                    
         MVC   WORK+4(2),=C'01'                                                 
*        GOTO1 DTCNV,DMCB,(0,WORK),(5,P+5)                                      
         GOTO1 DATCON,DMCB,(0,WORK),(9,P+5)                                     
         LA    R1,PROPUBS                                                       
         LA    R1,0(R4,R1)                                                      
         EDIT  (P8,(R1)),(4,P+15),0                                             
         MVC   P+20(4),=C'PUBS'                                                 
         CP    0(8,R1),=P'1'                                                    
         BNE   *+8                                                              
         MVI   P+23,C' '                                                        
         LA    R1,PROINS                                                        
         LA    R1,0(R4,R1)                                                      
         CP    0(8,R1),=P'0'                                                    
         BE    NOPINS                                                           
         EDIT  (P8,(R1)),(5,P+24),0                                             
         MVC   P+30(10),=C'INSERTIONS'                                          
         CP    0(8,R1),=P'1'                                                    
         BNE   *+8                                                              
         MVI   P+39,C' '                                                        
NOPINS   LA    R1,ACCNUM*8(R1)                                                  
         CLI   QMEDIA,C'N'                                                      
         BNE   PACTV5                                                           
         CP    0(8,R1),=P'0'                                                    
         BE    PACTV5                                                           
         CLI   PROGPROF,C'I'                                                    
         BNE   PACTV3                                                           
         EDIT  (P8,(R1)),(9,P+42),2                                             
         MVI   P+51,C'I'                                                        
         B     PACTV5                                                           
PACTV3   EDIT  (P8,(R1)),(6,P+44),0                                             
         MVI   P+50,C'L'                                                        
PACTV5   LA    R1,ACCNUM*8(R1)                                                  
         EDIT  (P8,(R1)),(14,P+53),2,COMMAS=YES,MINUS=YES                       
         LA    R1,ACCNUM*8(R1)                                                  
         EDIT  (P8,(R1)),(14,P+68),2,COMMAS=YES,MINUS=YES                       
         LA    R1,ACCNUM*8(R1)                                                  
         EDIT  (P8,(R1)),(12,P+82),2,COMMAS=YES,MINUS=YES                       
         LA    R1,ACCNUM*8(R1)                                                  
         LA    R3,P+95                                             L01          
         CLI   PAGYNAT,C'C'                                        L01          
         BNE   NTCAND6                                             L01          
         EDIT  (P8,(R1)),(12,(R3)),2,COMMAS=YES,MINUS=YES          L01          
         LA    R3,14(R3)                                           L01          
NTCAND6  DS    0H                                                  L01          
         LA    R1,ACCNUM*8(R1)    BUMP PAST GST                    L01          
*                                                                               
         EDIT  (P8,(R1)),(14,(R3)),2,COMMAS=YES,MINUS=YES                       
         GOTO1 VPRINTIT                                                         
         B     PRDEND18                                                         
*                                                                               
PRDEND20 MVC   P+5(5),=C'TOTAL'                                                 
         L     R2,PTOTPUBS                                                      
         EDIT  (R2),(4,P+15),0                                                  
         MVC   P+20(4),=C'PUBS'                                                 
         C     R2,=F'1'                                                         
         BNE   *+8                                                              
         MVI   P+23,C' '                                                        
         ZAP   WKDUB,PROINS                                                     
         LA    R3,ACCNUM-1                                                      
         LA    R4,PROINS+8                                                      
PRDEND22 AP    WKDUB,0(8,R4)                                                    
         LA    R4,8(R4)                                                         
         BCT   R3,PRDEND22                                                      
         CP    WKDUB,=P'0'                                                      
         BE    NOPTINS                                                          
         EDIT  WKDUB,(5,P+24),0                                                 
         MVC   P+30(10),=C'INSERTIONS'                                          
         CP    WKDUB,=P'1'                                                      
         BNE   *+8                                                              
         MVI   P+39,C' '                                                        
NOPTINS  DS    0H                                                               
         LA    R6,PROLINES                                                      
         LA    R7,6                WAS 5                           L01          
         LA    R8,TOTALS                                                        
PRDEND23 ZAP   DUB,0(8,R6)                                                      
         LA    R3,ACCNUM-1                                                      
         LA    R4,8(R6)                                                         
PRDEND24 ZAP   DOUBLE,0(8,R4)                                                   
         AP    DUB,DOUBLE                                                       
         LA    R4,8(R4)                                                         
         BCT   R3,PRDEND24                                                      
         MVC   0(8,R8),DUB         SAVE RESULT IN TOTALS                        
         LA    R8,8(R8)                                                         
         LA    R6,ACCNUM*8(R6)                                                  
         BCT   R7,PRDEND23                                                      
*                                                                               
         LA    R2,TOTALS                                                        
         CLI   QMEDIA,C'N'                                                      
         BNE   PRDEND26                                                         
* CHANGE 4/22                                                                   
         CP    0(8,R2),=P'0'                                                    
         BE    PRDEND26                                                         
*                                                                               
         CLI   PROGPROF,C'I'                                                    
         BNE   PRDEND25                                                         
         EDIT  (P8,0(R2)),(10,P+41),2                                           
         MVI   P+51,C'I'                                                        
         MVI   P+52,C'*'                                                        
         B     PRDEND26                                                         
PRDEND25 EDIT  (P8,0(R2)),(7,P+43),0                                            
         MVI   P+50,C'L'                                                        
         MVI   P+51,C'*'                                                        
*                                                                               
PRDEND26 LA    R2,8(R2)                                                         
         EDIT  (P8,00(R2)),(14,P+53),2,COMMAS=YES,MINUS=YES                     
         CP    00(8,R2),=P'0'                                                   
         BL    *+8                                                              
         MVI   P+66,C'*'                                                        
         EDIT  (P8,08(R2)),(14,P+68),2,COMMAS=YES,MINUS=YES                     
         CP    08(8,R2),=P'0'                                                   
         BL    *+8                                                              
         MVI   P+81,C'*'                                                        
         EDIT  (P8,16(R2)),(12,P+82),2,COMMAS=YES,MINUS=YES                     
         CP    16(8,R2),=P'0'                                                   
         BL    *+8                                                              
         MVI   P+93,C'*'                                                        
         LA    R3,P+95                                             L01          
         CLI   PAGYNAT,C'C'                                        L01          
         BNE   NTCAND7                                             L01          
         EDIT  (P8,24(R2)),(12,(R3)),2,COMMAS=YES,MINUS=YES        L01          
         LA    R3,14(R3)                                           L01          
NTCAND7  DS    0H                                                  L01          
*                                                                               
         EDIT  (P8,32(R2)),(14,(R3)),2,COMMAS=YES,MINUS=YES        L01          
         MVI   14(R3),C'*'                                                      
         CP    32(8,R1),=P'0'                                                   
         BL    *+10                                                             
*        MVI   P+107,C'*'                                                       
         MVC   13(2,R3),=C'* '                                                  
         GOTO1 VPRINTIT                                                         
         LA    R3,PROTOTS                                                       
         LA    R4,ACCNUM*8         WAS 7                           L01          
PRDEND30 ZAP   0(8,R3),=P'0'                                                    
         LA    R3,8(R3)                                                         
         BCT   R4,PRDEND30                                                      
         XC    PTOTPUBS,PTOTPUBS                                                
         DROP  R9                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*NOF*    CLI   QOPT7,C'F'          SEPARATE FOREIGN EXCHANGE ?                  
*NOF*    BNE   CLRPRD              NO - DONE WITH PRODUCT END                   
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
*                                  PRODUCTS SEPARATELY (FGN EXCHANGE)           
         L     R9,VPROCSCF                                                      
         USING PRODSCTF,R9                                                      
*                                                                               
         LA    R8,ACCNUM           SEE IF ANY FOREIGN EXCHANGE                  
         LA    R2,PROGOF                                                        
PROFXCK  CP    0(8,R2),=P'0'       NOT ZERO ?                                   
         BNE   PROENF0             YES - HAS FOREIGN EXCHANGE                   
         LA    R2,8(R2)            BUMP TO NEXT "MONTH" VALUES                  
         BCT   R8,PROFXCK                                                       
         B     CLRPRD              NO FOREIGN EXCHANGE FOUND - DONE             
*                                                                               
PROENF0  DS    0H                                                               
*                                                                               
         MVI   PUBPSW,0            SO I WON'T PRINT PUB CONTINUED MSG           
*****    MVI   FORCEHED,C'Y'                                                    
         MVC   P+1(35),=C'* PRODUCT FOREIGN EXCHANGE TOTALS *'                  
         GOTO1 VPRINTIT                                                         
         LA    R8,ACCNUM                                                        
         LA    R4,0                                                             
PRDENF15 LA    R2,PROINSF                                                       
         LA    R2,0(R4,R2)                                                      
         LA    R7,7                WAS 6                           L01          
PRDENF17 CP    0(8,R2),=P'0'                                                    
         BNE   PACTVF                                                           
         LA    R2,ACCNUM*8(R2)                                                  
         BCT   R7,PRDENF17                                                      
PRDENF18 LA    R4,8(R4)                                                         
         BCT   R8,PRDENF15                                                      
         B     PRDENF20                                                         
*                                                                               
PACTVF   LAY   R1,MTHTAB                                                        
         LA    R1,0(R4,R1)                                                      
         MVC   WORK(4),0(R1)                                                    
         MVC   WORK+4(2),=C'01'                                                 
*        GOTO1 DTCNV,DMCB,(0,WORK),(5,P+5)                                      
         GOTO1 DATCON,DMCB,(0,WORK),(9,P+5)                                     
         LA    R1,PROPUBSF                                                      
         LA    R1,0(R4,R1)                                                      
         EDIT  (P8,(R1)),(4,P+15),0                                             
         MVC   P+20(4),=C'PUBS'                                                 
         CP    0(8,R1),=P'1'                                                    
         BNE   *+8                                                              
         MVI   P+23,C' '                                                        
         LA    R1,PROINSF                                                       
         LA    R1,0(R4,R1)                                                      
         CP    0(8,R1),=P'0'                                                    
         BE    NOPINSF                                                          
         EDIT  (P8,(R1)),(5,P+24),0                                             
         MVC   P+30(10),=C'INSERTIONS'                                          
         CP    0(8,R1),=P'1'                                                    
         BNE   *+8                                                              
         MVI   P+39,C' '                                                        
NOPINSF  LA    R1,ACCNUM*8(R1)                                                  
         CLI   QMEDIA,C'N'                                                      
         BNE   PACTV5F                                                          
         CP    0(8,R1),=P'0'                                                    
         BE    PACTV5F                                                          
         CLI   PROGPROF,C'I'                                                    
         BNE   PACTV3F                                                          
         EDIT  (P8,(R1)),(9,P+42),2                                             
         MVI   P+51,C'I'                                                        
         B     PACTV5F                                                          
PACTV3F  EDIT  (P8,(R1)),(6,P+44),0                                             
         MVI   P+50,C'L'                                                        
PACTV5F  LA    R1,ACCNUM*8(R1)                                                  
         EDIT  (P8,(R1)),(14,P+53),2,COMMAS=YES,MINUS=YES                       
         LA    R1,ACCNUM*8(R1)                                                  
         EDIT  (P8,(R1)),(14,P+68),2,COMMAS=YES,MINUS=YES                       
         LA    R1,ACCNUM*8(R1)                                                  
         EDIT  (P8,(R1)),(12,P+82),2,COMMAS=YES,MINUS=YES                       
         LA    R1,ACCNUM*8(R1)                                                  
         LA    R3,P+95                                                          
         CLI   PAGYNAT,C'C'                                                     
         BNE   NTCAND6F                                                         
*****    EDIT  (P8,(R1)),(12,(R3)),2,COMMAS=YES,MINUS=YES                       
         LA    R3,14(R3)                                                        
NTCAND6F DS    0H                                                               
         LA    R1,ACCNUM*8(R1)    BUMP PAST GST                                 
*                                                                               
         EDIT  (P8,(R1)),(14,(R3)),2,COMMAS=YES,MINUS=YES                       
         GOTO1 VPRINTIT                                                         
         B     PRDENF18                                                         
*                                                                               
*                                                                               
PRDENF20 MVC   P+5(5),=C'TOTAL'                                                 
         L     R2,PTOTPUBF                                                      
         EDIT  (R2),(4,P+15),0                                                  
         MVC   P+20(4),=C'PUBS'                                                 
         C     R2,=F'1'                                                         
         BNE   *+8                                                              
         MVI   P+23,C' '                                                        
         ZAP   WKDUB,PROINSF                                                    
         LA    R3,ACCNUM-1                                                      
         LA    R4,PROINSF+8                                                     
PRDENF22 AP    WKDUB,0(8,R4)                                                    
         LA    R4,8(R4)                                                         
         BCT   R3,PRDENF22                                                      
         CP    WKDUB,=P'0'                                                      
         BE    NOPTINSF                                                         
         EDIT  WKDUB,(5,P+24),0                                                 
         MVC   P+30(10),=C'INSERTIONS'                                          
         CP    WKDUB,=P'1'                                                      
         BNE   *+8                                                              
         MVI   P+39,C' '                                                        
NOPTINSF DS    0H                                                               
         LA    R6,PROLINEF                                                      
         LA    R7,6                WAS 5                           L01          
         LA    R8,TOTALS                                                        
PRDENF23 ZAP   DUB,0(8,R6)                                                      
         LA    R3,ACCNUM-1                                                      
         LA    R4,8(R6)                                                         
PRDENF24 ZAP   DOUBLE,0(8,R4)                                                   
         AP    DUB,DOUBLE                                                       
         LA    R4,8(R4)                                                         
         BCT   R3,PRDENF24                                                      
         MVC   0(8,R8),DUB         SAVE RESULT IN TOTALS                        
         LA    R8,8(R8)                                                         
         LA    R6,ACCNUM*8(R6)                                                  
         BCT   R7,PRDENF23                                                      
*                                                                               
         LA    R2,TOTALS                                                        
         CLI   QMEDIA,C'N'                                                      
         BNE   PRDENF26                                                         
* CHANGE 4/22                                                                   
         CP    0(8,R2),=P'0'                                                    
         BE    PRDENF26                                                         
*                                                                               
         CLI   PROGPROF,C'I'                                                    
         BNE   PRDENF25                                                         
         EDIT  (P8,0(R2)),(10,P+41),2                                           
         MVI   P+51,C'I'                                                        
         MVI   P+52,C'*'                                                        
         B     PRDENF26                                                         
PRDENF25 EDIT  (P8,0(R2)),(7,P+43),0                                            
         MVI   P+50,C'L'                                                        
         MVI   P+51,C'*'                                                        
*                                                                               
PRDENF26 LA    R2,8(R2)                                                         
         EDIT  (P8,00(R2)),(14,P+53),2,COMMAS=YES,MINUS=YES                     
         CP    00(8,R2),=P'0'                                                   
         BL    *+8                                                              
         MVI   P+66,C'*'                                                        
         EDIT  (P8,08(R2)),(14,P+68),2,COMMAS=YES,MINUS=YES                     
         CP    08(8,R2),=P'0'                                                   
         BL    *+8                                                              
         MVI   P+81,C'*'                                                        
         EDIT  (P8,16(R2)),(12,P+82),2,COMMAS=YES,MINUS=YES                     
         CP    16(8,R2),=P'0'                                                   
         BL    *+8                                                              
         MVI   P+93,C'*'                                                        
         LA    R3,P+95                                                          
         CLI   PAGYNAT,C'C'                                                     
         BNE   NTCAND7F                                                         
*****    EDIT  (P8,24(R2)),(12,(R3)),2,COMMAS=YES,MINUS=YES                     
         LA    R3,14(R3)                                                        
NTCAND7F DS    0H                                                               
*                                                                               
         EDIT  (P8,32(R2)),(14,(R3)),2,COMMAS=YES,MINUS=YES                     
         MVI   14(R3),C'*'                                                      
         CP    32(8,R1),=P'0'                                                   
         BL    *+10                                                             
*        MVI   P+107,C'*'                                                       
         MVC   13(2,R3),=C'* '                                                  
         GOTO1 VPRINTIT                                                         
         LA    R3,PROTOTSF                                                      
         LA    R4,ACCNUM*8         WAS 7                                        
PRDENF30 ZAP   0(8,R3),=P'0'                                                    
         LA    R3,8(R3)                                                         
         BCT   R4,PRDENF30                                                      
*                                                                               
CLRPRD   DS    0H                                                               
         DROP  R9                                                               
*                                                                               
         L     R9,VPROCSCT                                                      
         USING PRODSCT,R9                                                       
*                                                                               
         LA    R3,7               WAS 6                                         
         LA    R4,PRDTOTS                                                       
CLRPRD5  ZAP   0(8,R4),=P'0'                                                    
         LA    R4,8(R4)                                                         
         BCT   R3,CLRPRD5                                                       
         XC    PRDMTHS,PRDMTHS                                                  
         XC    SAVEPRD,SAVEPRD                                                  
         XC    SAVEYMD,SAVEYMD                                                  
         MVI   MTHACT,0                                                         
         MVI   PRDACT,0                                                         
*                                 CLEAR FOREIGN EXCHANGE ACCUMULATORS           
         L     R9,VPROCSCF                                                      
         USING PRODSCTF,R9                                                      
*                                                                               
         LA    R3,7               WAS 6                                         
         LA    R4,PRDTOTSF                                                      
CLRPRD5F ZAP   0(8,R4),=P'0'                                                    
         LA    R4,8(R4)                                                         
         BCT   R3,CLRPRD5F                                                      
         XC    PRDMTHSF,PRDMTHSF                                                
         XC    PTOTPUBF,PTOTPUBF                                                
*                                                                               
         DROP  R9                                                               
PRDENDX  XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
PUBEND   CSECT                                                                  
         NMOD1 0,PUBEND                                                         
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         USING PPFILED,RC                                                       
         L     R5,VPAYWORK                                                      
         USING PAYWORKD,R5                                                      
         MVI   PUBSW,0                                                          
         CLI   PUBACT,C'Y'                                                      
*SMY*    BNE   PUBEND30                 NO ACTIVITY                             
         BNE   PUBENF30                 NO ACTIVITY ("FX" END)                  
         GOTO1 VMTHEND                                                          
*                                                                               
         CLI   PRDSW,1                  SEE IF DOING PRDS SEPERATELY            
         BE    PUBEND3                                                          
*                                                                               
         GOTO1 VPRDEND                                                          
         CLC   PUBPRDS,=F'1'                                                    
         BNH   PUBROLL                                                          
         SR    R0,R0                                                            
         IC    R0,LINE                                                          
         AH    R0,=H'3'   NEED 4 LINES                                          
         STC   R0,SAVELINE                                                      
         CLC   SAVELINE,MAXLINES                                                
         BNH   *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
         GOTO1 VPRINTIT                                                         
         MVC   P+2(16),=C'** PUB TOTALS **'                                     
         GOTO1 VPRINTIT                                                         
         XC    PUBMTHS,PUBMTHS                                                  
         LA    R3,ACCNUM                                                        
         LA    R4,0                                                             
PUBEND1  LA    R6,PUBINS                                                        
         LA    R6,0(R4,R6)                                                      
         LA    R7,7                WAS 6                           L01          
PUBEND2  CP    0(8,R6),=P'0'                                                    
         BE    *+12                                                             
         BAS   R8,PRTMTH                                                        
         B     *+12                                                             
         LA    R6,ACCNUM*8(R6)                                                  
         BCT   R7,PUBEND2                                                       
         LA    R4,8(R4)                                                         
         BCT   R3,PUBEND1                                                       
*                                                                               
         B     PUBEND3                                                          
*                                                                               
PRTMTH   EQU   *                                                                
         L     R2,PUBMTHS                                                       
         A     R2,=F'1'                                                         
         ST    R2,PUBMTHS                                                       
         LAY   R2,MTHTAB       R4 HAS MTH DISP                                  
         LA    R2,0(R4,R2)                                                      
         MVC   WORK(4),0(R2)                                                    
         MVC   WORK+4(2),=C'01'                                                 
*        GOTO1 DTCNV,DMCB,(0,WORK),(5,P+5)                                      
         GOTO1 DATCON,DMCB,(0,WORK),(9,P+5)                                     
         LA    R2,PUBINS                                                        
         LA    R2,0(R4,R2)                                                      
         CP    0(8,R2),=P'0'                                                    
         BE    NOPPINS             NO INSERTIONS                                
         EDIT  (P8,(R2)),(4,P+24),0                                             
         MVC   P+29(10),=C'INSERTIONS'                                          
         CP    0(8,R2),=P'1'                                                    
         BNE   *+8                                                              
         MVI   P+38,C' '                                                        
NOPPINS  LA    R2,ACCNUM*8(R2)                                                  
         CLI   QMEDIA,C'N'                                                      
         BNE   PRTMTH1                                                          
         CP    0(8,R2),=P'0'                                                    
         BE    PRTMTH1             NO LINES                                     
         CLI   PROGPROF,C'I'                                                    
         BNE   PRTMTHA                                                          
         EDIT  (P8,(R2)),(9,P+41),2                                             
         MVI   P+50,C'I'                                                        
         B     PRTMTH1                                                          
PRTMTHA  EDIT  (P8,(R2)),(6,P+44),0                                             
         MVI   P+50,C'L'                                                        
PRTMTH1  LA    R2,ACCNUM*8(R2)                                                  
         EDIT  (P8,(R2)),(14,P+53),2,COMMAS=YES,MINUS=YES                       
         LA    R2,ACCNUM*8(R2)                                                  
         EDIT  (P8,(R2)),(14,P+68),2,COMMAS=YES,MINUS=YES                       
         LA    R2,ACCNUM*8(R2)                                                  
         EDIT  (P8,(R2)),(12,P+82),2,COMMAS=YES,MINUS=YES                       
         LA    R2,ACCNUM*8(R2)                                                  
         LA    RF,P+95                                             L01          
         CLI   PAGYNAT,C'C'                                        L01          
         BNE   NTCAND8                                             L01          
         EDIT  (P8,(R2)),(12,(RF)),2,COMMAS=YES,MINUS=YES        L01            
         LA    RF,14(RF)                                           L01          
NTCAND8  DS    0H                                                  L01          
         LA    R2,ACCNUM*8(R2)                                     L01          
*                                                                               
         EDIT  (P8,(R2)),(14,(RF)),2,COMMAS=YES,MINUS=YES          L01          
*                                                                               
         GOTO1 VPRINTIT                                                         
         BR    R8                                                               
         EJECT                                                                  
*                                                                               
*                                                                               
PUBEND3  CLI   PRDSW,0              SEE IF DOING PRDS SEPERATELY                
         BE    PUBEND3C                                                         
         CLI   QBPDATE,C'B'                                                     
         BE    PUBEND3D                                                         
*                                                                               
PUBEND3C CLC   PUBMTHS,=F'1'                                                    
         BNH   PUBROLL                                                          
PUBEND3D MVC   P+5(5),=C'TOTAL'                                                 
         CLI   PRDSW,0                                                          
         BE    *+10                                                             
         MVC   P+2(15),=C'** PUB TOTAL **'                                      
         ZAP   WKDUB,PUBINS                                                     
         LA    R6,ACCNUM-1                                                      
         LA    R2,PUBINS                                                        
PUBEND4  AP    WKDUB,8(8,R2)                                                    
         LA    R2,8(R2)                                                         
         BCT   R6,PUBEND4                                                       
         CP    WKDUB,=P'0'                                                      
         BZ    NOPPTINS            NO INSERTIONS                                
         EDIT  WKDUB,(5,P+23),0                                                 
         MVC   P+29(10),=C'INSERTIONS'                                          
         CP    WKDUB,=P'1'                                                      
         BNE   *+8                                                              
         MVI   P+38,C' '                                                        
NOPPTINS CLI   QMEDIA,C'N'                                                      
         BNE   PUBEND6                                                          
         ZAP   WKDUB,PUBLINES                                                   
         LA    R2,PUBLINES                                                      
         LA    R6,ACCNUM-1                                                      
PUBEND5  AP    WKDUB,8(8,R2)                                                    
         LA    R2,8(R2)                                                         
         BCT   R6,PUBEND5                                                       
         CP    WKDUB,=P'0'                                                      
         BE    PUBEND6             NO LINES                                     
         CLI   PROGPROF,C'I'                                                    
         BNE   PUBEND5A                                                         
         EDIT  WKDUB,(9,P+41),2                                                 
         MVI   P+50,C'I'                                                        
         MVI   P+51,C'*'                                                        
         B     PUBEND6                                                          
PUBEND5A EDIT  WKDUB,(6,P+44),0                                                 
         MVI   P+50,C'L'                                                        
         MVI   P+51,C'*'                                                        
PUBEND6  ZAP   WKDUB,PUBGO                                                      
         LA    R2,PUBGO                                                         
         LA    R6,ACCNUM-1                                                      
PUBEND7  AP    WKDUB,8(8,R2)                                                    
         LA    R2,8(R2)                                                         
         BCT   R6,PUBEND7                                                       
         EDIT  WKDUB,(14,P+53),2,COMMAS=YES,MINUS=YES                           
         LTR   R3,R3                                                            
         BL    *+8                                                              
         MVI   P+66,C'*'                                                        
         ZAP   WKDUB,PUBGLAC                                                    
         LA    R2,PUBGLAC                                                       
         LA    R6,ACCNUM-1                                                      
PUBEND8  AP    WKDUB,8(8,R2)                                                    
         LA    R2,8(R2)                                                         
         BCT   R6,PUBEND8                                                       
         EDIT  WKDUB,(14,P+68),2,COMMAS=YES,MINUS=YES                           
         CP    WKDUB,=P'0'                                                      
         BL    *+8                                                              
         MVI   P+81,C'*'                                                        
         ZAP   WKDUB,PUBCHCD                                                    
         LA    R2,PUBCHCD                                                       
         LA    R6,ACCNUM-1                                                      
PUBEND9  AP    WKDUB,8(8,R2)                                                    
         LA    R2,8(R2)                                                         
         BCT   R6,PUBEND9                                                       
         EDIT  WKDUB,(12,P+82),2,COMMAS=YES,MINUS=YES                           
         CP    WKDUB,=P'0'                                                      
         BL    *+8                                                              
         MVI   P+93,C'*'                                                        
         LA    R3,P+95                                             L01          
         CLI   PAGYNAT,C'C'                                        L01          
         BNE   NTCAND9                                             L01          
         ZAP   WKDUB,PUBGSTX                                       L01          
         LA    R2,PUBGSTX                                          L01          
         LA    R6,ACCNUM-1                                         L01          
PUBEND1A AP    WKDUB,8(8,R2)                                       L01          
         LA    R2,8(R2)                                            L01          
         BCT   R6,PUBEND1A                                         L01          
         EDIT  WKDUB,(12,(R3)),2,COMMAS=YES,MINUS=YES              L01          
         MVI   12(R3),C'*'                                                      
         CP    WKDUB,=P'0'                                                      
         BL    *+10                                                             
         MVC   11(2,R3),=C'* '                                                  
         LA    R3,14(R3)                                           L01          
NTCAND9  DS    0H                                                  L01          
*                                                                               
         LA    R2,PUBNP                                                         
         ZAP   WKDUB,0(8,R2)                                                    
         LA    R6,ACCNUM-1                                                      
PUBEND10 AP    WKDUB,8(8,R2)                                                    
         LA    R2,8(R2)                                                         
         BCT   R6,PUBEND10                                                      
         EDIT  WKDUB,(14,(R3)),2,COMMAS=YES,MINUS=YES                           
         MVI   14(R3),C'*'                                                      
         CP    WKDUB,=P'0'                                                      
         BL    *+10                                                             
         MVC   13(2,R3),=C'* '                                                  
         SR    R3,R3                                                            
         IC    R3,MAXLINES                                                      
         MVI   MAXLINES,99      ALWAYS PRINT TOTAL                              
         GOTO1 VPRINTIT                                                         
         STC   R3,MAXLINES                                                      
         CLC   LINE,MAXLINES                                                    
         BNL   PUBROLL                                                          
*                                                                               
         GOTO1 VPRINTIT                                                         
*     ROLL TO CLT TOTALS                                                        
PUBROLL  DS    0H                                                               
         L     R9,VCLTCSCT                                                      
         USING CLTDSCT,R9                                                       
*                                                                               
         LA    R3,ACCNUM                                                        
         LA    R4,0                                                             
PUBENDA  LA    R6,PUBINS                                                        
         LA    R6,0(R4,R6)                                                      
         LA    R7,7                WAS 6                           L01          
PUBENDB  CP    0(8,R6),=P'0'                                                    
         BE    *+12                                                             
         BAS   R8,BUMPPUB                                                       
         B     *+12                                                             
         LA    R6,ACCNUM*8(R6)                                                  
         BCT   R7,PUBENDB                                                       
         LA    R4,8(R4)                                                         
         BCT   R3,PUBENDA                                                       
         B     PUBENDC                                                          
*                                                                               
BUMPPUB  LA    R2,CLTPUBS    CLT/MTH PUB  TOTALS                                
         LA    R2,0(R4,R2)                                                      
         AP    0(8,R2),=P'1'                                                    
         CLI   PRDSW,0       SEE IF COMBINING PRDS                              
         BER   R8                                                               
         L     R1,VPROCSCT                                                      
         USING PRODSCT,R1                                                       
         LA    R2,PROPUBS                                                       
         LA    R2,0(R4,R2)                                                      
         AP    0(8,R2),=P'1'                                                    
         DROP  R1                                                               
         BR    R8                                                               
*                                                                               
PUBENDC  LA    R2,ACCNUM*7         WAS 6                           L01          
         LA    R3,CLTINS                                                        
         LA    R4,PUBINS                                                        
PUBEND11 AP    0(8,R3),0(8,R4)                                                  
         LA    R3,8(R3)                                                         
         LA    R4,8(R4)                                                         
         BCT   R2,PUBEND11                                                      
         L     R8,CTOTPUBS         ADD 1 TO PUB ACCUM                           
         AH    R8,=H'1'                                                         
         ST    R8,CTOTPUBS                                                      
         DROP  R9                                                               
****                                                                            
         L     R9,VREPCSCT                                                      
         USING REPDSCT,R9                                                       
         LA    R2,ACCNUM*7         WAS 6                           L01          
         LA    R3,REPINS                                                        
         LA    R4,PUBINS                                                        
PUBENDD  AP    0(8,R3),0(8,R4)                                                  
         LA    R3,8(R3)                                                         
         LA    R4,8(R4)                                                         
         BCT   R2,PUBENDD                                                       
         DROP  R9                                                               
*                                                                               
         CLI   PRDSW,0             SEE IF COMBINING PRODUCTS                    
         BE    PUBEND15                                                         
         L     R9,VPROCSCT                                                      
         USING PRODSCT,R9                                                       
         L     R1,PTOTPUBS                                                      
         AH    R1,=H'1'                                                         
         ST    R1,PTOTPUBS                                                      
         LA    R2,ACCNUM*7         WAS 6                           L01          
         LA    R3,PROINS                                                        
         LA    R4,PUBINS                                                        
PUBEND13 AP    0(8,R3),0(8,R4)                                                  
         LA    R3,8(R3)                                                         
         LA    R4,8(R4)                                                         
         BCT   R2,PUBEND13                                                      
         DROP  R9                                                               
*                                                                               
****                                                                            
*                                  CLEAR  PUB ACCUMS                            
PUBEND15 LA    R6,ACCNUM*7         WAS6                            L01          
         LA    R7,PUBINS                                                        
PUBEND17 ZAP   0(8,R7),=P'0'                                                    
         LA    R7,8(R7)                                                         
         BCT   R6,PUBEND17                                                      
PUBEND30 DS    0H                                                               
***      XC    PUBPRDS,PUBPRDS     THE FOLLOWING ARE DONE IN PUBENF30           
***      XC    PUBMTHS,PUBMTHS                                                  
***      XC    SAVEPUB,SAVEPUB                                                  
***      XC    SAVEYMD,SAVEYMD                                                  
***      MVI   MTHACT,0                                                         
***      MVI   PUBACT,0                                                         
***      CLI   PRDSW,1                SEE IF DOING PRDS SEPERATELY              
***      BE    PUBENDFX                                                         
***      XC    PRDMTHS,PRDMTHS                                                  
***      XC    SAVEPRD,SAVEPRD                                                  
***      MVI   PRDACT,0                                                         
*                                                                               
PUBENDFX DS    0H                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                           NOW CHECK FOR AND PROCESS FOREIGN EXCHANGE          
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         CLI   PUBACT,C'Y'                                                      
         BNE   PUBENF30            NO ACTIVITY                                  
*                                                                               
*NOF*    CLI   QOPT7,C'F'          SEPARATE FOREIGN EXCHANGE ?                  
*NOF*    BNE   PUBENF30            NO - DONE WITH PUB END                       
*                                                                               
         LA    R8,ACCNUM           SEE IF ANY FOREIGN EXCHANGE                  
         LAY   R2,PUBGOF                                                        
PUBFXCK  CP    0(8,R2),=P'0'       NOT ZERO ?                                   
         BNE   PUBENF0             YES - HAS FOREIGN EXCHANGE                   
         LA    R2,8(R2)            BUMP TO NEXT "MONTH" VALUES                  
         BCT   R8,PUBFXCK                                                       
         B     PUBENF30            NO FOREIGN EXCHANGE FOUND - DONE             
*                                                                               
PUBENF0  DS    0H                                                               
         CLI   PRDSW,1                  SEE IF DOING PRDS SEPERATELY            
         BE    PUBENF3                                                          
*                                                                               
         CLC   PUBPRDS,=F'1'        NOTE - TESTING PUBPRDS NOT PUBPRDSF         
         BNH   PUBROLFX                                                         
         SR    R0,R0                                                            
         IC    R0,LINE                                                          
         AH    R0,=H'3'   NEED 4 LINES                                          
         STC   R0,SAVELINE                                                      
         CLC   SAVELINE,MAXLINES                                                
         BNH   *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
*****    GOTO1 VPRINTIT                                                         
         MVC   P+2(33),=C'** PUB FOREIGN EXCHANGE TOTALS **'                    
         GOTO1 VPRINTIT                                                         
         XC    PUBMTHSF,PUBMTHSF                                                
         LA    R3,ACCNUM                                                        
         LA    R4,0                                                             
PUBENF1  LAY   R6,PUBINSF                                                       
         LA    R6,0(R4,R6)                                                      
         LA    R7,7                WAS 6                                        
PUBENF2  CP    0(8,R6),=P'0'                                                    
         BE    *+12                                                             
         BAS   R8,PRTMTHF                                                       
         B     *+12                                                             
         LA    R6,ACCNUM*8(R6)                                                  
         BCT   R7,PUBENF2                                                       
         LA    R4,8(R4)                                                         
         BCT   R3,PUBENF1                                                       
         B     PUBENF3                                                          
*                                                                               
PRTMTHF  EQU   *                                                                
         L     R2,PUBMTHSF                                                      
         A     R2,=F'1'                                                         
         ST    R2,PUBMTHSF                                                      
         LAY   R2,MTHTAB       R4 HAS MTH DISP                                  
         LA    R2,0(R4,R2)                                                      
         MVC   WORK(4),0(R2)                                                    
         MVC   WORK+4(2),=C'01'                                                 
*        GOTO1 DTCNV,DMCB,(0,WORK),(5,P+5)                                      
         GOTO1 DATCON,DMCB,(0,WORK),(9,P+5)                                     
         LAY   R2,PUBINSF                                                       
         LA    R2,0(R4,R2)                                                      
         CP    0(8,R2),=P'0'                                                    
         BE    NOPPINSF            NO INSERTIONS                                
         EDIT  (P8,(R2)),(4,P+24),0                                             
         MVC   P+29(10),=C'INSERTIONS'                                          
         CP    0(8,R2),=P'1'                                                    
         BNE   *+8                                                              
         MVI   P+38,C' '                                                        
NOPPINSF LA    R2,ACCNUM*8(R2)                                                  
         CLI   QMEDIA,C'N'                                                      
         BNE   PRTMTH1F                                                         
         CP    0(8,R2),=P'0'                                                    
         BE    PRTMTH1F            NO LINES                                     
         CLI   PROGPROF,C'I'                                                    
         BNE   PRTMTHAF                                                         
         EDIT  (P8,(R2)),(9,P+41),2                                             
         MVI   P+50,C'I'                                                        
         B     PRTMTH1F                                                         
PRTMTHAF EDIT  (P8,(R2)),(6,P+44),0                                             
         MVI   P+50,C'L'                                                        
PRTMTH1F LA    R2,ACCNUM*8(R2)                                                  
         EDIT  (P8,(R2)),(14,P+53),2,COMMAS=YES,MINUS=YES                       
         LA    R2,ACCNUM*8(R2)                                                  
         EDIT  (P8,(R2)),(14,P+68),2,COMMAS=YES,MINUS=YES                       
         LA    R2,ACCNUM*8(R2)                                                  
         EDIT  (P8,(R2)),(12,P+82),2,COMMAS=YES,MINUS=YES                       
         LA    R2,ACCNUM*8(R2)                                                  
         LA    RF,P+95                                                          
         CLI   PAGYNAT,C'C'                                                     
         BNE   NTCAND8F                                                         
******   EDIT  (P8,(R2)),(12,(RF)),2,COMMAS=YES,MINUS=YES                       
         LA    RF,14(RF)                                                        
NTCAND8F DS    0H                                                               
         LA    R2,ACCNUM*8(R2)                                                  
*                                                                               
         EDIT  (P8,(R2)),(14,(RF)),2,COMMAS=YES,MINUS=YES                       
*                                                                               
         GOTO1 VPRINTIT                                                         
         BR    R8             RETURN                                            
         EJECT                                                                  
PUBENF3  CLI   PRDSW,0              SEE IF DOING PRDS SEPERATELY                
         BE    PUBENF3C                                                         
         CLI   QBPDATE,C'B'                                                     
         BE    PUBENF3D                                                         
*                                                                               
PUBENF3C CLC   PUBMTHS,=F'1'       NOTE - NOT PUBMTHSF                          
         BNH   PUBROLFX                                                         
PUBENF3D MVC   P+5(5),=C'TOTAL'                                                 
         CLI   PRDSW,0                                                          
         BE    *+10                                                             
         MVC   P+2(18),=C'** PUB FX TOTAL **'                                   
         LAY   R2,PUBINSF                                                       
         ZAP   WKDUB,0(8,R2)                                                    
         LA    R6,ACCNUM-1                                                      
PUBENF4  AP    WKDUB,8(8,R2)                                                    
         LA    R2,8(R2)                                                         
         BCT   R6,PUBENF4                                                       
         CP    WKDUB,=P'0'                                                      
         BZ    NOPPTINF            NO INSERTIONS                                
         EDIT  WKDUB,(5,P+23),0                                                 
         MVC   P+29(10),=C'INSERTIONS'                                          
         CP    WKDUB,=P'1'                                                      
         BNE   *+8                                                              
         MVI   P+38,C' '                                                        
NOPPTINF CLI   QMEDIA,C'N'                                                      
         BNE   PUBENF6                                                          
         LAY   R2,PUBLINEF                                                      
         ZAP   WKDUB,0(8,R2)                                                    
         LA    R6,ACCNUM-1                                                      
PUBENF5  AP    WKDUB,8(8,R2)                                                    
         LA    R2,8(R2)                                                         
         BCT   R6,PUBENF5                                                       
         CP    WKDUB,=P'0'                                                      
         BE    PUBENF6             NO LINES                                     
         CLI   PROGPROF,C'I'                                                    
         BNE   PUBENF5A                                                         
         EDIT  WKDUB,(9,P+41),2                                                 
         MVI   P+50,C'I'                                                        
         MVI   P+51,C'*'                                                        
         B     PUBENF6                                                          
PUBENF5A EDIT  WKDUB,(6,P+44),0                                                 
         MVI   P+50,C'L'                                                        
         MVI   P+51,C'*'                                                        
PUBENF6  DS    0H                                                               
         LAY   R2,PUBGOF                                                        
         ZAP   WKDUB,0(8,R2)                                                    
         LA    R6,ACCNUM-1                                                      
PUBENF7  AP    WKDUB,8(8,R2)                                                    
         LA    R2,8(R2)                                                         
         BCT   R6,PUBENF7                                                       
         EDIT  WKDUB,(14,P+53),2,COMMAS=YES,MINUS=YES                           
         LTR   R3,R3                                                            
         BL    *+8                                                              
         MVI   P+66,C'*'                                                        
         LAY   R2,PUBGLACF                                                      
*****    ZAP   WKDUB,PUBGLACF                                                   
         ZAP   WKDUB,0(8,R2)                                                    
         LA    R6,ACCNUM-1                                                      
PUBENF8  AP    WKDUB,8(8,R2)                                                    
         LA    R2,8(R2)                                                         
         BCT   R6,PUBENF8                                                       
         EDIT  WKDUB,(14,P+68),2,COMMAS=YES,MINUS=YES                           
         CP    WKDUB,=P'0'                                                      
         BL    *+8                                                              
         MVI   P+81,C'*'                                                        
         LAY   R2,PUBCHCDF                                                      
*****    ZAP   WKDUB,PUBCHCDF                                                   
         ZAP   WKDUB,0(8,R2)                                                    
         LA    R6,ACCNUM-1                                                      
PUBENF9  AP    WKDUB,8(8,R2)                                                    
         LA    R2,8(R2)                                                         
         BCT   R6,PUBENF9                                                       
         EDIT  WKDUB,(12,P+82),2,COMMAS=YES,MINUS=YES                           
         CP    WKDUB,=P'0'                                                      
         BL    *+8                                                              
         MVI   P+93,C'*'                                                        
         LA    R3,P+95                                                          
         CLI   PAGYNAT,C'C'                                                     
         BNE   NTCAND9F                                                         
*****    ZAP   WKDUB,PUBGSTXF                                                   
*****    LAY   R2,PUBGSTXF                                                      
*****    LA    R6,ACCNUM-1                                                      
*****PUBENF1A AP    WKDUB,8(8,R2)                                               
*****    LA    R2,8(R2)                                                         
*****    BCT   R6,PUBENF1A                                                      
*****    EDIT  WKDUB,(12,(R3)),2,COMMAS=YES,MINUS=YES                           
*****    MVI   12(R3),C'*'                                                      
*****    CP    WKDUB,=P'0'                                                      
*****    BL    *+10                                                             
*****    MVC   11(2,R3),=C'* '                                                  
         LA    R3,14(R3)                                           L01          
NTCAND9F DS    0H                                                  L01          
*                                                                               
         LAY   R2,PUBNPF                                                        
         ZAP   WKDUB,0(8,R2)                                                    
         LA    R6,ACCNUM-1                                                      
PUBENF10 AP    WKDUB,8(8,R2)                                                    
         LA    R2,8(R2)                                                         
         BCT   R6,PUBENF10                                                      
         EDIT  WKDUB,(14,(R3)),2,COMMAS=YES,MINUS=YES                           
         MVI   14(R3),C'*'                                                      
         CP    WKDUB,=P'0'                                                      
         BL    *+10                                                             
         MVC   13(2,R3),=C'* '                                                  
         SR    R3,R3                                                            
         IC    R3,MAXLINES                                                      
         MVI   MAXLINES,99      ALWAYS PRINT TOTAL                              
         GOTO1 VPRINTIT                                                         
         STC   R3,MAXLINES                                                      
         CLC   LINE,MAXLINES                                                    
         BNL   PUBROLFX                                                         
*                                                                               
         GOTO1 VPRINTIT                                                         
*                                                                               
*     ROLL TO CLT TOTALS                                                        
PUBROLFX L     R9,VCLTCSCF                                                      
         USING CLTDSCTF,R9                                                      
*                                                                               
         LA    R3,ACCNUM                                                        
         LA    R4,0                                                             
PUBENFA  LAY   R6,PUBINSF                                                       
         LA    R6,0(R4,R6)                                                      
         LA    R7,7                WAS 6                           L01          
PUBENFB  CP    0(8,R6),=P'0'                                                    
         BE    *+12                                                             
         BAS   R8,BUMPPUBF                                                      
         B     *+12                                                             
         LA    R6,ACCNUM*8(R6)                                                  
         BCT   R7,PUBENFB                                                       
         LA    R4,8(R4)                                                         
         BCT   R3,PUBENFA                                                       
         B     PUBENFC                                                          
*                                                                               
BUMPPUBF LA    R2,CLTPUBSF   CLT/MTH PUB  TOTALS                                
         LA    R2,0(R4,R2)                                                      
         AP    0(8,R2),=P'1'                                                    
         CLI   PRDSW,0       SEE IF COMBINING PRDS                              
         BER   R8                                                               
         L     R1,VPROCSCF                                                      
         USING PRODSCTF,R1                                                      
         LA    R2,PROPUBSF                                                      
         LA    R2,0(R4,R2)                                                      
         AP    0(8,R2),=P'1'                                                    
         DROP  R1                                                               
         BR    R8                                                               
*                                                                               
PUBENFC  LA    R2,ACCNUM*7         WAS 6                           L01          
         LA    R3,CLTINSF                                                       
         LAY   R4,PUBINSF                                                       
PUBENF11 AP    0(8,R3),0(8,R4)                                                  
         LA    R3,8(R3)                                                         
         LA    R4,8(R4)                                                         
         BCT   R2,PUBENF11                                                      
         L     R8,CTOTPUBF         ADD 1 TO PUB ACCUM                           
         AH    R8,=H'1'                                                         
         ST    R8,CTOTPUBF                                                      
         DROP  R9                                                               
****                                                                            
         L     R9,VREPCSCF                                                      
         USING REPDSCTF,R9                                                      
         LA    R2,ACCNUM*7         WAS 6                           L01          
         LA    R3,REPINSF                                                       
         LAY   R4,PUBINSF                                                       
PUBENFD  AP    0(8,R3),0(8,R4)                                                  
         LA    R3,8(R3)                                                         
         LA    R4,8(R4)                                                         
         BCT   R2,PUBENFD                                                       
         DROP  R9                                                               
*                                                                               
         CLI   PRDSW,0             SEE IF COMBINING PRODUCTS                    
         BE    PUBENF15                                                         
         L     R9,VPROCSCF                                                      
         USING PRODSCTF,R9                                                      
         L     R1,PTOTPUBF                                                      
         AH    R1,=H'1'                                                         
         ST    R1,PTOTPUBF                                                      
         LA    R2,ACCNUM*7         WAS 6                           L01          
         LA    R3,PROINSF                                                       
         LAY   R4,PUBINSF                                                       
PUBENF13 AP    0(8,R3),0(8,R4)                                                  
         LA    R3,8(R3)                                                         
         LA    R4,8(R4)                                                         
         BCT   R2,PUBENF13                                                      
         DROP  R9                                                               
*                                                                               
****                                                                            
*                                  CLEAR  PUB ACCUMS                            
PUBENF15 LA    R6,ACCNUM*7         WAS6                            L01          
         LAY   R7,PUBINSF                                                       
PUBENF17 ZAP   0(8,R7),=P'0'                                                    
         LA    R7,8(R7)                                                         
         BCT   R6,PUBENF17                                                      
PUBENF30 DS    0H                                                               
         XC    PUBPRDS,PUBPRDS                                                  
         XC    PUBPRDSF,PUBPRDSF                                                
         XC    PUBMTHS,PUBMTHS                                                  
         XC    PUBMTHSF,PUBMTHSF                                                
         XC    SAVEPUB,SAVEPUB                                                  
         XC    SAVEYMD,SAVEYMD                                                  
         MVI   MTHACT,0                                                         
         MVI   PUBACT,0                                                         
         CLI   PRDSW,1                SEE IF DOING PRDS SEPERATELY              
         BE    PUBENDX                                                          
         XC    PRDMTHS,PRDMTHS                                                  
         XC    PRDMTHSF,PRDMTHSF                                                
         XC    SAVEPRD,SAVEPRD                                                  
         MVI   PRDACT,0                                                         
*                                                                               
PUBENDX  XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
REPEND   CSECT                                                                  
         NMOD1 0,REPEND                                                         
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         USING PPFILED,RC                                                       
         L     R5,VPAYWORK                                                      
         USING PAYWORKD,R5                                                      
         L     R9,VREPCSCT                                                      
         USING REPDSCT,R9                                                       
         GOTO1 VMTHEND                                                          
*                                                                               
         CLI   REPACT,C'Y'                                                      
*****    BNE   REPEND30                                                         
         BNE   REPENF30                                                         
*                                                                               
         SR    R0,R0                                                            
         IC    R0,LINE                                                          
         AH    R0,=H'3'   NEED 4 LINES                                          
         STC   R0,SAVELINE                                                      
         CLC   SAVELINE,MAXLINES                                                
         BNH   *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
         GOTO1 VPRINTIT                                                         
*                                                                               
         MVI   PUBPSW,0              SO PUB CONTINUED MESSAGE WON'T             
*                                    PRINT IN REP TOTALS                        
         CLI   PREPKEY+3,X'11'                                                  
         BNE   REPE15                                                           
         MVC   P+2(16),=C'** REP TOTALS **'                                     
         B     REPE20                                                           
REPE15   CLC   PREPNAME,SPACES                                                  
         BE    REPEND15                                                         
         MVC   P+2(16),=C'** ADR TOTALS **'                                     
REPE20   GOTO1 VPRINTIT                                                         
         XC    PUBMTHS,PUBMTHS                                                  
         LA    R3,ACCNUM                                                        
         LA    R4,0                                                             
REPEND1  LA    R6,REPINS                                                        
         LA    R6,0(R4,R6)                                                      
         LA    R7,7                WAS 6                           L01          
REPEND2  CP    0(8,R6),=P'0'                                                    
         BE    *+12                                                             
         BAS   R8,REPMTH                                                        
         B     *+12                                                             
         LA    R6,ACCNUM*8(R6)                                                  
         BCT   R7,REPEND2                                                       
         LA    R4,8(R4)                                                         
         BCT   R3,REPEND1                                                       
         B     REPEND3                                                          
*                                                                               
REPMTH   EQU   *                                                                
         L     R2,PUBMTHS                                                       
         A     R2,=F'1'                                                         
         ST    R2,PUBMTHS                                                       
         LAY   R2,MTHTAB       R4 HAS MTH DISP                                  
         LA    R2,0(R4,R2)                                                      
         MVC   WORK(4),0(R2)                                                    
         MVC   WORK+4(2),=C'01'                                                 
*        GOTO1 DTCNV,DMCB,(0,WORK),(5,P+5)                                      
         GOTO1 DATCON,DMCB,(0,WORK),(9,P+5)                                     
         LA    R2,REPINS                                                        
         LA    R2,0(R4,R2)                                                      
         CP    0(8,R2),=P'0'                                                    
         BE    RNOPINS              NO INSERTIONS                               
         EDIT  (P8,(R2)),(4,P+24),0                                             
         MVC   P+29(10),=C'INSERTIONS'                                          
         CP    0(8,R2),=P'1'                                                    
         BNE   *+8                                                              
         MVI   P+38,C' '                                                        
RNOPINS  LA    R2,ACCNUM*8(R2)                                                  
         CLI   QMEDIA,C'N'                                                      
         BNE   REPMTH1                                                          
         CP    0(8,R2),=P'0'                                                    
         BE    REPMTH1             NO LINES                                     
         CLI   PROGPROF,C'I'                                                    
         BNE   REPMTHA                                                          
         EDIT (P8,(R2)),(9,P+41),2                                              
         MVI   P+50,C'I'                                                        
         B     REPMTH1                                                          
REPMTHA  EDIT (P8,(R2)),(6,P+44),0                                              
         MVI   P+50,C'L'                                                        
REPMTH1  LA    R2,ACCNUM*8(R2)                                                  
         EDIT (P8,(R2)),(14,P+53),2,COMMAS=YES,MINUS=YES                        
         LA    R2,ACCNUM*8(R2)                                                  
         EDIT (P8,(R2)),(14,P+68),2,COMMAS=YES,MINUS=YES                        
         LA    R2,ACCNUM*8(R2)                                                  
         EDIT (P8,(R2)),(12,P+82),2,COMMAS=YES,MINUS=YES                        
         LA    R2,ACCNUM*8(R2)                                                  
         LA    RF,P+95                                             L01          
         CLI   PAGYNAT,C'C'                                        L01          
         BNE   NTCANDC                                             L01          
         EDIT  (P8,(R2)),(12,(RF)),2,COMMAS=YES,MINUS=YES          L01          
         LA    RF,14(RF)                                           L01          
NTCANDC  DS    0H                                                  L01          
         LA    R2,ACCNUM*8(R2)    BUMP PAST GST                    L01          
*                                                                               
         EDIT (P8,(R2)),(14,(RF)),2,COMMAS=YES,MINUS=YES                        
*                                                                               
         GOTO1 VPRINTIT                                                         
         BR    R8             RETURN                                            
         EJECT                                                                  
REPEND3  MVC   P+5(5),=C'TOTAL'                                                 
         ZAP   WKDUB,REPINS                                                     
         LA    R6,ACCNUM-1                                                      
         LA    R2,REPINS                                                        
REPEND4  AP    WKDUB,8(8,R2)                                                    
         LA    R2,8(R2)                                                         
         BCT   R6,REPEND4                                                       
         CP    WKDUB,=P'0'                                                      
         BE    RNOPTINS             NO INSERTIONS                               
         EDIT  WKDUB,(5,P+23),0                                                 
         MVC   P+29(10),=C'INSERTIONS'                                          
         CP    WKDUB,=P'1'                                                      
         BNE   *+8                                                              
         MVI   P+38,C' '                                                        
RNOPTINS CLI   QMEDIA,C'N'                                                      
         BNE   REPEND6                                                          
         ZAP   WKDUB,REPLINES                                                   
         LA    R2,REPLINES                                                      
         LA    R6,ACCNUM-1                                                      
REPEND5  AP    WKDUB,8(8,R2)                                                    
         LA    R2,8(R2)                                                         
         BCT   R6,REPEND5                                                       
         CP    WKDUB,=P'0'                                                      
         BE    REPEND6             NO LINES                                     
         CLI   PROGPROF,C'I'                                                    
         BNE   REPEND5A                                                         
         EDIT  WKDUB,(9,P+41),2                                                 
         MVI   P+50,C'I'                                                        
         MVI   P+51,C'*'                                                        
         B     REPEND6                                                          
REPEND5A EDIT  WKDUB,(6,P+44),0                                                 
         MVI   P+50,C'L'                                                        
         MVI   P+51,C'*'                                                        
REPEND6  ZAP   WKDUB,REPGO                                                      
         LA    R2,REPGO                                                         
         LA    R6,ACCNUM-1                                                      
REPEND7  AP    WKDUB,8(8,R2)                                                    
         LA    R2,8(R2)                                                         
         BCT   R6,REPEND7                                                       
         EDIT  WKDUB,(14,P+53),2,COMMAS=YES,MINUS=YES                           
         CP    WKDUB,=P'0'                                                      
         BL    *+8                                                              
         MVI   P+66,C'*'                                                        
         ZAP   WKDUB,REPGLAC                                                    
         LA    R2,REPGLAC                                                       
         LA    R6,ACCNUM-1                                                      
REPEND8  AP    WKDUB,8(8,R2)                                                    
         LA    R2,8(R2)                                                         
         BCT   R6,REPEND8                                                       
         EDIT  WKDUB,(14,P+68),2,COMMAS=YES,MINUS=YES                           
         CP    WKDUB,=P'0'                                                      
         BL    *+8                                                              
         MVI   P+81,C'*'                                                        
         ZAP   WKDUB,REPCD                                                      
         LA    R2,REPCD                                                         
         LA    R6,ACCNUM-1                                                      
REPEND9  AP    WKDUB,8(8,R2)                                                    
         LA    R2,8(R2)                                                         
         BCT   R6,REPEND9                                                       
         EDIT  WKDUB,(12,P+82),2,COMMAS=YES,MINUS=YES                           
         CP    WKDUB,=P'0'                                                      
         BL    *+8                                                              
         MVI   P+93,C'*'                                                        
         LA    R3,P+95                                             L01          
         CLI   PAGYNAT,C'C'                                        L01          
         BNE   NTCANDA                                             L01          
         ZAP   WKDUB,REPGST                                        L01          
         LA    R2,REPGST                                           L01          
         LA    R6,ACCNUM-1                                         L01          
REPEND1A AP    WKDUB,8(8,R2)                                       L01          
         LA    R2,8(R2)                                            L01          
         BCT   R6,REPEND1A                                         L01          
         EDIT  WKDUB,(12,(R3)),2,COMMAS=YES,MINUS=YES              L01          
         MVI   12(R3),C'*'                                                      
         CP    WKDUB,=P'0'                                                      
         BL    *+10                                                             
         MVC   11(2,R3),=C'* '                                                  
         LA    R3,14(R3)                                           L01          
NTCANDA  DS    0H                                                  L01          
*                                                                               
         ZAP   WKDUB,REPNP                                                      
         LA    R2,REPNP                                                         
         LA    R6,ACCNUM-1                                                      
REPEND10 AP    WKDUB,8(8,R2)                                                    
         LA    R2,8(R2)                                                         
         BCT   R6,REPEND10                                                      
         EDIT  WKDUB,(14,(R3)),2,COMMAS=YES,MINUS=YES                           
         MVI   14(R3),C'*'                                                      
         CP    WKDUB,=P'0'                                                      
         BL    *+10                                                             
         MVC   13(2,R3),=C'* '                                                  
         SR    R3,R3                                                            
         IC    R3,MAXLINES                                                      
         MVI   MAXLINES,99      ALWAYS PRINT TOTAL                              
         GOTO1 VPRINTIT                                                         
         STC   R3,MAXLINES                                                      
         CLC   LINE,MAXLINES                                                    
         BNL   REPROLL                                                          
         GOTO1 VPRINTIT                                                         
*                                                                               
*                                                                               
REPROLL  L     R8,RTOTPUBS         ADD 1 TO REP ACCUM                           
         AH    R8,=H'1'                                                         
         ST    R8,RTOTPUBS                                                      
*                                  CLEAR  REP ACCUMS                            
REPEND15 LA    R6,ACCNUM*8          WAS 7                                       
         LA    R7,REPINS                                                        
REPEND12 ZAP   0(8,R7),=P'0'                                                    
         LA    R7,8(R7)                                                         
         BCT   R6,REPEND12                                                      
         MVI   PRDACT,C'Y'                                                      
*                                                                               
REPEND30 DS    0H                                                               
*****    XC    PRDMTHS,PRDMTHS     ALL NOW HANDLED IN REPENF30                  
*****    XC    PUBPRDS,PUBPRDS                                                  
*****    XC    PUBMTHS,PUBMTHS                                                  
*****    XC    SAVEPUB,SAVEPUB                                                  
*****    XC    SAVEPRD,SAVEPRD                                                  
*****    XC    SAVEYMD,SAVEYMD                                                  
*****    MVI   MTHACT,0                                                         
*****    MVI   PUBACT,0                                                         
*****    MVI   REPACT,0                                                         
*                                                                               
         DROP  R9                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
REPENDFX DS    0H           NOW CHECK FOR AND PROCESS FOREIGN EXCHANGE          
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         L     R9,VREPCSCF                                                      
         USING REPDSCTF,R9                                                      
*                                                                               
         CLI   REPACT,C'Y'                                                      
         BNE   REPENF30            NO ACTIVITY                                  
*                                                                               
*NOF*    CLI   QOPT7,C'F'          SEPARATE FOREIGN EXCHANGE ?                  
*NOF*    BNE   REPENF30            NO - DONE WITH PUB END                       
*                                                                               
         LA    R8,ACCNUM           SEE IF ANY FOREIGN EXCHANGE                  
         LA    R2,REPGOF                                                        
REPFXCK  CP    0(8,R2),=P'0'       NOT ZERO ?                                   
         BNE   REPENF0             YES - HAS FOREIGN EXCHANGE                   
         LA    R2,8(R2)            BUMP TO NEXT "MONTH" VALUES                  
         BCT   R8,REPFXCK                                                       
         B     REPENF30            NO FOREIGN EXCHANGE FOUND - DONE             
*                                                                               
REPENF0  DS    0H                                                               
*                                                                               
         SR    R0,R0                                                            
         IC    R0,LINE                                                          
         AH    R0,=H'3'   NEED 4 LINES                                          
         STC   R0,SAVELINE                                                      
         CLC   SAVELINE,MAXLINES                                                
         BNH   *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
*****    GOTO1 VPRINTIT                                                         
*                                                                               
         MVI   PUBPSW,0              SO PUB CONTINUED MESSAGE WON'T             
*                                    PRINT IN REP TOTALS                        
         CLI   PREPKEY+3,X'11'                                                  
         BNE   REPE15F                                                          
         MVC   P+2(33),=C'** REP FOREIGN EXCHANGE TOTALS **'                    
         B     REPE20F                                                          
REPE15F  CLC   PREPNAME,SPACES                                                  
         BE    REPENF15                                                         
         MVC   P+2(33),=C'** ADR FOREIGN EXCHANGE TOTALS **'                    
REPE20F  GOTO1 VPRINTIT                                                         
         XC    PUBMTHSF,PUBMTHSF                                                
         LA    R3,ACCNUM                                                        
         LA    R4,0                                                             
REPENF1  LA    R6,REPINSF                                                       
         LA    R6,0(R4,R6)                                                      
         LA    R7,7                WAS 6                           L01          
REPENF2  CP    0(8,R6),=P'0'                                                    
         BE    *+12                                                             
         BAS   R8,REPMTHF                                                       
         B     *+12                                                             
         LA    R6,ACCNUM*8(R6)                                                  
         BCT   R7,REPENF2                                                       
         LA    R4,8(R4)                                                         
         BCT   R3,REPENF1                                                       
         B     REPENF3                                                          
*                                                                               
REPMTHF  EQU   *                                                                
         L     R2,PUBMTHSF                                                      
         A     R2,=F'1'                                                         
         ST    R2,PUBMTHSF                                                      
         LAY   R2,MTHTAB       R4 HAS MTH DISP                                  
         LA    R2,0(R4,R2)                                                      
         MVC   WORK(4),0(R2)                                                    
         MVC   WORK+4(2),=C'01'                                                 
*        GOTO1 DTCNV,DMCB,(0,WORK),(5,P+5)                                      
         GOTO1 DATCON,DMCB,(0,WORK),(9,P+5)                                     
         LA    R2,REPINSF                                                       
         LA    R2,0(R4,R2)                                                      
         CP    0(8,R2),=P'0'                                                    
         BE    RNOPINSF             NO INSERTIONS                               
         EDIT  (P8,(R2)),(4,P+24),0                                             
         MVC   P+29(10),=C'INSERTIONS'                                          
         CP    0(8,R2),=P'1'                                                    
         BNE   *+8                                                              
         MVI   P+38,C' '                                                        
RNOPINSF LA    R2,ACCNUM*8(R2)                                                  
         CLI   QMEDIA,C'N'                                                      
         BNE   REPMTH1F                                                         
         CP    0(8,R2),=P'0'                                                    
         BE    REPMTH1F            NO LINES                                     
         CLI   PROGPROF,C'I'                                                    
         BNE   REPMTHAF                                                         
         EDIT (P8,(R2)),(9,P+41),2                                              
         MVI   P+50,C'I'                                                        
         B     REPMTH1F                                                         
REPMTHAF EDIT (P8,(R2)),(6,P+44),0                                              
         MVI   P+50,C'L'                                                        
REPMTH1F LA    R2,ACCNUM*8(R2)                                                  
         EDIT (P8,(R2)),(14,P+53),2,COMMAS=YES,MINUS=YES                        
         LA    R2,ACCNUM*8(R2)                                                  
         EDIT (P8,(R2)),(14,P+68),2,COMMAS=YES,MINUS=YES                        
         LA    R2,ACCNUM*8(R2)                                                  
         EDIT (P8,(R2)),(12,P+82),2,COMMAS=YES,MINUS=YES                        
         LA    R2,ACCNUM*8(R2)                                                  
         LA    RF,P+95                                             L01          
         CLI   PAGYNAT,C'C'                                        L01          
         BNE   NTCANDCF                                            L01          
*****    EDIT  (P8,(R2)),(12,(RF)),2,COMMAS=YES,MINUS=YES          L01          
*                                                                               
NTCANDCF DS    0H                                                  L01          
         LA    RF,14(RF)                                           L01          
         LA    R2,ACCNUM*8(R2)    BUMP PAST GST                    L01          
*                                                                               
         EDIT (P8,(R2)),(14,(RF)),2,COMMAS=YES,MINUS=YES                        
*                                                                               
         GOTO1 VPRINTIT                                                         
         BR    R8             RETURN                                            
         EJECT                                                                  
REPENF3  MVC   P+5(5),=C'TOTAL'                                                 
         ZAP   WKDUB,REPINSF                                                    
         LA    R6,ACCNUM-1                                                      
         LA    R2,REPINSF                                                       
REPENF4  AP    WKDUB,8(8,R2)                                                    
         LA    R2,8(R2)                                                         
         BCT   R6,REPENF4                                                       
         CP    WKDUB,=P'0'                                                      
         BE    RNOPTINF             NO INSERTIONS                               
         EDIT  WKDUB,(5,P+23),0                                                 
         MVC   P+29(10),=C'INSERTIONS'                                          
         CP    WKDUB,=P'1'                                                      
         BNE   *+8                                                              
         MVI   P+38,C' '                                                        
RNOPTINF CLI   QMEDIA,C'N'                                                      
         BNE   REPENF6                                                          
         ZAP   WKDUB,REPLINEF                                                   
         LA    R2,REPLINEF                                                      
         LA    R6,ACCNUM-1                                                      
REPENF5  AP    WKDUB,8(8,R2)                                                    
         LA    R2,8(R2)                                                         
         BCT   R6,REPENF5                                                       
         CP    WKDUB,=P'0'                                                      
         BE    REPENF6             NO LINES                                     
         CLI   PROGPROF,C'I'                                                    
         BNE   REPENF5A                                                         
         EDIT  WKDUB,(9,P+41),2                                                 
         MVI   P+50,C'I'                                                        
         MVI   P+51,C'*'                                                        
         B     REPENF6                                                          
REPENF5A EDIT  WKDUB,(6,P+44),0                                                 
         MVI   P+50,C'L'                                                        
         MVI   P+51,C'*'                                                        
REPENF6  ZAP   WKDUB,REPGOF                                                     
         LA    R2,REPGOF                                                        
         LA    R6,ACCNUM-1                                                      
REPENF7  AP    WKDUB,8(8,R2)                                                    
         LA    R2,8(R2)                                                         
         BCT   R6,REPENF7                                                       
         EDIT  WKDUB,(14,P+53),2,COMMAS=YES,MINUS=YES                           
         CP    WKDUB,=P'0'                                                      
         BL    *+8                                                              
         MVI   P+66,C'*'                                                        
         ZAP   WKDUB,REPGLACF                                                   
         LA    R2,REPGLACF                                                      
         LA    R6,ACCNUM-1                                                      
REPENF8  AP    WKDUB,8(8,R2)                                                    
         LA    R2,8(R2)                                                         
         BCT   R6,REPENF8                                                       
         EDIT  WKDUB,(14,P+68),2,COMMAS=YES,MINUS=YES                           
         CP    WKDUB,=P'0'                                                      
         BL    *+8                                                              
         MVI   P+81,C'*'                                                        
         ZAP   WKDUB,REPCDF                                                     
         LA    R2,REPCDF                                                        
         LA    R6,ACCNUM-1                                                      
REPENF9  AP    WKDUB,8(8,R2)                                                    
         LA    R2,8(R2)                                                         
         BCT   R6,REPENF9                                                       
         EDIT  WKDUB,(12,P+82),2,COMMAS=YES,MINUS=YES                           
         CP    WKDUB,=P'0'                                                      
         BL    *+8                                                              
         MVI   P+93,C'*'                                                        
         LA    R3,P+95                                             L01          
         CLI   PAGYNAT,C'C'                                        L01          
         BNE   NTCANDAF                                            L01          
         ZAP   WKDUB,REPGSTF                                       L01          
         LA    R2,REPGSTF                                          L01          
         LA    R6,ACCNUM-1                                         L01          
REPENF1A AP    WKDUB,8(8,R2)                                       L01          
         LA    R2,8(R2)                                            L01          
         BCT   R6,REPENF1A                                         L01          
         EDIT  WKDUB,(12,(R3)),2,COMMAS=YES,MINUS=YES              L01          
         MVI   12(R3),C'*'                                                      
         CP    WKDUB,=P'0'                                                      
         BL    *+10                                                             
         MVC   11(2,R3),=C'* '                                                  
*****    LA    R3,14(R3)                                           L01          
NTCANDAF DS    0H                                                  L01          
         LA    R3,14(R3)                                           L01          
*                                                                               
         ZAP   WKDUB,REPNPF                                                     
         LA    R2,REPNPF                                                        
         LA    R6,ACCNUM-1                                                      
REPENF10 AP    WKDUB,8(8,R2)                                                    
         LA    R2,8(R2)                                                         
         BCT   R6,REPENF10                                                      
         EDIT  WKDUB,(14,(R3)),2,COMMAS=YES,MINUS=YES                           
         MVI   14(R3),C'*'                                                      
         CP    WKDUB,=P'0'                                                      
         BL    *+10                                                             
         MVC   13(2,R3),=C'* '                                                  
         SR    R3,R3                                                            
         IC    R3,MAXLINES                                                      
         MVI   MAXLINES,99      ALWAYS PRINT TOTAL                              
         GOTO1 VPRINTIT                                                         
         STC   R3,MAXLINES                                                      
         CLC   LINE,MAXLINES                                                    
         BNL   REPROLLF                                                         
         GOTO1 VPRINTIT                                                         
*                                                                               
*                                                                               
REPROLLF LY    R8,RTOTPUBF         ADD 1 TO REP ACCUM                           
         AH    R8,=H'1'                                                         
         ST    R8,RTOTPUBF                                                      
*                                  CLEAR  REP ACCUMS                            
REPENF15 LA    R6,ACCNUM*8          WAS 7                                       
         LA    R7,REPINSF                                                       
REPENF12 ZAP   0(8,R7),=P'0'                                                    
         LA    R7,8(R7)                                                         
         BCT   R6,REPENF12                                                      
         MVI   PRDACT,C'Y'                                                      
*                                                                               
REPENF30 DS    0H                                                               
         XC    PRDMTHS,PRDMTHS                                                  
         XC    PUBPRDS,PUBPRDS                                                  
         XC    PUBMTHS,PUBMTHS                                                  
         XC    PRDMTHSF,PRDMTHSF                                                
         XC    PUBPRDSF,PUBPRDSF                                                
         XC    PUBMTHSF,PUBMTHSF                                                
         XC    SAVEPUB,SAVEPUB                                                  
         XC    SAVEPRD,SAVEPRD                                                  
         XC    SAVEYMD,SAVEYMD                                                  
         MVI   MTHACT,0                                                         
         MVI   PUBACT,0                                                         
         MVI   REPACT,0                                                         
*                                                                               
         DROP  R9                                                               
*                                                                               
REPENDX  XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
CLTEND   CSECT                                                                  
         NMOD1 0,CLTEND                                                         
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         USING PPFILED,RC                                                       
         L     R5,VPAYWORK                                                      
         USING PAYWORKD,R5                                                      
         L     R9,VCLTCSCT                                                      
         USING CLTDSCT,R9                                                       
         CLI   CLTACT,C'Y'                                                      
         BNE   CLTENDX                                                          
         GOTO1 VMTHEND                                                          
*                                                                               
         CLI   PRDSW,0                 SEE IF COMBINING PRDS                    
         BE    CLTE5                                                            
         GOTO1 VPUBEND                                                          
         GOTO1 VPRDEND                                                          
         B     CLTE10                                                           
*                                                                               
CLTE5    GOTO1 VPRDEND                                                          
         GOTO1 VPUBEND                                                          
*                                                                               
CLTE10   CLI   PRDSW,0                SEE IF COMBINING PRDS                     
         BE    CLTE15                                                           
         CLC   QPRODUCT,=C'ALL'                                                 
         BE    CLTE15                                                           
*****    B     CLTEN12X                                                         
         B     CLTFXEND                                                         
*                                                                               
CLTE15   MVI   FORCEHED,C'Y'                                                    
         MVI   PUBPSW,0                                                         
         MVC   P+1(20),=C' ** CLIENT TOTALS **'                                 
         GOTO1 VPRINTIT                                                         
         LA    R8,ACCNUM                                                        
         LA    R4,0                                                             
CLTEND1  LA    R2,CLTINS                                                        
         LA    R2,0(R4,R2)                                                      
         LA    R7,7                WAS 6                           L01          
CLTEND2  CP    0(8,R2),=P'0'                                                    
         BNE   ACTIVITY                                                         
         LA    R2,ACCNUM*8(R2)                                                  
         BCT   R7,CLTEND2                                                       
CLTEND3  LA    R4,8(R4)                                                         
         BCT   R8,CLTEND1                                                       
         B     CLTEND5        GO TO TOTALS                                      
*                                                                               
ACTIVITY LAY   R1,MTHTAB                                                        
         LA    R1,0(R4,R1)                                                      
         MVC   WORK(4),0(R1)                                                    
         MVC   WORK+4(2),=C'01'                                                 
*        GOTO1 DTCNV,DMCB,(0,WORK),(5,P+5)                                      
         GOTO1 DATCON,DMCB,(0,WORK),(9,P+5)                                     
*                                                                               
         CLI   PRDSW,1                     SEE IF DOING PRDS SEP                
         BE    ACTV5                                                            
*                                                                               
         LA    R1,CLTPUBS                                                       
         LA    R1,0(R4,R1)                                                      
         ZAP   WKDUB,0(8,R1)                                                    
         EDIT  WKDUB,(4,P+12),0                                                 
         MVC   P+17(4),=C'PUBS'                                                 
         CP    WKDUB,=P'1'                                                      
         BNE   *+8                                                              
         MVI   P+20,C' '                                                        
ACTV5    LA    R1,CLTINS                                                        
         LA    R1,0(R4,R1)                                                      
         ZAP   WKDUB,0(8,R1)                                                    
         CP    WKDUB,=P'0'                                                      
         BE    NOCINS              NO INSERTIONS                                
         EDIT  WKDUB,(5,P+23),0                                                 
         MVC   P+29(10),=C'INSERTIONS'                                          
         CP    WKDUB,=P'1'                                                      
         BNE   *+8                                                              
         MVI   P+38,C' '                                                        
NOCINS   LA    R1,ACCNUM*8(R1)                                                  
         CLI   QMEDIA,C'N'                                                      
         BNE   ACTIV1                                                           
         ZAP   WKDUB,0(8,R1)                                                    
         CP    WKDUB,=P'0'                                                      
         BE    ACTIV1              NO LINES                                     
         CLI   PROGPROF,C'I'                                                    
         BNE   ACTIVA                                                           
         EDIT  WKDUB,(10,P+41),2                                                
         MVI   P+51,C'I'                                                        
         B     ACTIV1                                                           
ACTIVA   EDIT  WKDUB,(7,P+43),0                                                 
         MVI   P+50,C'L'                                                        
ACTIV1   LA    R1,ACCNUM*8(R1)                                                  
         ZAP   WKDUB,0(8,R1)                                                    
         EDIT  WKDUB,(14,P+53),2,COMMAS=YES,MINUS=YES                           
         LA    R1,ACCNUM*8(R1)                                                  
         ZAP   WKDUB,0(8,R1)                                                    
         EDIT  WKDUB,(14,P+68),2,COMMAS=YES,MINUS=YES                           
         LA    R1,ACCNUM*8(R1)                                                  
         ZAP   WKDUB,0(8,R1)                                                    
         EDIT  WKDUB,(12,P+82),2,COMMAS=YES,MINUS=YES                           
         LA    R1,ACCNUM*8(R1)                                                  
         LA    R3,P+95                                             L01          
         CLI   PAGYNAT,C'C'                                        L01          
         BNE   NTCANDD                                             L01          
         ZAP   WKDUB,0(8,R1)                                       L01          
         EDIT  WKDUB,(12,(R3)),2,COMMAS=YES,MINUS=YES              L01          
         LA    R3,14(R3)                                           L01          
NTCANDD  DS    0H                                                  L01          
         LA    R1,ACCNUM*8(R1)    BUMP PAST GST                    L01          
*                                                                               
         ZAP   WKDUB,0(8,R1)                                                    
         EDIT  WKDUB,(14,(R3)),2,COMMAS=YES,MINUS=YES                           
         GOTO1 VPRINTIT                                                         
         B     CLTEND3                                                          
*                                                                               
*                                                                               
CLTEND5  MVC   P+5(5),=C'TOTAL'                                                 
*                                                                               
         CLI   PRDSW,1              SEE IF DOING PRDS SEP                       
         BE    CLTEND5C                                                         
*                                                                               
         L     R2,CTOTPUBS                                                      
         EDIT  (R2),(4,P+12),0                                                  
         MVC   P+17(4),=C'PUBS'                                                 
         C     R2,=F'1'                                                         
         BNE   *+8                                                              
         MVI   P+20,C' '                                                        
CLTEND5C ZAP   WKDUB,CLTINS                                                     
         LA    R3,ACCNUM-1                                                      
         LA    R4,CLTINS+8                                                      
CLTEND6  AP    WKDUB,0(8,R4)                                                    
         LA    R4,8(R4)                                                         
         BCT   R3,CLTEND6                                                       
         CP    WKDUB,=P'0'                                                      
         BE    NOCTINS             NO INSERTIONS                                
         EDIT  WKDUB,(5,P+23),0                                                 
         MVC   P+29(10),=C'INSERTIONS'                                          
         CP    WKDUB,=P'1'                                                      
         BNE   *+8                                                              
         MVI   P+38,C' '                                                        
NOCTINS  CLI   QMEDIA,C'N'                                                      
         BNE   CLTEND8                                                          
         ZAP   WKDUB,CLTLINES                                                   
         LA    R3,ACCNUM-1                                                      
         LA    R4,CLTLINES+8                                                    
CLTEND7  AP    WKDUB,0(8,R4)                                                    
         LA    R4,8(R4)                                                         
         BCT   R3,CLTEND7                                                       
         CP    WKDUB,=P'0'                                                      
         BE    CLTEND8             NO LINES                                     
         CLI   PROGPROF,C'I'                                                    
         BNE   CLTEND7A                                                         
         EDIT  WKDUB,(10,P+41),2                                                
         MVI   P+51,C'I'                                                        
         MVI   P+52,C'*'                                                        
         B     CLTEND8                                                          
CLTEND7A EDIT  WKDUB,(7,P+43),0                                                 
         MVI   P+50,C'L'                                                        
         MVI   P+51,C'*'                                                        
CLTEND8  ZAP   WKDUB,CLTGO                                                      
         LA    R3,ACCNUM-1                                                      
         LA    R4,CLTGO+8                                                       
CLTEND9  AP    WKDUB,0(8,R4)                                                    
         LA    R4,8(R4)                                                         
         BCT   R3,CLTEND9                                                       
         EDIT  WKDUB,(15,P+52),2,COMMAS=YES,MINUS=YES                           
         CP    WKDUB,=P'0'                                                      
         BL    *+8                                                              
         MVI   P+66,C'*'                                                        
         ZAP   WKDUB,CLTGLAC                                                    
         LA    R3,ACCNUM-1                                                      
         LA    R4,CLTGLAC+8                                                     
CLTEND10 AP    WKDUB,0(8,R4)                                                    
         LA    R4,8(R4)                                                         
         BCT   R3,CLTEND10                                                      
         EDIT  WKDUB,(15,P+67),2,COMMAS=YES,MINUS=YES                           
         CP    WKDUB,=P'0'                                                      
         BL    *+8                                                              
         MVI   P+81,C'*'                                                        
         ZAP   WKDUB,CLTCD                                                      
         LA    R3,ACCNUM-1                                                      
         LA    R4,CLTCD+8                                                       
CLTEND11 AP    WKDUB,0(8,R4)                                                    
         LA    R4,8(R4)                                                         
         BCT   R3,CLTEND11                                                      
         CP    WKDUB,=P'99999999'      IF OVER 99,999.99 NO COMMAS              
         BH    CLTE11B                                                          
         EDIT  WKDUB,(12,P+82),2,COMMAS=YES,MINUS=YES                           
         B     CLTE11C                                                          
CLTE11B  EDIT  WKDUB,(12,P+82),2,MINUS=YES                                      
CLTE11C  CP    WKDUB,=P'0'                                                      
         BL    *+8                                                              
         MVI   P+93,C'*'                                                        
*===============                                                                
         LA    R1,P+95                                             L01          
         CLI   PAGYNAT,C'C'                                        L01          
         BNE   NTCANDAW                                            L01          
         ZAP   WKDUB,CLTGST                                        L01          
         LA    R3,ACCNUM-1                                         L01          
         LA    R4,CLTGST+8                                         L01          
CLTEND41 AP    WKDUB,0(8,R4)                                       L01          
         LA    R4,8(R4)                                            L01          
         BCT   R3,CLTEND41                                         L01          
         EDIT  WKDUB,(12,(R1)),2,COMMAS=YES,MINUS=YES             L01           
         MVI   12(R1),C'*'                                                      
         CP    WKDUB,=P'0'                                                      
         BL    *+10                                                             
         MVC   11(2,R1),=C'* '                                                  
         LA    R1,14(R1)                                           L01          
NTCANDAW DS    0H                                                  L01          
*===============                                                                
         ZAP   WKDUB,CLTNP                                                      
         LA    R3,ACCNUM-1                                                      
         LA    R4,CLTNP+8                                                       
CLTEND12 AP    WKDUB,0(8,R4)                                                    
         LA    R4,8(R4)                                                         
         BCT   R3,CLTEND12                                                      
         CP    WKDUB,=P'9999999999'   IF OVER 99,999,999.99                     
         BH    CLTE12B                                                          
         EDIT  WKDUB,(14,(R1)),2,COMMAS=YES,MINUS=YES              L01          
         B     CLTE12C                                                          
CLTE12B  EDIT  WKDUB,(14,(R1)),2,MINUS=YES                         L01          
CLTE12C  CP    WKDUB,=P'0'                                                      
         MVI   14(R1),C'*'                                         L01          
         BL    *+10                                                             
         MVC   13(2,R1),=C'* '                                     L01          
         GOTO1 VPRINTIT                                                         
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                           NOW CHECK FOR AND PROCESS FOREIGN EXCHANGE          
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
CLTENDFX DS    0H                                                  L01          
*                                                                               
*NOF*    CLI   QOPT7,C'F'          SEPARATE FOREIGN EXCHANGE ?                  
*NOF*    BNE   CLTFXEND            NO - FINISH CLTEND                           
*                                                                               
         L     R9,VCLTCSCF                                                      
         USING CLTDSCTF,R9                                                      
*                                                                               
CLTE10F  CLI   PRDSW,0                SEE IF COMBINING PRDS                     
         BE    CLTE15F                                                          
         CLC   QPRODUCT,=C'ALL'                                                 
         BE    CLTE15F                                                          
         B     CLTFXEND                                                         
*                                                                               
CLTE15F  DS    0H                  SEE IF ANY FOREIGN EXCHANGE                  
         LA    R8,ACCNUM                                                        
         LA    R2,CLTGOF                                                        
CLTFXCK  CP    0(8,R2),=P'0'       NOT ZERO ?                                   
         BNE   CLTE15G             YES - HAS FOREIGN EXCHANGE                   
         LA    R2,8(R2)            BUMP TO NEXT "MONTH" VALUES                  
         BCT   R8,CLTFXCK                                                       
         B     CLTFXEND            NO FOREIGN EXCHANGE FOUND - DONE             
*                                                                               
CLTE15G  DS    0H                                                               
*****    MVI   FORCEHED,C'Y'                                                    
         MVI   PUBPSW,0                                                         
         MVC   P+1(37),=C' ** CLIENT FOREIGN EXCHANGE TOTALS **'                
         GOTO1 VPRINTIT                                                         
         LA    R8,ACCNUM                                                        
         LA    R4,0                                                             
CLTENF1  LA    R2,CLTINSF                                                       
         LA    R2,0(R4,R2)                                                      
         LA    R7,7                WAS 6                           L01          
CLTENF2  CP    0(8,R2),=P'0'                                                    
         BNE   ACTIVITF                                                         
         LA    R2,ACCNUM*8(R2)                                                  
         BCT   R7,CLTENF2                                                       
CLTENF3  LA    R4,8(R4)                                                         
         BCT   R8,CLTENF1                                                       
         B     CLTENF5        GO TO TOTALS                                      
*                                                                               
ACTIVITF LAY   R1,MTHTAB                                                        
         LA    R1,0(R4,R1)                                                      
         MVC   WORK(4),0(R1)                                                    
         MVC   WORK+4(2),=C'01'                                                 
*        GOTO1 DTCNV,DMCB,(0,WORK),(5,P+5)                                      
         GOTO1 DATCON,DMCB,(0,WORK),(9,P+5)                                     
*                                                                               
         CLI   PRDSW,1                     SEE IF DOING PRDS SEP                
         BE    ACTV5F                                                           
*                                                                               
         LA    R1,CLTPUBSF                                                      
         LA    R1,0(R4,R1)                                                      
         ZAP   WKDUB,0(8,R1)                                                    
         EDIT  WKDUB,(4,P+12),0                                                 
         MVC   P+17(4),=C'PUBS'                                                 
         CP    WKDUB,=P'1'                                                      
         BNE   *+8                                                              
         MVI   P+20,C' '                                                        
ACTV5F   LA    R1,CLTINSF                                                       
         LA    R1,0(R4,R1)                                                      
         ZAP   WKDUB,0(8,R1)                                                    
         CP    WKDUB,=P'0'                                                      
         BE    NOCINSF             NO INSERTIONS                                
         EDIT  WKDUB,(5,P+23),0                                                 
         MVC   P+29(10),=C'INSERTIONS'                                          
         CP    WKDUB,=P'1'                                                      
         BNE   *+8                                                              
         MVI   P+38,C' '                                                        
NOCINSF  LA    R1,ACCNUM*8(R1)                                                  
         CLI   QMEDIA,C'N'                                                      
         BNE   ACTIV1F                                                          
         ZAP   WKDUB,0(8,R1)                                                    
         CP    WKDUB,=P'0'                                                      
         BE    ACTIV1F             NO LINES                                     
         CLI   PROGPROF,C'I'                                                    
         BNE   ACTIVAF                                                          
         EDIT  WKDUB,(10,P+41),2                                                
         MVI   P+51,C'I'                                                        
         B     ACTIV1F                                                          
ACTIVAF  EDIT  WKDUB,(7,P+43),0                                                 
         MVI   P+50,C'L'                                                        
ACTIV1F  LA    R1,ACCNUM*8(R1)                                                  
         ZAP   WKDUB,0(8,R1)                                                    
         EDIT  WKDUB,(14,P+53),2,COMMAS=YES,MINUS=YES                           
         LA    R1,ACCNUM*8(R1)                                                  
         ZAP   WKDUB,0(8,R1)                                                    
         EDIT  WKDUB,(14,P+68),2,COMMAS=YES,MINUS=YES                           
         LA    R1,ACCNUM*8(R1)                                                  
         ZAP   WKDUB,0(8,R1)                                                    
         EDIT  WKDUB,(12,P+82),2,COMMAS=YES,MINUS=YES                           
         LA    R1,ACCNUM*8(R1)                                                  
         LA    R3,P+95                                                          
         CLI   PAGYNAT,C'C'                                                     
         BNE   NTCANDDF                                                         
*****    ZAP   WKDUB,0(8,R1)                                                    
*****    EDIT  WKDUB,(12,(R3)),2,COMMAS=YES,MINUS=YES                           
         LA    R3,14(R3)                                                        
NTCANDDF DS    0H                                                               
         LA    R1,ACCNUM*8(R1)    BUMP PAST GST                                 
*                                                                               
         ZAP   WKDUB,0(8,R1)                                                    
         EDIT  WKDUB,(14,(R3)),2,COMMAS=YES,MINUS=YES                           
         GOTO1 VPRINTIT                                                         
         B     CLTENF3                                                          
*                                                                               
*                                                                               
CLTENF5  MVC   P+5(05),=C'TOTAL'                                                
*                                                                               
         CLI   PRDSW,1              SEE IF DOING PRDS SEP                       
         BE    CLTENF5C                                                         
*                                                                               
         L     R2,CTOTPUBF                                                      
         EDIT  (R2),(4,P+12),0                                                  
         MVC   P+17(4),=C'PUBS'                                                 
         C     R2,=F'1'                                                         
         BNE   *+8                                                              
         MVI   P+20,C' '                                                        
CLTENF5C ZAP   WKDUB,CLTINSF                                                    
         LA    R3,ACCNUM-1                                                      
         LA    R4,CLTINSF+8                                                     
CLTENF6  AP    WKDUB,0(8,R4)                                                    
         LA    R4,8(R4)                                                         
         BCT   R3,CLTENF6                                                       
         CP    WKDUB,=P'0'                                                      
         BE    NOCTINSF            NO INSERTIONS                                
         EDIT  WKDUB,(5,P+23),0                                                 
         MVC   P+29(10),=C'INSERTIONS'                                          
         CP    WKDUB,=P'1'                                                      
         BNE   *+8                                                              
         MVI   P+38,C' '                                                        
NOCTINSF CLI   QMEDIA,C'N'                                                      
         BNE   CLTENF8                                                          
         ZAP   WKDUB,CLTLINEF                                                   
         LA    R3,ACCNUM-1                                                      
         LA    R4,CLTLINEF+8                                                    
CLTENF7  AP    WKDUB,0(8,R4)                                                    
         LA    R4,8(R4)                                                         
         BCT   R3,CLTENF7                                                       
         CP    WKDUB,=P'0'                                                      
         BE    CLTENF8             NO LINES                                     
         CLI   PROGPROF,C'I'                                                    
         BNE   CLTENF7A                                                         
         EDIT  WKDUB,(10,P+41),2                                                
         MVI   P+51,C'I'                                                        
         MVI   P+52,C'*'                                                        
         B     CLTENF8                                                          
CLTENF7A EDIT  WKDUB,(7,P+43),0                                                 
         MVI   P+50,C'L'                                                        
         MVI   P+51,C'*'                                                        
CLTENF8  ZAP   WKDUB,CLTGOF                                                     
         LA    R3,ACCNUM-1                                                      
         LA    R4,CLTGOF+8                                                      
CLTENF9  AP    WKDUB,0(8,R4)                                                    
         LA    R4,8(R4)                                                         
         BCT   R3,CLTENF9                                                       
         EDIT  WKDUB,(15,P+52),2,COMMAS=YES,MINUS=YES                           
         CP    WKDUB,=P'0'                                                      
         BL    *+8                                                              
         MVI   P+66,C'*'                                                        
         ZAP   WKDUB,CLTGLACF                                                   
         LA    R3,ACCNUM-1                                                      
         LA    R4,CLTGLACF+8                                                    
CLTENF10 AP    WKDUB,0(8,R4)                                                    
         LA    R4,8(R4)                                                         
         BCT   R3,CLTENF10                                                      
         EDIT  WKDUB,(15,P+67),2,COMMAS=YES,MINUS=YES                           
         CP    WKDUB,=P'0'                                                      
         BL    *+8                                                              
         MVI   P+81,C'*'                                                        
         ZAP   WKDUB,CLTCDF                                                     
         LA    R3,ACCNUM-1                                                      
         LA    R4,CLTCDF+8                                                      
CLTENF11 AP    WKDUB,0(8,R4)                                                    
         LA    R4,8(R4)                                                         
         BCT   R3,CLTENF11                                                      
         CP    WKDUB,=P'99999999'      IF OVER 99,999.99 NO COMMAS              
         BH    CLTE11BF                                                         
         EDIT  WKDUB,(12,P+82),2,COMMAS=YES,MINUS=YES                           
         B     CLTE11CF                                                         
CLTE11BF EDIT  WKDUB,(12,P+82),2,MINUS=YES                                      
CLTE11CF CP    WKDUB,=P'0'                                                      
         BL    *+8                                                              
         MVI   P+93,C'*'                                                        
*===============                                                                
         LA    R1,P+95                                                          
         CLI   PAGYNAT,C'C'                                                     
         BNE   NTCANDFW                                                         
*****    ZAP   WKDUB,CLTGSTF                                                    
*****    LA    R3,ACCNUM-1                                                      
*****    LA    R4,CLTGSTF+8                                                     
*****CLTENF41 AP    WKDUB,0(8,R4)                                               
*****    LA    R4,8(R4)                                                         
*****    BCT   R3,CLTENF41                                                      
*****    EDIT  WKDUB,(12,(R1)),2,COMMAS=YES,MINUS=YES                           
*****    MVI   12(R1),C'*'                                                      
*****    CP    WKDUB,=P'0'                                                      
*****    BL    *+10                                                             
*****    MVC   11(2,R1),=C'* '                                                  
         LA    R1,14(R1)                                                        
NTCANDFW DS    0H                                                               
*===============                                                                
         ZAP   WKDUB,CLTNPF                                                     
         LA    R3,ACCNUM-1                                                      
         LA    R4,CLTNPF+8                                                      
CLTENF12 AP    WKDUB,0(8,R4)                                                    
         LA    R4,8(R4)                                                         
         BCT   R3,CLTENF12                                                      
         CP    WKDUB,=P'9999999999'   IF OVER 99,999,999.99                     
         BH    CLTE12BF                                                         
         EDIT  WKDUB,(14,(R1)),2,COMMAS=YES,MINUS=YES                           
         B     CLTE12CF                                                         
CLTE12BF EDIT  WKDUB,(14,(R1)),2,MINUS=YES                                      
CLTE12CF CP    WKDUB,=P'0'                                                      
         MVI   14(R1),C'*'                                                      
         BL    *+10                                                             
         MVC   13(2,R1),=C'* '                                                  
         GOTO1 VPRINTIT                                                         
*                                                                               
CLTFXEND DS    0H                                                               
         XC    PRDMTHSF,PRDMTHSF                                                
         XC    PUBPRDSF,PUBPRDSF                                                
         XC    PUBMTHSF,PUBMTHSF                                                
         LA    R3,CLTINSF                                                       
         LA    R4,ACCNUM*8                  WAS 7                               
CLTFXE20 ZAP   0(8,R3),=P'0'                                                    
         LA    R3,8(R3)                                                         
         BCT   R4,CLTFXE20                                                      
         XC    CTOTPUBF,CTOTPUBF                                                
         DROP  R9                                                               
*                                                                               
         L     R9,VCLTCSCT                                                      
         USING CLTDSCT,R9                                                       
*                                                                               
         XC    MTHACT(5),MTHACT                                                 
         XC    PRDMTHS,PRDMTHS                                                  
         XC    PUBPRDS,PUBPRDS                                                  
         XC    PUBMTHS,PUBMTHS                                                  
         LA    R3,CLTINS                                                        
         LA    R4,ACCNUM*8                  WAS 7                               
CLTENF13 ZAP   0(8,R3),=P'0'                                                    
         LA    R3,8(R3)                                                         
         BCT   R4,CLTENF13                                                      
         XC    CTOTPUBS,CTOTPUBS                                                
*                                                                               
CLTENDX  XIT1                                                                   
         DROP  R9                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
CLIF     CSECT                                                                  
         NMOD1 0,CLIF                                                           
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         USING PPFILED,RC                                                       
         L     R5,VPAYWORK                                                      
         USING PAYWORKD,R5                                                      
         MVI   FORCEHED,C'Y'                                                    
         XC    SAVEYMD,SAVEYMD                                                  
         XC    SAVEPRD,SAVEPRD                                                  
         XC    SAVEPUB,SAVEPUB                                                  
*              FIRST BUILD LIST OF PRD AND NAMES                                
         MVC   PPGKEY,KEY                                                       
         CLC   QCLIENT,=C'ALL'                                                  
         BE    CLIF1                                                            
         CLI   QCLIENT,C'&&'       GROUP                                        
         BE    CLIF1                                                            
         CLI   QCLIENT,C'$'        OFFICE LIST                                  
         BE    CLIF0                                                            
         CLI   QCLIENT,C'*'        OFFICE REQS                                  
         BNE   CLIEXT                                                           
*                                                                               
CLIF0    DS    0H       SAVE OFFICE FOR OFFICE LIST AND REQS HEADERS            
         GOTOR VPRNTOFC,DMCB,(0,PCLTOFF),(C'L',SVPTOFC),VOFFICER,      X        
               QAGENCY,VCOMFACS                                                 
*                                                                               
CLIF1    DS    0H                                                               
         L     R6,APRDTAB                                                       
*                                                                               
         CLI   QPUB,C'0'           SEE IF DOING ONE PUB                         
         BNL   CLIF6               YES - ONLY READ PRDS WHEN NEEDED             
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),QAGENCY                                                   
         MVC   KEY+2(1),QMEDIA                                                  
         MVI   KEY+3,X'06'                                                      
         MVC   KEY+4(3),PCLTKCLT                                                
         LA    R4,DMRDHI                                                        
         MVC   KEYSAVE,KEY                                                      
         B     CLIF3                                                            
*                                                                               
CLIF2    LA    R4,DMRSEQ                                                        
CLIF3    BAS   RE,DIRRD                                                         
         CLC   KEY(7),KEYSAVE                                                   
         BNE   CLIF6                                                            
*                                                                               
*                                                                               
CLIF4    LA    R4,GETREC                                                        
         LA    R3,PPRDREC                                                       
         BAS   RE,FILERD                                                        
CLIF5    MVC   0(3,R6),PPRDKPRD                                                 
         MVC   3(20,R6),PPRDNAME                                                
         LA    R6,23(R6)                                                        
         B     CLIF2                                                            
*                                                                               
CLIF6    MVC   0(3,R6),=X'FFFFFF'  SET END OF TABLE                             
         B     PPGEXT                                                           
*                                                                               
DIRRD    NTR                                                                    
         GOTO1 DATAMGR,DMCB,(DMINBTS,(R4)),=C'PRTDIR',KEY,KEY,(0,0)             
         CLI   DMCB+8,0                                                         
         BE    DIRX                                                             
         CLI   DMCB+8,X'02'         PASS DELETES                                
         BE    DIRX                                                             
         DC    H'0'                                                             
DIRX     XIT                                                                    
*                                                                               
FILERD   NTR                                                                    
         GOTO1 DATAMGR,DMCB,(DMINBTS,(R4)),=C'PRTFILE',KEY+27,         X        
               (R3),(0,DMWORK)                                                  
         CLI   DMCB+8,0                                                         
         BE    FILX                                                             
         CLI   DMCB+8,X'02'            PASS DELETES                             
         BE    FILX                                                             
         DC    H'0'                                                             
FILX     XIT                                                                    
*                                                                               
PPGEXT   MVC   KEY,PPGKEY                                                       
         LA    R4,DMRDHI                                                        
         BAS   RE,DIRRD                                                         
CLIEXT   XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
PUBF     CSECT                                                                  
         NMOD1 0,PUBF                                                           
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         USING PPFILED,RC                                                       
         L     R5,VPAYWORK                                                      
         USING PAYWORKD,R5                                                      
         MVI   PUBSW,0                                                          
         CLI   QOPT3,C' '                                                       
         BE    SETFLT              DOING ALL PUBS                               
*                                                                               
         LAY   R3,PUBREC+33                                                     
         USING PUBGENEL,R3                                                      
         CLI   0(R3),X'10'                                                      
         BE    *+6                                                              
         DC    H'0'                NO NAME ELEMENT                              
         SR    R0,R0                                                            
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         CLI   0(R3),X'20'                                                      
         BE    HAVEL       HAVE PROD ELEMENT                                    
         CLI   QOPT3,C'C'                                                       
         BE    GETNEXT                                                          
         B     SETFLT                                                           
*                                                                               
HAVEL    DS    0H                                                               
*                                                                               
         CLC   PUBCDDAT,CDPDATE    SEE IF I CAN IGNORE                          
         BNL   *+10                                                             
         XC    PUBCDDAT,PUBCDDAT   CLEAR IF LOW                                 
*                                                                               
         OC    PUBCDDAT,PUBCDDAT   CHK FOR CD EFF DATE                          
         BNZ   HAVEL5              IF PRESENT THIS IS OR WAS A CD PUB           
         CP    PUBCD,=P'0'                                                      
         BE    NCHDIS              NO CASH DISCOUNT                             
HAVEL5   CLI   QOPT3,C'C'                                                       
         BE    SETFLT                                                           
         B     GETNEXT                                                          
*                                                                               
NCHDIS   CLI   QOPT3,C'C'                                                       
         BE    GETNEXT                                                          
         B     SETFLT                                                           
*                                                                               
GETNEXT  MVI   PUBSW,1             DON'T PROCESS THIS PUB                       
         B     PUBFEXT                                                          
*                                                                               
         DROP  R3                                                               
SETFLT   DS    0H                                                               
PUBFEXT  XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
REPCSCT  CSECT                                                                  
         DS   ((48*8*8)+4)C FOR REPTOTS  48 X 8 X 8 (+4)   3076C                
CLTCSCT  CSECT                                                                  
         DS   ((48*8*8)+4)C  FOR CLTTOTS  48 X 8 X 8 (+4)                       
PROCSCT  CSECT                                                                  
         DS    F                                                                
         DS    ((48*8*8)+4)C  FOR PROTOTS  48 X 8 X 8 (+4)                      
REPCSCTF CSECT                                                                  
         DS   ((48*8*8)+4)C FOR REPTOTS  48 X 8 X 8 (+4)   FGN EXCH             
CLTCSCTF CSECT                                                                  
         DS   ((48*8*8)+4)C  FOR CLTTOTS  48 X 8 X 8 (+4)  FGN EXCH             
PROCSCTF CSECT                                                                  
         DS    F                                                                
         DS    ((48*8*8)+4)C  FOR PROTOTS  48 X 8 X 8 (+4) FGN EXCH             
*                                                                               
PAYWORK  CSECT                                                                  
*NOP*    DS    16500C                                                           
         DS    CL(PAYWRKLN)    PAYWRKLN IS EQUATED LENGTH                       
*                              OF PAYWORKD DSECT                                
REPDSCT  DSECT                                                                  
REPTOTS  DS    0D                                                               
REPINS   DS    48PL8                                                            
REPLINES DS    48PL8                                                            
REPGO    DS    48PL8                                                            
REPGLAC  DS    48PL8                                                            
REPCD    DS    48PL8                                                            
REPGST   DS    48PL8                                               L01          
REPNP    DS    48PL8                                                            
REPPUBS  DS    48PL8                                                            
RTOTPUBS DS    F                                                                
*                                                                               
*                                                                               
CLTDSCT  DSECT                                                                  
CLTTOTS  DS    0D                                                               
CLTINS   DS    48PL8                                                            
CLTLINES DS    48PL8                                                            
CLTGO    DS    48PL8                                                            
CLTGLAC  DS    48PL8                                                            
CLTCD    DS    48PL8                                                            
CLTGST   DS    48PL8                                               L01          
CLTNP    DS    48PL8                                                            
CLTPUBS  DS    48PL8                                                            
CTOTPUBS DS    F                                                                
*                                                                               
PRODSCT  DSECT                                                                  
PROTOTS  DS    0D                                                               
PROINS   DS    48PL8                                                            
PROLINES DS    48PL8                                                            
PROGO    DS    48PL8                                                            
PROGLAC  DS    48PL8                                                            
PROCD    DS    48PL8                                                            
PROGST   DS    48PL8                                               L01          
PRONP    DS    48PL8                                                            
PROPUBS  DS    48PL8                                                            
PTOTPUBS DS    F                                                                
*                                                                               
*                                                                               
REPDSCTF DSECT                                                                  
REPTOTSF DS    0D                  FGN EXCH                                     
REPINSF  DS    48PL8                                                            
REPLINEF DS    48PL8                                                            
REPGOF   DS    48PL8                                                            
REPGLACF DS    48PL8                                                            
REPCDF   DS    48PL8                                                            
REPGSTF  DS    48PL8                                                            
REPNPF   DS    48PL8                                                            
REPPUBSF DS    48PL8                                                            
RTOTPUBF DS    F                                                                
*                                                                               
CLTDSCTF DSECT                                                                  
CLTTOTSF DS    0D                  FGN EXCH                                     
CLTINSF  DS    48PL8                                                            
CLTLINEF DS    48PL8                                                            
CLTGOF   DS    48PL8                                                            
CLTGLACF DS    48PL8                                                            
CLTCDF   DS    48PL8                                                            
CLTGSTF  DS    48PL8                                                            
CLTNPF   DS    48PL8                                                            
CLTPUBSF DS    48PL8                                                            
CTOTPUBF DS    F                                                                
*                                                                               
PRODSCTF DSECT                                                                  
PROTOTSF DS    0D                  FGN EXCH                                     
PROINSF  DS    48PL8                                                            
PROLINEF DS    48PL8                                                            
PROGOF   DS    48PL8                                                            
PROGLACF DS    48PL8                                                            
PROCDF   DS    48PL8                                                            
PROGSTF  DS    48PL8                                                            
PRONPF   DS    48PL8                                                            
PROPUBSF DS    48PL8                                                            
PTOTPUBF DS    F                                                                
*                                                                               
PAYWORKD DSECT                                                                  
*                                                                               
AADCGWRK DS    F                                                                
APRDTAB  DS    F                                                                
STRHIYR  DS    XL1      USED IN BLDMUP.. AND BLDMDN.. PROCS                     
SVPTOFC  DS    CL24     SAVED OFFICE FROM OFFOUT CALL                           
MTHACT   DS    CL1                                                              
PRDACT   DS    CL1                                                              
PUBACT   DS    CL1                                                              
REPACT   DS    CL1                                                              
CLTACT   DS    CL1                                                              
SAVELINE DS    CL1                                                              
PAIDSW   DS    CL1      X'01' IF DATED PAY ELEM FOUND                           
PUBSW    DS    CL1                                                              
PUBPSW   DS    CL1      X'01' IF PUB NAME PRINTED                               
PRDSW    DS    CL1      X'01' IF DOING PRDS SEPERATELY                          
LASTYM   DS    CL2                                                              
SAVEYMD  DS    CL3                                                              
SAVEPRD  DS    CL3                                                              
SAVEPUB  DS    CL6                                                              
PPGKEY   DS    CL32                                                             
SAVEKEY  DS    CL32                                                             
SAVEPKEY DS    CL32                                                             
STKEY    DS    CL32                                                             
REQERR   DS    CL1                                                              
REQST    DS    CL3                                                              
REQEND   DS    CL3                                                              
ASOFDTE  DS    CL3                    AS OF DATE YMD                            
ASDATE   DS    CL8           AS  OF DATE MMDD/YY                                
*                                                                               
CDPDATE  DS    XL3    FIRST DAY OF 3RD MONTH BEFORE QSTART                      
*                     CONTROLS THE IGNORING OF PUB CD EFFECTIVE DATE            
*                                                                               
REQEST   DS    H                                                                
WKDUB    DS    PL8                                                              
REQPUB   DS    CL6                                                              
SVMEDCLI DS    CL4                 SAVED MEDIA/CLIENT                           
*                                                                               
SAVPLIN1 DS    CL132                                                            
SAVPLIN2 DS    CL132                                                            
*                                                                               
VOFFICER DS    A                   A(OFFICER)                                   
VPRNTOFC DS    A                   A(PRNTOFC)                                   
*                                                                               
PRDMTHS  DS    F                                                                
PUBPRDS  DS    F                                                                
PUBMTHS  DS    F                                                                
*                                                                               
PRDMTHSF DS    F                   FX (FOREIGN EXCHANGE) OF ABOVE               
PUBPRDSF DS    F                                                                
PUBMTHSF DS    F                                                                
*                                                                               
VCLIFRST DS    V                                                                
VPUBFRST DS    V                                                                
VMTHEND  DS    V                                                                
VPRDEND  DS    V                                                                
VPUBEND  DS    V                                                                
VCLTEND  DS    V                                                                
VPRINTIT DS    V                                                                
VBLDMLST DS    V                                                                
VPAYWORK DS    V                                                                
VREPEND  DS    V                                                                
VPUBFLOT DS    V                                                                
VPRNTPUB DS    V                                                                
VREPCSCT DS    V                                                                
VCLTCSCT DS    V                                                                
VPROCSCT DS    V                                                                
VREPCSCF DS    V                                                                
VCLTCSCF DS    V                                                                
VPROCSCF DS    V                                                                
VPPGETCG DS    V                   NEW 03/26/01                                 
         DS    V                                                                
TOTALS   DS    6D                  WAS 5D                          L01          
         DS    0F                                                               
BUYOUTA  DS    600C                OUTPUT AREA FOR PPBUYOUT                     
         SPACE                                                                  
RELO     DS    F                                                                
*                                                                               
*                                                                               
BUYTOTS  DS    0D             BUY LINE TOTALS                                   
BUYGO    DS    F                                                                
BUYGLAC  DS    F                                                                
BUYCD    DS    F                                                                
BUYGST   DS    F                                                   L01          
BUYNP    DS    F                                                                
*                                                                               
BUYTOTSF DS    0D             FX BUY LINE TOTALS     (FGN EXCH)                 
BUYGOF   DS    F                                                                
BUYGLACF DS    F                                                                
BUYCDF   DS    F                                                                
BUYGSTF  DS    F                                                                
BUYNPF   DS    F                                                                
*                                                                               
MTHTOTS  DS    0D             MONTH TOTALS                                      
MTHINS   DS    PL8                                                              
MTHLINES DS    PL8                                                              
MTHGO    DS    PL8                                                              
MTHGLAC  DS    PL8                                                              
MTHCD    DS    PL8                                                              
MTHGST   DS    PL8                                                 L01          
MTHNP    DS    PL8                                                              
*                                                                               
MTHTOTSF DS    0D             FX MONTH TOTALS        (FGN EXCH)                 
MTHINSF  DS    PL8                                                              
MTHLINEF DS    PL8                                                              
MTHGOF   DS    PL8                                                              
MTHGLACF DS    PL8                                                              
MTHCDF   DS    PL8                                                              
MTHGSTF  DS    PL8                                                              
MTHNPF   DS    PL8                                                              
*                                                                               
PRDTOTS  DS    0D                                                               
PRDINS   DS    PL8                                                              
PRDLINES DS    PL8                                                              
PRDGO    DS    PL8                                                              
PRDGLAC  DS    PL8                                                              
PRDCD    DS    PL8                                                              
PRDGST   DS    PL8                                                 L01          
PRDNP    DS    PL8                                                              
*                                                                               
PRDTOTSF DS    0D             FX PRODUCT TOTALS      (FGN EXCH)                 
PRDINSF  DS    PL8                                                              
PRDLINEF DS    PL8                                                              
PRDGOF   DS    PL8                                                              
PRDGLACF DS    PL8                                                              
PRDCDF   DS    PL8                                                              
PRDGSTF  DS    PL8                                                              
PRDNPF   DS    PL8                                                              
*                                                                               
PUBTOTS  DS    0D                                                               
PUBINS   DS    48PL8                                                            
PUBLINES DS    48PL8                                                            
PUBGO    DS    48PL8                                                            
PUBGLAC  DS    48PL8                                                            
PUBCHCD  DS    48PL8                                                            
PUBGSTX  DS    48PL8                                               L01          
PUBNP    DS    48PL8                                                            
*                                                                               
PUBTOTSF DS    0D             FX MONTHLY PUB TOTALS  (FGN EXCH)                 
PUBINSF  DS    48PL8                                                            
PUBLINEF DS    48PL8                                                            
PUBGOF   DS    48PL8                                                            
PUBGLACF DS    48PL8                                                            
PUBCHCDF DS    48PL8                                                            
PUBGSTXF DS    48PL8                                                            
PUBNPF   DS    48PL8                                                            
*                                                                               
FWORK    DS    20F                                                              
*                                                                               
TSTPAID  DS    20F            GETINS VALUES INCLUDING FX "CHARGES"              
*                                                                               
ACCNUM   EQU   48                  NUMBER OF MTH ACCUMS                         
*                            3 YEARS + 6 MTHS BACK + 6 MTHS FORWARD +1          
MTHTAB   DS    CL((ACCNUM*8)+8)   48 X 8 +8                      L01            
*MTHTAB   DS    CL392         48 X 8 +8                                         
*                                                                               
ADCGWRK  DS    CL(ADCDLNTH)   FOR ADDITIONAL CHARGES PRINT BLOCK                
*                             OUTPUT BY PPGETCG                                 
PRDTAB   DS    CL34500             TABLE OF PRD CDS AND NAMES                   
*                                  23 X 500 PRDS = CL11500                      
*                                  23 X 600 PRDS = CL13800 (04/30/01)           
*                                 23 X 1000 PRDS = CL23000 (01/15/03)           
*                                 23 X 1500 PRDS = CL34500 (05/04/06)           
*                                                                               
PAYWRKLN EQU   *-PAYWORKD                                                       
*                                                                               
         PRINT OFF                                                 L01          
       ++INCLUDE PPMODEQU                                                       
       ++INCLUDE PPREPWORK                                                      
       ++INCLUDE PPNEWFILE                                                      
*                                                                               
GVALUESD DSECT                                                                  
       ++INCLUDE GVALUES                                                        
       ++INCLUDE PPGETCGD     ADDCHGD DSECT FOR ADCGWRK                         
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'045PPREP2702 07/09/14'                                      
         END                                                                    
