*          DATA SET PPBUY06    AT LEVEL 090 AS OF 10/14/20                      
*PHASE T41106A                                                                  
*INCLUDE PPBVAL                                                                 
*INCLUDE PPFMTINO                                                               
*INCLUDE PPGETCU                                                                
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         TITLE 'PPBUY06 - PRINTPAK BUY DISPLAY - PART II'                       
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* KWAN MAR20/20 RECALL PAYMENT FIX FOR MULTIPLE INVOICES (SPEC-36222)           
*                                                                               
* KWAN OCT15/15 DISPLAY AUTOPAY FLAG                                            
*                                                                               
* KWAN SEP19/14 DISPLAY PRISMA/RADIA ORIGIN                                     
*                                                                               
* KWAN APR15/14 DISPLAY PLANNED COST FOR MEDIA S (SEARCH)                       
*                                                                               
* KWAN MAR05/14 DISPLAY SCRIPT PAY UPLOAD FLAG                                  
*                                                                               
* KWAN JAN15/14 DISPLAY ENLARGED DAILY EFFECTIVE CIRCULATION (DLC=)             
*                                                                               
* KWAN APR16/13 DISPLAY PURCHASE ORDER ACCORDING TO LEVEL                       
*                                                                               
* KWAN OCT08/12 DISPLAY <P16> AS USER NAME IF PID IS NOT PRESENT                
*                                                                               
* KWAN 10/18/10 DISPLAY COS2 $                                                  
*                                                                               
* KWAN  01/10   ALLOW FOR RETENTION AND DISPLAY OF "S" COST INDICATOR           
*                 IN "FREE" BUYS                                                
*                                                                               
* BOBY 02/00/08 UPDATE RX RECALL TO FOREIGN EXCHANGE                            
*                                                                               
* BOBY 01/00/07 UPDATE RX RECALL TO HANDLE NEW CLRST RECORDS                    
*                                                                               
* KWAN 10/12/06 RECALL PURCHASE ORDER #                                         
*                                                                               
* KWAN 12/14/05 FIX ACTIVITY RECALL PROBLEM                                     
*                                                                               
* SMYE 08/24/05 CHANGES FOR AD-ID IN DSPADID                                    
*                                                                               
* SMYE  07/05   DISPLAY AD-ID IF USED                                           
*                                                                               
* SMYE 09/01/04 DISPLAY WEB INSERTION ORDERS                                    
*                                                                               
* KWAN 03/25/03 SREP DISPLAY BUG (DID NOT BUMP OVER PROPERLY)                   
*                                                                               
* YKAP 01/23/03 DISPLAY NAME (PID)                                              
*                                                                               
* YKAP 10/01/02 DISPLAY ISSUE NAME                                              
*                                                                               
* KWAN 06/04/02 LOOK UP CONTRACT UNIT IF NO OVERRIDE FOUND IN BUYREC            
*                                                                               
* KWAN 05/09/02 DISPLAY INVOICE NUMBER (PROFILE SAVED IN PPBUY01)               
*                                                                               
* KWAN 03/21/02 DISPLAY INTERNET CONTRACT NUMBER AND SITE LOCATION              
*                                                                               
* KWAN 12/10/01 DISPLAY ACTUAL AND ESTIMATED CPMS                               
*                                                                               
* KWAN 08/13/01 DISPLAY "NO TRAFFIC" FOR "RI" (PBDSTAT BIT IS X'20')            
*                                                                               
* KWAN 07/02/01 FOR RECALL INSERTION (RI) DISPLAY LW COMMENT CODES              
*                                                                               
* KWAN 06/21/01 DISPLAY LEGAL WARNINGS                                          
*                                                                               
* KWAN 04/04/01 DISPLAY SEQ NUMBER FOR "RX" TRANSACTION                         
*                                                                               
* KWAN 03/01    DISPLAY ADDITIONAL CHARGES CHANGE ELEM                          
*                                                                               
* KWAN 01/01    DISPLAY NEW EXTENSION DATE ELEM (EXDATE)                        
*                                                                               
* KWAN 05/00    CHANGE IMPS TO EIMPS (DISPLAY FOR ESTIMATED IMPS)               
*                                                                               
* KWAN 05/00    DISPLAY AIMPS (ACTUAL IMPRESSION)                               
*                                                                               
* KWAN 02/00    ADD PRD EXCLUSION CHECKING FOR WARNING MSG                      
*                                                                               
* KWAN 01/99    MODIFY WARNING MSG FOR EXCLUSION CLASS (EXCLWRN)                
*                                                                               
* KWAN 12/99    ADD WARNING FOR EXCLUSION CLASS                                 
*                                                                               
* BPLA 11/99    NEW NO CHG SINCE... MESSAGE FOR NEW P72APROF                    
*               RATE CHANGE OPTION                                              
*                                                                               
* BPLA 10/99    CHECK P72A PROFILE TO SEE IF RATE CHAGE SHOULD                  
*               CAUSE I/O MESSAGE (WITH "RI" RECALL)                            
*                                                                               
* KWAN 06/99    DISPLAY CLOSE AND ON SALE DATES FOR MEDIA N                     
*                                                                               
* BPLA 05/99    FIX PAYMENT DISPLAY                                             
*                                                                               
* KWAN 04/99    DISPLAY OF IMPRESSION, CORRECT CV AND PV DISPLAY                
*                                                                               
* BPLA 02/99    DISPLAY OF COST2 FACTOR                                         
*                                                                               
* KWAN 12/23/98 DISPLAY EXTENSION DAYS (EXDAYS=NNN)                             
*                                                                               
* BPLA 06/98    CHECK P72APROF FOR AUTI I/O SUPPRESSION                         
*                                                                               
* BPLA 11/97    DISPLAY SFH STATUS AND PCHGIND4                                 
*                                                                               
* BPLA 04/97    ADD DISPLAY OF GST CODE                                         
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
T41106   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORK06X-WORK06D,T41106,RR=RE,CLEAR=YES                           
*                                                                               
         LR    R7,RC                                                            
         USING WORK06D,R7          R7 = A(GLOBAL STORAGE)                       
*                                                                               
         BASR  R9,0                                                             
         AHI   R9,GLOBALS-*                                                     
         USING GLOBALS,R9          R9 = A(GLOBAL LITERALS)                      
*                                                                               
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         L     RA,4(R1)                                                         
         USING T411FFD,RA                                                       
*                                                                               
         ST    RE,RELOBY06                                                      
         LR    RE,R7                                                            
         A     RE,=A(WKAIO1-WORK06D)                                            
         ST    RE,AWKAIO1                                                       
         MVI   SPACES,C' '                                                      
         MVC   SPACES+1(L'SPACES-1),SPACES                                      
*                                                                               
         CLI   REC+33,0            HAVE BUY ELEM?                               
         BE    EXXMOD                                                           
*                                                                               
         BRAS  RE,BUMPFLD2         POINT TO FIRST DISPLAY LINE                  
         BRAS  RE,DSPW             DISPLAY WARNING MSGS                         
*                                                                               
         CLC   =C'RY',TRCODE       TEST INVOICE RECALL                          
         BE    DSPL10                                                           
         CLC   =C'RA',TRCODE       TEST ACTIVITY RECALL                         
         BE    DSPL20                                                           
         CLC   =C'RX',TRCODE       TEST PAY/BILL RECALL                         
         BE    DSPL30                                                           
         CLC   =C'RZ',TRCODE       TEST AUTO-BL/PY DATE RECALL                  
         BE    DSPL30                                                           
         CLC   =C'R ',TRCODE       TEST NORMAL RECALL                           
         BE    DSPL30                                                           
         CLC   =C'RO',TRCODE       TEST OPEN BILLING RECALL                     
         BE    DSPL30                                                           
*                                                                               
         MVI   ELCODE,X'6A'                                                     
         CLC   =C'RS',TRCODE       SRC COMMENTS RECALL                          
         BE    DSPL50              SAME CODE AS 'RI' RECALL                     
         MVI   ELCODE,X'68'                                                     
         CLC   =C'RP',TRCODE       POSITION INSTRUCTIONS RECALL                 
         BE    DSPL50              SAME CODE AS 'RI' RECALL                     
         MVI   ELCODE,X'67'                                                     
         CLC   =C'RI',TRCODE       INSERTION ORDER RECALL                       
         BE    DSPL50                                                           
         B     EXXMOD                                                           
*                                                                               
DSPL10   BRAS  RE,DISINV           DISPLAY INVOICE NUMBER                       
         B     EXXMOD                                                           
*                                                                               
DSPL20   BRAS  RE,DISACT           DISPLAY ACTIVITY DATA                        
         B     EXXMOD                                                           
*                                                                               
DSPL30   BRAS  RE,DISBDATA         DISPLAY BUY DATA                             
         B     EXXMOD                                                           
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* INSERTION ORDER RECALL                                                        
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
DSPL50   ST    R2,PARS                                                          
         L     R2,TRADDR           CLEAR ALL COMMENT LINES                      
DSPL50A  BRAS  RE,BUMPFLD                                                       
         TM    1(R2),X'20'                                                      
         BZ    DSPL50A                                                          
*                                                                               
* FIRST FIELD AFTER A PROTECTED FIELD                                           
*                                                                               
         BRAS  RE,BUMPFLD                                                       
         LR    R6,R2               SAVE ADDR OF 1ST COMMENT FLD FOR BCT         
         LA    R4,5                                                             
DSPL50B  XC    8(COMLEN-8,R2),8(R2)                                             
         FOUT  (R2)                                                             
         LA    R2,COMLEN(R2)                                                    
         BCT   R4,DSPL50B                                                       
*                                                                               
* SO REGULAR COMMENTS WON'T BE DELETED IF THIS RI IS FOLLOWED BY A CHG          
*                                                                               
         XC    PRVTAB,PRVTAB                                                    
*                                                                               
         L     R2,PARS                                                          
*                                                                               
* DISPLAY IOC                                                                   
*                                                                               
         LA    R8,5                FOR BCT                                      
         LR    R2,R6               RESET R2 TO FIRST COMMENT FIELD              
         LA    R5,REC+33                                                        
*                                  ELCODE SET EARLIER                           
DSPL50C  BRAS  RE,NXTELEM                                                       
         BNE   DSPL51                                                           
         SR    R4,R4                                                            
         IC    R4,1(R5)                                                         
         AHI   R4,-3                                                            
*                                                                               
         CLI   ELCODE,X'6A'        SRC COMMENT                                  
         BE    DSPL50D                                                          
*                                                                               
         MVC   8(3,R2),=C'IC='                                                  
         CLI   ELCODE,X'67'                                                     
         BE    *+10                                                             
         MVC   8(3,R2),=C'PI='     POSITION INSTRUCTIONS                        
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   11(0,R2),2(R5)                                                   
         B     DSPL50E                                                          
*                                                                               
DSPL50D  DS    0H                                                               
         MVC   8(4,R2),=C'SRC='                                                 
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   12(0,R2),2(R5)                                                   
         B     DSPL50E                                                          
*                                                                               
DSPL50E  DS    0H                                                               
         FOUT  (R2)                                                             
         LA    R2,COMLEN(R2)                                                    
         BCT   R8,DSPL50C                                                       
*                                                                               
DSPL51   L     R2,PARS             RESTORE R2 TO DISPLAY LINE                   
         XC    PARS+4(4),PARS+4                                                 
*                                                                               
         BRAS  RE,NOTRAFF          CHECK FOR NO TRAFFIC STATUS                  
         BNE   DSPL90              NO MORE ROOM ON SCREEN                       
         ST    R2,PARS             A LINE IS USED FOR "NO TRAFFIC"              
*                                                                               
         MVI   ELCODE,X'70'                                                     
         LA    R5,REC+33                                                        
DSPL52   BRAS  RE,NXTELEM                                                       
         BNE   DSPL59              DONE                                         
         USING PIOELD,R5                                                        
         OC    PIODATE,PIODATE                                                  
         BZ    DSPL52                                                           
         ST    R5,PARS+4           SAVE A(LAST IO ELEM)                         
         C     R2,PARS             FIRST TIME                                   
         BNE   DSPL54                                                           
         MVC   8(L'IORHD,R2),IORHD                                              
*                                                                               
DSPL54   LA    R8,L'IORHD+9(R2)                                                 
         GOTO1 VDATCON,DMCB,(3,PIODATE),(5,(R8))                                
*                                                                               
         LA    R8,9(R8)                                                         
         MVC   0(1,R8),PBUYKMED                                                 
         GOTO1 VDATCON,DMCB,(3,PIODATE),(0,1(R8))                               
*                                                                               
         MVI   1(R8),C'-'                                                       
         MVI   7(R8),C'-'                                                       
         MVC   HALF,PIONUM                                                      
         LH    R0,HALF                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  8(4,R8),DUB                                                      
*                                                                               
         LA    R8,13(R8)                                                        
         MVC   0(3,R8),=C'CAN'                                                  
         CLI   PIOTYP,C'D'                                                      
         BE    DSPL55                                                           
         MVC   0(3,R8),=C'CHA'                                                  
         CLI   PIOTYP,C'C'                                                      
         BE    DSPL55                                                           
         MVC   0(3,R8),=C'NEW'     PIOTYP 'N'                                   
*                                                                               
DSPL55   LA    R8,4(R8)                                                         
         MVC   0(2,R8),=C'RQ'                                                   
         CLI   PIOTURN,C'R'                                                     
         BE    DSPL56                                                           
         MVC   0(2,R8),=C'TA'                                                   
         CLI   PIOTURN,C'T'                                                     
         BE    DSPL56                                                           
         MVC   0(2,R8),=C'MN'      MANUAL IO                                    
         CLI   PIOTURN,C'M'                                                     
         BE    DSPL56                                                           
         MVC   0(2,R8),=C'OL'                                                   
*                                                                               
DSPL56   LA    R8,3(R8)                                                         
         CLI   PIOFAX,C'F'         SEE IF FAXED                                 
         BNE   DSPL57                                                           
         MVC   0(3,R8),=C'FAX'                                                  
         LA    R8,4(R8)                                                         
*                                                                               
DSPL57   OC    PIOCDATE,PIOCDATE   CK FOR CONTROL DATE                          
         BZ    DSPL58                                                           
         MVC   0(4,R8),=C'CDT='                                                 
         GOTO1 VDATCON,DMCB,(3,PIOCDATE),(5,4(R8))                              
         LA    R8,10(R8)                                                        
*                                                                               
DSPL58   CLI   PIOLWCD,0           ANYTHING IN LEGAL WARNING CODE?              
         BE    DSPL58B             NO, CHECK QUARTERLY CODE                     
         MVC   0(3,R8),=C'LW='                                                  
         MVC   3(1,R8),PIOLWCD                                                  
         CLI   PIOQUCD,0           ANYTHING IN QUARTERLY CODE?                  
         BE    DSPL58X             NO - DONE                                    
         MVC   4(1,R8),PIOQUCD                                                  
         B     DSPL58X                                                          
*                                                                               
DSPL58B  CLI   PIOQUCD,0           QUARTERLY CODE ONLY?                         
         BE    DSPL58X             BOTH CODES ARE NOT FOUND, DONE               
         MVC   0(3,R8),=C'LW='                                                  
         MVC   3(1,R8),PIOQUCD                                                  
*                                                                               
DSPL58X  FOUT  (R2)                END OF RI DISPLAY                            
*                                                                               
         BRAS  RE,BUMPFLD                                                       
         BNE   DSPL52                                                           
*                                                                               
DSPL59   DS    0H                                                               
         BRAS  RE,WEBIODIS         CHECK FOR AND DISPLAY WEB INS ORDERS         
         BNE   DSPL90                                                           
*                                                                               
DSPL60   C     R2,PARS             TEST ANY IO'S                                
         BE    DSPL62                                                           
         BRAS  RE,BUMPFLD                                                       
         BZ    DSPL90                                                           
*                                                                               
DSPL62   MVI   BYTE,1                                                           
         CLC   SVAGPROF+22(2),=C'00'                                            
         BE    DSPL63K                                                          
         CLI   SVAGPROF+22,C'N'                                                 
         BNE   DSPL63                                                           
         MVC   8(L'SYSMSG,R2),SYSMSG                                            
         FOUT  (R2)                                                             
         BRAS  RE,BUMPFLD                                                       
         BNE   DSPL63K                                                          
         B     DSPL90                                                           
*                                                                               
DSPL63   MVI   BYTE,0                                                           
         CLI   PBDIODAT,X'FF'                                                   
         BNE   DSPL63A                                                          
         MVI   BYTE,1                                                           
         MVC   8(L'IODMSG,R2),IODMSG                                            
         FOUT  (R2)                                                             
         BRAS  RE,BUMPFLD                                                       
         BZ    DSPL90                                                           
*                                                                               
DSPL63A  OC    PBDJOB,PBDJOB                                                    
         BNZ   DSPL63B                                                          
         MVI   BYTE,1                                                           
         MVC   8(L'NOJMSG,R2),NOJMSG                                            
         FOUT  (R2)                                                             
         BRAS  RE,BUMPFLD                                                       
         BZ    DSPL90                                                           
*                                                                               
DSPL63B  CLI   PBDIODAT,0                                                       
         BNE   DSPL63K                                                          
         OC    PARS+4(4),PARS+4    TEST PREVIOS IO                              
         BNZ   DSPL63E             YES                                          
*                                                                               
* NO PREV IO                                                                    
*                                                                               
         TM    PBUYCNTL,X'80'                                                   
         BZ    DSPL63I                                                          
*                                                                               
DSPL63D2 MVI   BYTE,1                                                           
         B     DSPL63K                                                          
*                                                                               
* HAVE PREV IO                                                                  
*                                                                               
DSPL63E  L     R5,PARS+4                                                        
         TM    PBUYCNTL,X'80'      TEST DELETED                                 
         BZ    DSPL63E2                                                         
         CLI   PIOTYP,C'D'         TEST LAST IO A CANCEL                        
         BE    DSPL63D2            YES - NO IO                                  
         MVI   BYTE,3                                                           
         B     DSPL63I                                                          
*                                                                               
DSPL63E2 CLC   PBUYKDAT,PIOIDATE   FIND ANY CHANGE                              
         BNE   DSPL63H                                                          
         CLC   PBDJOB,PIOJOB                                                    
         BNE   DSPL63H                                                          
         CLI   PBUYKMED,C'N'                                                    
         BE    DSPL63F                                                          
         CLC   PBDSPACE,PIOSPACE                                                
         BNE   DSPL63H                                                          
         B     DSPL63G             NO CHANGE                                    
*                                                                               
DSPL63F  CP    PBDUNITS,PIOUNITS                                                
         BNE   DSPL63H                                                          
         CP    PBDCLMS,PIOCLMS                                                  
         BNE   DSPL63H                                                          
         CLC   PBDUIND,PIOUIND                                                  
         BNE   DSPL63H                                                          
         CLC   PBDCL,PIOPRM                                                     
         BNE   DSPL63H             NO CHANGE                                    
*                                                                               
DSPL63G  MVC   DUB(3),PIODATE      SAVE DATE OF LAST IO                         
*                                                                               
         GOTO1 VDATCON,DMCB,(3,PIODATE),(2,DUB)                                 
         LA    R5,REC+33                                                        
         MVI   ELCODE,X'24'                                                     
DSPL63G5 BRAS  RE,NXTELEM                                                       
         BNE   DSPL63G8                                                         
         USING PCHGELEM,R5                                                      
         CLC   PCHGDAT,DUB                                                      
         BL    DSPL63G5                                                         
         CLI   P72APROF+14,C'Y'    RATE CHANGE TRIGGER                          
         BNE   DSPL63G6                                                         
         TM    PCHGIND1,X'40'      RATE CHANGE                                  
         BO    DSPL63H                                                          
DSPL63G6 TM    PCHGIND3,X'01'      PI= CHANGE                                   
         BZ    DSPL63G5            KEEP LOOKING                                 
         B     DSPL63H             CHANGE                                       
         DROP  R5                                                               
*                                                                               
DSPL63G8 MVI   BYTE,1                                                           
         CLI   P72APROF+14,C'Y'    CK RATE CHG TRIGGER OPTION                   
         BE    DSPL63G9                                                         
         MVC   8(L'NOCHMSG,R2),NOCHMSG                                          
         B     DSPL63GX                                                         
*                                                                               
DSPL63G9 MVC   8(L'NOCHMSGR,R2),NOCHMSGR                                        
DSPL63GX FOUT  (R2)                                                             
         BRAS  RE,BUMPFLD                                                       
         BZ    DSPL90                                                           
         B     DSPL63K                                                          
*                                                                               
DSPL63H  MVI   BYTE,2              THERE HAS BEEN A CHANGE                      
*                                                                               
DSPL63I  CLC   PBUYKDAT,BTODAY     TEST INS DATE PAST                           
         BNL   DSPL63K                                                          
         TM    PBUYCNTL,X'80'      OK FOR DELETES                               
         BNZ   DSPL63K                                                          
         MVI   BYTE,1                                                           
         MVC   8(L'INSDATM,R2),INSDATM                                          
         FOUT  (R2)                                                             
         BRAS  RE,BUMPFLD                                                       
         BZ    DSPL90                                                           
         B     DSPL63K                                                          
*                                                                               
DSPL63K  CLI   BYTE,1                                                           
         BNE   DSPL64                                                           
*                                                                               
DSPL63Z  MVC   8(L'NOIOMSG,R2),NOIOMSG                                          
         FOUT  (R2)                                                             
         B     DSPL64P                                                          
*                                                                               
DSPL64   XC    FULL,FULL                                                        
         OC    FULL(3),PBDIODAT                                                 
         BNZ   DSPL64M                                                          
         CLI   BYTE,2                                                           
         BL    DSPL64B                                                          
         MVC   FULL(3),BTODAY                                                   
         B     DSPL64M                                                          
*                                                                               
DSPL64B  CLI   SVAGPROF+22,C'C'    FIND IO PRINT DATE                           
         BE    DSPL63Z                                                          
         OC    SVAGPROF+22(2),=C'00'                                            
         PACK  DUB,SVAGPROF+22(2)                                               
         CVB   R0,DUB                                                           
         LCR   R0,R0                                                            
         LA    R4,PBUYKDAT                                                      
         CLI   PBUYKMED,C'N'                                                    
         BE    DSPL64D                                                          
         OC    PBDCDATE,PBDCDATE                                                
         BZ    DSPL64F                                                          
         LA    R4,PBDCDATE                                                      
*                                                                               
DSPL64D  GOTO1 VDATCON,DMCB,(3,(R4)),(0,WORK)                                   
         GOTO1 VADDAY,DMCB,WORK,WORK,(R0)                                       
         GOTO1 VDATCON,DMCB,(0,WORK),(3,FULL)                                   
         B     DSPL64M                                                          
*                                                                               
DSPL64F  CLC   SVAGPROF+24(2),=C'00'                                            
         BL    DSPL64H                                                          
         OC    SVAGPROF+24(2),=C'00'                                            
         PACK  DUB,SVAGPROF+24(2)                                               
         CVB   R0,DUB                                                           
         LCR   R0,R0                                                            
         LA    R4,PBUYKDAT                                                      
         B     DSPL64D                                                          
*                                                                               
DSPL64H  MVI   BYTE,1              NO CLOSE DATE                                
         MVC   8(L'NOCLS,R2),NOCLS                                              
         FOUT  (R2)                                                             
         B     DSPL63K                                                          
*                                                                               
DSPL64M  MVC   WORK(8),=CL8'TODAY'                                              
         CLC   FULL(3),BTODAY      BEFORE OR EQUAL TO TODAY?                    
         BNH   DSPL64N                                                          
*                                                                               
DSPL64M2 GOTO1 VDATCON,DMCB,(3,FULL),(5,WORK)                                   
*                                                                               
DSPL64N  CLI   P72APROF+3,C'Y'     SEE IF SUPPRESSING AUTO I/O'S                
         BE    DSPL63Z             IF YES - SEND NO I/O MESSAGE                 
         CLI   P72APROF+3,C'C'     SEE NOT SUPPRESSING CHG/CAN                  
         BNE   DSPL64N5                                                         
         CLI   BYTE,2              CHANGE                                       
         BE    DSPL64N5            SEND I/O MESSAGE                             
         CLI   BYTE,3              OR CANCEL                                    
         BE    DSPL64N5            SEND I/O MESSAGE                             
         B     DSPL63Z             SEND NO I/O MESSAGE                          
*                                                                               
DSPL64N5 MVC   8(L'NORMSG,R2),NORMSG                                            
         MVC   9+L'NORMSG(8,R2),WORK                                            
         MVC   18+L'NORMSG(8,R2),=C'(CHANGE)'                                   
         CLI   BYTE,2                                                           
         BE    DSPL64P                                                          
         MVC   18+L'NORMSG(9,R2),=CL9'(DELETED)'                                
         CLI   BYTE,3                                                           
         BE    DSPL64P                                                          
         MVC   18+L'NORMSG(9,R2),=CL9'(AUTO)'                                   
         OC    PBDIODAT,PBDIODAT                                                
         BZ    *+10                                                             
         MVC   18+L'NORMSG(9,R2),=CL9'(ID=)'                                    
*                                                                               
DSPL64P  FOUT  (R2)                                                             
*                                                                               
         BRAS  RE,BUMPFLD          SHIPPING LIST INFO                           
         BZ    DSPL90                                                           
         ST    R2,PARS                                                          
         MVI   ELCODE,X'79'                                                     
         LA    R5,REC+33                                                        
*                                                                               
DSPL65   BRAS  RE,NXTELEM                                                       
         BNE   DSPL90                                                           
         OC    2(3,R5),2(R5)                                                    
         BZ    DSPL65                                                           
         C     R2,PARS                                                          
         BNE   *+10                                                             
         MVC   8(L'SHPHD,R2),SHPHD                                              
         LA    R8,L'SHPHD+9(R2)                                                 
         GOTO1 VDATCON,DMCB,(3,2(R5)),(5,(R8))                                  
*                                                                               
         FOUT  (R2)                                                             
         BRAS  RE,BUMPFLD                                                       
         BZ    DSPL90                                                           
         B     DSPL65                                                           
*                                                                               
DSPWED   EDITR (P8,DUB),(5,0(R5)),ALIGN=LEFT                                    
         BR    RE                                                               
*                                                                               
DSPL90   B     EXXMOD                                                           
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKDSPLIN LR    RF,RE                                                            
         CR    R4,R1               ENOUGH ROOM TO DISPLAY DATA?                 
         BNHR  RE                                                               
         BRAS  RE,BUMPFLD          NO, BUMP TO NEXT LINE                        
         LA    R4,8(R2)                                                         
         OI    6(R2),X'80'         DISPLAY LINE                                 
         LR    RE,RF                                                            
         BR    RE                                                               
*                                                                               
BUMPFLD2 SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
BUMPFLD  SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BR    RE                                                               
*                                                                               
NXTELEM  ZIC   R0,1(R5)                                                         
         AR    R5,R0                                                            
         CLC   ELCODE,0(R5)                                                     
         JE    NXTELX              CC IS EQUAL                                  
         CLI   0(R5),0                                                          
         JNE   NXTELEM                                                          
         LTR   R5,R5               CC IS NOT EQUAL                              
NXTELX   BR    RE                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
EXIT     XIT1                                                                   
X_R2     XIT1  REGS=(R2)                                                        
X_R2R4   XIT1  REGS=(R2,R4)                                                     
X_R2R3   XIT1  REGS=(R2,R3)                                                     
*                                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
PRT_READ LR    R0,RE                                                            
         MVC   COMMAND,=C'DMREAD'                                               
         J     PRT_DDIR                                                         
*                                                                               
PRT_WRIT LR    R0,RE                                                            
         MVC   COMMAND,=C'DMWRT '                                               
         J     PRT_DDIR                                                         
*                                                                               
PRT_RSEQ LR    R0,RE                                                            
         MVC   COMMAND,=C'DMRSEQ'                                               
         J     PRT_DDIR                                                         
*                                                                               
PRT_ADD_ LR    R0,RE                                                            
         MVC   COMMAND,=C'DMADD '                                               
         J     PRT_DDIR                                                         
*                                                                               
PRT_RDHI LR    R0,RE                                                            
         MVC   COMMAND,=C'DMRDHI'                                               
         MVC   KEYSAVE,KEY                                                      
*                                                                               
PRT_DDIR LR    R0,RE                                                            
         GOTO1 VDATAMGR,DMCB,(DMINBTS,COMMAND),=C'PRTDIR  ',KEY,KEY,   +        
               (TERMNAL,0)                                                      
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
PRT_GETR LR    R0,RE                                                            
         MVC   COMMAND,=C'GETREC'                                               
         J     PRT_DFIL                                                         
*                                                                               
PRT_ADDR LR    R0,RE                                                            
         MVC   COMMAND,=C'ADDREC'                                               
         J     PRT_DFIL                                                         
*                                                                               
PRT_PUTR LR    R0,RE                                                            
         MVC   COMMAND,=C'PUTREC'                                               
*                                                                               
PRT_DFIL LA    RF,KEY+27                                                        
         CLI   COMMAND,C'A'                                                     
         JNE   *+8                                                              
         LA    RF,KEY                                                           
         GOTO1 VDATAMGR,DMCB,(DMINBTS,COMMAND),=C'PRTFILE ',           +        
               (RF),AREC,(TERMNAL,DMWORK)                                       
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
PUB_READ LR    R0,RE                                                            
         MVC   COMMAND,=C'DMREAD'                                               
         J     PUB_DDIR                                                         
*                                                                               
PUB_RDHI LR    R0,RE                                                            
         MVC   COMMAND,=C'DMRDHI'                                               
         MVC   KEYSAVE,KEY                                                      
*                                                                               
PUB_DDIR LR    R0,RE                                                            
         GOTO1 VDATAMGR,DMCB,(DMINBTS,COMMAND),=C'PUBDIR  ',KEY,KEY,   +        
               (TERMNAL,0)                                                      
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
PUB_GETR LR    R0,RE                                                            
         MVC   COMMAND,=C'GETREC'                                               
         LA    RF,KEY+27                                                        
         CLI   COMMAND,C'A'                                                     
         JNE   *+8                                                              
         LA    RF,KEY                                                           
         GOTO1 VDATAMGR,DMCB,(DMINBTS,COMMAND),=C'PUBFILE ',           +        
               (RF),APUBIO,(TERMNAL,DMWORK)                                     
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
GET_ETXT LR    R0,RE               R3 HAS ERROR NUMBER                          
         XC    BUYMSG,BUYMSG                                                    
         L     RF,ACOMFACS                                                      
         L     RF,(CGETTXT-COMFACSD)(RF)                                        
         GOTO1 (RF),DMCB+12,(R3),0,(C'E',DMCB),0,0,0                            
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
ERROR    L     R4,ERRAREA                                                       
         MVI   ERRAREA,X'FF'                                                    
         BRAS  RE,GET_ETXT                                                      
         OI    6(R2),OI1C          INSERT CURSOR                                
         L     R4,ERRAREA                                                       
         FOUT  (R4)                                                             
*                                                                               
EXXMOD   XMOD1 1                                                                
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
DISBDATA NTR1  BASE=*,LABEL=*      DISPLAY BUY DATA                             
*                                                                               
         ST    R2,SAVR2                                                         
         LA    R2,BUYTR1H                                                       
*                                                                               
         TM    1(R2),X'20'         PROTECTED?                                   
         BNZ   *+12                                                             
         BRAS  RE,BUMPFLD                                                       
         B     *-12                                                             
*                                                                               
         FOUT  (R2),=C'OPTIONAL DATA',13                                        
         CLI   PBDELEM+1,X'69'                                                  
         BL    DSPL30C                                                          
         CLI   PBDIDAT2,0                                                       
         BE    DSPL30C                                                          
*                                                                               
         ZIC   RF,0(R2)                                                         
         AHI   RF,-9                                                            
         EX    RF,*+8                                                           
         B     *+10                                                             
         XC    8(0,R2),8(R2)                                                    
         FOUT  (R2)                                                             
*                                                                               
         GOTO1 VDATCON,DMCB,(3,PBDIDAT2),(4,WORK)                               
         CLI   PBDFREQ,C'M'                                                     
         BNE   *+10                                                             
         MVC   WORK+3(2),=2C' '                                                 
         MVC   WORK+5(1),PBDEMIND                                               
         MVC   10(6,R2),WORK                                                    
         B     DSPL30D                                                          
*                                                                               
DSPL30C  LA    R5,REC+33                                                        
         MVI   ELCODE,X'A6'                                                     
         BRAS  RE,NXTELEM          ISSUE NAME ELEM FOUND?                       
         BNE   DSPL30D                                                          
         ZIC   RF,0(R2)                                                         
         AHI   RF,-9                                                            
         EX    RF,*+8                                                           
         B     *+10                                                             
         XC    8(0,R2),8(R2)                                                    
         FOUT  (R2)                                                             
         MVC   10(11,R2),2(R5)                                                  
DSPL30D  L     R2,SAVR2                                                         
         FOUT  (R2)                                                             
*                                                                               
*        BUY VALUES WITHOUT FOREIGN EXCHANGE                                    
*                                                                               
         GOTO1 VGETINS,DMCB,REC,PVALUES,(C'Y',REC+7),(C'F',0)                   
         LA    R4,8(R2)                                                         
         MVC   0(15,R4),=CL15'GROSS ORDERED ='                                  
         LA    R4,15(R4)                                                        
         L     R5,GROSS                                                         
         BRAS  RE,EDITORD                                                       
         LA    R4,12(R4)                                                        
*                                                                               
         MVC   0(13,R4),=CL13'NET PAYABLE ='                                    
         LA    R4,13(R4)                                                        
         L     R5,PYABLE                                                        
         A     R5,CSHDSC                                                        
         BRAS  RE,EDITORD                                                       
         LA    R4,12(R4)                                                        
*                                                                               
         MVC   0(13,R4),=CL13'NET PYBL-CD ='                                    
         LA    R4,13(R4)                                                        
         L     R5,PYABLE                                                        
         BRAS  RE,EDITORD                                                       
*                                                                               
*        DISPLAY FOREIGN EXCHANGE IF PRESENT                                    
*                                                                               
         CLI   NATION,C'C'         SKIP IF NOT CANADIAN AGENCY                  
         BNE   DSPL30E                                                          
*                                                                               
*        SERACH FO FX ADDITIONAL CHARGE                                         
*                                                                               
         MVI   ELCODE,X'44'        ADDITIONAL CHARGE ELEMENT                    
         LA    R5,REC+33           1ST ELEMENT IN RECORD                        
         USING PACELEM,R5          ESTABLISH AS ADDITINAL CHG ELM               
*                                                                               
         BRAS  RE,NXTELEM          LOOK FOR ADDITIONAL CHARGE ELEMENT           
         BNE   DSPL30E                NONE FOUND                                
         CLC   =C'FX',PACCODE      SKIP IF NOT FX ELEM                          
         BNE   *-14                                                             
*                                                                               
         BRAS  RE,BUMPFLD          BUMP TO NEXT DISPLAY LINE                    
         FOUT  (R2)                FORCE RE-DISPLAY OF LINE                     
*                                                                               
         LA    R4,8(R2)                                                         
*                                                                               
*        DISPLAY FOREIGN EXCHANGE RATE                                          
*                                                                               
         MVI   ELCODE,X'CC'        CUSTCOL ELEMENT                              
         LA    R5,REC+33           1ST ELEMENT IN RECORD                        
         USING BYCCELD,R5          ESTABLISH AS CUSTOM COLUMN ELM               
*                                                                               
         BRAS  RE,NXTELEM          LOOK FOR STDCOL !FXRATE                      
         BNE   DSPL30D5               NONE FOUND                                
         CLC   =AL2(8207),BYCCSQN  SKIP IF NOT !FXRATE ELM                      
         BNE   *-14                                                             
*                                                                               
         MVC   0(3,R4),=CL3'FX='                                                
*                                                                               
         EDITR (P8,BYCCDATA),(11,3(R4)),5,FLOAT=-,ALIGN=LEFT,TRAIL=%            
*                                                                               
DSPL30D5 DS    0H                                                               
*                                                                               
         LA    R4,15(R4)                                                        
*                                                                               
*        DISPLAY FOREIGN EXCHANGE AMOUNTS                                       
*                                                                               
*              FIND AMOUNTS FOR FX ADDITIONAL CHARGE                            
*                                                                               
         GOTO1 VGETINS,DMCB,REC,PVALUES,(C'Y',REC+7),(C'A',0),0,=C'FX'          
*                                                                               
         L     R5,GROSS                                                         
         BRAS  RE,EDITORD                                                       
         LA    R4,12(R4)                                                        
*                                                                               
         LA    R4,13(R4)                                                        
         L     R5,PYABLE                                                        
         A     R5,CSHDSC                                                        
         BRAS  RE,EDITORD                                                       
         LA    R4,12(R4)                                                        
*                                                                               
         LA    R4,13(R4)                                                        
         L     R5,PYABLE                                                        
         BRAS  RE,EDITORD                                                       
*                                                                               
DSPL30E  DS    0H                                                               
*                                                                               
         BRAS  RE,BUMPFLD                                                       
         FOUT  (R2)                                                             
         LA    R4,8(R2)                                                         
         MVC   0(8,R4),=C'PAYABLE-'                                             
         GOTO1 VDATCON,DMCB,(3,PBDPDATE),(5,8(R4))                              
*                                                                               
         LA    R4,18(R4)                                                        
         MVC   0(9,R4),=C'BILLABLE-'                                            
         GOTO1 (RF),(R1),(3,BLBLDT),(5,9(R4))                                   
         LA    R4,19(R4)                                                        
*                                                                               
         MVI   BYTE,0              DISPLAY RATE CODE, TAC, DLC                  
         MVI   BYTE2,0             SAME AS ABOVE FLAG                           
*                                                                               
         XC    X+80(20),X+80       CLEAR PST OUTPUT BLOCK                       
         CLI   NATION,C'C'         CANADIAN?                                    
         BNE   DSPL310X                                                         
         CLI   PBDGST,C' '                                                      
         BNH   *+16                                                             
         MVC   X+80(4),=C'GST='                                                 
         MVC   X+84(1),PBDGST                                                   
         LA    R5,REC+33                                                        
         MVI   ELCODE,X'84'                                                     
         BRAS  RE,NXTELEM          FOUND PST ELEM?                              
         BNE   DSPL310X                                                         
*                                                                               
         LA    RE,X                                                             
         USING PSTBLKD,RE                                                       
         XC    X(PSTLNQ),X         CLEAR INTERFACE BLOCK                        
         MVI   PSTACT,PSTFMTQ      ACTION = FORMAT                              
         LA    R1,2(R5)                                                         
         ST    R1,PSTADIN          INPUT ADDRESS                                
         LA    R1,X+80             WILL HAVE DISPLAYABLE PSTS                   
         CLC   0(4,R1),=C'GST='                                                 
         BNE   *+8                                                              
         LA    R1,6(R1)                                                         
         ST    R1,PSTADOUT         OUTPUT ADDRESS                               
         MVC   PSTACOM,ACOMFACS    A(COMFACS)                                   
         DROP  RE                                                               
*                                                                               
         XC    DMCB(12),DMCB                                                    
         MVC   DMCB+4(4),=X'D9000A6B'                                           
         GOTO1 VCALLOV,DMCB                                                     
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,X         GO VALIDATE PST                              
*                                                                               
DSPL310X CLI   BUYMD,C'N'          NEWSPAPER?                                   
         BNE   DSPL31F                                                          
         CLC   PBDSPACE(2),C_7B00  X'7B00'(SPECIAL NO ASC BUY)?                 
         BE    DSPL31F             TREAT IT AS NON-SPACE BUY                    
         CLC   PBDSPACE(2),C_7B40  X'7B40'(SPECIAL NO ASC BUY)?                 
         BE    DSPL31F             TREAT IT AS NON-SPACE BUY                    
         CLC   PBDSPACE(2),C_5C40  X'5C40'(SPACE BUY)?                          
         BNH   DSPL31F             TREAT IT AS NON-SPACE BUY                    
*                                                                               
         CLI   PBDUIND,X'89'       SEE IF LOWER CASE I                          
         BNE   DSPL31A                                                          
         MVC   0(4,R4),=C'CLE='    YES - DISPLAY COL INCHES                     
         LA    R5,X                                                             
         XC    X(10),X             2 DECIMALS                                   
         EDIT  PBDUNITS,(6,X),2,ALIGN=LEFT                                      
         AR    R5,R0                                                            
         MVI   0(R5),C'I'                                                       
         MVC   4(7,R4),X                                                        
         B     DSPL31G             FOR THESE BUYS SREP WILL BE ON               
*                                                                               
DSPL31A  MVC   0(4,R4),=C'CLE='                                                 
         XC    X(10),X                                                          
         LA    R5,X                                                             
         ZAP   DUB,PBDUNITS                                                     
         BRAS  RE,DSPWED                                                        
         AR    R5,R0                                                            
         CLI   PBDUIND,C'I'                                                     
         BNE   *+12                                                             
         MVI   0(R5),C'I'                                                       
         LA    R5,1(R5)                                                         
         CP    PBDCLMS,=P'0'                                                    
         BE    DSPL31D                                                          
         MVI   0(R5),C'/'                                                       
         LA    R5,1(R5)                                                         
         ZAP   DUB,PBDCLMS                                                      
         BRAS  RE,DSPWED                                                        
*                                                                               
DSPL31D  MVC   4(8,R4),X                                                        
         B     DSPL31G             FOR THESE BUYS SREP IS ON NEXT LINE          
*                                                                               
DSPL31F  LA    R5,REC+33           SREP FOR NON-NEWS OR NON-SPACE NEWS          
         MVI   ELCODE,X'80'                                                     
         BRAS  RE,NXTELEM          SPECIAL REP ELEM?                            
         BNE   DSPL31F5                                                         
         MVC   0(5,R4),=C'SREP='                                                
         MVC   5(4,R4),2(R5)       REP CODE FROM ELEM                           
         B     DSPL31G                                                          
*                                                                               
DSPL31F5 OC    PBDRCODE,PBDRCODE   RATE CODE?                                   
         BZ    DSPL31F8                                                         
         MVC   0(2,R4),=C'R='                                                   
         MVC   2(3,R4),PBDRCODE                                                 
         OI    BYTE,1              SET RATE CODE DISPLAYED                      
         B     DSPL31G                                                          
*                                                                               
DSPL31F8 LA    R5,REC+33                                                        
         MVI   ELCODE,X'81'                                                     
         BRAS  RE,NXTELEM          DLC ELEM?                                    
         BNE   DSPL31F9                                                         
         MVC   0(4,R4),=C'DLC='                                                 
         EDIT  (P6,2(R5)),(9,4(R4)),0,ALIGN=LEFT                                
         OI    BYTE,X'04'          SET DLC DISPLAYED                            
         B     DSPL31G                                                          
*                                                                               
DSPL31F9 LA    R5,REC+33                                                        
         MVI   ELCODE,X'82'                                                     
         BRAS  RE,NXTELEM          FSI ELEM?                                    
         BNE   DSPL31FA                                                         
         MVC   0(4,R4),=C'FSI='                                                 
         USING PBFSIELD,R5                                                      
         EDIT  PBFSI,(8,4(R4)),0,ALIGN=LEFT,ZERO=NOBLANK                        
         OI    BYTE,X'08'          SET FSI DISPLAYED                            
         B     DSPL31G                                                          
         DROP  R5                                                               
*                                                                               
DSPL31FA CLI   X+80,0              PST CODES PRESENT?                           
         BE    DSPL31FC                                                         
         MVC   0(10,R4),X+80       ROOM FOR GST + 1 PROVINCE                    
         XC    X+80(20),X+80       CLEAR PSTS                                   
         B     DSPL31G                                                          
*                                                                               
DSPL31FC LA    R5,REC+33                                                        
         MVI   ELCODE,X'85'                                                     
         BRAS  RE,NXTELEM          RPT ELEM?                                    
         BNE   DSPL31G                                                          
         MVC   0(4,R4),=C'RPT='                                                 
         EDIT  (P3,2(R5)),(3,4(R4)),0,ALIGN=LEFT,ZERO=NOBLANK                   
         OI    BYTE,X'10'          SET RPT DISPLAYED                            
*                                                                               
DSPL31G  LA    R4,14(R4)                                                        
         MVC   0(3,R4),=C'CD='                                                  
         MVC   3(5,R4),=X'4021204B20'                                           
         ED    3(5,R4),PBDCD                                                    
         MVI   8(R4),C'%'                                                       
*                                                                               
         LA    R4,12(R4)                                                        
         MVC   0(3,R4),=C'AC='                                                  
         OC    PBDACP,PBDACP                                                    
         BNZ   *+10                                                             
         ZAP   PBDACP,=P'15000'                                                 
         ZAP   DUB,PBDACP                                                       
         BNM   *+10                                                             
         ZAP   DUB,=P'100000'      NOTE: -1 IS 100 PCT                          
         EDIT  (P8,DUB),(7,3(R4)),3                                             
         CLC   8(2,R4),=C'00'                                                   
         BNE   *+10                                                             
         MVC   8(2,R4),=C'  '                                                   
         LA    R4,10(R4)                                                        
*                                                                               
         CLI   BUYMD,C'N'          NEWSPAPER?                                   
         BNE   DSPL31H                                                          
         CLC   PBDSPACE(2),C_7B00  X'7B00'(SPECIAL NO ASC BUY)?                 
         BE    DSPL31H             TREAT IT AS NON-SPACE BUY                    
         CLC   PBDSPACE(2),C_7B40  X'7B40'(SPECIAL NO ASC BUY)?                 
         BE    DSPL31H             TREAT IT AS NON-SPACE BUY                    
         CLC   PBDSPACE(2),C_5C40  X'5C40'(SPACE BUY)?                          
         BNH   DSPL31H             TREAT IT AS NON-SPACE BUY                    
*                                                                               
* FOR ABOVE BUYS SREP WAS SHOWN INSTEAD OF CLE ON LAST LINE                     
*                                                                               
         LA    R5,REC+33                                                        
         MVI   ELCODE,X'80'                                                     
         BRAS  RE,NXTELEM          SPECIAL REP ELEM?                            
         BNE   DSPL31H                                                          
         BRAS  RE,BUMPFLD                                                       
         LA    R4,8(R2)                                                         
         FOUT  (R2)                                                             
         MVC   0(5,R4),=C'SREP='                                                
         MVC   5(4,R4),2(R5)       REP CODE FROM ELEM                           
         LA    R4,10(R4)           BUMP OVER SREP DISPLAY                       
         B     DSPL31H1                                                         
*                                                                               
DSPL31H  CLI   PBDELEM+1,X'69'     OLD INSERTION?                               
         BH    *+6                                                              
         DC    H'0'                DSPL31H0 - TO STOP DUMP                      
*                                                                               
         CLI   PBUYKMED,C'S'       SEARCH?                                      
         JE    DSPL31HK                                                         
         CLI   PBUYKMED,C'I'       INTERACTIVE?                                 
         JE    DSPL31HK                                                         
         CLI   PBUYKMED,C'L'       SOCIAL?                                      
         JE    DSPL31HK                                                         
         CLI   PBUYKMED,C'B'       MOBILE?                                      
         JE    DSPL31HK                                                         
         CLI   PBUYKMED,C'D'       DIGITAL AUDIO?                               
         JE    DSPL31HK                                                         
         CLI   PBUYKMED,C'V'       NATIONAL VIDEO (NVIDEO)?                     
         JE    DSPL31HK                                                         
         CLI   PBUYKMED,C'W'       LOCAL VIDEO (LVIDEO)?                        
         JNE   *+20                                                             
DSPL31HK LA    R5,REC+33                                                        
         MVI   ELCODE,BYPCIDQ                                                   
         BRAS  RE,NXTELEM          PLANNED COST ELEM FOUND?                     
         JE    DSPL31H0                                                         
*                                                                               
         CLI   PBDPLCOS,X'FF'      PLANNED COST?                                
         BE    *+14                                                             
         OC    PBDPLCOS,PBDPLCOS   PLANNED COST?                                
         BNZ   DSPL31H0                                                         
*                                                                               
         OC    PBDTAX,PBDTAX       TAX?                                         
         BNZ   DSPL31H0                                                         
         OC    PBDCU,PBDCU         CONTRACT UNITS?                              
         BNZ   DSPL31H0                                                         
         LA    R5,REC+33                                                        
         MVI   ELCODE,X'30'                                                     
         BRAS  RE,NXTELEM          OPEN RATE ELEM?                              
         BE    DSPL31H0                                                         
         LA    R5,REC+33                                                        
         MVI   ELCODE,X'81'                                                     
         BRAS  RE,NXTELEM          DLC ELEM?                                    
         BNE   DP31H0                                                           
         TM    BYTE,X'04'          DLC DISPLAYED?                               
         BNO   DSPL31H0                                                         
*                                                                               
DP31H0   LA    R5,REC+33                                                        
         MVI   ELCODE,X'82'                                                     
         BRAS  RE,NXTELEM          FSI ELEM?                                    
         BNE   DP31H0C                                                          
         TM    BYTE,X'08'          FSI DISPLAYED?                               
         BNO   DSPL31H0                                                         
*                                                                               
DP31H0C  OC    PBDRCODE,PBDRCODE   RATE CODE?                                   
         BZ    DP31H0D                                                          
         TM    BYTE,1              RATE CODE DISPLAYED?                         
         BNO   DSPL31H0                                                         
*                                                                               
DP31H0D  CLI   X+80,0              PST CODES?                                   
         BE    DSPL31H6            NO - THEN DONE                               
*                                                                               
DSPL31H0 BRAS  RE,BUMPFLD                                                       
         LA    R4,8(R2)                                                         
         FOUT (R2)                                                              
*                                                                               
DSPL31H1 BRAS  RE,DSP_PLCO         DISPLAY PLANNED COST                         
*                                                                               
         OC    PBDRCODE,PBDRCODE   ANY RATE CODE?                               
         BZ    DSPL31H5                                                         
         TM    BYTE,X'01'          ALREADY DISPLAYED?                           
         BO    DSPL31H5                                                         
         MVC   0(2,R4),=C'R='                                                   
         MVC   2(3,R4),PBDRCODE                                                 
         LA    R4,6(R4)                                                         
*                                                                               
DSPL31H5 CLI   PBDELEM+1,X'69'     VERY OLD BUY DESCRIPTION ELEM?               
         BL    DSPL31H6                                                         
         OC    PBDTAX,PBDTAX                                                    
         BZ    DSPL31H6                                                         
         CLI   PBDTAX+2,X'01'                                                   
         BE    DSPL31H6                                                         
         OI    BYTE,X'02'          TAX IS DISPLAYED                             
         MVC   0(4,R4),=C'TAX='                                                 
         EDIT  (B3,PBDTAX),(8,4(R4)),4,ALIGN=LEFT,TRAIL=C'%'                    
         LA    R4,5(R4)                                                         
         AR    R4,R0               ADD DISPLAY LENGTH                           
         MVC   0(5,R4),=CL5'TAX$='                                              
         LA    R4,5(R4)                                                         
         L     R5,TAX                                                           
         EDIT  (R5),(11,00(R4)),2,COMMAS=YES,FLOAT=-,ALIGN=LEFT                 
*****    BRAS  RE,EDITORD                                                       
         LA    R4,1(R4)                                                         
         AR    R4,R0               ADD DISPLAY LENGTH                           
*                                                                               
DSPL31H6 TM    BYTE,X'04'          DLC DISPLAYED?                               
         BO    DP31H7F                                                          
         LA    R5,REC+33                                                        
         MVI   ELCODE,X'81'        DLC ELEM?                                    
         BRAS  RE,NXTELEM                                                       
         BNE   DP31H7F                                                          
         LA    R1,72(R2)                                                        
         CR    R4,R1               ENOUGH DISPLAY ROOM?                         
         BNH   DP31H7C                                                          
         BRAS  RE,BUMPFLD          SKIP TO NEXT LINE                            
         LA    R4,8(R2)                                                         
         FOUT  (R2)                                                             
DP31H7C  MVC   0(4,R4),=C'DLC='                                                 
         EDIT  (P6,2(R5)),(7,4(R4)),0,ALIGN=LEFT                                
         OI    BYTE,X'04'          SET DLC DISPLAYED                            
         LA    R4,5(R4)                                                         
         AR    R4,R0               ADD DISPLAY LENGTH                           
*                                                                               
DP31H7F  TM    BYTE,X'08'          FSI DISPLAYED?                               
         BO    DSPL31H8                                                         
         LA    R5,REC+33                                                        
         MVI   ELCODE,X'82'        FSI ELEM?                                    
         BRAS  RE,NXTELEM                                                       
         BNE   DSPL31H8                                                         
         LA    R1,72(R2)                                                        
         CR    R4,R1               ENOUGH DISPLAY ROOM?                         
         BNH   DP31H7H                                                          
         BRAS  RE,BUMPFLD          SKIP TO NEXT LINE                            
         LA    R4,8(R2)                                                         
         FOUT  (R2)                                                             
         USING PBFSIELD,R5                                                      
DP31H7H  MVC   0(4,R4),=C'FSI='                                                 
         EDIT  PBFSI,(8,4(R4)),0,ALIGN=LEFT,ZERO=NOBLANK                        
         OI    BYTE,X'08'          SET FSI DISPLAYED                            
         LA    R4,5(R4)                                                         
         AR    R4,R0               ADD DISPLAY LENGTH                           
         DROP  R5                                                               
*                                                                               
DSPL31H8 LA    R5,REC+33                                                        
         MVI   ELCODE,X'30'                                                     
         BRAS  RE,NXTELEM          OPEN RATE ELEM?                              
         BNE   DSPL31HF                                                         
         LA    R1,72(R2)                                                        
         CR    R4,R1               ENOUGH DISPLAY ROOM?                         
         BNH   DSPL31H9                                                         
         BRAS  RE,BUMPFLD          SKIP TO NEXT LINE                            
         LA    R4,8(R2)                                                         
         FOUT  (R2)                                                             
DSPL31H9 MVC   0(3,R4),=C'OR='                                                  
*                                                                               
         USING PORELEM,R5                                                       
         TM    PORCOSS1,PORCOS$Q   COS2 $ NON-FINANCIAL?                        
         JZ    DSPL31_4                                                         
         MVC   0(4,R4),=C'C2$='                                                 
         CLI   PORC$TYP,PORC$NEQ   COS2 $ ENTERED AS NET?                       
         JNE   DSPL31_3                                                         
         ZAP   DUB,PORCOS                                                       
         CVB   R1,DUB                                                           
         ZAP   DUB,REC+33+(PBDACP-PBDELEM)(L'PBDACP)                            
         CVB   RF,DUB                                                           
         S     RF,=F'100000'                                                    
         LCR   RF,RF               = NET PCT                                    
         MR    R0,RF                                                            
         L     RF,=F'100000'                                                    
         SLDA  R0,1                                                             
         DR    R0,RF                                                            
         LTR   R1,R1                                                            
         BNP   *+8                                                              
         AHI   R1,1                                                             
         SRA   R1,1                                                             
         MVC   4(L'PORC$TYP,R4),PORC$TYP                                        
         EDITR (R1),(9,4+L'PORC$TYP(R4)),2,FLOAT=-,ALIGN=LEFT                   
         AHI   R4,L'PORC$TYP                                                    
DSPL31_2 LA    R4,4+1(R4)          FOR C2$= PLUS ONE                            
         J     DSPL31HC                                                         
*                                                                               
DSPL31_3 EDIT  PORCOS,(9,4(R4)),2,FLOAT=-,ALIGN=LEFT                            
         J     DSPL31_2                                                         
*                                                                               
DSPL31_4 CLI   2(R5),C'U'          UNIT COST GIVEN?                             
         BE    DSPL31HA            YES -  5 DECIMALS  ELSE 2 DECIMALS           
DSPL31_6 EDIT  (P5,3(R5)),(9,3(R4)),2,FLOAT=-,ALIGN=LEFT                        
         B     DSPL31HB                                                         
DSPL31HA EDIT  (P5,3(R5)),(11,3(R4)),5,FLOAT=-,ALIGN=LEFT                       
         LA    R1,3(R4)                                                         
         AR    R1,R0               LENGTH OF DISPLAY                            
         AHI   R1,-3                                                            
         CLC   =C'000',0(R1)                                                    
         BNE   DSPL31HB                                                         
         MVC   0(3,R1),=C'   '                                                  
         AHI   R0,-3               ADJUST DISPLAY LENGTH                        
*                                                                               
DSPL31HB LA    R4,4(R4)            FOR OR= PLUS ONE                             
DSPL31HC AR    R4,R0               ADD LENGTH OF DISPLAY                        
         LA    R1,72(R2)                                                        
         CR    R4,R1               ENOUGH DISPLAY ROOM?                         
         BNH   DSPL31HD                                                         
         BRAS  RE,BUMPFLD          SKIP TO NEXT LINE                            
         LA    R4,8(R2)                                                         
         FOUT  (R2)                                                             
*                                                                               
DSPL31HD TM    PORCOSS1,PORCOS$Q   COS2 $ NON-FINANCIAL?                        
         JNZ   DSPL31HF                                                         
         MVC   0(3,R4),=C'OC='     DISPLAY OPEN COST                            
         LA    R4,3(R4)                                                         
         GOTO1 VGETINS,DMCB,REC,(C'O',PVALUES),(C'Y',REC+7)                     
         L     R5,GROSS                                                         
         BRAS  RE,EDITORD                                                       
         LA    R4,1(R4)                                                         
         AR    R4,R0                                                            
         DROP  R5                                                               
*                                                                               
DSPL31HF BRAS  RE,DSPCODT          DISPLAY CLOSE AND ON SALE DATE               
*                                                                               
DSP31H8  LA    R5,REC+33                                                        
         MVI   ELCODE,X'91'        COST 2 FAACTOR ELEM?                         
         BRAS  RE,NXTELEM                                                       
         BNE   DSP31HX                                                          
         LA    R1,67(R2)                                                        
         CR    R4,R1               ENOUGH DISPLAY ROOM?                         
         BNH   DSP31H9                                                          
         BRAS  RE,BUMPFLD          SKIP TO NEXT LINE                            
         LA    R4,8(R2)                                                         
         FOUT  (R2)                                                             
DSP31H9  MVC   0(5,R4),=C'COS2='                                                
DSP31HA  EDIT  (P5,2(R5)),(8,5(R4)),6,FLOAT=-,ALIGN=LEFT                        
         LA    R1,5(R4)                                                         
         AR    R1,R0               LENGTH OF DISPLAY                            
         AHI   R1,-3                                                            
         CLC   =C'000',0(R1)                                                    
         BNE   DSP31HB                                                          
         MVC   0(3,R1),=C'   '                                                  
         AHI   R0,-3               ADJUST DISPLAY LENGTH                        
DSP31HB  LA    R4,6(R4)            FOR COS2= PLUS ONE                           
         AR    R4,R0               ADD LENGTH OF DISPLAY                        
*                                                                               
DSP31HX  OC    PBDCU,PBDCU         SEE IF I HAVE CONTRACT UNITS                 
         BNZ   DSP31J              OVERRIDE PRESENT, GO DISPLAY IT              
         BRAS  RE,DSPCU            NOT IN BUYREC, LOOK UP CU                    
         JNE   ERROR               MSG AND CURSOR ARE SET                       
         J     DSP31JX             DONE DISPLAYING CONTRACT UNIT                
*                                                                               
DSP31J   LA    R1,68(R2)                                                        
         CR    R4,R1               ENOUGH DISPLAY ROOM?                         
         BNH   DSP31JD                                                          
         BRAS  RE,BUMPFLD          SKIP TO NEXT LINE                            
         LA    R4,8(R2)                                                         
         FOUT  (R2)                                                             
*                                                                               
DSP31JD  MVC   0(3,R4),=C'CU='                                                  
         CLC   PBDCU,=X'000001'    ZERO?                                        
         BNE   DSP31JG                                                          
         MVI   3(R4),C'0'                                                       
         LA    R4,5(R4)                                                         
         B     DSP31JX                                                          
*                                                                               
DSP31JG  XC    FULL,FULL                                                        
         MVC   FULL+1(3),PBDCU                                                  
         EDIT  (B4,FULL),(9,3(R4)),4,ALIGN=LEFT,DROP=3                          
         AR    R4,R0               ADD DISPLAY LENGTH                           
         LA    R4,4(R4)            FOR CU= AND 1 MORE                           
*                                                                               
DSP31JX  LA    R5,REC+33                                                        
         MVI   ELCODE,X'83'                                                     
         BRAS  RE,NXTELEM          REFERENCE NUMBER ELEM?                       
         BNE   DP31HI7                                                          
         LA    R1,70(R2)                                                        
         CR    R4,R1               ENOUGH DISPLAY ROOM?                         
         BNH   DP31HI5                                                          
         BRAS  RE,BUMPFLD          SKIP TO NEXT LINE                            
         LA    R4,8(R2)                                                         
         FOUT  (R2)                                                             
         USING PBREFELD,R5                                                      
DP31HI5  MVC   0(4,R4),=C'REF='                                                 
         MVC   4(10,R4),PBREFNO                                                 
         LA    R4,5(R4)                                                         
         AHI   R4,10               ADD DISPLAY LENGTH                           
         DROP  R5                                                               
*                                                                               
DP31HI7  LA    R5,REC+33                                                        
         MVI   ELCODE,X'86'                                                     
         BRAS  RE,NXTELEM          SHIP DATE ELEM?                              
         BNE   DSPL31HJ                                                         
         LA    R1,70(R2)           ENOUGH DISPLAY ROOM?                         
         CR    R4,R1                                                            
         BNH   DP31HI8                                                          
         BRAS  RE,BUMPFLD          SKIP TO NEXT LINE                            
         LA    R4,8(R2)                                                         
         FOUT  (R2)                                                             
DP31HI8  MVC   0(3,R4),=C'SD='                                                  
         GOTO1 VDATCON,DMCB,(3,2(R5)),(5,3(R4))                                 
         LA    R4,4(R4)                                                         
         AHI   R4,8                ADD DISPLAY LENGTH                           
*                                                                               
DSPL31HJ TM    BYTE,X'10'          RPT HAS BEEN DISPLAYED?                      
         BO    DP31HJ6                                                          
         LA    R5,REC+33                                                        
         MVI   ELCODE,X'85'                                                     
         BRAS  RE,NXTELEM          RPT ELEM?                                    
         BNE   DP31HJ6                                                          
         LA    R1,72(R2)                                                        
         CR    R4,R1               ENOUGH DISPLAY ROOM?                         
         BNH   DP31HJ5                                                          
         BRAS  RE,BUMPFLD          SKIP TO NEXT LINE                            
         LA    R4,8(R2)                                                         
         FOUT  (R2)                                                             
DP31HJ5  MVC   0(4,R4),=C'RPT='                                                 
         EDIT  (P3,2(R5)),(3,4(R4)),0,ALIGN=LEFT,ZERO=NOBLANK                   
         LA    R4,5(R4)                                                         
         AR    R4,R0               ADD DISPLAY LENGTH                           
*                                                                               
DP31HJ6  TM    BYTE,X'80'          IF PV= HAS BEEN DISPLAYED?                   
         BO    DP31HJ8                                                          
         LA    R5,REC+33                                                        
         MVI   ELCODE,X'87'        PAGE VIEW ELEM?                              
         BRAS  RE,NXTELEM                                                       
         BNE   DP31HJ8                                                          
         LA    R1,66(R2)                                                        
         CR    R4,R1               ENOUGH DISPLAY ROOM?                         
         BNH   DP31HJ7                                                          
         BRAS  RE,BUMPFLD          SKIP TO NEXT LINE                            
         LA    R4,8(R2)                                                         
         FOUT  (R2)                                                             
DP31HJ7  MVC   0(3,R4),=C'PV='                                                  
         EDIT  (P5,2(R5)),(11,3(R4)),0,ALIGN=LEFT,ZERO=NOBLANK,        +        
               COMMAS=YES                                                       
         OI    BYTE,X'80'          PV DISPLAYED                                 
         LA    R4,4(R4)                                                         
         AR    R4,R0               ADD DISPLAY LENGTH                           
*                                                                               
DP31HJ8  TM    BYTE,X'40'          CT= HAS BEEN DISPLAYED?                      
         BO    DP31HJ10                                                         
         LA    R5,REC+33                                                        
         MVI   ELCODE,X'88'        CLICK THROUGH ELEM?                          
         BRAS  RE,NXTELEM                                                       
         BNE   DP31HJ10                                                         
         LA    R1,66(R2)                                                        
         CR    R4,R1               ENOUGH DISPLAY ROOM?                         
         BNH   DP31HJ9                                                          
         BRAS  RE,BUMPFLD          SKIP TO NEXT LINE                            
         LA    R4,8(R2)                                                         
         FOUT  (R2)                                                             
DP31HJ9  MVC   0(3,R4),=C'CT='                                                  
         EDIT  (P5,2(R5)),(11,3(R4)),0,ALIGN=LEFT,ZERO=NOBLANK,        X        
               COMMAS=YES                                                       
         OI    BYTE,X'40'          CT DISPLAYED                                 
         LA    R4,4(R4)                                                         
         AR    R4,R0               ADD DISPLAY LENGTH                           
*                                                                               
DP31HJ10 TM    BYTE,X'20'          EXDAYS=NNN DISPLAYED?                        
         BO    DP31HJ20                                                         
         LA    R5,REC+33                                                        
         MVI   ELCODE,X'89'                                                     
         BRAS  RE,NXTELEM          EXTENSION DAYS ELEM?                         
         BNE   DP31HJ20                                                         
         LA    R1,70(R2)                                                        
         CR    R4,R1               ENOUGH DISPLAY ROOM?                         
         BNH   DP31HJ11                                                         
         BRAS  RE,BUMPFLD          SKIP TO NEXT LINE                            
         LA    R4,8(R2)                                                         
         FOUT  (R2)                                                             
DP31HJ11 MVC   0(7,R4),=C'EXDAYS='                                              
         EDIT  (P2,2(R5)),(3,7(R4)),0,ALIGN=LEFT,ZERO=NOBLANK                   
         OI    BYTE,X'20'          EXTENSION DAYS DISPLAYED                     
         LA    R4,8(R4)                                                         
         AR    R4,R0               ADD DISPLAY LENGTH                           
*                                                                               
DP31HJ20 BRAS  RE,DSPEXDT          DISPLAY EXTENSION DATE                       
         BRAS  RE,DSPIMPS          DISPLAY IMPRESSIONS                          
         BRAS  RE,DSPLEGW          DISPLAY LEGAL WARNINGS                       
         BRAS  RE,DSPNTRA          DISPLAY NO TRAFFIC, ACPM, ECPM               
         OC    PBDJOB,PBDJOB       HAVE JOB CODE ?                              
         BZ    *+8                 NO                                           
         BRAS  RE,DSPADID          DISPLAY AD ID CODE IF IN JOB RECORD          
         BRAS  RE,DSP_PO#          DISPLAY PURCHASE ORDER #                     
*                                                                               
         CLI   X+80,0              PST CODES DISPLAYED?                         
         BE    DP31HK7                                                          
         LA    R1,69(R2)                                                        
         CR    R4,R1               ENOUGH DISPLAY ROOM?                         
         BNH   DP31HK5                                                          
         BRAS  RE,BUMPFLD          SKIP TO NEXT LINE                            
         LA    R4,8(R2)                                                         
         FOUT  (R2)                                                             
DP31HK5  MVC   0(10,R4),X+80                                                    
         LA    R4,10(R4)                                                        
*                                                                               
DP31HK7  TM    PBDSTAT,X'0C'       SFH?                                         
         BZ    DSPL31HL                                                         
         LA    R1,70(R2)                                                        
         CR    R4,R1               ENOUGH DISPLAY ROOM?                         
         BNH   DP31HK8                                                          
         BRAS  RE,BUMPFLD          SKIP TO NEXT LINE                            
         LA    R4,8(R2)                                                         
         FOUT  (R2)                                                             
DP31HK8  MVC   0(7,R4),=C'SFH=REL'                                              
         TM    PBDSTAT,X'08'       HOLD BIT IS ON?                              
         BNO   DSPL31HL                                                         
         MVC   4(4,R4),=C'HOLD'                                                 
*                                                                               
DSPL31HL CLC   =C'RZ',TRCODE                                                    
         BE    DSPL31HM                                                         
         CLC   =C'R ',TRCODE       NORMAL RECALL?                               
         BE    DSPL31HM                                                         
*                                                                               
         BRAS  RE,D_BILPAY         DISPLAY BILL/PAY DATA                        
         B     DSPL_X                                                           
*                                                                               
DSPL31HM XC    TRCODE,TRCODE                                                    
*                                                                               
DSPL_X   J     EXIT                                                             
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
D_BILPAY NTR1  BASE=*,LABEL=*      DISPLAY BILL/PAY DATA                        
*                                                                               
         BRAS  RE,BUMPFLD                                                       
         ST    R2,PARS             SAVE FIRST LINE ADDRESS                      
         MVC   HALF,LASTPBEL       DISPLAY PAID/BILLED STATUS                   
         LH    R5,HALF                                                          
         LTR   R5,R5               CONTINUED DISPLAY?                           
         BZ    DSPL31M                                                          
*                                                                               
         XC    HALF,HALF                                                        
         LA    R5,REC(R5)          R5 = A(LAST SHOWN ELEM)                      
         ZIC   R0,1(R5)                                                         
         LR    RF,R5                                                            
         AR    RF,R0                                                            
         CLI   0(RF),0             EOR?                                         
         BNE   DSPL31J                                                          
         CLI   0(R5),X'25'         LAST ELEM IS A PAY ELEM?                     
         BE    DSPL40                                                           
         B     D_BP_X                                                           
*                                                                               
DSPL31J  CLI   0(R5),X'25'                                                      
         BNE   DSPL41              BILLS CONTINUED                              
         B     DSPL31P                                                          
*                                                                               
DSPL31M  LA    R5,REC+33                                                        
         USING PPAYELEM,R5                                                      
DSPL31P  MVC   8(38,R2),PAYHD                                                   
         FOUT  (R2)                                                             
         BRAS  RE,BUMPFLD                                                       
         MVI   ELCODE,X'25'                                                     
DSPL32   LA    R8,8(R2)                                                         
         AH    R8,HALF                                                          
DSPL34   BRAS  RE,NXTELEM                                                       
         BNE   DSPL40                                                           
         OC    PPDDATE,PPDDATE                                                  
         BZ    DSPL34                                                           
         FOUT  (R2)                                                             
         GOTO1 VDATCON,DMCB,(3,PPDDATE),(5,0(R8))                               
*                                                                               
         CLI   PPAYELEM+1,22       ELEM CONTAIN SEQ NUMBER INFO?                
         BNH   DSPL34B                                                          
         CLI   PPDSEQNO,1          SEQ NUMBER GREATER THAN ONE?                 
         BNH   DSPL34B                                                          
         MVI   8(R8),C'-'                                                       
         EDIT  (B1,PPDSEQNO),(1,9(R8)),0,ALIGN=LEFT                             
*                                                                               
DSPL34B  EDIT  PPGROSS,(10,10(R8)),2,FLOAT=-                                    
*                                                                               
         ICM   R0,15,PPGROSS       LOAD INSTRUCTION GOT BOUNDARY PROB           
         ICM   RE,15,PPAGYCOM                                                   
         SR    R0,RE                                                            
         EDIT  (R0),(10,20(R8)),2,FLOAT=-                                       
         EDIT  PPCSHDSC,(08,30(R8)),2,FLOAT=-                                   
*                                                                               
         ST    R8,SVR8             A(DOLLAR DISPLAY)                            
*                                                                               
         BRAS  RE,BUMPFLD                                                       
         BNZ   DSPL34B1                                                         
         OC    HALF,HALF                                                        
         BNZ   DSPL47                                                           
         LA    R0,40               2ND HALF OF SCREEN                           
         STH   R0,HALF                                                          
         L     R2,PARS                                                          
DSPL34B1 DS    0H                                                               
         LA    R8,8(R2)                                                         
         AH    R8,HALF                                                          
*                                                                               
DSPL34C  DS    0H                                                               
*                                                                               
DSPL34E  BRAS  RE,GETCLST          STATUS INFO                                  
*                                                                               
         CLI   0(R2),0             DONE IF NO MORE DISPLAY AREA                 
         BNE   DSPL34              GO FIND NEXT PAY ELEM                        
*                                                                               
         B     DSPL47              DISPLAY DONE                                 
*                                                                               
DSPL40   LA    R5,REC+33                                                        
DSPL41   MVI   ELCODE,X'26'                                                     
         BRAS  RE,DSPBILL          DISPLAY BILLING                              
         B     D_BP_X                                                           
*                                                                               
DSPL47   LA    R0,REC              SAVE LAST SHOWN ELEM                         
         SR    R5,R0                                                            
         STH   R5,HALF                                                          
         MVC   LASTPBEL,HALF                                                    
*                                                                               
D_BP_X   J     EXIT                                                             
*                                                                               
EDITORD  C     R5,=F'100000000'    EXCEEDS 1 MILLION?                           
         JL    EDITOD5                                                          
         EDITR (R5),(11,00(R4)),2,FLOAT=-                                       
*        EDITR (R5),(11,00(R4)),2,FLOAT=-,ALIGN=LEFT                            
         BR    RE                                                               
EDITOD5  EDITR (R5),(11,00(R4)),2,COMMAS=YES,FLOAT=-                            
*DITOD5  EDITR (R5),(11,00(R4)),2,COMMAS=YES,FLOAT=-,ALIGN=LEFT                 
         BR    RE                                                               
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
DSPCU    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         OC    SVCON(L'SVCON-1),SVCON                                           
         BZ    DSPCUX              NO CONTRACT IS OPEN, NO CU LOOK UP           
*                                                                               
         L     RF,ACONIO                                                        
         XC    0(256,RF),0(RF)     INIT 256, WILL BE USED FOR "OC"              
         XC    KEY,KEY                                                          
         MVC   AREC,ACONIO         READ INTO CONIO                              
*                                                                               
         GOTO1 VDATCON,DMCB,(3,PBUYKDAT),(2,DUB)                                
*                                                                               
         SR    R6,R6               COUNTER                                      
         LA    R8,SVCON                                                         
*                                                                               
DSPCU05  CHI   R6,24               MAXIMUM NUMBER OF ENTRIES REACHED?           
         BNL   DSPCU40                                                          
         OC    4(4,R8),4(R8)       DISK ADDRESS PRESENT?                        
         BZ    DSPCU30             NO, CK NEXT ENTRY IN TABLE                   
         TM    0(R8),X'F0'         CONTRACT IS LOCKED?                          
         BO    DSPCU10             YES, NEED TO GET CONTRACT RECORD             
*                                                                               
         CLC   0(2,R8),DUB         CONTRACT START DT > INSERTION DT?            
         BH    DSPCU30                                                          
         CLC   2(2,R8),DUB         CONTRACT END DT < INSERTION DT?              
         BL    DSPCU30                                                          
*                                                                               
DSPCU10  TM    SVAORC,X'08'        CONTRACT RATE LOOK-UP                        
         BZ    DSPCU15                                                          
         CLC   SVAOR,AGYALPHA      AOR?                                         
         BE    DSPCU15                                                          
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CSWITCH-COMFACSD(RF)                                          
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB(1),SVAORSE                                                  
         GOTO1 (RF),DMCB                                                        
         CLI   4(R1),0                                                          
         BE    DSPCU15                                                          
         LA    R3,ADVFERR          *** ADVERTISER FILE NOT ACTIVE ***           
         LA    R2,BUYMDH                                                        
         J     DSPCUERR                                                         
*                                                                               
DSPCU15  MVC   KEY+27(4),4(R8)                                                  
         BRAS  RE,PRT_GETR                                                      
*                                                                               
         TM    SVAORC,X'08'        CONTRACT RATE LOOK-UP                        
         BZ    DSPCU20                                                          
         CLC   SVAOR,AGYALPHA      AOR?                                         
         BE    DSPCU20                                                          
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CSWITCH-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,=C'PRINT',0                                            
         CLI   4(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                MUST BE ABLE TO SWITCH BACK                  
*                                                                               
DSPCU20  L     RF,ACONIO                                                        
         USING PCONRECD,RF                                                      
         CLC   PCONSDT,PBUYKDAT    CONTRACT START DT > INSERTION DT?            
         BH    DSPCU30                                                          
         CLC   PCONEDT,PBUYKDAT    CONTRACT END DT < INSERTION DT?              
         BL    DSPCU30                                                          
         J     DSPCU40             RIGHT CONTRACT FOUND                         
         DROP  RF                                                               
*                                                                               
DSPCU30  LA    R8,8(R8)                                                         
         AHI   R6,1                ONE ENTRY IS SEARCHED, MAX IS 24             
         B     DSPCU05                                                          
*                                                                               
DSPCU40  LA    RF,REC                                                           
         ST    RF,AREC             RESTORE AREC                                 
         L     RF,ACONIO                                                        
         OC    0(256,RF),0(RF)     CONTRACT RECORD PRESENT?                     
         BZ    DSPCUX                                                           
*                                                                               
         XC    DCUVALUE,DCUVALUE                                                
         L     RF,ACONIO                                                        
         GOTO1 =V(PPGETCU),DMCB,PBUYREC,(RF),VDATAMGR,RR=RELOBY06               
         CLI   DMCB,X'FF'                                                       
         BE    *+10                                                             
         MVC   DCUVALUE,DMCB+5     LAST 3 BYTES                                 
*                                                                               
         OC    DCUVALUE,DCUVALUE   LOOKED UP CU VALUE PRESENT?                  
         JZ    DSPCUX              NO, DONE DISPLAYING CU                       
*                                                                               
         LA    R1,68(R2)           ANY ROOM ON THIS DIPLAY LINE?                
         CR    R4,R1                                                            
         BNH   DSPCU50             YES                                          
         BRAS  RE,BUMPFLD          NO SKIP TO NEXT LINE                         
         LA    R4,8(R2)                                                         
         FOUT  (R2)                                                             
*                                                                               
DSPCU50  MVC   0(3,R4),=C'CU='                                                  
         CLC   DCUVALUE,=X'000001' MEANS ZERO                                   
         BNE   DSPCU60                                                          
         MVI   3(R4),C'0'                                                       
         MVI   4(R4),C'*'          LOOKED UP CU IS ZERO                         
         LA    R4,6(R4)                                                         
         J     DSPCUX                                                           
*                                                                               
DSPCU60  XC    FULL,FULL                                                        
         MVC   FULL+1(3),DCUVALUE                                               
         AHI   R4,3                CU= OVERHEAD                                 
         EDIT  (B4,FULL),(9,0(R4)),4,ALIGN=LEFT,ZERO=NOBLANK                    
         AR    R4,R0               ADD DISPLAY LENGTH                           
         BCTR  R4,0                BACK UP 1                                    
DSPCU63  CLI   0(R4),C'0'          TRAILING ZERO?                               
         BE    *+12                                                             
         CLI   0(R4),C'.'          DECIMAL POINT?                               
         BNE   DSPCU65                                                          
         MVI   0(R4),0             CLEAR TRAILING ZERO                          
         BCTR  R4,0                                                             
         B     DSPCU63             GO BACK FOR MORE                             
*                                                                               
DSPCU65  AHI   R4,1                POINT TO END AGAIN                           
         MVI   0(R4),C'*'          "*" INDICATES LOOKED UP, 3 FOR CU=           
         AHI   R4,1+1              1 FOR "*", 1 FOR BLANK                       
*                                                                               
DSPCUX   CR    RB,RB               EQUAL                                        
         J     *+6                                                              
DSPCUERR LTR   RB,RB               NOT EQUAL (ERROR)                            
         JE    X_R2R4              NEED TO RETURN FIELD POINTER                 
         J     X_R2R3              ERROR FOUND, RETURN FLD AND MSG              
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
DSPBILL  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLC   =C'RO',TRCODE       OPEN BILLING RECALL                          
         BNE   *+8                 FOR DOREMUS FINANCIAL CLIENTS                
         MVI   ELCODE,X'28'        OPEN BILLING ELEMS                           
         LH    R8,HALF                                                          
         AR    R8,R2                                                            
         MVC   8(38,R8),BLLHD                                                   
         FOUT  (R2)                                                             
         B     DSPBI50                                                          
*                                                                               
DSPBI20  LH    R8,HALF                                                          
         LA    R8,8(R2,R8)                                                      
DSPBI30  BRAS  RE,NXTELEM                                                       
         JNE   EXIT                                                             
*                                                                               
         USING PBILELEM,R5                                                      
         OC    PBLDATE,PBLDATE                                                  
         BZ    DSPBI30                                                          
         FOUT  (R2)                                                             
         CLC   =C'ZZZ',BUYPR                                                    
         BNE   *+10                                                             
         MVC   0(3,R8),PBPRD                                                    
         GOTO1 VDATCON,DMCB,(3,PBLDATE),(5,4(R8))                               
*                                                                               
         GOTO1 =V(PPBVAL),DMCB,(C'E',PBILELEM),AWRKREC,RR=RELOBY06              
         L     R1,AWRKREC                                                       
         USING PPBVALDD,R1                                                      
         MVC   WORK(12),PPBVEEG    "EFFECTIVE" GROSS,AC,CD                      
         DROP  R1                                                               
*                                                                               
         TM    PBBILST,X'01'       SEE IF UFC COMM                              
         BZ    *+8                                                              
         MVI   12(R8),C'C'                                                      
         TM    PBBILST,X'02'       SEE IF UFC NET                               
         BZ    *+8                                                              
         MVI   12(R8),C'N'                                                      
         MVC   WORK+20(12),WORK    SAVE SINCE EDIT CLOBBERS WORK                
         EDIT  (B4,WORK),(12,13(R8)),2,FLOAT=-                                  
         MVC   WORK(12),WORK+20    RESTORE                                      
         L     R0,WORK                                                          
         S     R0,WORK+4                                                        
         S     R0,WORK+8                                                        
         EDIT  (R0),(12,26(R8)),2,FLOAT=-                                       
*                                                                               
         CLI   T411FFD+01,C'*'     DDS TERMINAL?                                
         BE    *+12                                                             
         TM    T411FFD+12,X'02'    NO ACCESS TO BILLING INV NUMBER?             
         BO    DSPBI50                                                          
*                                                                               
         BRAS  RE,BUMPFLD                                                       
         BNE   DSPBI40                                                          
         OC    HALF,HALF                                                        
         BNZ   DSPBI60                                                          
         LA    R0,40               2ND HALF OF SCREEN                           
         STH   R0,HALF                                                          
         L     R2,PARS                                                          
*                                                                               
DSPBI40  LH    R8,HALF                                                          
         LA    R8,8(R2,R8)                                                      
         XC    DUB,DUB                                                          
         GOTO1 VDATCON,DMCB,(3,PBLDATE),(0,DUB)                                 
         GOTO1 =V(PPFMTINO),DMCB,DUB,(2,PBINVNO),(PBUYKMED,B1PROF),    +        
               B1XPROF,RR=RELOBY06                                              
         L     RE,DMCB                                                          
         MVC   00(10,R8),0(RE)                                                  
*                                                                               
         TM    PBBILST,X'80'       REVERSED?                                    
         BZ    *+10                                                             
         MVC   11(08,R8),=C'REVERSED'                                           
         TM    PBBILST,X'40'       REVERSAL?                                    
         BZ    *+10                                                             
         MVC   11(08,R8),=C'REVERSAL'                                           
         TM    PBBILST,X'80'+X'40' BOTH REVERSED AND REVERSAL?                  
         BNO   DSPBI45                                                          
         MVC   11(08,R8),=C'REVERSED'                                           
         MVC   20(03,R8),=C'AND'                                                
         MVC   24(08,R8),=C'REVERSAL'                                           
DSPBI45  FOUT  (R2)                                                             
*                                                                               
DSPBI50  BRAS  RE,BUMPFLD                                                       
         BNE   DSPBI20                                                          
         OC    HALF,HALF                                                        
         BNZ   DSPBI60                                                          
         LA    R0,40               2ND HALF OF SCREEN                           
         STH   R0,HALF                                                          
         L     R2,PARS                                                          
         B     DSPBI20                                                          
*                                                                               
DSPBI60  LA    R0,REC              SAVE LAST SHOWN ELEM                         
         SR    R5,R0                                                            
         STH   R5,HALF                                                          
         MVC   LASTPBEL,HALF                                                    
         J     EXIT                                                             
*                                                                               
         DROP  RB,R5                                                            
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
NOTRAFF  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         TM    PBDSTAT,X'20'       NO TRAFFIC STATUS?                           
         BZ    *+18                                                             
         MVC   8(L'NTRATXT,R2),NTRATXT                                          
         BRAS  RE,BUMPFLD                                                       
         BZ    NOTRAERR                                                         
*                                                                               
NOTRAX   CR    RB,RB               EQUAL                                        
         B     *+6                                                              
NOTRAERR LTR   RB,RB               NOT EQUAL (ERROR)                            
         J     X_R2                NEED TO RETURN FIELD POINTER                 
*                                                                               
         DROP  RB                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         DS    0D                                                               
GETCLST  NTR1  BASE=*,LABEL=*      EXTRACT CLEARANCE STATUS                     
*                                                                               
         USING PPAYELEM,R5                                                      
*                                                                               
         MVC   WORK,SPACES         INIT WORKAREA                                
*                                                                               
         CLI   PPAYELEM+1,22       BIG PAY ELEM?                                
         BNH   GCSREP                                                           
*                                                                               
         CLI   PPDSEQNO,0          HAVE SEQ NUMBER?                             
         BE    GCSREP                                                           
*                                                                               
*        CHECK SECURITY ACCESS TO CLRST DATA                                    
*                                                                               
         CLI   T411FFD+1,C'*'      DDS TERMINAL?                                
         BE    *+12                                                             
         TM    T411FFD+12,X'02'    HAVE ACCESS?                                 
         BO    GCSREP                                                           
*                                                                               
         MVC   SV_AREC_,AREC       SAVE ORIGINAL AIO POINTER                    
*                                                                               
         XC    KEY,KEY             BUILD KEY OF FIRST STATUS RECORD             
         LA    R4,KEY              ESTABLISH CLRST REC KEY                      
         USING PPCLKEY,R4                                                       
*                                                                               
         MVC   PPCLAGY,AGYALPHA    AGENCY                                       
         MVC   PPCLMED,PBUYKMED    MEDIA                                        
         MVI   PPCLTYPE,X'25'      RECORD ID                                    
         MVC   PPCLCLT,PBUYKCLT    CLIENT                                       
         MVC   PPCLPUB(4),BPUB     PUB - MINUS ZONE & EDT                       
*                                                                               
         MVC   AREC,AWRKREC        USE CONIO                                    
*                                                                               
         BRAS  RE,PRT_RDHI         READ FIRST KEY FOR PUB                       
*                                                                               
         DROP  R4                                                               
*                                                                               
GCSRECLP DS    0H                                                               
*                                                                               
         CLC   KEY(11),KEYSAVE     TEST THROUGH PUB                             
         BNE   GCSRECDN            NO - BIG PROBLEM HERE                        
*                                                                               
         BRAS  RE,PRT_GETR         READ IN RECORD                               
*                                                                               
         L     R3,AREC             POINT TO FIRST ELEMENT IN CLRST REC          
         LA    R3,33(R3)                                                        
*                                                                               
GCSELMLP DS    0H                                                               
*                                                                               
         CLI   0(R3),0             IF END OF RECORD                             
         BE    GCSELMDN               TRY NEXT                                  
*                                                                               
         CLI   0(R3),X'01'         FIND 01 ELEMENT                              
         BNE   GCSELMCN                                                         
*                                                                               
         USING PPCLEL01,R3         ESTABLISH CLEARANCE ELEMENT                  
*                                                                               
         CLC   PPCLCLRD,PPDDATE    MATCH CLEARANCE DATE                         
         BNE   GCSELMCN                                                         
*                                                                               
         CLC   PPCLCLSQ,PPDSEQNO   MATCH SEQNUM                                 
         BNE   GCSELMCN                                                         
*                                                                               
         CLI   PPDSEQNO,X'FF'      OKAY IF NOT 255                              
         BNE   GCSELMFD                                                         
*                                                                               
         TM    PPDSTAT,X'01'       OKAY IF NO SECONDARY SQN                     
         BNO   GCSELMFD                                                         
*                                                                               
         LLC   RF,PPAYELEM+1       GET ELEMENT LENGTH                           
         SHI   RF,2                POINT TO LAST 2 BYTES                        
         LA    RF,PPAYELEM(RF)                                                  
*                                                                               
         CLC   PPCLCLS2,0(RF)      MATCH ON SECONDARY SQN                       
         BE    GCSELMFD                                                         
*                                                                               
GCSELMCN DS    0H                                                               
*                                                                               
         LLC   RF,1(R3)            GET ELEMENT LENGTH                           
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         AR    R3,RF               BUMP TO NEXT ELEMENT                         
*                                                                               
         B     GCSELMLP                                                         
*                                                                               
GCSELMDN DS    0H                  END OF RECORD                                
*                                                                               
GCSRECCN DS    0H                  READ NEXT CLRST REC ON FILE                  
*                                                                               
         L     RF,AREC             POINT TO RECORD IN CORE                      
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(25),0(RF)       MOVE KEY FROM RECORD                         
*                                                                               
         BRAS  RE,PRT_RDHI         RE-READ KEY                                  
*                                                                               
         BRAS  RE,PRT_RSEQ         READ NEXT KEY                                
*                                                                               
         B     GCSRECLP                                                         
*                                                                               
GCSRECDN DS    0H                                                               
*                                                                               
         B     GETCLSX             NO CLRST ELEMENT FOUND                       
*                                                                               
*        CLEARANCE STATUS ELEMENT FOUND                                         
*                                                                               
GCSELMFD DS    0H                                                               
*                                                                               
         TM    PPCLSTAT,X'02'      DIFFERENT IF INVOICES                        
         BO    GCSINV                 FOLLOWING                                 
*                                                                               
*        PRINT CHECK DATA IN OLD STYLE                                          
*                                                                               
GCSCHK24 DS    0H                  COME HERE IF NO INV ELMS FD                  
*                                                                               
         OC    PPCLCHK,PPCLCHK     SKIP IF NO CHECK YET                         
         BZ    GCSCHKX                                                          
*                                                                               
         MVC   WORK(3),=C'CK='                                                  
         MVC   WORK+3(6),PPCLCHK                                                
*                                                                               
         CLC   PPCLCHK,=6C' '    IF SPACES CHK WAS VOIDED                       
         BE    *+10                                                             
         CLC   =C'VOID',PPCLCHK  OR VOID   CHK WAS VOIDED                       
         BNE   GCSCHK25                                                         
*                                                                               
         MVC   WORK+3(6),=C'VOIDED'                                             
*                                                                               
         B     GCSCHKX                                                          
*                                                                               
GCSCHK25 TM    PPCLSTAT,X'80'      TEST RECONCILED                              
         BZ    *+8                                                              
         MVI   WORK+10,C'*'                                                     
*                                                                               
         GOTO1 VDATCON,DMCB,(2,PPCLCHDT),(8,WORK+11) CHK DATE                   
*                                                                               
GCSCHKX  DS    0H                                                               
*                                                                               
         CLC   WORK,SPACES         SKIP IF I HAVE CHECK DATA                    
         BH    GCSCTLX                                                          
*                                                                               
         OC    PPDCKDAT,PPDCKDAT   IF WE HAVE CHECK CONTROL DATE                
         BZ    GCSCTLX                                                          
*                                                                               
         CLC   PPDCKDAT,PPDDATE       DIFFERENT FROM CLEARANCE DATE             
         BE    GCSCTLX                                                          
*                                                                               
         MVC   WORK(14),=C'CHK CNTL DATE='                                      
*                                                                               
         GOTO1 VDATCON,DMCB,(3,PPDCKDAT),(5,WORK+14)                            
*                                                                               
GCSCTLX  DS    0H                                                               
*                                                                               
*        DISPLAY REP                                                            
*                                                                               
GCSREP   DS    0H                                                               
*                                                                               
         CLI   PPAYELEM+1,X'16'    NEW ELEM WITH REP DATA?                      
         BL    GCSREPX                                                          
*                                                                               
         CLC   PPDDATE,=X'520A01'  IF PD IS OCT01/82 OR LATER?                  
         BL    GCSREP10                                                         
*                                                                               
         TM    PPREP,X'80'         IF CK                                        
         BZ    *+10                                                             
         MVC   WORK+23(2),=C'CK'      SHOW CK                                   
*                                                                               
         TM    PPREP,X'40'         IF CR                                        
         BZ    *+10                                                             
         MVC   WORK+23(2),=C'CR'      SHOW CR                                   
*                                                                               
GCSREP10 DS    0H                                                               
*                                                                               
         MVC   X(2),PPREP          HOLD REP                                     
         NI    X,X'FF'-X'C0'       STRIP HOBS                                   
*                                                                               
         MVC   WORK+26(12),=C'PAYEE=DIRECT'                                     
*                                                                               
         OC    X(2),X              SKIP IF NO REP                               
         BZ    GCSREPX                                                          
*                                                                               
         MVC   DUB,X               DISPLAY REP NUMBER                           
         LH    R0,DUB                                                           
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK+32(4),DUB                                                   
         MVC   WORK+36(2),=C'  '   CLEAR LAST OF DISPLAY AREA                   
*                                                                               
GCSREPX  DS    0H                                                               
*                                                                               
         BRAS  RE,GCSOUT           DISPLAY DATA                                 
*                                                                               
GCSOLDX  DS    0H                                                               
         B     GETCLSX                                                          
*                                                                               
*        PAYMENT HAS INVOICE DATA                                               
*                                                                               
GCSINV   DS    0H                                                               
*                                                                               
         ST    R3,GCSEL01A         SAVE A(CURRENT CLRST ELM 01)                 
         XC    SVELM03,SVELM03     INIT SAVEAREAS                               
         XC    SVELM05,SVELM05                                                  
*                                                                               
         BRAS  RE,GETTOTS          GET POSITIVE AND NEGATIVE TOTALS             
*                                                                               
         BRAS  RE,GCDTOTS          SAME FOR CD AMOUNTS                          
*                                                                               
         LR    R1,R6               SAVE POINTER TO LAST ELEMENT                 
         LLC   RF,PPCLEL01+1       GET ELEMENT LENGTH                           
         LA    R6,PPCLEL01(RF)     BUMP TO NEXT ELEMENT                         
*                                                                               
GCSINVLP DS    0H                                                               
*                                                                               
         CLI   0(R6),0             DONE IF END OF RECORD                        
         BE    GCSINVDN                                                         
*                                                                               
         CLI   0(R6),X'01'         DONE IF ANOTHER X'01' ELEMENT                
         BE    GCSINVDN                                                         
*                                                                               
         CLI   0(R6),X'03'         MUST BE INVOICE ELEMENT                      
         BNE   GCSINVCN                                                         
*                                                                               
         MVI   PAYRTFLG,0          INIT PAYMENT RATIO CALCULATION FLAG          
         BRAS  RE,TSTBYINV         TEST BUY INVOICE                             
         JNE   *+12                                                             
         BRAS  RE,CMPBYINV         COMPARE BUY INVOICE                          
         JNE   GCSINVCN                                                         
*                                                                               
GCSINV1  L     RF,SVR8             POINT TO DOLLARS ON PREVIOUS LINE            
         XC    10(28,RF),10(RF)    CLEAR DOLLARS DISPLAY                        
*                                                                               
         TM    PPCLSTAT,X'08'      SCRIPT PAY UPLOAD?                           
         JZ    *+10                                                             
         MVC   32(7,RF),=C'SCRIPT '                                             
*                                                                               
         TM    PPCLSTAT,X'10'      AUTOPAY?                                     
         JZ    *+10                                                             
         MVC   32(7,RF),=C'AUTOPAY'                                             
*                                                                               
         CLI   PPCLSRCE,PPCLSPRQ   PRISMA?                                      
         JNE   *+10                                                             
         MVC   32(7,RF),=C'PRISMA '                                             
*                                                                               
         CLI   PPCLSRCE,PPCLSRAQ   RADIA?                                       
         JNE   *+10                                                             
         MVC   32(7,RF),=C'RADIA  '                                             
*                                                                               
         USING PPCLEL03,R6         ESTABLISH INVOICE ELEMENT                    
*                                                                               
         MVC   WORK(11),PPCLINV    DISPLAY INVOICE NUMBER                       
*                                                                               
         CLC   PPCLINV,PPCLINV-PPCLEL03+SVELM03  IF NEW INVOICE                 
         BE    *+10                                                             
         XC    SVELM05,SVELM05     CLEAR OLD ELM05                              
*                                                                               
         MVC   SVELM03,PPCLEL03    SAVE 03 ELM                                  
*                                                                               
         LLC   RF,1(R6)            GET ELEMENT LENGTH                           
         AR    R6,RF               BUMP TO NEXT ELEMENT                         
*                                                                               
         CLI   0(R6),X'05'         MUST BE A CHECK ELEMENT                      
         BNE   GCSINVCN                                                         
*                                                                               
         USING PPCLEL05,R6         ESTABLISH CHECK ELEMENT                      
*                                                                               
*        CALCULATE PERCENT OF PAYMENT ON THIS INVOICE                           
*                                                                               
         ZAP   GCSRTIOD,=P'0'      INIT DEBIT  RATIO                            
         ZAP   GCSRTIOC,=P'0'      INIT CREDIT RATIO                            
*                                                                               
         LA    RE,GCSRTIOD            POINT TO DEBIT RATIO                      
*                                                                               
         LA    R4,GCSDEBN          ASSUME NET DEBIT TOTALS                      
         ICM   RF,15,PCL5NET       GET CLEARED NET                              
         TM    PPCLSTAT,X'20'      IF NOT CLEARED NET                           
         BO    *+12                                                             
         LA    R4,GCSDEBG             POINT TO TOTAL DEBIT GROSS                
         ICM   RF,15,PCL5GRS          GET CLEARED GROSS                         
*                                                                               
         LTR   RF,RF               IF NEGATIVE AMOUNT                           
         BNM   *+12                                                             
         LA    R4,16(R4)              USE CREDIT TOTALS                         
         LA    RE,16(RE)              USE CREDIT RATIO                          
*                                                                               
         CVD   RF,GCSPL8           CVD                                          
         ZAP   GCSPL16,GCSPL8      SAVE                                         
*                                                                               
*****    ICM   RF,15,0(R4)         GET TOTAL CLEARED                            
*****    BZ    GCSINV2                                                          
*                                                                               
         ICM   RF,15,0(R4)         GET TOTAL CLEARED                            
         BNZ   *+14                                                             
         ZAP   0(8,RE),=P'1000000'   IF ZERO, RATIO IS 1                        
         B     GCSINV2                                                          
*                                                                               
         CVD   RF,GCSPL8           CVD                                          
*                                                                               
         SRP   GCSPL16,7,0         * 10**7 FOR ROUNDING                         
*                                                                               
         DP    GCSPL16,GCSPL8      CALCULATE RATIO INVOICE TO TOTAL             
         SRP   GCSPL16(8),64-1,5   ROUND TO 4 DECIMALS                          
         ZAP   0(8,RE),GCSPL16(8)  SAVE RATIO                                   
*                                                                               
GCSINV2 DS     0H                                                               
*                                                                               
         CP    GCSRTIOC,=P'0'      IF NO CREDIT RATIO                           
         BNZ   *+10                                                             
         ZAP   GCSRTIOC,GCSRTIOD      DEFAULT TO DEBIT RATIO                    
*                                                                               
         CP    GCSRTIOD,=P'0'      IF NO DEBIT RATIO                            
         BNZ   *+10                                                             
         ZAP   GCSRTIOD,GCSRTIOC      DEFAULT TO CREDIT RATIO                   
*                                                                               
         LA    R4,GCSRTIOD         ASSUME DEBIT                                 
*                                                                               
         ICM   RF,15,PPGROSS       GET GROSS CLEARED FOR BUY                    
         BNM   *+8                                                              
         LA    R4,GCSRTIOC            USE CREDIT RATIO IF NEGATIVE              
*                                                                               
         CVD   RF,GCSPL8           CVD                                          
         ZAP   GCSPL16,GCSPL8      MOVE TO WORKAREA                             
*                                                                               
*                                  *RATIO                                       
         MP    GCSPL16,0(8,R4)     CALCULATE GROSS ON THIS INVOICE              
         SRP   GCSPL16,64-6,5      ROUND TO PENNIES                             
*                                                                               
         CLI   PAYRTFLG,C'Y'       NEED TO CALCULATE PAYMENT RATIO?             
         JNE   GCSINV2D                                                         
         MVC   FULL,PCL5GRS                                                     
         OC    PCL5GRS,PCL5GRS     CLEARED GROSS?                               
         JNZ   GCSINV2C                                                         
         OC    PCL5NET,PCL5NET     CLEARED NET?                                 
         JZ    GCSINV2C                                                         
         CLC   PPAGYCOM,PPGROSS    AGENCY COMMISSION IS 100%?                   
         JNE   *+14                                                             
         MVC   FULL,PPGROSS                                                     
         J     GCSINV2C                                                         
         OC    PPAGYCOM,PPAGYCOM   HAVE AGENCY COMMISSION?                      
         JNZ   GCSINV2B                                                         
         ICM   RF,15,PCL5NET                                                    
         ICM   RE,15,PCL5CD                                                     
         AR    RF,RE                                                            
         STCM  RF,15,FULL                                                       
         J     GCSINV2C                                                         
GCSINV2B ICM   RF,15,PPAGYCOM                                                   
         CVD   RF,WRKPL8                                                        
         MP    WRKPL8,=P'100000'                                                
         ZAP   WRKPL16,WRKPL8                                                   
         ICM   RF,15,PPGROSS                                                    
         CVD   RF,WRKPL8                                                        
         DP    WRKPL16,WRKPL8      CALCULATE AGENCY COMMISSION %                
         ZAP   WRKPL8,=P'100000'                                                
         SP    WRKPL8,WRKPL16(8)   100% - AC% = NET%                            
         ICM   RF,15,PCL5NET                                                    
         ICM   RE,15,PCL5CD                                                     
         AR    RF,RE                                                            
         ZAP   WRKPL16,=P'0'                                                    
         CVD   RF,WRKPL16+8                                                     
         MP    WRKPL16,=P'100000'                                               
         DP    WRKPL16,WRKPL8      CALCULATE GROSS AMOUNT                       
         EDIT  (P8,WRKPL16),(9,WORK+11),2,FLOAT=-,WRK=GCSWORK                   
         J     GCSINV2E                                                         
GCSINV2C EDIT  (B4,FULL),(9,WORK+11),2,FLOAT=-,WRK=GCSWORK                      
         J     GCSINV2E                                                         
*                                                                               
*        HANDLE LARGE CREDIT                                                    
*                                                                               
         CP    GCSPL16+8(8),=P'-10000000' IF LARGE CREDIT                       
         BNL   GCSINV2D                                                         
*                                                                               
         EDIT  (P8,GCSPL16+8),(10,WORK+10),2,FLOAT=-,WRK=GCSWORK                
*                                                                               
         B     GCSINV2E                                                         
*                                                                               
GCSINV2D DS    0H                  ELSE                                         
*                                  DISPLAY GROSS                                
         EDIT  (P8,GCSPL16+8),(9,WORK+11),2,FLOAT=-,WRK=GCSWORK                 
*                                                                               
GCSINV2E DS    0H                                                               
*                                                                               
         ICM   RF,15,PPGROSS       GET GROSS    CLEARED FOR BUY                 
         ICM   RE,15,PPAGYCOM      GET AGY COMM CLEARED FOR BUY                 
         SR    RF,RE               GET NET      CLEARED FOR BUY                 
*                                                                               
         CVD   RF,GCSPL8           CVD                                          
         ZAP   GCSPL16,GCSPL8      MOVE TO WORKAREA                             
*                                                                               
*                                  *RATIO                                       
         MP    GCSPL16,0(8,R4)     CALCULATE NET ON THIS INVOICE                
         SRP   GCSPL16,64-6,5      ROUND TO PENNIES                             
*                                                                               
* DISPLAY NET                                                                   
*                                                                               
         CLI   PAYRTFLG,C'Y'       NEED TO CALCULATE PAYMENT RATIO?             
         JNE   GCSINV2K                                                         
         OC    PCL5NET,PCL5NET     HAVE CLEARED NET LESS CD PAID?               
         JZ    GCSINV2H                                                         
GCSINV2F ICM   RE,15,PCL5NET                                                    
         ICM   RF,15,PCL5CD                                                     
         AR    RE,RF               GET NET PAID                                 
         STCM  RE,15,FULL                                                       
         EDIT  (B4,FULL),(10,WORK+20),2,FLOAT=-,WRK=GCSWORK                     
         J     GCSINV2M                                                         
GCSINV2H OC    PPAGYCOM,PPAGYCOM   HAVE AGENCY COMMISSION?                      
         JZ    GCSINV2F                                                         
         ICM   RF,15,PPAGYCOM                                                   
         CVD   RF,WRKPL8                                                        
         MP    WRKPL8,=P'100000'                                                
         ZAP   WRKPL16,WRKPL8                                                   
         ICM   RF,15,PPGROSS                                                    
         CVD   RF,WRKPL8                                                        
         DP    WRKPL16,WRKPL8      CALCULATE AGENCY COMMISSION %                
         ZAP   WRKPL8,WRKPL16(8)                                                
         ZAP   WRKPL16,=P'100000'                                               
         SP    WRKPL16,WRKPL8      100% - AC% = NET%                            
         ICM   RF,15,PCL5GRS                                                    
         CVD   RF,WRKPL8                                                        
         MP    WRKPL16,WRKPL8      GET NET AMOUNT                               
         ZAP   WRKPL8,=P'100000'                                                
         DP    WRKPL16,WRKPL8                                                   
         EDIT  (P8,WRKPL16),(10,WORK+20),2,FLOAT=-,WRK=GCSWORK                  
         J      GCSINV2M                                                        
GCSINV2K EDIT  (P8,GCSPL16+8),(10,WORK+20),2,FLOAT=-,WRK=GCSWORK                
*                                                                               
*        CALCULATE PERCENT OF PAYMENT OF CD ON THIS INVOICE                     
*                                                                               
GCSINV2M ZAP   GCSRTCDD,=P'0'      INIT DEBIT  RATIO                            
         ZAP   GCSRTCDC,=P'0'      INIT CREDIT RATIO                            
*                                                                               
         LA    R4,GCSDEBCD         ASSUME CD DEBIT TOTALS                       
         ICM   RF,15,PCL5CD        GET CLEARED NET                              
         BNM   *+8                 IF NEGATIVE                                  
         LA    R4,GCSCRDCD            USE CREDIT TOTALS                         
*                                                                               
         CVD   RF,GCSPL8           CVD                                          
         ZAP   GCSPL16,GCSPL8      SAVE                                         
*                                                                               
*****    ICM   RF,15,0(R4)         GET TOTAL CLEARED                            
*****    BZ    GCSINV3             SKIP IF ZERO                                 
*                                                                               
         ICM   RF,15,0(R4)         GET TOTAL CLEARED                            
         BNZ   *+14                                                             
         ZAP   4(8,R4),=P'1000000'   IF ZERO, RATIO IS 1                        
         B     GCSINV3                                                          
*                                                                               
         CVD   RF,GCSPL8           CVD                                          
*                                                                               
         SRP   GCSPL16,7,0         * 10**7 FOR ROUNDING                         
*                                                                               
         DP    GCSPL16,GCSPL8      CALCULATE RATIO INVOICE TO TOTAL             
         SRP   GCSPL16(8),64-1,5   ROUND TO 6 DECIMALS                          
         ZAP   4(8,R4),GCSPL16(8)  SAVE RATIO                                   
*                                                                               
GCSINV3 DS     0H                                                               
*                                                                               
         CP    GCSRTCDC,=P'0'      IF NO CREDIT RATIO                           
         BNZ   *+10                                                             
         ZAP   GCSRTCDC,GCSRTCDD      DEFAULT TO DEBIT RATIO                    
*                                                                               
         CP    GCSRTIOD,=P'0'      IF NO DEBIT RATIO                            
         BNZ   *+10                                                             
         ZAP   GCSRTCDD,GCSRTCDC      DEFAULT TO CREDIT RATIO                   
*                                                                               
         LA    R4,GCSRTCDD         ASSUME DEBIT                                 
*                                                                               
         ICM   RF,15,PPCSHDSC      GET CD CLEARED FOR BUY                       
         BNM   *+8                                                              
         LA    R4,GCSRTCDC            USE CREDIT RATIO IF NEGATIVE              
*                                                                               
         CVD   RF,GCSPL8           CVD                                          
         ZAP   GCSPL16,GCSPL8      MOVE TO WORKAREA                             
*                                                                               
*                                  *RATIO                                       
         MP    GCSPL16,0(8,R4)     CALCULATE CD ON THIS INVOICE                 
         SRP   GCSPL16,64-6,5      ROUND TO PENNIES                             
*                                                                               
*                                  DISPLAY CASH DISCOUNT                        
*                                                                               
         CLI   PAYRTFLG,C'Y'       NEED TO CALCULATE PAYMENT RATIO?             
         JNE   GCSINV3K                                                         
         EDIT  (B4,PCL5CD),(8,WORK+30),2,FLOAT=-,WRK=GCSWORK                    
         J     GCSINV3M                                                         
GCSINV3K EDIT  (P8,GCSPL16+8),(8,WORK+30),2,FLOAT=-,WRK=GCSWORK                 
*                                                                               
*        DISPLAY INVOICE DATA                                                   
*                                                                               
GCSINV3M BRAS  RE,GCSOUT           DISPLAY INVOICE                              
         BZ    GETCLSX             NO MORE DISPLAY ROOM                         
*                                                                               
*        FORMAT CHECK DATA FOR INVOICE                                          
*                                                                               
         OC    PCL5CHK,PCL5CHK     IF NO CHECK ISSUED                           
         BNZ   GCSICK4                                                          
*                                                                               
         OC    PCL5CHK-PPCLEL05+SVELM05,PCL5CHK-PPCLEL05+SVELM05                
         BZ    GCSICKX                SKIP IF INV NOT PAID BEFORE               
*                                  ELSE COPY CHECK DATA                         
         MVC   PCL5CHK,PCL5CHK-PPCLEL05+SVELM05                                 
         MVC   PCL5CHDT,PCL5CHDT-PPCLEL05+SVELM05                               
         MVC   PCL5BKDT,PCL5BKDT-PPCLEL05+SVELM05                               
         MVC   PCL5STAT,PCL5STAT-PPCLEL05+SVELM05                               
*                                  ELSE COPY CHECK DATA                         
GCSICK4  DS    0H                                                               
*                                                                               
         MVC   WORK(3),=C'CK='     DISPLAY CHECK                                
         MVC   WORK+3(6),PCL5CHK                                                
*                                                                               
         CLC   PCL5CHK,=6C' '    IF SPACES CHK WAS VOIDED                       
         BE    *+10                                                             
         CLC   =C'VOID',PCL5CHK  OR VOID   CHK WAS VOIDED                       
         BNE   GCSICK5                                                          
*                                                                               
         MVC   WORK+3(6),=C'VOIDED'                                             
*                                                                               
         B     GCSICKX                                                          
*                                                                               
GCSICK5  TM    PCL5STAT,X'80'      TEST RECONCILED                              
         BZ    *+8                                                              
         MVI   WORK+10,C'*'                                                     
*                                                                               
         GOTO1 VDATCON,DMCB,(2,PCL5CHDT),(8,WORK+11) CK DTE                     
*                                                                               
GCSICKX DS     0H                                                               
*                                                                               
         MVC   SVELM05,PPCLEL05    SAVE 05 ELEMENT                              
*                                                                               
         CLC   WORK,SPACES         SKIP IF I HAVE CHECK DATA                    
         BH    GCSICTLX                                                         
*                                                                               
         OC    PPDCKDAT,PPDCKDAT   IF WE HAVE CHECK CONTROL DATE                
         BZ    GCSICTLX                                                         
*                                                                               
         CLC   PPDCKDAT,PPDDATE       DIFFERENT FROM CLEARANCE DATE             
         BE    GCSICTLX                                                         
*                                                                               
         MVC   WORK(14),=C'CHK CNTL DATE='                                      
*                                                                               
         GOTO1 VDATCON,DMCB,(3,PPDCKDAT),(5,WORK+14)                            
*                                                                               
GCSICTLX DS    0H                                                               
*                                                                               
*        DISPLAY CR/CK                                                          
*                                                                               
         TM    PCL5STAT,PCL5STAT_CK   IF CK                                     
         BZ    *+10                                                             
         MVC   WORK+23(2),=C'CK'      SHOW CK                                   
*                                                                               
         TM    PCL5STAT,PCL5STAT_CR   IF CR                                     
         BZ    *+10                                                             
         MVC   WORK+23(2),=C'CR'      SHOW CR                                   
*                                                                               
         MVC   X(2),PPREP          HOLD REP                                     
         NI    X,X'FF'-X'C0'       STRIP HOBS                                   
*                                                                               
         MVC   WORK+26(12),=C'PAYEE=DIRECT'                                     
*                                                                               
         OC    X(2),X              SKIP IF NO REP                               
         BZ    GCSIREPX                                                         
*                                                                               
         MVC   DUB,X               DISPLAY REP NUMBER                           
         LH    R0,DUB                                                           
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK+32(4),DUB                                                   
         MVC   WORK+36(2),=C'  '   CLEAR LAST OF DISPLAY AREA                   
*                                                                               
GCSIREPX DS    0H                                                               
*                                                                               
         BRAS  RE,GCSOUT           DISPLAY DATA                                 
         BZ    GETCLSX             NO DISPLAY ROOM LEFT                         
*                                                                               
GCSINVCN DS    0H                                                               
*                                                                               
         LR    R1,R6               SAVE POINTER TO LAST ELEMENT                 
         LLC   RF,PPCLEL05+1       GET ELEMENT LENGTH                           
         LA    R6,PPCLEL05(RF)     BUMP TO NEXT ELEMENT                         
*                                                                               
         B     GCSINVLP                                                         
*                                                                               
GCSINVDN DS    0H                                                               
*                                                                               
         CLI   0(R1),X'05'         SKIP IF CHECK ELEM LAST DONE                 
         BE    GETCLSX                                                          
*                                                                               
         B     GCSCHK24            GO HANDLE IN OLD SYLE                        
*                                                                               
GETCLSX  MVC   AREC,SV_AREC_       SAVE ORIGINAL AIO POINTER                    
*                                                                               
         XIT1  REGS=(R2,R8)                                                     
*                                                                               
TSTBYINV CLI   BYPROF+15,C'Y'      PAY DETAIL FOR LINKED INV ONLY?              
         JNE   BYINVNEQ                                                         
         LA    R1,PBUYREC+33       TEST BUY HAS INVOICES                        
         CLI   0(R1),X'20'         BUY DESCRIPTION ELEMENT PRESENT?             
         JE    *+6                                                              
         DC    H'0'                                                             
TBYINV10 CLI   0(R1),0             INVOICE# NOT FOUND?                          
         JE    BYINVNEQ                                                         
         CLI   0(R1),X'CC'         CUSTOM COLUMN ELEMENT?                       
         JE    TBYINV30                                                         
         CLI   0(R1),X'51'         NEW INVOICE ELEMENT?                         
         JE    BYINVEQ                                                          
TBYINV16 LLC   R0,1(R1)                                                         
         AR    R1,R0               BUMP TO NEXT ELEMENT                         
         J     TBYINV10                                                         
TBYINV30 CLC   =AL2(8213),(BYCCSQN-BYCCELM)(R1)                                 
         JNE   TBYINV16                                                         
         J     BYINVEQ                                                          
*                                                                               
CMPBYINV LA    R1,PBUYREC+33       COMPARE INVOICE# WITH CLEARANCE REC          
         CLI   0(R1),X'20'         BUY DECRIPTION ELEMENT PRESENT?              
         JE    *+6                                                              
         DC    H'0'                                                             
CBYINV10 CLI   0(R1),0             NO MATCHING INVOICE#?                        
         JE    BYINVNEQ                                                         
         CLI   0(R1),X'CC'         CUSTOM COLUMN ELEMENT?                       
         JE    CBYINV30                                                         
         CLI   0(R1),X'51'         NEW INVOICE ELEMENT?                         
         JE    CBYINV50                                                         
CBYINV16 LLC   R0,1(R1)                                                         
         AR    R1,R0               BUMP TO NEXT ELEMENT                         
         J     CBYINV10                                                         
CBYINV30 CLC   =AL2(8213),(BYCCSQN-BYCCELM)(R1)                                 
         JNE   CBYINV16                                                         
         CLC   (PPCLINV-PPCLEL03)(L'PPCLINV,R6),BYCCHDRL(R1)                    
         JE    CBYINV70                                                         
         J     CBYINV16                                                         
*                                                                               
CBYINV50 CLC   (PPCLINV-PPCLEL03)(L'PPCLINV,R6),(PBNVINV#-PBNVELM)(R1)          
         JE    CBYINV70                                                         
         J     CBYINV16                                                         
*                                                                               
CBYINV70 MVI   PAYRTFLG,C'Y'       DON'T NEED TO CALC PAYMENT RATIO             
*                                                                               
BYINVEQ  CR    RE,RE               EQUAL                                        
         J     *+6                                                              
BYINVNEQ LTR   RE,RE               NOT EQUAL                                    
         BR    RE                                                               
*                                                                               
*        ROUTINE TO PUT DATA IN WORK TO SCREEN                                  
*                                                                               
         DS 0D                                                                  
GCSOUT   NTR1  LABEL=*                                                          
*                                                                               
         CLC   WORK,SPACES         SKIP IF NO DATA TO DISPLAY                   
         BNH   GCSOUTX                                                          
*                                                                               
         MVC   0(38,R8),WORK       DISPLAY DATA                                 
         MVC   WORK,SPACES         INIT WORKAREA                                
*                                                                               
         FOUT  (R2)                PUT TO SCREEN                                
*                                                                               
         BRAS  RE,BUMPFLD          BUMP TO NEXT FIELD                           
         BNE   GCSOUT10               NOT END OF SCREEN                         
*                                                                               
*        FIND NEXT DISPLAY AREA                                                 
*                                                                               
         OC    HALF,HALF           DONE IF DOING SECOND HALF OF SCREEN          
         BNZ   GCSOUT20               NO MORE DISPLAY AREA AVAILABLE            
*                                                                               
         L     R2,PARS             FIRST LINE OF DISPLAY AREA                   
*                                                                               
         LA    R0,40               2ND HALF OF SCREEN                           
         STH   R0,HALF                                                          
*                                                                               
GCSOUT10 DS    0H                                                               
*                                                                               
         LA    R8,8(R2)            POINT TO NEXT DISPLAY AREA                   
         AH    R8,HALF             POSSIBLY SECOND HALF OF SCREEN               
         LTR   RB,RB               MORE ROOM AVAILABLE ON SCREEN                
*                                                                               
         B     GCSOUTX                                                          
*                                                                               
GCSOUT20 DS    0H                                                               
         CR    RB,RB               SET END OF SCREEN INDICATOR                  
*                                                                               
GCSOUTX  DS    0H                                                               
*                                                                               
         XIT1  REGS=(R2,R8)                                                     
*                                                                               
         DROP  R5                                                               
*                                                                               
*        ROUTINE TO GET POSITIVE AND NEGATIVE TOTALS FOR PAYMENT                
*              DONE BY SUMMING DEBITS AND CREDITS SEPARATELY                    
*                                                                               
*        R3==> PPCLEL01                                                         
*        R5==> PPAYELEM                                                         
*                                                                               
         DS 0D                                                                  
GETTOTS  NTR1  LABEL=*                                                          
*                                                                               
         USING PPAYELEM,R5         ESTABLISH PAY ELEMENT                        
*                                                                               
         XC    GCSDEBG,GCSDEBG     INIT TOTAL DEBITS  GROSS                     
         XC    GCSDEBN,GCSDEBN     INIT TOTAL DEBITS  NET                       
         XC    GCSCRDG,GCSCRDG     INIT TOTAL CREDITS GROSS                     
         XC    GCSCRDN,GCSCRDN     INIT TOTAL CREDITS NET                       
*                                                                               
         ICM   R6,15,GCSEL01A      POINT TO CURRENT 01 ELEMENT                  
         BZ    GETTOTSX            NONE - GET OUT                               
*                                                                               
         ICM   R0,15,PPGROSS       GET GROSS PAID                               
*                                                                               
         TM    PPCLSTAT,X'20'      IF PAID NET                                  
         BNO   GTT10                                                            
*                                                                               
         ICM   RF,15,PPAGYCOM         GET AGENCY COMMISSION                     
         ICM   RE,15,PPCSHDSC         GET CD                                    
         SR    R0,RE                  CALC NET                                  
         SR    R0,RF                                                            
*                                                                               
GTT10    DS    0H                                                               
*                                                                               
         LTR   R0,R0               MUST BE NON-NEGATIVE                         
         BZ    GTT20                                                            
*                                                                               
         ICM   RE,15,PPCLGRS       GET CHECK TOTAL GROSS                        
*                                                                               
         TM    PPCLSTAT,X'20'      IF PAID NET                                  
         BNO   *+8                                                              
         ICM   RE,15,PPCLNET          GET CHECK TOTAL NET                       
*                                                                               
         CR    R0,RE               SKIP IF NOT EQUAL - MULTIPLE BUYS            
         BNE   GTT20                                                            
*                                                                               
         STCM  R0,15,GCSDEBN       SET NET TOTALS                               
         STCM  R0,15,GCSCRDN       SET NET TOTALS                               
*                                                                               
         TM    PPCLSTAT,X'20'      SKIP IF PAID NET                             
         BO    *+12                                                             
         STCM  R0,15,GCSDEBG          SET GROSS TOTALS                          
         STCM  R0,15,GCSCRDG          SET GROSS TOTALS                          
*                                                                               
         B     GETTOTSX                                                         
*                                                                               
GTT20    DS    0H                                                               
*                                                                               
         LLC   RF,PPCLEL01+1       GET ELEMENT LENGTH                           
         LA    R6,PPCLEL01(RF)     BUMP TO NEXT ELEMENT                         
*                                                                               
GTTLOOP  DS    0H                                                               
*                                                                               
         CLI   0(R6),0             DONE IF END OF RECORD                        
         BE    GTTDONE                                                          
*                                                                               
         CLI   0(R6),X'01'         DONE IF ANOTHER X'01' ELEMENT                
         BE    GTTDONE                                                          
*                                                                               
         CLI   0(R6),X'05'         MUST BE 05      ELEMENT                      
         BNE   GTTCONT                                                          
*                                                                               
         USING PPCLEL05,R6         ESTABLISH 05 ELEMENT                         
*                                                                               
         TM    PPCLSTAT,X'20'      SKIP IF NOT CLEARED NET                      
         BNO   GTTNETN                                                          
*                                                                               
         LA    R1,GCSDEBN          POINT TO CURRENT DEBIT TOTAL NET             
*                                                                               
         ICM   RF,15,PCL5NET       GET CLEARED NET                              
         BNM   *+8                                                              
         LA    R1,GCSCRDN             POINT TO CREDIT TOTAL IF MINUS            
*                                                                               
         ICM   RE,15,0(R1)         UPDATE TOTAL                                 
         AR    RE,RF                                                            
         STCM  RE,15,0(R1)                                                      
*                                                                               
         B     GTTGRSX                                                          
*                                                                               
GTTNETN  DS    0H                  PAID GROSS                                   
*                                                                               
         LA    R1,GCSDEBG          POINT TO CURRENT DEBIT TOTAL GRS             
*                                                                               
         ICM   RF,15,PCL5GRS       GET CLEARED GROSS                            
         BNM   *+8                                                              
         LA    R1,GCSCRDG             POINT TO CREDIT TOTAL IF MINUS            
*                                                                               
         ICM   RE,15,0(R1)         UPDATE TOTAL                                 
         AR    RE,RF                                                            
         STCM  RE,15,0(R1)                                                      
*                                                                               
GTTGRSX  DS    0H                                                               
*                                                                               
GTTCONT  DS    0H                                                               
*                                                                               
         LLC   RF,PPCLEL05+1       GET ELEMENT LENGTH                           
         LA    R6,PPCLEL05(RF)     BUMP TO NEXT ELEMENT                         
*                                                                               
         B     GTTLOOP                                                          
*                                                                               
GTTDONE  DS    0H                                                               
*                                                                               
GETTOTSX DS    0H                                                               
         XIT1                                                                   
*                                                                               
*                                                                               
*        ROUTINE TO GET POSITIVE AND NEGATIVE TOTALS FOR CD PAYMENT             
*              DONE BY SUMMING DEBITS AND CREDITS SEPARATELY                    
*                                                                               
*        R3==> PPCLEL01                                                         
*        R5==> PPAYELEM                                                         
*                                                                               
         DS 0D                                                                  
GCDTOTS  NTR1  LABEL=*                                                          
*                                                                               
         XC    GCSDEBCD,GCSDEBCD   INIT TOTAL DEBIT  CD                         
         XC    GCSCRDCD,GCSCRDCD   INIT TOTAL CREDIT CD                         
*                                                                               
         ICM   R0,15,PPCSHDSC      GET CASH DISCOUNT PAID                       
         BZ    GCT20                  MUST BE NON-NEGATIVE                      
*                                                                               
         ICM   RF,15,PCL5CD        GET CLEARED NET CD                           
*                                                                               
         CR    R0,RE               SKIP IF NOT EQUAL - MULTIPLE BUYS            
         BNE   GCT20                                                            
*                                                                               
         STCM  R0,15,GCSDEBCD      SET CD TOTALS                                
         STCM  R0,15,GCSCRDCD      SET CD TOTALS                                
*                                                                               
         B     GETTOTSX                                                         
*                                                                               
GCT20    DS    0H                                                               
*                                                                               
         ICM   R6,15,GCSEL01A      POINT TO CURRENT 01 ELEMENT                  
         BZ    GETTOTSX            NONE - GET OUT                               
*                                                                               
         LLC   RF,PPCLEL01+1       GET ELEMENT LENGTH                           
         LA    R6,PPCLEL01(RF)     BUMP TO NEXT ELEMENT                         
*                                                                               
GCDLOOP  DS    0H                                                               
*                                                                               
         CLI   0(R6),0             DONE IF END OF RECORD                        
         BE    GCDDONE                                                          
*                                                                               
         CLI   0(R6),X'01'         DONE IF ANOTHER X'01' ELEMENT                
         BE    GCDDONE                                                          
*                                                                               
         CLI   0(R6),X'05'         MUST BE 05      ELEMENT                      
         BNE   GCDCONT                                                          
*                                                                               
         USING PPCLEL05,R6         ESTABLISH 05 ELEMENT                         
*                                                                               
         LA    R1,GCSDEBCD         POINT TO CURRENT DEBIT TOTAL CD              
*                                                                               
         ICM   RF,15,PCL5CD        GET CLEARED NET CD                           
         BNM   *+8                                                              
         LA    R1,GCSCRDCD            POINT TO CREDIT TOTAL CD IF MINUS         
*                                                                               
         ICM   RE,15,0(R1)         UPDATE TOTAL                                 
         AR    RE,RF                                                            
         STCM  RE,15,0(R1)                                                      
*                                                                               
GCDCONT  DS    0H                                                               
*                                                                               
         LLC   RF,PPCLEL05+1       GET ELEMENT LENGTH                           
         LA    R6,PPCLEL05(RF)     BUMP TO NEXT ELEMENT                         
*                                                                               
         B     GCDLOOP                                                          
*                                                                               
GCDDONE  DS    0H                                                               
*                                                                               
GCDTOTSX DS    0H                                                               
         XIT1                                                                   
*                                                                               
         DROP  RB                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
DSPCODT  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   BUYMD,C'N'                                                       
         BNE   DSPCODTX            ONLY NEED TO THIS FOR MEDIA N                
*                                                                               
         OC    PBDCDATE,PBDCDATE   CLOSE DATE PRESENT?                          
         BZ    DSPCODT5                                                         
*                                                                               
         LA    R1,66(R2)           SEE IF I HAVE ROOM                           
         CR    R4,R1                                                            
         BNH   DSPCODT3            YES                                          
         BRAS  RE,BUMPFLD          NO SKIP TO NEXT LINE                         
         LA    R4,8(R2)                                                         
         FOUT  (R2)                                                             
*                                                                               
DSPCODT3 MVC   0(6,R4),=C'CLOSE-'                                               
         GOTO1 VDATCON,DMCB,(3,PBDCDATE),(5,6(R4))                              
*                                                                               
         AHI   R4,15               ADD LENGTH OF DISPLAY                        
*                                                                               
* MEDIA O AND I NEED ONLY TO DISP OSD                                           
*                                                                               
DSPCODT5 OC    PBDSDATE,PBDSDATE   ON SALE DATE PRESENT?                        
         BZ    DSPCODTX                                                         
*                                                                               
         LA    R1,64(R2)           SEE IF I HAVE ROOM                           
         CR    R4,R1                                                            
         BNH   DSPCODT7            YES                                          
         BRAS  RE,BUMPFLD          NO SKIP TO NEXT LINE                         
         LA    R4,8(R2)                                                         
         FOUT  (R2)                                                             
DSPCODT7 MVC   0(8,R4),=C'ON-SALE-'                                             
         GOTO1 VDATCON,DMCB,(3,PBDSDATE),(5,8(R4))                              
*                                                                               
         AHI   R4,17               ADD LENGTH OF DISPLAY                        
*                                                                               
DSPCODTX J     X_R2R4              NEED TO RETURN FIELD POINTERS                
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
DSPLEGW  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R5,REC+33           POINT TO FIRST ELEM                          
         MVI   ELCODE,X'94'                                                     
         BRAS  RE,NXTELEM          LEGAL WARNING ELEM FOUND IN BUY?             
         BNE   DSPLW60             NO, CHECK IN PRODUCT RECORD                  
*                                                                               
         LA    R1,75(R2)           ENOUGH ROOM? (LW=A1)                         
         CR    R4,R1                                                            
         BNH   DSPLW40             YES                                          
         BRAS  RE,BUMPFLD          NO SKIP TO NEXT LINE                         
         LA    R4,8(R2)                                                         
         FOUT  (R2)                                                             
DSPLW40  MVC   0(3,R4),=C'LW='                                                  
         MVC   3(2,R4),2(R5)       LW CODE AND QUARTERLY CODE                   
*                                                                               
         CLI   2(R5),C'X'          LEGAL WARNING CODE IS X?                     
         BNE   DSPLW45                                                          
         MVC   3(4,R4),=C'NONE'    X=NONE                                       
         AHI   R4,8                LW=NONE AND EXTRA SPACE                      
         B     DSPLEGWX                                                         
*                                                                               
DSPLW45  CLI   3(R5),C'X'          QUARTERLY CODE IS X?                         
         BNE   DSPLW50                                                          
         MVC   4(4,R4),=C'NONE'    X=NONE (QUARTERLY CODE IS X)                 
         AHI   R4,9                LW=ANONE AND EXTRA SPACE                     
         B     DSPLEGWX                                                         
*                                                                               
DSPLW50  AHI   R4,5                ADD LENGTH OF DISPLAY                        
         CLI   3(R5),0                                                          
         BE    DSPLEGWX                                                         
         AHI   R4,1                EXTRA CHAR FOR DIGIT (IF PRESENT)            
         B     DSPLEGWX                                                         
*                                                                               
DSPLW60  MVC   WKSVKEY,KEY         SAVE KEY JUST IN CASE                        
         XC    KEY,KEY                                                          
         MVC   KEY(10),PBUYREC     BUILT KEY TO LOOK UP PRODUCT RECORD          
         MVI   KEY+3,X'06'         PRODUCT RECORD CODE                          
         BRAS  RE,PRT_RDHI                                                      
         CLC   KEY(10),KEYSAVE     PRODUCT FOUND?                               
         BE    DSPLW70                                                          
         MVC   0(07,R4),=C'LW=ERR!'                                             
         AHI   R4,8                LW=ERR! AND EXTRA SPACE                      
         B     DSPLW80                                                          
*                                                                               
DSPLW70  MVC   SV_AREC_,AREC                                                    
         MVC   AREC,AWKAIO1                                                     
         BRAS  RE,PRT_GETR                                                      
         MVC   AREC,SV_AREC_                                                    
         L     R5,AWKAIO1                                                       
         LA    R5,33(R5)                                                        
         MVI   ELCODE,X'40'        LEGAL WARNING ELEM CODE IN PRD               
         BRAS  RE,NXTELEM                                                       
         BNE   DSPLW80             NO LW COMMENT CODES FOUND IN PRD REC         
*                                                                               
         USING PPRDLWEL,R5                                                      
         BRAS  RE,LWQUARTR         QUARTER WILL BE RETURNED IN LWINSDT          
         MVC   0(03,R4),=C'LW='                                                 
*                                                                               
         CLI   LWINSDT,C'1'                                                     
         BNE   DSPLW75D                                                         
         MVC   3(01,R4),PPRDROTA+0                                              
         MVI   4(R4),C'1'                                                       
         B     DSPLW75Z                                                         
*                                                                               
DSPLW75D CLI   LWINSDT,C'2'                                                     
         BNE   DSPLW75E                                                         
         MVC   3(01,R4),PPRDROTA+1                                              
         MVI   4(R4),C'2'                                                       
         B     DSPLW75Z                                                         
*                                                                               
DSPLW75E CLI   LWINSDT,C'3'                                                     
         BNE   DSPLW75F                                                         
         MVC   3(01,R4),PPRDROTA+2                                              
         MVI   4(R4),C'3'                                                       
         B     DSPLW75Z                                                         
*                                                                               
DSPLW75F CLI   LWINSDT,C'4'                                                     
         BNE   DSPLW75X                                                         
         MVC   3(01,R4),PPRDROTA+3                                              
         MVI   4(R4),C'4'                                                       
         B     DSPLW75Z                                                         
*                                                                               
         DROP  R5                                                               
*                                                                               
DSPLW75X DC    H'0'                NOT POSSIBLE!                                
*                                                                               
DSPLW75Z AHI   R4,6                LW=A1 AND EXTRA SPACE                        
*                                                                               
DSPLW80  MVC   KEY(L'WKSVKEY),WKSVKEY                                           
*                                                                               
DSPLEGWX J     X_R2R4              NEED TO RETURN FIELD POINTERS                
*                                                                               
* DETERMING QUARTER FROM INS DATE                                               
*                                                                               
LWQUARTR CLI   PBUYKDAT+1,X'03'    MONTH IS GREATER THAN MARCH?                 
         BH    *+10                                                             
         MVI   LWINSDT,C'1'        QUARTER 1                                    
         BR    RE                                                               
         CLI   PBUYKDAT+1,X'06'    MONTH IS GREATER THAN JUNE?                  
         BH    *+10                                                             
         MVI   LWINSDT,C'2'        QUARTER 2                                    
         BR    RE                                                               
         CLI   PBUYKDAT+1,X'09'    MONTH IS GREATER THAN SEPTEMBER?             
         BH    *+10                                                             
         MVI   LWINSDT,C'3'        QUARTER 3                                    
         BR    RE                                                               
         CLI   PBUYKDAT+1,X'0C'    MONTH IS GREATER THAN DECEMBER?              
         BH    *+10                                                             
         MVI   LWINSDT,C'4'        QUARTER 4                                    
         BR    RE                                                               
         DC    H'0'                GREATER THAN DECEMBER, BAD!                  
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* DISPLAYS NO TRAFFIC STATUS, ACTUAL CPMS, ESTIMATED CPMS                       
*          INTERNET CONTRACT NUMBER, INTERNET SITE LOCATION                     
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
DSPNTRA  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R5,REC+33           POINT TO FIRST ELEM                          
         CLI   0(R5),X'20'                                                      
         BE    *+6                                                              
         DC    H'0'                BUY DESCRIPTION ELEM MUST EXIST!             
*                                                                               
         USING PBDELEM,R5                                                       
         TM    PBDSTAT,X'20'       NO TRAFFIC STATUS IS ON?                     
         BZ    DSPNT60             NO, CK OTHER INFO TO BE DISPLAYED            
         DROP  R5                                                               
*                                                                               
         LA    R1,73(R2)           ENOUGH ROOM? (TRAFF=Y OR TRAFF=N)            
         BRAS  RE,CKDSPLIN                                                      
         MVC   0(6,R4),=C'TRAFF='                                               
         MVI   6(R4),C'N'          TRAFF=N                                      
         AHI   R4,8                7 TOTAL AND AN EXTRA SPACE                   
*                                                                               
DSPNT60  LA    R5,REC+33                                                        
         MVI   ELCODE,X'A0'                                                     
         BRAS  RE,NXTELEM          ESTIMATED CPM ELEM FOUND?                    
         BNE   DSPNT65                                                          
         LA    R1,63(R2)           ENOUGH ROOM? (ECPM=9,999,999.99)             
         BRAS  RE,CKDSPLIN                                                      
         MVC   0(5,R4),=C'ECPM='                                                
         EDIT  (P5,2(R5)),(12,5(R4)),2,ALIGN=LEFT,ZERO=NOBLANK,        +        
               COMMAS=YES                                                       
         AR    R4,R0               ADD DISPLAYED LENGTH FROM EDIT               
         AHI   R4,6                5 TOTAL AND AN EXTRA SPACE                   
*                                                                               
DSPNT65  LA    R5,REC+33                                                        
         MVI   ELCODE,X'A1'                                                     
         BRAS  RE,NXTELEM          ACTUAL CPM ELEM FOUND?                       
         BNE   DSPNT70                                                          
         LA    R1,63(R2)           ENOUGH ROOM? (ACPM=9,999,999.99)             
         BRAS  RE,CKDSPLIN                                                      
         MVC   0(5,R4),=C'ACPM='                                                
         EDIT  (P5,2(R5)),(12,5(R4)),2,ALIGN=LEFT,ZERO=NOBLANK,        +        
               COMMAS=YES                                                       
         AR    R4,R0               ADD DISPLAYED LENGTH FROM EDIT               
         AHI   R4,6                5 TOTAL AND AN EXTRA SPACE                   
*                                                                               
DSPNT70  LA    R5,REC+33                                                        
         MVI   ELCODE,X'97'                                                     
         USING PICONELD,R5                                                      
         BRAS  RE,NXTELEM          INTERNET CONTRACT ELEM FOUND?                
         BNE   DSPNT75                                                          
         LA    R1,(87-8-6)(R2)     LENGTH FOR ICON=(NONE)                       
         BRAS  RE,CKDSPLIN                                                      
         MVC   0(5,R4),=C'ICON='                                                
         CLC   2(8,R5),SPACES      HAVE DISPLAYABLE DATA?                       
         BH    *+18                                                             
         MVC   5(6,R4),=C'(NONE)'  SHOULD NOT HAPPEN (JUST IN CASE)             
         AHI   R4,5+6+1                                                         
         B     DSPNT75                                                          
*                                                                               
         LA    RF,PICCONN+(L'PICCONN-1)                                         
         BRAS  RE,LAST_CHR                                                      
         LR    R6,RF                                                            
         AHI   R6,1                POINT TO CHAR AFTER LAST CHAR                
         LA    RF,PICCONN                                                       
         SR    R6,RF               TOTAL # OF CHARS TO BE DISPLAYED             
         LA    R1,87(R2)                                                        
         SR    R1,R6                                                            
         SHI   R1,5                OVERHEAD                                     
         BRAS  RE,CKDSPLIN                                                      
         CHI   R6,0                                                             
         BH    *+6                                                              
         DC    H'0'                BAD LENGTH (ELEM IS EMPTY)!                  
         BCTR  R6,0                                                             
         EX    R6,*+8                                                           
         B     *+10                                                             
         MVC   5(0,R4),PICCONN     INTERNET CONTRACT #                          
         AHI   R6,1+5+1            1 FROM EX, 5 FOR ICON=, 1 SPACE              
         AR    R4,R6                                                            
*                                                                               
DSPNT75  LA    R5,REC+33                                                        
         MVI   ELCODE,X'98'                                                     
         USING PISITEEL,R5                                                      
         BRAS  RE,NXTELEM          INTERNET SITE LOCATION ELEM FOUND?           
         BNE   DSPNT80                                                          
         SR    R1,R1                                                            
         IC    R1,1(R5)                                                         
         SHI   R1,2                DATA LENGTH                                  
         LA    RF,PISITE                                                        
         AR    RF,R1                                                            
         BCTR  RF,0                POINT TO LAST CHAR IN ELEM                   
         BRAS  RE,LAST_CHR                                                      
         LA    RE,PISITE                                                        
         SR    RF,RE                                                            
         AHI   RF,5+1              TOTAL # OF CHARS TO BE DISPLAYED             
         LR    R6,RF                                                            
         LA    R1,87(R2)           POINT TO LAST CHAR ON DISPLAY LINE           
         SR    R1,R6               ADJUST TO MAX DISPLAYABLE LENGTH             
         BRAS  RE,CKDSPLIN                                                      
         MVC   0(05,R4),=C'SITE='                                               
         SHI   R6,(1+5)            1 FOR EX AND 5 FOR OVERHEAD                  
         CHI   R6,0                                                             
         BNL   *+6                                                              
         DC    H'0'                BAD LENGTH (ELEM IS EMPTY)!                  
         EX    R6,*+8                                                           
         B     *+10                                                             
         MVC   5(0,R4),2(R5)       SITE LOCATION (20 CHARACTERS MAX)            
         AHI   R6,1+5+1            1 FROM EX, 5 FOR SITE=, 1 SPACE              
         AR    R4,R6                                                            
*                                                                               
DSPNT80  DS    0H                  FOR FUTURE USES                              
*                                                                               
DSPNTRAX J     X_R2R4              NEED TO RETURN FIELD POINTERS                
*                                                                               
         DROP  RB,R5                                                            
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
DSPIMPS  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
DPIMPS03 TM    BYTE2,X'80'         IMPRESSION DISPLAYED?                        
         BO    DPIMPS05                                                         
         LA    R5,REC+33           POINT TO FIRST ELEM                          
         MVI   ELCODE,X'92'                                                     
         BRAS  RE,NXTELEM                                                       
         BE    DPIMPS10                                                         
*                                                                               
DPIMPS05 TM    BYTE2,X'40'         ACTUAL IMPRESSION DISPLAYED?                 
         BO    DSPIMPSX                                                         
         LA    R5,REC+33           POINT TO FIRST ELEM                          
         MVI   ELCODE,X'93'                                                     
         BRAS  RE,NXTELEM                                                       
         BNE   DSPIMPSX                                                         
*                                                                               
DPIMPS10 LA    R1,63(R2)           ENOUGH ROOM? (EIMPS=999,999,999)             
         CR    R4,R1                            (AIMPS=999,999,999)             
         BNH   DPIMPS30            YES                                          
         BRAS  RE,BUMPFLD          NO SKIP TO NEXT LINE                         
         LA    R4,8(R2)                                                         
         FOUT  (R2)                                                             
*                                                                               
DPIMPS30 CLI   ELCODE,X'92'        IMPRESSION?                                  
         BNE   DPIMPS35                                                         
         MVC   0(6,R4),=C'EIMPS='                                               
         LA    R1,6(R4)                                                         
         B     DPIMPS40                                                         
*                                                                               
DPIMPS35 CLI   ELCODE,X'93'        ACTUAL IMPRESSION?                           
         BE    *+6                                                              
         DC    H'0'                WRONG ELCODE DETECTED                        
         MVC   0(6,R4),=C'AIMPS='                                               
         LA    R1,6(R4)                                                         
*                                                                               
DPIMPS40 EDIT  (P5,2(R5)),(11,(R1)),0,ALIGN=LEFT,ZERO=NOBLANK,         +        
               COMMAS=YES                                                       
*                                                                               
         CLI   ELCODE,X'92'        IMPRESSION?                                  
         BNE   *+12                                                             
         OI    BYTE2,X'80'         IMPRESSION IS DISPLAYED                      
         B     *+8                                                              
         OI    BYTE2,X'40'         ACTUAL IMPRESSION IS DISPLAYED               
         LA    R4,7(R4)                                                         
         AR    R4,R0               ADD DISPLAYED LENGTH FROM EDIT               
*                                                                               
         B     DPIMPS03            SHOULD NOT REPEAT ITSELF...                  
*                                                                               
DSPIMPSX J     X_R2R4              NEED TO RETURN FIELD POINTERS                
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
DSPEXDT  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R5,REC+33           POINT TO FIRST ELEM                          
         MVI   ELCODE,X'96'                                                     
         BRAS  RE,NXTELEM                                                       
         BNE   DSPEXDTX            NOT FOUND, DONE                              
*                                                                               
         LA    R1,65(R2)           ENOUGH ROOM? (EXDATE=MMMDD/YY)               
         CR    R4,R1                                                            
         BNH   DPEXDT30            YES                                          
         BRAS  RE,BUMPFLD          NO SKIP TO NEXT LINE                         
         LA    R4,8(R2)                                                         
         FOUT  (R2)                                                             
*                                                                               
DPEXDT30 MVC   0(7,R4),=C'EXDATE='                                              
         LA    R4,7(R4)                                                         
*                                                                               
         GOTO1 VDATCON,DMCB,(3,2(R5)),(5,0(R4))                                 
*                                                                               
         AHI   R4,9                DATE(8)+SPACE(1)                             
*                                                                               
DSPEXDTX J     X_R2R4              NEED TO RETURN FIELD POINTERS                
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
DISACT   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   8(13,R2),=CL13'NEW BUY DATE'                                     
         GOTO1 VDATCON,DMCB,(3,PBDBUYDT),(5,36(R2))                             
*                                                                               
         LA    R5,REC+33           DISPLAY PID CREATER                          
         MVI   ELCODE,X'A7'                                                     
         BRAS  RE,NXTELEM                                                       
         BNE   DISA20                                                           
         GOTOR LDAUTH,1            GET PERSON NAME (CREATOR)                    
         MVC   46(33,R2),DUMEL+20                                               
         TM    PBDSTAT2,X'80'      ADDED BY ADBUYER USER?                       
         BZ    *+10                                                             
         MVC   23(9,R2),=C'(ADBUYER)'                                           
         TM    PBDSTAT2,X'20'      ADDED BY IDESK USER?                         
         BZ    *+10                                                             
         MVC   23(7,R2),=C'(IDESK)'                                             
         USING PPIDELM,R5                                                       
         CLI   PPIDPRG,PPIDPRMQ    PRISMA ORIGIN?                               
         JNE   *+10                                                             
         MVC   23(8,R2),=C'(PRISMA)'                                            
         CLI   PPIDPRG,PPIDRADQ    RADIA ORIGIN?                                
         JNE   *+10                                                             
         MVC   23(7,R2),=C'(RADIA)'                                             
         DROP  R5                                                               
         LA    R5,REC+33           PRISMA UPLOAD ELEMENT                        
         MVI   ELCODE,PBYDKELQ                                                  
         BRAS  RE,NXTELEM                                                       
         JNE   DISA10X                                                          
         USING PBYDKELD,R5                                                      
         TM    PBYDKST4,BYPRMIVQ   PRISMA INVOICE ENABLED CAMPAIGN?             
         JZ    DISA10X                                                          
         CLC   23(8,R2),=C'(PRISMA)'                                            
         JNE   *+10                                                             
         MVC   23(10,R2),=C'(PRISMA+I)'                                         
         CLC   23(7,R2),=C'(RADIA)'                                             
         JNE   *+10                                                             
         MVC   23(9,R2),=C'(RADIA+I)'                                           
         DROP  R5                                                               
DISA10X  B     DISA20D                                                          
*                                                                               
DISA20   OC    PBDDATE,PBDDATE     CK FOR ACTIVITY                              
         BNZ   DISA20B                                                          
         MVC   46(3,R2),PBDBUYER   NO ACTIVITY-DISPLAY BUYER HERE               
DISA20B  TM    PBDSTAT2,X'80'      ADDED BY ADBUYER USER?                       
         BZ    *+10                                                             
         MVC   23(9,R2),=C'(ADBUYER)'                                           
         B     DISA20D                                                          
*                                                                               
DISA20D  FOUT  (R2)                                                             
         CLI   PBDRLIND,0                                                       
         BE    DISA20F                                                          
         BRAS  RE,BUMPFLD                                                       
         FOUT  (R2)                                                             
         MVC   8(16,R2),=C'LAST RATE LOOKUP'                                    
         GOTO1 VDATCON,DMCB,(3,PBDRLDT),(5,36(R2))                              
         LA    R8,46(R2)                                                        
         TM    PBDRLIND,X'80'                                                   
         BZ    *+14                                                             
         MVC   0(5,R8),=C'RATE/'                                                
         LA    R8,5(R8)                                                         
         TM    PBDRLIND,X'40'                                                   
         BZ    *+14                                                             
         MVC   0(13,R8),=C'CD AND/OR AC/'                                       
         LA    R8,13(R8)                                                        
         TM    PBDRLIND,X'20'                                                   
         BZ    *+14                                                             
         MVC   0(5,R8),=C'PREM/'                                                
         LA    R8,5(R8)                                                         
         TM    PBDRLIND,X'01'                                                   
         BZ    *+14                                                             
         MVC   0(6,R8),=C'SPACE/'                                               
         LA    R8,6(R8)                                                         
         BCTR  R8,0                                                             
         MVI   0(R8),C' '                                                       
*                                                                               
DISA20F  LA    R5,REC+33           DISPLAY FSI LOOK-UP INFO                     
         MVI   ELCODE,X'82'                                                     
         BRAS  RE,NXTELEM                                                       
         BNE   DISA20U                                                          
         BRAS  RE,BUMPFLD                                                       
         FOUT  (R2)                                                             
         MVC   8(13,R2),=C'FSI LOOKED UP'                                       
         USING PBFSIELD,R5                                                      
         TM    PBFSIIND,X'01'                                                   
         BO    *+10                                                             
         MVC   12(10,R2),=C'OVERRIDDEN'                                         
*                                                                               
         GOTO1 VDATCON,DMCB,(3,PBFSILDT),(5,36(R2))                             
         EDIT  PBFSI,(8,26(R2)),0,ALIGN=LEFT,ZERO=NOBLANK                       
*                                                                               
         GOTOR LDAUTH,0            GET PERSON NAME                              
         MVC   46(33,R2),DUMEL+20                                               
*                                                                               
DISA20U  LA    R5,REC+33           SEARCH FOR UPLOAD ELEM                       
         MVI   ELCODE,X'90'                                                     
         BRAS  RE,NXTELEM                                                       
         BNE   DISA20W                                                          
         BRAS  RE,BUMPFLD                                                       
         FOUT  (R2)                                                             
         MVC   8(10,R2),=C'UPLOAD ID='                                          
         USING PIUPELD,R5                                                       
         MVC   18(8,R2),PIUPUSEQ                                                
         CLI   PIUPELLN,PIUPELXQ   HAVE EXTENDED UNIQUE SEQUENCE#?              
         JL    DISA20W                                                          
         MVC   26(7,R2),PIUPUQXT                                                
*                                                                               
DISA20W  LA    R5,REC+33         SEARCH FOR MATCH ELEM                          
         MVI   ELCODE,X'50'                                                     
         BRAS  RE,NXTELEM                                                       
         BNE   DISA20X                                                          
         BRAS  RE,BUMPFLD                                                       
         FOUT  (R2)                                                             
         MVC   8(07,R2),=C'MATCHED'                                             
         USING PBINVELM,R5                                                      
         GOTO1 VDATCON,DMCB,(3,PBINVMDT),(5,36(R2))                             
         MVC   17(4,R2),=C'INV='                                                
         MVC   21(11,R2),PBINVNUM                                               
         GOTOR LDAUTH,0            GET PERSON NAME                              
         MVC   46(33,R2),DUMEL+20                                               
         B     DISA20Y                                                          
*                                                                               
DISA20X  TM    PBDSTAT,X'40'       CHECK STATUS IF NO MATCHED ELEM              
         BNO   DISA20Y                                                          
         BRAS  RE,BUMPFLD                                                       
         FOUT  (R2)                                                             
         MVC   8(07,R2),=C'MATCHED'                                             
         TM    PBDSTAT,X'10'       TEARSHEET INDICATOR                          
         BNO   DISA20Y                                                          
         MVC   16(3,R2),=C'T/S'                                                 
*                                                                               
DISA20Y  LA    R5,REC+33           SEARCH FOR TEARSHEET ELEM                    
         MVI   ELCODE,X'95'                                                     
         BRAS  RE,NXTELEM                                                       
         BNE   DISA21                                                           
         BRAS  RE,BUMPFLD                                                       
         FOUT  (R2)                                                             
         MVC   8(03,R2),=C'T/S'                                                 
         USING PTSHTEL,R5                                                       
         LA    RF,PTSHCDAT                                                      
         OC    PTSHCDAT,PTSHCDAT   SEE IF I HAVE A CHANGE DATE                  
         BNZ   *+8                                                              
         LA    RF,PTSHIDAT         USE DATE ADDED                               
*                                                                               
         GOTO1 VDATCON,DMCB,(3,0(RF)),(5,36(R2))                                
         MVC   12(7,R2),=C'STATUS='                                             
         MVC   19(1,R2),PTSHSTAT                                                
*                                                                               
         GOTOR LDAUTH,0            GET PERSON NAME                              
         MVC   46(33,R2),DUMEL+20                                               
         CLC   46(11,R2),=11C' '                                                
         BNE   *+10                                                             
         MVC   26(3,R2),PTSHBID                                                 
*                                                                               
DISA21   BRAS  RE,BUMPFLD                                                       
         OC    LASTCELD,LASTCELD   SEE IF CONTINUATION                          
         BZ    DISA21M                                                          
         LA    R5,REC+33                                                        
         MVI   ELCODE,X'24'        FIND NEXT PRIOR CHANGE ELEM                  
         ZIC   R8,CHGELCNT         SET R8 TO NUMBER OF ELEMS TO SKIP            
DISA21C  BRAS  RE,NXTELEM                                                       
         BE    DISA21D                                                          
         XC    LASTCELD,LASTCELD                                                
         MVI   CHGELCNT,0                                                       
         B     DISA21M                                                          
*                                                                               
DISA21D  BCT   R8,DISA21C                                                       
         USING PCHGELD,R5                                                       
         MVC   8(13,R2),=C'ACTIVITY DATE'                                       
         GOTO1 VDATCON,DMCB,(2,PCHGDAT),(5,36(R2))                              
*                                                                               
         GOTOR LDAUTH,0            GET PERSON NAME                              
         MVC   46(33,R2),DUMEL+20                                               
         BRAS  RE,BUMPFLD                                                       
*                                                                               
         B     DISA21V                                                          
*                                                                               
DISA21M  MVC   8(13,R2),=C'LAST ACTIVITY'                                       
         MVC   36(4,R2),=C'NONE'                                                
         FOUT  (R2)                                                             
         OC    PBDDATE,PBDDATE                                                  
         BZ    DISAX                                                            
         MVI   CHGELCNT,0                                                       
         SR    R1,R1                                                            
         LA    R5,REC+33                                                        
         MVI   ELCODE,X'24'                                                     
DISA21P  BRAS  RE,NXTELEM                                                       
         BNE   DISA21Q                                                          
         LA    R1,1(R1)                                                         
         B     DISA21P                                                          
*                                                                               
DISA21Q  STC   R1,CHGELCNT         SAVE COUNT OF X'24' ELEMS                    
         LTR   R1,R1                                                            
         BNZ   DISA21Q5            NO CHG ELEMS                                 
*                                                                               
         CLC   36(4,R2),=C'NONE'                                                
         BNE   DISA21Q4                                                         
         TM    PBUYCNTL,X'80'      SEE IF DELETED                               
         BZ    DISA21Q4                                                         
*                                                                               
         GOTO1 VDATCON,DMCB,(3,PBDDATE),(5,36(R2))                              
         LA    R5,REC+33                                                        
         MVI   ELCODE,X'A7'                                                     
         BRAS  RE,NXTELEM                                                       
         BNE   DISA21Q2                                                         
         GOTOR LDAUTH,0            GET PERSON NAME                              
         MVC   46(33,R2),DUMEL+20                                               
         B     DISA21Q3                                                         
*                                                                               
DISA21Q2 MVC   46(3,R2),PBDBUYER   NO ACTIVITY-DISPLAY BUYER HERE               
*                                                                               
DISA21Q3 BRAS  RE,BUMPFLD                                                       
         LA    R0,36                                                            
         STH   R0,HALF                                                          
         LH    RF,HALF                                                          
         LA    RE,0(R2,RF)                                                      
         MVC   0(7,RE),=C'DELETED'                                              
         FOUT  (R2)                                                             
*                                                                               
DISA21Q4 BRAS  RE,BUMPFLD                                                       
         LA    R0,36                                                            
         STH   R0,HALF                                                          
         B     DISA27              GO TO "NO PRIOR CHGS" MESSAGE                
*                                                                               
DISA21Q5 LA    R5,REC+33                                                        
         MVI   ELCODE,X'24'                                                     
DISA21R  BRAS  RE,NXTELEM                                                       
         BE    DISA21S                                                          
         DC    H'0'                SHOULD NEVER HAPPEN                          
*                                                                               
DISA21S  BCT   R1,DISA21R          SHOULD GET ME TO LAST X'24' ELEM             
*                                                                               
DISA21V  OC    LASTCELD,LASTCELD   DON'T BUMP IF CONTINUATION                   
         BNZ   DISA21W                                                          
*                                                                               
         GOTO1 VDATCON,DMCB,(2,PCHGDAT),(5,36(R2))                              
*                                                                               
         GOTOR LDAUTH,0            GET PERSON NAME                              
         MVC   46(33,R2),DUMEL+20                                               
*                                                                               
         CLC   8(13,R2),=C'LAST ACTIVITY'     IF WE DON'T KNOW                  
         BNE   DISA21V1                       WHO WAS THE LAST                  
         CLC   46(11,R2),=11C' '              ONE WHO CHANGED                   
         BNE   DISA21V1                       INSERTION OUTPUT                  
         MVC   46(3,R2),PBDBUYER              BUYERS INIT.                      
*                                                                               
DISA21V1 ST    R2,FULL                                                          
         BRAS  RE,BUMPFLD                                                       
*                                                                               
DISA21W  ST    R2,PARS                                                          
         LA    R0,36                                                            
         STH   R0,HALF                                                          
         SR    R4,R4                                                            
         IC    R4,PCHGIND1                                                      
         SLL   R4,8                                                             
         IC    R4,PCHGIND2                                                      
         SLL   R4,8                                                             
         IC    R4,PCHGIND3                                                      
         SLL   R4,8                                                             
         IC    R4,PCHGIND4                                                      
         OC    LASTCELD,LASTCELD   SEE IF CONTINUATION                          
         BNZ   DISA22              YES - DON'T DISPLAY DELETED                  
         TM    PBUYCNTL,X'80'      SEE IF DELETED                               
         BZ    DISA22                                                           
*                                                                               
DISA21Z  SR    R4,R4                                                            
         LA    R4,1                                                             
         ZIC   R1,CHGELCNT         BUMP CHGELCNT                                
         AHI   R1,1                SO I WILL REDO LATEST CHG ELEM               
         STC   R1,CHGELCNT                                                      
*                                                                               
DISA22   LA    R6,CHGLIST                                                       
         MVC   LASTCELD,PCHGDAT    DATE OF CHG ELEM DISPLAYED                   
*                                                                               
DISA23   SRDL  R4,1                USES EVEN ODD PAIR                           
         LTR   R5,R5               R5 DOES NOT POINT TO CHGELEM                 
         BNM   DISA24                                                           
         LH    RF,HALF                                                          
         LA    R1,0(R2,RF)                                                      
         MVC   0(16,R1),0(R6)                                                   
         CLC   0(7,R1),=C'DELETED'                                              
         BNE   DISA23C                                                          
*                                                                               
         L     R2,FULL                                                          
*                                                                               
         GOTO1 VDATCON,DMCB,(3,PBDDATE),(5,36(R2))                              
         LA    R5,REC+33                                                        
         MVI   ELCODE,X'A7'                                                     
         BRAS  RE,NXTELEM                                                       
         BNE   DISA23B                                                          
         GOTOR LDAUTH,0            GET PERSON NAME                              
         MVC   46(33,R2),DUMEL+20                                               
         BRAS  RE,BUMPFLD                                                       
         B     DISA23C                                                          
*                                                                               
DISA23B  MVC   46(3,R2),PBDBUYER   NO ACTIVITY-DISPLAY BUYER HERE               
         BRAS  RE,BUMPFLD                                                       
*                                                                               
DISA23C  FOUT  (R2)                                                             
         BRAS  RE,BUMPFLD                                                       
         BNE   DISA24                                                           
         MVC   18(25,R1),=CL25'*FULL ACT. NOT DISPLAYED*'                       
         L     R2,PARS                                                          
         OI    6(R2),X'80'         TRANSMIT                                     
         B     DISAX                                                            
*                                                                               
DISA24   LTR   R4,R4               END                                          
         BZ    DISA26                                                           
         LA    R6,16(R6)                                                        
         B     DISA23                                                           
*                                                                               
DISA26   ZIC   R1,CHGELCNT                                                      
         AHI   R1,-1                                                            
         BNP   DISA27                                                           
         STC   R1,CHGELCNT                                                      
         LH    RF,HALF                                                          
         LA    RE,0(R2,RF)                                                      
         MVC   0(25,RE),=CL25'*HIT ENTER FOR PRIOR CHG*'                        
         FOUT  BUYTR1H,=C'RA',2                                                 
         OI    BUYTR1H+1,X'01'     SET MODIFIED                                 
         FOUT  (R2)                                                             
         B     DISAX                                                            
*                                                                               
DISA27   XC    LASTCELD,LASTCELD   LAST ELEM DISPLAYED                          
         MVI   CHGELCNT,0          CLEAR SO NEXT "RA" WILL DO LATEST            
         LH    RF,HALF                                                          
         LA    RE,0(R2,RF)                                                      
         MVC   0(19,RE),=CL19'*NO PRIOR CHANGES*'                               
         FOUT  (R2)                                                             
         B     DISAX                                                            
*                                                                               
DISAX    J     EXIT                                                             
*                                                                               
CHGLIST  DS    0C                                                               
         DC    CL16'DELETED         '    PCHGIND4 X'01' (RESERVED)              
         DC    CL16'ENHANCED IO SENT'             X'02'                         
         DC    CL16'SRC COMMENT     '             X'04'                         
         DC    CL16'LEGAL WARNING   '             X'08'                         
         DC    CL16'ADDITIONAL CHRGS'             X'10'                         
         DC    CL16'IMPS,PVS OR CTS '             X'20'                         
         DC    CL16'COST2           '             X'40'                         
         DC    CL16'SFH STATUS      '             X'80'                         
*                                                                               
         DC    CL16'POSITION INSTRNS'    PCHGIND3 X'01'                         
         DC    CL16'MAT. CLOSE DATE '             X'02'                         
         DC    CL16'MADE LIVE       '             X'04'                         
         DC    CL16'TAX CHANGE      '             X'08'                         
         DC    CL16'PLANNED COST    '             X'10'                         
         DC    CL16'REP CHANGE      '             X'20'                         
         DC    CL16'AD NUMBER ADDED '             X'40'                         
         DC    CL16'2ND INS. DATE   '             X'80'                         
*                                                                               
         DC    CL16'INS. ORDER DATE '    PCHGIND2 X'01'                         
         DC    CL16'CASH DISCOUNT   '             X'02'                         
         DC    CL16'AGENCY COMM     '             X'04'                         
         DC    CL16'AD NUMBER       '             X'08'                         
         DC    CL16'PAYABLE DATE    '             X'10'                         
         DC    CL16'BILLABLE DATE   '             X'20'                         
         DC    CL16'SALE DATE       '             X'40'                         
         DC    CL16'CLOSE DATE      '             X'80'                         
*                                                                               
         DC    CL16'INS. ORDER COMM '    PCHGIND1 X'01'                         
         DC    CL16'COMMENT         '             X'02'                         
         DC    CL16'PREMIUM         '             X'04'                         
         DC    CL16'INSERTION DATE  '             X'08'                         
         DC    CL16'SPACE DESC.     '             X'10'                         
         DC    CL16'UNITS           '             X'20'                         
         DC    CL16'RATE            '             X'40'                         
         DC    CL16'POOL ALLOCATION '             X'80'                         
*                                                                               
         DROP  RB,R5                                                            
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* DISPLAY INVOICE NUMBER, DETAIL NUMBER, SERIAL NUMBER (DDS ONLY),              
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
DISINV   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         OC    INVELCNT,INVELCNT   IF ITS CLEAR                                 
         BNZ   *+8                                                              
         MVI   INVELCNT,X'01'      MOVE 1 TO COUNT                              
*                                                                               
         CLC   INVELDTE,BUYDT1     CHECK IF DATE CHANGED                        
         BE    *+8                                                              
         MVI   INVELCNT,X'01'      START FROM FIRST ELEM                        
*                                                                               
         MVC   30(27,R2),=C'INVOICE INFORMATION ELEMENT'                        
         FOUT  (R2)                                                             
         BRAS  RE,BUMPFLD          BUPM TO THE NEXT LINE                        
*                                                                               
         LA    R5,REC+33           DISPLAY INVOICE INFO                         
*                                                                               
         CLC   BUYNM(3),=C'TST'    MOVE 01 INTO BYTE2 TO INDICATE THAT          
         BNE   *+8                 BUYER IS "TST" DDS ONLY                      
         OI    BYTE2,X'01'                                                      
*                                                                               
         ZIC   R4,INVELCNT         MOVE COUNT TO R1                             
*                                                                               
DISINV05 DS    0H                                                               
*                                                                               
         MVI   ELCODE,X'51'        MOVE "51" CODE TO FIND --->                  
         BRAS  RE,NXTELEM          NEW INVOICE ELEM                             
         BNE   DISINV90                                                         
*                                                                               
         BCT   R4,DISINV05                                                      
*                                                                               
         USING PBNVELMD,R5         DSECT INV ELEM                               
*                                                                               
         MVC   8(17,R2),=C'INVOICE NUMBER  :'                                   
         MVC   27(14,R2),PBNVINV#  OUTPUT INVOICE #                             
         FOUT  (R2)                                                             
         BRAS  RE,BUMPFLD                                                       
*                                                                               
         MVC   8(17,R2),=C'DETAIL SEQ #    :'                                   
         EDIT  PBNVDSQN,(5,27(R2)),0,ALIGN=LEFT                                 
         FOUT  (R2)                                                             
         BRAS  RE,BUMPFLD                                                       
*                                                                               
         TM    BYTE2,X'01'          CHECK IF TST                                
         BZ    DISINV10                                                         
*                                                                               
*                                                                               
*                                                                               
         MVC   8(17,R2),=C'INVOICE SERIAL# :'                                   
         XC    WORK,WORK           INIT WORK AREA                               
         UNPK  WORK(2*L'PBNVSER#+1),PBNVSER#(L'PBNVSER#+1) UNPACK               
         MVI   WORK+2*L'PBNVSER#,C' ' KILL EXTRA BYTE                           
         MVC   27(2*L'PBNVSER#,R2),WORK   DISPLAY SERIAL NUMBER                 
         FOUT  (R2)                                                             
         BRAS  RE,BUMPFLD                                                       
*                                                                               
DISINV10 DS    0H                                                               
*                                                                               
         MVC   8(17,R2),=C'MATCHING STATUS :'                                   
         LA    R4,MTCHSTAT                                                      
         USING MTCHSTTD,R4                                                      
*                                                                               
DISINV20 DS    0H                                                               
         CLI   MTCHCODE,X'FF'                                                   
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   MTCHCODE,PBNVMTCH                                                
         BE    DISINV30                                                         
         LA    R4,MTCHLENQ(R4)                                                  
         B     DISINV20                                                         
*                                                                               
DISINV30 DS    0H                                                               
         MVC   27(L'MTCHNAME,R2),MTCHNAME DISPLAY NAME OF STATUS                
         FOUT  (R2)                                                             
         BRAS  RE,BUMPFLD                                                       
*                                                                               
         DROP  R4                                                               
*                                                                               
         MVC   8(17,R2),=C'DISCREPANCY STAT:'                                   
         LA    R4,DISCSTAT                                                      
         USING DISCSTTD,R4                                                      
*                                                                               
DISINV40 DS    0H                                                               
         CLI   DISCCODE,X'FF'                                                   
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   DISCCODE,PBNVMISC                                                
         BE    DISINV50                                                         
         LA    R4,DISCLENQ(R4)                                                  
         B     DISINV40                                                         
*                                                                               
DISINV50 DS    0H                                                               
         MVC   27(L'DISCNAME,R2),DISCNAME DISPLAY NAME OF STATUS                
         FOUT  (R2)                                                             
         BRAS  RE,BUMPFLD                                                       
         DROP  R4                                                               
*                                                                               
         BRAS  RE,NXTELEM          NEW INVOICE ELEM                             
         BNE   DISINV91                                                         
*                                                                               
         MVC   27(36,R2),=C' HIT ENTER FOR MORE LINKED INVOICES '               
         FOUT  BUYTR1H,=C'RY',2                                                 
         OI    BUYTR1H+1,X'01'     SET MODIFIED                                 
         FOUT  (R2)                                                             
*                                                                               
         ZIC   R4,INVELCNT                                                      
         AHI   R4,1                INCREASE COUNT BY 1                          
         STC   R4,INVELCNT         STORE IT                                     
         MVC   INVELDTE,BUYDT1                                                  
*                                                                               
         B     DISINVX                                                          
*                                                                               
DISINV90 DS    0H                                                               
         CLI   INVELCNT,X'01'                                                   
         BE    DISINV95                                                         
*                                                                               
DISINV91 DS    0H                                                               
         BRAS  RE,BUMPFLD                                                       
         MVC   27(36,R2),=C'NO MORE INVOICES LINKED TO INSERTION'               
         FOUT  (R2)                                                             
         XC    INVELCNT,INVELCNT    CLEAR COUNT                                 
         B     DISINVX                                                          
*                                                                               
DISINV95 DS    0H                                                               
         BRAS  RE,BUMPFLD                                                       
         MVC   27(36,R2),=C'** NO INVOICE LINKED TO INSERTION **'               
         FOUT  (R2)                                                             
         XC    INVELCNT,INVELCNT    CLEAR COUNT                                 
         B     DISINVX                                                          
*                                                                               
MTCHSTAT DS    0H                                                               
         DC    X'00',CL10'NO INVOICE'                                           
         DC    C'P',CL10'PENDING   '                                            
         DC    C'M',CL10'MATCHED   '                                            
         DC    C'D',CL10'DISCREPANT'                                            
         DC    X'FF'                                                            
*                                                                               
DISCSTAT DS    0H                                                               
         DC    X'00',CL14'NOT APPLICABLE'                                       
         DC    C'P',CL14'NEEDS REVIEW'                                          
         DC    C'M',CL14'REVIEWING'                                             
         DC    C'D',CL14'RESOLVED'                                              
         DC    X'FF'                                                            
*                                                                               
DISINVX  J     EXIT                                                             
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
DSPW     NTR1  BASE=*,LABEL=*                                                   
*                                  DISPLAY WARNING MSGS                         
         L     R6,AJOBIO                                                        
         USING PJOBRECD,R6                                                      
         TM    WARN,X'80'          SPACE WARNING                                
         BZ    DSPWA                                                            
         MVC   8(L'SPCWRN,R2),SPCWRN                                            
*                                                                               
         CLI   3(R6),X'15'         SEE IF JOBREC IS THERE                       
         BNE   DSPWS6              MAY HAVE BEEN CLOBBERED BY GETADVC           
*                                                                               
         FOUT  (R2)                                                             
         BRAS  RE,BUMPFLD                                                       
         MVC   20(17,R2),=C'AD RECORD SPACE ='                                  
         CLI   PBUYKMED,C'N'                                                    
         BE    DSPWS2                                                           
DSPWS1   MVC   38(17,R2),PJOBSPC                                                
         B     DSPWS6                                                           
*                                                                               
DSPWS2   XC    X(25),X                                                          
         LA    R5,X                                                             
         CLI   PJOBSPC,C' '        SEE IF JOB HAS SPACE                         
         BNH   DSPWS2D                                                          
         B     DSPWS2F                                                          
*                                                                               
DSPWS2D  CLC   PBDSPACE(2),C_7B00  X'7B00'(SPECIAL NO ASC BUY)?                 
         BE    DSPWS3              TREAT IT AS NON-SPACE BUY                    
         CLC   PBDSPACE(2),C_7B40  X'7B40'(SPECIAL NO ASC BUY)?                 
         BE    DSPWS3              TREAT IT AS NON-SPACE BUY                    
         CLC   PBDSPACE(2),C_5C40  X'5C40'(SPACE BUY)?                          
         BNH   DSPWS3              IF NOT DO LINES AND COLS                     
DSPWS2F  MVC   X(8),PJOBSPC                                                     
         LA    R5,8(R5)                                                         
         CP    PJOBTUNS,=P'0'      SEE IF JOB ALSO HAS UNITS                    
         BE    DSPW4               NO                                           
         LA    R5,2(R5)                                                         
*                                                                               
DSPWS3   ZAP   DUB,PJOBTUNS                                                     
         CLI   PJOBUIND,X'89'      SEE IF INCHES WITH 2 DECIMALS                
         BNE   DSPWS3C                                                          
         BRAS  RE,WSPWED2          2 DECIMALS                                   
         B     DSPWS3D                                                          
DSPWS3C  BRAS  RE,WSPWED                                                        
*                                                                               
DSPWS3D  AR    R5,R0                                                            
         CLI   PJOBUIND,C'I'                                                    
         BNE   DSPWS3G                                                          
         MVI   0(R5),C'I'                                                       
         LA    R5,1(R5)                                                         
         B     DSPWS3H                                                          
*                                                                               
DSPWS3G  CLI   PJOBUIND,X'89'      INCHES WITH 2 DECIMALS                       
         BNE   DSPWS3H                                                          
         MVI   0(R5),C'I'                                                       
         LA    R5,1(R5)                                                         
*                                                                               
DSPWS3H  CP    PJOBCOLS,=P'0'                                                   
         BE    DSPW4                                                            
         MVI   0(R5),C'/'                                                       
         LA    R5,1(R5)                                                         
         ZAP   DUB,PJOBCOLS                                                     
         BRAS  RE,WSPWED                                                        
         AR    R5,R0                                                            
*                                                                               
DSPW4    CLI   PJOBPRM,C' '                                                     
         BNH   *+14                                                             
         MVC   2(1,R5),PJOBPRM                                                  
         MVI   3(R5),C'C'                                                       
         MVC   38(25,R2),X                                                      
*                                                                               
DSPWS6   FOUT  (R2)                                                             
         BRAS  RE,BUMPFLD                                                       
*                                                                               
DSPWA    TM    WARN,X'40'          ALO WARNING                                  
         BZ    DSPWD                                                            
         MVC   8(L'ALOWRN,R2),ALOWRN                                            
*                                                                               
         CLI   3(R6),X'15'         SEE IF JOBREC IS THERE                       
         BNE   DSPWAX              MAY HAVE BEEN CLOBBERED BY GETADVC           
*                                                                               
         FOUT  (R2)                                                             
         BRAS  RE,BUMPFLD                                                       
         MVC   20(22,R2),=C'AD RECORD ALLOCATION ='                             
         MVC   43(35,R2),PJOBALO                                                
*                                                                               
DSPWAX   FOUT  (R2)                                                             
         BRAS  RE,BUMPFLD                                                       
*                                                                               
DSPWD    TM    WARN,X'20'          DELETED BUY WARNING                          
         BZ    DSPW7                                                            
         MVC   8(L'DELWRN,R2),DELWRN                                            
         FOUT (R2)                                                              
         BRAS  RE,BUMPFLD                                                       
*                                                                               
DSPW7    TM    WARN,X'10'          EXCLUSION CLASS WARNING                      
         BO    DSPW7S                                                           
*                                                                               
         CLI   BYPROF+6,C'W'                                                    
         BNE   DSPW8               NO WARNING MSG IS NEEDED                     
*                                                                               
         MVC   WORK(L'KEY),KEY     SAVE OFF KEY (JUST IN CASE)                  
         XC    KEY,KEY                                                          
         MVC   KEY+00(01),PBUYKMED                                              
         MVC   KEY+01(06),BPUB     PUB/ZONE/EDITION                             
         MVC   KEY+07(02),AGYALPHA                                              
         MVI   KEY+09,X'81'                                                     
         BRAS  RE,PUB_RDHI                                                      
         BRAS  RE,PUB_GETR                                                      
*                                                                               
         MVC   KEY,WORK            RESTORE KEY                                  
         L     R5,APUBIO                                                        
         LA    R5,33(R5)           POINT TO ELEMENTS                            
         MVI   ELCODE,X'20'                                                     
         BRAS  RE,NXTELEM          LOOKING FOR PRODUCTION ELEM                  
         BNE   DSPW8               NOT FOUND, DONE WITH EXCL CLASS              
         USING PUBGENEL,R5                                                      
         CLI   PUBEXCL,0                                                        
         BE    DSPW8               NO EXCL CLASS, DONE                          
         MVC   BYTE,PUBEXCL                                                     
         NC    BYTE,SVPEXCL                                                     
         BZ    DSPW8               NO CONFLICTS IN EXCL CLASS, DONE             
         DROP  R5                                                               
*                                                                               
         MVC   8(L'EXCLWRN2,R2),EXCLWRN2                                        
         FOUT (R2)                                                              
         BRAS  RE,BUMPFLD                                                       
         B     DSPW8                                                            
*                                                                               
DSPW7S   MVC   8(L'EXCLWRN1,R2),EXCLWRN1                                        
         FOUT (R2)                                                              
         BRAS  RE,BUMPFLD                                                       
*                                                                               
DSPW8    CLI   PBUYKMED,C'N'       CLE WARNING                                  
         BNE   DSPW9                                                            
         CLC   PBDSPACE(2),C_7B00  X'7B00'(SPECIAL NO ASC BUY)?                 
         BE    DSPW9               TREAT IT AS NON-SPACE BUY                    
         CLC   PBDSPACE(2),C_7B40  X'7B40'(SPECIAL NO ASC BUY)?                 
         BE    DSPW9               TREAT IT AS NON-SPACE BUY                    
         CLC   PBDSPACE(2),C_5C40  X'5C40'(SPACE BUY)?                          
         BNH   DSPW9               TREAT IT AS NON-SPACE BUY                    
         CP    PBDUNITS,=P'0'                                                   
         BNE   DSPW9                                                            
         MVC   8(L'CLEWRN,R2),CLEWRN                                            
         FOUT  (R2)                                                             
         BRAS  RE,BUMPFLD                                                       
*                                                                               
DSPW9    J     X_R2                R2 IS FIELD POINTER                          
         DROP  R6                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
WSPWED   EDIT  (P8,DUB),(5,0(R5)),ALIGN=LEFT                                    
         BR    RE                                                               
*                                                                               
WSPWED2  EDIT  (P8,DUB),(6,0(R5)),2,ALIGN=LEFT                                  
         BR    RE                                                               
*                                                                               
SPCWRN   DC    C'**WARNING - SPACE DESC. CONFLICTS WITH AD RECORD'              
ALOWRN   DC    C'**WARNING - PRODUCT ALLOC. CONFLICTS WITH AD RECORD'           
DELWRN   DC    C'**NOTE- BUY IS DELETED**'                                      
CLEWRN   DC    C'** WARNING - NO CONTRACT LINEAGE EQUIVALENT'                   
EXCLWRN1 DC    C'** WARNING - EXCLUSION CLASS CONFLICT, BUY ADDED'              
EXCLWRN2 DC    C'** WARNING - EXCLUSION CLASS CONFLICT'                         
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
LDAUTH   NTR1  BASE=*,LABEL=*      GET PERSONS NAME, R5 POINTS TO PID           
*                                                                               
         XC    DUMEL+20(50),DUMEL+20                                            
*                                                                               
         OC    0(2,R5),0(R5)       HAVE PID?                                    
         BZ    LDAUTH4                                                          
*                                                                               
         CLC   0(2,R5),=X'240B'    SHORT CHG ELEM?                              
         BNE   *+12                                                             
         LA    R5,PCHGOLDS(R5)                                                  
         B     LDAUTH0                                                          
*                                                                               
         CLC   0(2,R5),=X'2417'    LONG CHG ELEM?                               
         BNE   *+12                                                             
         LA    R5,PCHGOLDL(R5)                                                  
         B     LDAUTH0                                                          
*                                                                               
         CLI   0(R5),PBINVELQ      MATCH ELEM PID?                              
         BNE   *+12                                                             
         LA    R5,PBINVPID-PBINVELM(R5)                                         
         B     LDAUTH0                                                          
*                                                                               
         CLI   0(R5),PTSHTELQ      T/S ELEM PID?                                
         BNE   *+12                                                             
         LA    R5,PTSHPID-PTSHTEL(R5)                                           
         B     LDAUTH0                                                          
*                                                                               
         CLI   0(R5),PBFSIELQ      FSI ELEM PID?                                
         BNE   *+12                                                             
         LA    R5,PBFSIPID-PBFSIEL(R5)                                          
         B     LDAUTH0                                                          
*                                                                               
         CLC   0(2,R5),=X'A704'    PID ELEM (OLD FORMAT)?                       
         BNE   *+12                                                             
         LA    R5,PPIDADD-PPIDELM(R5)                                           
         B     LDAUTH0                                                          
*                                                                               
*NOP*    CLC   0(2,R5),=X'A706'    PID ELEM (NEW FORMAT)?                       
         CLI   0(R5),X'A7'         PID ELEM (GT 4 BYTES) ?                      
         BNE   LDAUTH4                                                          
         CLC   4(2,R5),=X'0000'                                                 
         BNE   *+12                                                             
         LA    R5,PPIDADD-PPIDELM(R5)                                           
         B     LDAUTH0                                                          
         CHI   R1,1                GET CREATOR NAME?                            
         BE    *-12                                                             
         LA    R5,PPIDDEL-PPIDELM(R5)                                           
*                                                                               
LDAUTH0  OC    0(2,R5),0(R5)       HAVE PID?                                    
         BZ    LDAUTH4                                                          
*                                                                               
         XC    WORK(25),WORK                                                    
         MVI   WORK,C'0'                                                        
         MVC   WORK+1(2),SVSECAGY  SET SECURITY AGENCY                          
         CLC   SVSECAGY,=2C' '                                                  
         BH    *+10                                                             
         MVC   WORK+1(2),AGYALPHA                                               
         MVC   WORK+23(2),0(R5)                                                 
*                                                                               
LDAUTH1  GOTO1 VDATAMGR,DMCB,=C'DMRDHI',=C'CTFILE',WORK,AWRKREC                 
         L     R8,AWRKREC                                                       
         CLC   WORK(25),0(R8)                                                   
         BNE   LDAUTH4                                                          
         LA    RE,28(R8)                                                        
*                                                                               
LDAUTH2  CLC   =X'C30A',0(RE)      - NEW SECURITY - PERSON ELEMENT              
         BE    LDAUTH3                                                          
         ZIC   R0,1(RE)                                                         
         AR    RE,R0                                                            
         CLI   0(RE),0                                                          
         BNE   LDAUTH2                                                          
         B     LDAUTH4                                                          
*                                                                               
LDAUTH2A XC    WORK(25),WORK                                                    
         MVI   WORK,C'0'                                                        
         MVC   WORK+1(2),SVSECAGY  SET SECURITY AGENCY                          
         CLC   SVSECAGY,=2C' '                                                  
         BH    *+10                                                             
         MVC   WORK+1(2),AGYALPHA                                               
         MVC   WORK+15(10),2(RE)                                                
         B     LDAUTH1                                                          
*                                                                               
*                                                                               
LDAUTH3  XC    WORK(25),WORK                                                    
         MVI   WORK,C'F'           GET PERSON RECORD                            
         MVI   WORK+1,X'04'                                                     
         MVC   WORK+13(2),SVSECAGY SET SECURITY AGENCY                          
         CLC   SVSECAGY,=2C' '                                                  
         BH    *+10                                                             
         MVC   WORK+13(2),AGYALPHA                                              
         MVC   WORK+15(8),2(RE)                                                 
         GOTO1 VDATAMGR,DMCB,=C'DMRDHI',=C'CTFILE',WORK,AWRKREC                 
         L     R8,AWRKREC                                                       
         CLC   WORK(23),0(R8)                                                   
         BNE   LDAUTH8                                                          
         LA    RE,28(R8)                                                        
*                                                                               
LDAUTH3D CLI   0(RE),X'C5'                                                      
         BE    LDAUTH3C                                                         
         ZIC   R0,1(RE)                                                         
         AR    RE,R0                                                            
         CLI   0(RE),0                                                          
         BNE   LDAUTH3D                                                         
         B     LDAUTH8                                                          
*                                                                               
LDAUTH3A CLI   0(RE),X'C6'                                                      
         BE    LDAUTH3B                                                         
         ZIC   R0,1(RE)                                                         
         AR    RE,R0                                                            
         CLI   0(RE),0                                                          
         BNE   LDAUTH3A                                                         
         B     LDAUTH8                                                          
*                                                                               
LDAUTH3B MVC   DUMEL+20(2),2(RE)   OFFICE                                       
         B     LDAUTH8                                                          
*                                                                               
LDAUTH3C XC    DUMEL+20(50),DUMEL+20                                            
         LA    RF,11(RE)                                                        
         ZIC   R1,0(RF)                                                         
         AHI   R1,-1                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   DUMEL+20(0),1(RF)                                                
         ZIC   R1,0(RF)                                                         
         AR    RF,R1                                                            
*                                                                               
         TM    10(RE),X'40'                                                     
         BZ    LDAUTH3E                                                         
*                                                                               
         LA    RF,1(RF)                                                         
         ZIC   R1,0(RF)                                                         
         AR    RF,R1                                                            
*                                                                               
LDAUTH3E LA    RF,1(RF)            LAST NAME                                    
         ZIC   R1,0(RF)                                                         
         AHI   R1,-1                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   DUMEL+40(0),1(RF)                                                
         B     LDAUTH8                                                          
*                                                                               
LDAUTH4  MVC   DUMEL+20(5),=C'<P16>'                                            
         B     LDAUTH8                                                          
*                                                                               
LDAUTH6  MVC   DUMEL+20(2),2(RE)   OFFICE                                       
         MVC   DUMEL+23(18),4(RE)  LAST NAME                                    
         MVC   DUMEL+42(1),22(RE)  FIRST INIT                                   
         MVC   DUMEL+44(1),23(RE)                                               
*                                                                               
LDAUTH8  MVC   DMCB+4(4),=X'D9000A0D'           LOAD SQUASHER                   
         GOTO1 VCALLOV,DMCB,0                                                   
         L     RF,DMCB                                                          
         GOTO1 (RF),(R1),DUMEL+20,40            SQUASHER CALL                   
EXIT1    J     EXIT                                                             
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
WEBIODIS NTR1  BASE=*,LABEL=*      DISPLAY ENHANCED INSERTION ORDERS            
*                                                                               
         MVI   ELCODE,X'71'                                                     
         LA    R5,REC+33                                                        
         BRAS  RE,NXTELEM                                                       
         BNE   WIODEQ              DONE - NO WEBIO INS ORDERS                   
*                                                                               
         ST    R2,PARS                                                          
*                                                                               
         MVI   ELCODE,X'71'                                                     
         LA    R5,REC+33                                                        
WIOD52   BRAS  RE,NXTELEM                                                       
         BNE   WIODEQ              DONE                                         
         USING PWIOELD,R5                                                       
         OC    PWIODATE,PWIODATE                                                
         BZ    WIOD52                                                           
         ST    R5,PARS+4           SAVE A(LAST IO ELEM)                         
         C     R2,PARS             FIRST TIME                                   
         BNE   WIOD54                                                           
         MVC   8(L'IORHD,R2),IORHD                                              
*                                                                               
WIOD54   LA    R8,L'IORHD+9(R2)                                                 
         GOTO1 VDATCON,DMCB,(3,PWIODATE),(5,(R8))                               
*                                                                               
         LA    R8,9(R8)                                                         
*                                                                               
         MVC   0(1,R8),PBUYKMED    SET MEDIA                                    
*                                                                               
         MVI   1(R8),C'-'                                                       
         AHI   R8,2                BUMP TO NEXT POSITION                        
*                                                                               
         SR    RF,RF                                                            
         IC    RF,PWIO#YER         GET IO# YEAR                                 
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'         FORCE SIGN                                   
         UNPK  0(2,R8),DUB         SET YEAR                                     
*                                                                               
         AHI   R8,2                BUMP TO CLIENT PART                          
         MVC   0(3,R8),PBUYKCLT    SET CLIENT                                   
         AHI   R8,2                BUMP POINTER                                 
         CLI   0(R8),C' '          IF EMPTY                                     
         BH    *+8                                                              
         MVI   0(R8),C'-'             FILL IN WITH DASH                         
*                                                                               
         AHI   R8,1                 BUMP PAST LAST OF CODE                      
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,7,PWIO#SQ#       GET SEQUENCE NUMBER                          
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'         FORCE SIGN                                   
         UNPK  0(4,R8),DUB         SEQUENCE NUMBER (4 DIGITS)                   
         CHI   RF,9999                                                          
         BNH   *+14                                                             
         UNPK  0(5,R8),DUB         SEQUENCE NUMBER (5 DIGITS)                   
         AHI   R8,1                                                             
*                                                                               
         AHI   R8,4                NEXT OUTPUT AREA                             
*                                                                               
         OC    PWIO#REV,PWIO#REV   SKIP IF NO REVISION NUMBER                   
         BNZ   WIOD54D                                                          
         AHI   R8,9                NEXT OUTPUT AREA                             
         B     WIOD54X                                                          
*                                                                               
WIOD54D  DS    0H                                                               
*                                                                               
         MVI   0(R8),C'-'          SET DASH                                     
*                                                                               
         AHI   R8,1                NEXT OUTPUT AREA                             
*                                                                               
         MVC   0(3,R8),=C'REV'     REVISION INDICATOR                           
         AHI   R8,3                                                             
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,PWIO#REV       GET REVISION NUMBER                          
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'         FORCE SIGN                                   
         UNPK  0(3,R8),DUB         SEQUENCE NUMBER                              
*                                                                               
         AHI   R8,5                NEXT OUTPUT AREA                             
*                                                                               
WIOD54X  DS    0H                                                               
*                                                                               
         MVC   0(3,R8),=C'CAN'                                                  
         CLI   PWIOMODC,C'D'                                                    
         BE    WIOD55                                                           
         MVC   0(3,R8),=C'CHA'                                                  
         CLI   PWIOMODC,C'C'                                                    
         BE    WIOD55                                                           
         MVC   0(3,R8),=C'UNC'                                                  
         CLI   PWIOMODC,C'U'                                                    
         BE    WIOD55                                                           
         MVC   0(3,R8),=C'NEW'                                                  
         CLI   PWIOMODC,C'N'                                                    
         BE    WIOD55                                                           
         XC    0(3,R8),0(R8)                                                    
         B     WIOD57              NO MODIFICATION CODE FOUND                   
*                                                                               
WIOD55   DS    0H                                                               
         LA    R8,4(R8)                                                         
*                                                                               
WIOD57   DS    0H                                                               
*NOP*    OC    PWIOCNDT,PWIOCNDT   CK FOR CONTROL DATE                          
*NOP*    BZ    WIOD58                                                           
*NOP*    MVC   0(4,R8),=C'CDT='                                                 
*NOP*    GOTO1 VDATCON,DMCB,(3,PWIOCNDT),(5,4(R8))                              
*NOP*    LA    R8,10(R8)                                                        
*                                                                               
WIOD58   CLI   PWIOLWCD,0          ANYTHING IN LEGAL WARNING CODE?              
         BE    WIOD58B             NO, CHECK QUARTERLY CODE                     
         MVC   0(3,R8),=C'LW='                                                  
         MVC   3(1,R8),PWIOLWCD                                                 
         CLI   PWIOQUCD,0          ANYTHING IN QUARTERLY CODE?                  
         BE    WIOD58X             NO, CHECK QUARTERLY CODE                     
         MVC   4(1,R8),PWIOQUCD                                                 
         B     WIOD58X                                                          
*                                                                               
WIOD58B  CLI   PWIOQUCD,0          QUARTERLY CODE ONLY?                         
         BE    WIOD58X             BOTH CODES ARE NOT FOUND, DONE               
         MVC   0(3,R8),=C'LW='                                                  
         MVC   3(1,R8),PWIOQUCD                                                 
*                                                                               
WIOD58X  FOUT  (R2)                END OF WEB RI DISPLAY                        
*                                                                               
         BRAS  RE,BUMPFLD                                                       
         BNE   WIOD52                                                           
         B     WIODNE              NO MORE ROOM                                 
*                                                                               
WIODEQ   CR    RB,RB               EQUAL                                        
         B     *+6                                                              
WIODNE   LTR   RB,RB               NOT EQUAL                                    
         J     X_R2                                                             
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
DSPADID  NTR1  BASE=*,LABEL=*      DISPLAY AD-ID                                
*                                                                               
         MVC   WKSVKEY,KEY         SAVE KEY JUST IN CASE                        
         XC    KEY,KEY                                                          
         MVC   KEY(10),PBUYREC     BUILD KEY TO LOOK UP JOB RECORD              
         MVI   KEY+3,X'15'         JOBREC RECORD CODE                           
         MVC   KEY+10(6),PBDJOB    JOB CODE                                     
         BRAS  RE,PRT_RDHI                                                      
         CLC   KEY(16),KEYSAVE     JOB RECORD FOUND ?                           
         BE    DSPID20             YES                                          
         LA    R1,75(R2)           ENOUGH ROOM? (ADID=ABCD12345678)             
         CR    R4,R1                                                            
         BNH   DSPID10             YES                                          
         BRAS  RE,BUMPFLD          NO SKIP TO NEXT LINE                         
         LA    R4,8(R2)                                                         
         FOUT  (R2)                                                             
DSPID10  DS    0H                                                               
         MVC   0(22,R4),=C'*JOB RECORD NOT FOUND*'                              
         AHI   R4,23               ABOVE MESSAGE AND EXTRA SPACE                
         B     DSPID80                                                          
*                                                                               
DSPID20  DS    0H                                                               
         L     R5,AJOBIO                                                        
         CLC   KEY(16),0(R5)       ALREADY HAVE JOB RECORD ?                    
         BE    DSPID25             YES                                          
         ST    R5,AREC                                                          
         BRAS  RE,PRT_GETR                                                      
         LA    RE,REC                                                           
         ST    RE,AREC             RESTORE                                      
*                                                                               
DSPID25  DS    0H                                                               
         USING PJOBRECD,R5                                                      
         CLI   PJOBADID,C' '       AD ID THERE ?                                
         BNH   DSPID80             NO                                           
*                                                                               
         LA    R1,75(R2)           ENOUGH ROOM? (ADID=ABCD12345678)             
         CR    R4,R1                                                            
         BNH   DSPID40             YES                                          
         BRAS  RE,BUMPFLD          NO SKIP TO NEXT LINE                         
         LA    R4,8(R2)                                                         
         FOUT  (R2)                                                             
DSPID40  MVC   0(5,R4),=C'ADID='                                                
         MVC   5(L'PJOBADID,R4),PJOBADID                                        
         AHI   R4,18               "ADID="(5)+12(ADID)+EXTRA SPACE              
         CLI   PJOBKJOB,X'FF'      AD-ID "ALONE" ?                              
         BNE   DSPID80             NO                                           
         MVC   BUYAD1,=C'*ADID*'                                                
         FOUT  BUYAD1H                                                          
         DROP  R5                                                               
*                                                                               
DSPID80  MVC   KEY,WKSVKEY                                                      
*                                                                               
DSPADIDX J     X_R2R4              NEED TO RETURN FIELD POINTERS                
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
DSP_PO#  NTR1  BASE=*,LABEL=*      DISPLAY PURCHASE ORDER #                     
*                                                                               
         MVC   WKSVKEY,KEY         SAVE ORIGINAL KEY                            
         MVI   ELCODE,PBYPOELQ                                                  
         LA    R5,PBUYREC+33                                                    
         BRAS  RE,NXTELEM          PURCHASE ORDER # ELEM?                       
         BNE   D_PO#_X                                                          
         USING PBYPOELD,R5                                                      
         TM    PBYPOSTA,BYPOZZZQ   PO# ELEM FOR ZZZ BUY?                        
         BZ    D_PO#20                                                          
         LA    R1,(87-8)(R2)       DATA LENGTH FOR PO=*ZZZ*                     
         BRAS  RE,CKDSPLIN                                                      
         MVC   0(8,R4),=C'PO=*ZZZ*'                                             
         AHI   R4,8+1              POINT TO NEXT DISPLAYING AREA                
         B     D_PO#_X                                                          
*                                                                               
D_PO#20  MVC   SVPOSEQ#,PBYPOSQ#   SAVE PURCHASE ORDER SEQ# FOR LOOKUP          
         XC    KEY,KEY                                                          
         LA    RE,KEY                                                           
         USING PPO#KEY,RE                                                       
         MVC   PPO#KAGY,PBUYKAGY                                                
         MVC   PPO#KMED,PBUYKMED                                                
         MVI   PPO#KRCD,PPO#KIDQ                                                
         MVC   PPO#KCLT,PBUYKCLT                                                
         CLI   SVCLTPLV,P_POLVCQ   CLIENT LEVEL?                                
         JE    D_PO#26                                                          
         MVC   PPO#KPRD,PBUYKPRD                                                
         CLI   SVCLTPLV,P_POLVPQ   PRODUCT LEVEL?                               
         JE    D_PO#26                                                          
         MVC   PPO#KEST,PBUYKEST                                                
         DROP  RE                                                               
D_PO#26  BRAS  RE,PRT_RDHI                                                      
         CLC   KEY(L'PPO#KEY),KEYSAVE                                           
         BE    D_PO#40                                                          
D_PO#34  LA    R1,(87-14)(R2)      DATA LENGTH FOR PO=*NOT FOUND*               
         BRAS  RE,CKDSPLIN                                                      
         MVC   0(14,R4),=C'PO=*NOT FOUND*'                                      
         AHI   R4,14+1             POINT TO NEXT DISPLAYING AREA                
         B     D_PO#_X                                                          
*                                                                               
D_PO#40  MVC   SV_AREC_,AREC                                                    
         MVC   AREC,AWKAIO1                                                     
         BRAS  RE,PRT_GETR                                                      
         MVC   AREC,SV_AREC_                                                    
         L     R5,AWKAIO1                                                       
         LA    R5,(PO#FIRST-PPO#KEY)(R5)                                        
         USING PO#DELMD,R5                                                      
         MVI   ELCODE,PO#DLIDQ                                                  
         CLI   PO#DELID,PO#DLIDQ   AT LEAST 1 PO# HEADER ELEM?                  
         BE    *+6                                                              
         DC    H'0'                BAD PURCHASE ORDER # RECORD                  
         CLC   SVPOSEQ#,PO#DID     PO SEQ# MATCH THAT OF BUY?                   
         BE    *+16                                                             
         BRAS  RE,NXTELEM                                                       
         BE    *-14                                                             
         B     D_PO#34                                                          
*                                                                               
         TM    PO#DACTV,PO#DINAQ   PURCHASE ORDER # IS INACTIVE?                
         BZ    D_PO#50                                                          
         LA    R1,87-13(R2)        DATA LENGTH FOR PO=*INACTIVE*                
         BRAS  RE,CKDSPLIN                                                      
         MVC   0(13,R4),=C'PO=*INACTIVE*'                                       
         AHI   R4,13+1             POINT TO NEXT DISPLAYING AREA                
         B     D_PO#_X                                                          
*                                                                               
D_PO#50  SR    R3,R3                                                            
         IC    R3,PO#DLEN                                                       
         SHI   R3,PO#DHDLQ         TO GET LENGTH OF PURCHASE ORDER #            
         LA    R1,87(R2)                                                        
         SR    R1,R3                                                            
         SHI   R1,3                DATA LENGTH FOR PO=123456... (25)            
         BRAS  RE,CKDSPLIN                                                      
         MVC   0(3,R4),=C'PO='                                                  
         LR    RE,R3                                                            
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   3(0,R4),PO#DPO#                                                  
         AHI   R4,3+1              DISPLAY LENGTH FOR PO= AND A SPACE           
         AR    R4,R3               DISPLAY LENGTH FOR PURCHASE ORDER #          
*                                                                               
D_PO#_X  MVC   KEY,WKSVKEY                                                      
         J     X_R2R4              NEED TO RETURN FIELD POINTERS                
*                                                                               
         DROP  RB,R5                                                            
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
DSP_PLCO NTR1  BASE=*,LABEL=*      DISPLAY PLANNED COST                         
*                                                                               
         CLI   PBUYKMED,C'I'       INTERACTIVE?                                 
         JE    D_PLC20                                                          
         CLI   PBUYKMED,C'L'       SOCIAL?                                      
         JE    D_PLC20                                                          
         CLI   PBUYKMED,C'S'       SEARCH?                                      
         JE    D_PLC20                                                          
         CLI   PBUYKMED,C'B'       MOBILE?                                      
         JE    D_PLC20                                                          
         CLI   PBUYKMED,C'D'       DIGITAL AUDIO?                               
         JE    D_PLC20                                                          
         CLI   PBUYKMED,C'V'       NATIONAL VIDEO (NVIDEO)?                     
         JE    D_PLC20                                                          
         CLI   PBUYKMED,C'W'       LOCAL VIDEO (LVIDEO)?                        
         JE    D_PLC20                                                          
         CLI   PBDELEM+1,X'69'     OLD BUY?                                     
         BL    D_PLC_X                                                          
         OC    PBDPLCOS,PBDPLCOS   HAVE PLANNED COST?                           
         BZ    D_PLC_X                                                          
         CLI   PBDPLCOS,X'FF'      HAVE PLANNED COST?                           
         BE    D_PLC_X                                                          
         EDIT  (B4,PBDPLCOS),(12,WKTEMP1),2,COMMAS=YES,ALIGN=LEFT               
         LR    R3,R0                                                            
         LA    R1,87(R2)                                                        
         SR    R1,R3                                                            
         SHI   R1,10               DATA LENGTH FOR PLANNED COST                 
         BRAS  RE,CKDSPLIN                                                      
         MVC   0(10,R4),=C'PLAN COST='                                          
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   10(0,R4),WKTEMP1                                                 
         AHI   R3,10+1+1           PLANNED COST NOTATION LENGTH                 
         AR    R4,R3               DISPLAY LENGTH FOR PLANNED COST              
         B     D_PLC_X                                                          
*                                                                               
D_PLC20  GOTOR FMT_COST,DMCB,(2,PBUYREC)                                        
         LH    R6,=Y(WKBLK_01-GENOLD)                                           
         AR    R6,RC                                                            
         USING WKBLK_01,R6                                                      
D        USING F_COST_D,WKBLK_01                                                
*                                                                               
         CLC   D.FC_OCOST,SPACES   HAVE PLANNED COST TO DISPLAY?                
         BNH   D_PLC_X                                                          
         LA    RF,D.FC_OCOST+(L'D.FC_OCOST-1)                                   
         BRAS  RE,LAST_CHR                                                      
         LA    RE,D.FC_OCOST                                                    
         SR    RF,RE                                                            
         AHI   RF,1                                                             
         LR    R3,RF                                                            
         LA    R1,87(R2)                                                        
         SR    R1,R3                                                            
         SHI   R1,10               DATA LENGTH FOR PLANNED COST                 
         BRAS  RE,CKDSPLIN                                                      
         MVC   0(10,R4),=C'PLAN COST='                                          
         LR    RE,R3                                                            
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   10(0,R4),D.FC_OCOST                                              
         AHI   R4,10+1             PLANNED COST NOTATION LENGTH                 
         AR    R4,R3               DISPLAY LENGTH FOR PLANNED COST              
*                                                                               
D_PLC_X  J     X_R2R4              NEED TO RETURN FIELD POINTERS                
*                                                                               
LAST_CHR CLI   0(RF),0                                                          
         JE    *+8                                                              
         CLI   0(RF),C' '                                                       
         JH    *+10                                                             
         BCTR  RF,0                                                             
         J     *-18                                                             
         BR    RE                                                               
*                                                                               
         DROP  D                                                                
         DROP  RB,R6                                                            
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
FMT_COST NTR1  BASE=*,LABEL=*      FORMAT COST                                  
*                                                                               
         LH    R6,=Y(WKBLK_01-GENOLD)                                           
         AR    R6,RC                                                            
         ICM   R5,15,0(R1)         POINT TO BUY RECORD                          
         USING WKBLK_01,R6                                                      
F        USING F_COST_D,WKBLK_01                                                
*                                                                               
         XC    F.F_COST_D(FC_BLKLQ),F.F_COST_D                                  
         MVC   F.FC_TMPWK,SPACES                                                
         MVC   F.FC_OCOST,SPACES                                                
*                                                                               
         LA    R5,33(R5)           POINT TO 1ST BUY ELEM                        
         MVC   F.FC_RTIND,PBDRLIND-PBDELEM(R5)                                  
         MVC   F.FC_ACPCT,PBDACP-PBDELEM(R5)                                    
*                                                                               
         CLI   0(R1),1             FORMATTING RATE?                             
         BNE   F_COS12                                                          
         ZAP   F.FC_COST_,PBDCOS-PBDELEM(L'PBDCOS,R5)                           
         MVC   F.FC_NETSW,PBDCTYP-PBDELEM(R5)                                   
         MVC   F.FC_COTYP,PBDCOSTY-PBDELEM(R5)                                  
         MVC   F.FC_COIND,PBDCOSIN-PBDELEM(R5)                                  
         B     F_COS30                                                          
*                                                                               
F_COS12  CLI   0(R1),2             FORMATTING PLANNED COST?                     
         BNE   F_COS20                                                          
         MVI   ELCODE,BYPCIDQ                                                   
         BRAS  RE,NXTELEM          PLANNED COST ELEM FOUND?                     
         BNE   F_COS_X                                                          
         USING BYPCELD,R5                                                       
         ZAP   F.FC_COST_,BYPCCST  COST                                         
         MVC   F.FC_NETSW,BYPCNIND NET COST SWITCH (ENTERED AS NET)             
         MVC   F.FC_COTYP,BYPCTYP  COST TYPE (U=UNIT COST)                      
         MVC   F.FC_COIND,BYPCIND  COST INDICATOR                               
         B     F_COS30                                                          
*                                                                               
F_COS20  DC    H'0'                NO OTHER COST AT THIS TIME                   
*                                                                               
F_COS30  CLI   PBUYKMED-PBUYKEY(R1),C'N'                                        
         BNE   F_COS40                                                          
         ZAP   DUB,F.FC_COST_      FORMAT COST FOR NEWSPAPER                    
         CVB   R1,DUB                                                           
         CLI   F.FC_NETSW,C'N'     ENTERED AS NET?                              
         BNE   F_COS32                                                          
         ZAP   DUB,F.FC_ACPCT                                                   
         CVB   RF,DUB                                                           
         S     RF,=F'100000'                                                    
         LCR   RF,RF               = NET PCT                                    
         MR    R0,RF                                                            
         L     RF,=F'100000'                                                    
         SLDA  R0,1                                                             
         DR    R0,RF                                                            
         LTR   R1,R1                                                            
         BNP   *+8                                                              
         AHI   R1,1                                                             
         SRA   R1,1                                                             
*                                                                               
F_COS32  CLI   F.FC_COTYP,C'U'     UNIT RATE?                                   
         BE    F_COS37                                                          
         C     R1,=F'99999999'     TOTAL RATE OVER 999,999.99?                  
         BNH   F_COS34                                                          
         LR    R0,R1                                                            
         SR    R1,R1                                                            
         SRDA  R0,31                                                            
         D     R0,=F'100'          HAVE ENTERED PENNIES WHEN BUYING             
         LTR   R1,R1               (NO ROOM)                                    
         BNP   *+8                                                              
         AHI   R1,1                                                             
         SRA   R1,1                                                             
F_COS33  EDITR (R1),(9,F.FC_TMPWK+5),0,FLOAT=-,ALIGN=LEFT,IZERO=Y               
         B     F_COS38                                                          
*                                                                               
F_COS34  CHI   R1,0                NEGATIVE RATE?                               
         BNL   F_COS36                                                          
         C     R1,=F'-99999999'    TOTAL RATE LESS THAN 99,999,999?             
         BH    F_COS35                                                          
         MHI   R1,-1               DROP PENNIES AND DIMES                       
         SR    R0,R0                                                            
         D     R0,=F'100'                                                       
         MHI   R1,-1                                                            
         B     F_COS33                                                          
*                                                                               
F_COS35  C     R1,=F'-999999'      TOTAL RATE LESS THAN 9,999.99?               
         BNL   F_COS36                                                          
         MHI   R1,-1               DROP PENNIES, LEAVE DIMES                    
         SR    R0,R0                                                            
         D     R0,=F'10'                                                        
         MHI   R1,-1                                                            
         EDITR (R1),(9,F.FC_TMPWK+5),1,FLOAT=-,ALIGN=LEFT,IZERO=Y               
         B     F_COS38                                                          
*                                                                               
F_COS36  EDITR (R1),(9,F.FC_TMPWK+5),2,FLOAT=-,ALIGN=LEFT,IZERO=Y               
         B     F_COS38                                                          
*                                                                               
F_COS37  EDITR (R1),(11,F.FC_TMPWK+5),5,FLOAT=-,ALIGN=LEFT,IZERO=Y              
*                                                                               
         LA    R1,F.FC_TMPWK+5     START OF OUTPUT                              
         AR    R1,R0               + LENGTH                                     
         SHI   R1,3                BACK UP TO LAST 3 BYTES                      
         CLC   =C'000',0(R1)                                                    
         BNE   *+10                                                             
         MVC   0(3,R1),SPACES      MOVE SOME BLANKS                             
*                                                                               
F_COS38  LA    R1,F.FC_TMPWK+5                                                  
         CLI   F.FC_COTYP,C'U'     UNIT RATE?                                   
         BE    *+12                                                             
         BCTR  R1,0                                                             
         MVC   0(1,R1),F.FC_COTYP                                               
         CLI   F.FC_COIND,C' '     DEFAULT COST TYPE?                           
         BE    *+12                                                             
         BCTR  R1,0                                                             
         MVC   0(1,R1),F.FC_COIND                                               
         CLI   F.FC_NETSW,C'N'     ENTERED AS NET?                              
         BNE   *+12                                                             
         BCTR  R1,0                                                             
         MVC   0(1,R1),F.FC_NETSW                                               
         TM    F.FC_RTIND,X'08'    FROZEN RATE?                                 
         BZ    *+10                                                             
         BCTR  R1,R0                                                            
         MVI   0(R1),C'*'                                                       
         MVC   F.FC_OCOST(11),0(R1)                                             
         CP    F.FC_COST_,=P'0'                                                 
         BNE   F_COS_X                                                          
         MVC   F.FC_OCOST,SPACES                                                
         MVC   F.FC_OCOST(4),=C'FREE'                                           
         CLI   F.FC_COIND,C'S'                                                  
         JNE   *+10                                                             
         MVC   F.FC_OCOST(5),=C'SFREE'                                          
         CLI   F.FC_COIND,C'R'                                                  
         JNE   *+10                                                             
         MVC   F.FC_OCOST(5),=C'RFREE'                                          
         B     F_COS_X                                                          
*                                                                               
F_COS40  ZAP   DUB,F.FC_COST_      FORMAT COST FOR NON-NEWSPAPER                
         CVB   R1,DUB                                                           
         CLI   F.FC_NETSW,C'N'     NET INPUT SO DISPLAY AS NET                  
         BNE   F_COS42                                                          
         ZAP   DUB,F.FC_ACPCT                                                   
         CVB   RF,DUB                                                           
         S     RF,=F'100000'                                                    
         LCR   RF,RF               =NET PCT                                     
         MR    R0,RF                                                            
         L     RF,=F'100000'                                                    
         SLDA  R0,1                                                             
         DR    R0,RF                                                            
         LTR   R1,R1                                                            
         BNP   *+8                                                              
         AHI   R1,1                                                             
         SRA   R1,1                                                             
*                                                                               
F_COS42  EDITR (R1),(10,F.FC_TMPWK+2),2,ALIGN=LEFT,FLOAT=-,IZERO=Y              
         LA    R1,F.FC_TMPWK+2                                                  
         CLI   F.FC_COIND,C' '     DEFAULT COST TYPE?                           
         BE    *+12                                                             
         BCTR  R1,0                                                             
         MVC   0(1,R1),F.FC_COIND                                               
         CLI   F.FC_NETSW,C'N'     ENTERED AS NET?                              
         BNE   *+12                                                             
         BCTR  R1,0                                                             
         MVC   0(1,R1),F.FC_NETSW                                               
         TM    F.FC_RTIND,X'08'    FROZEN RATE?                                 
         BZ    *+10                                                             
         BCTR  R1,R0                                                            
         MVI   0(R1),C'*'                                                       
         MVC   F.FC_OCOST(11),0(R1)                                             
         CP    F.FC_COST_,=P'0'                                                 
         BNE   F_COS_X                                                          
         MVC   F.FC_OCOST,SPACES                                                
         MVC   F.FC_OCOST(4),=C'FREE'                                           
         CLI   F.FC_COIND,C'S'                                                  
         JNE   *+10                                                             
         MVC   F.FC_OCOST(5),=C'SFREE'                                          
         CLI   F.FC_COIND,C'R'                                                  
         JNE   *+10                                                             
         MVC   F.FC_OCOST(5),=C'RFREE'                                          
*                                                                               
F_COS_X  J     EXIT                                                             
*                                                                               
         DROP  F                                                                
         DROP  RB,R6,R5                                                         
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
GLOBALS  DS    0D                                                               
*                                                                               
C_7B00   DC    X'7B00'             C'#' AND X'00'                               
C_7B40   DC    X'7B40'             C'#' AND C' '                                
C_5C40   DC    X'5C40'             C'*' AND C' '                                
*                                                                               
PAYHD    DC    CL38'**************  PAYMENTS  *************'                    
BLLHD    DC    CL38'**************  BILLINGS  *************'                    
NTRATXT  DC    C'** STATUS IS "NO TRAFFIC" **'                                  
NOIOMSG  DC    C'**NO INSERTION ORDER WILL BE AUTOMATICALLY PRINTED**'          
IORHD    DC    C'*INSERTION ORDER PRINTED '                                     
IODMSG   DC    C'ID=NONE'                                                       
NOCHMSG  DC    C'NO CHANGE IN DATE,SPACE OR AD SINCE LAST INS. ORD.'            
INSDATM  DC    C'INSERTION DATE ALREADY PAST'                                   
NOCLS    DC    C'CLOSE DATE MISSING'                                            
NORMSG   DC    C'INSERTION ORDER WILL BE PRODUCED'                              
SYSMSG   DC    C'INSERTION ORDER SYSTEM OPTION NOT SPECIFIED'                   
NOJMSG   DC    C'NO AD NUMBER PRESENT'                                          
SHPHD    DC    C'ON SHIPPING LIST DATED-'                                       
NOCHMSGR DC    C'NO CHANGE IN DATE,SPACE,RATE OR AD SINCE LAST INS. ORD+        
               .'                                                               
*                                                                               
         LTORG                                                                  
         DROP                                                                   
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
WORK06D  DSECT                     GLOBAL WORKING STORAGE AREA                  
*                                                                               
SPACES   DS    CL255               C' '                                         
*                                                                               
RELOBY06 DS    F                                                                
SV_AREC_ DS    A                                                                
AWKAIO1  DS    A                                                                
SVR8     DS    A                   A(DOLLARS DISPLAY FOR PAY)                   
*                                                                               
WKSVKEY  DS    XL(L'KEY)                                                        
WKTEMP1  DS    XL256                                                            
WKTEMP2  DS    XL256                                                            
*                                                                               
DCUVALUE DS    CL3                 LOOKED UP CU VALUE                           
*                                                                               
LWINSDT  DS    CL1                 QUARTER DIGIT (ALPHA)                        
*                                                                               
SVPOSEQ# DS    XL(L'PBYPOSQ#)                                                   
*                                                                               
PAYRTFLG DS    CL1                 PAYMENT RATIO CALCULATION FLAG               
*                                                                               
GCSEL01A DS    A                                                                
*                                                                               
GCSDEBN  DS    F                   CURRENT CLEARED DEBITS  NET                  
GCSDEBG  DS    F                   CURRENT CLEARED DEBITS  GROSS                
GCSRTIOD DS    PL8                 DEBIT  RATIO                                 
*                                                                               
GCSCRDN  DS    F                   CURRENT CLEARED CREDITS NET                  
GCSCRDG  DS    F                   CURRENT CLEARED CREDITS GROSS                
GCSRTIOC DS    PL8                 CREDIT RATIO                                 
*                                                                               
GCSDEBCD DS    F                   CURRENT CLEARED DEBITS  CD                   
GCSRTCDD DS    PL8                 DEBIT  RATIO                                 
*                                                                               
GCSCRDCD DS    F                   CURRENT CLEARED CREDITS CD                   
GCSRTCDC DS    PL8                 CREDIT RATIO                                 
*                                                                               
         DS    0D                  ALIGNMENT                                    
GCSPL8   DS    PL8                 PACKED WORKAREA                              
GCSPL16  DS    PL16                PACKED WORKAREA                              
GCSRATIO DS    PL8                 RATIO OF INVOICE $'S TO TOTAL $'S            
GCSWORK  DS    XL32                WORKAREA FOR EDIT                            
*                                                                               
WRKPL8   DS    PL8                                                              
WRKPL16  DS    PL16                                                             
*                                                                               
SVELM03  DS    XL32                SAVEAREA FOR 03 ELMEMENT                     
SVELM05  DS    XL64                SAVEAREA FOR 05 ELEMENT                      
*                                                                               
WKAIO1   DS    XL4096                                                           
*                                                                               
WORK06X  EQU   *                                                                
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE PPBUYWRK1                                                      
         EJECT                                                                  
*                                                                               
         ORG   REC                 MAP BUY RECORD TO REC                        
*                                                                               
       ++INCLUDE PPBUYWRK2                                                      
         EJECT                                                                  
         PRINT ON                                                               
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'090PPBUY06   10/14/20'                                      
         END                                                                    
