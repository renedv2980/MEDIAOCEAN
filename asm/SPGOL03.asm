*          DATA SET SPGOL03    AT LEVEL 043 AS OF 08/24/20                      
*PHASE T20203C                                                                  
         SPACE 2                                                                
**********************************************************************          
*                                                                    *          
* 23APR87 ADD A SUBROUTINE TO APPORTION SPLIT RECORDS AMONG DAYPARTS *          
*         BEFORE ADDING RECORDS TO INSURE ALLOCATIONS SUM TO         *          
*         ORIGINAL INPUT                                             *          
*                                                                    *          
* 16OCT87 FIX BUG IN PASSIVE SUBROUTINE THAT DESTROYS SAVED KEY      *          
* 23MAY90 ALLOW DISP. OF OUT-OF-WEEK DATA & ALLOW NET DOLLAR INPUT   *          
* 30MAY90 FIX BUG IN READING SPLIT RECORDS FOR DAYPART * INPUT       *          
* 12MAY92 ALLOW DELETE OF ALL TOTAL ELEMENTS IF DATES = S-E          *          
* 14SEP95 IDR OPTION                                                 *          
* 07JUL00 SPRI/AATK CALL SPAUTH TO ADD SUPERDESK AUTH MKT LEVEL RECS *          
* 29NOV00 MHER  ALLOW 0 POINTS AND DOLS TO MEAN HIATUS               *          
* 06DEC00 MHER  AND IGNORE DELETE ERRORS BECAUSE THE UPLOAD PROGRAM  *          
*               ATTEMPTS TO DELETE WHEN GOALS/DOLLARS=0              *          
* 05APR01 MHER  SUPPORT PURPOSE CODES                                *          
* 06JUL01 MHER  SUPPORT 0 POINTS/DOLS FOR DAYPART '*'                *          
* 26JUL01 MHER  FIX FOR GENGOALS WHICH DELETES 0 POINTS/DOLS WITH    *          
*               ACTION D                                             *          
* 23OCT01 MHER  USE CORERES SPAUTH                                   *          
* 08OCT10 AKAT  DO NOT ALLOW A CPP OVER 1 MILLION PER POINT          *          
**********************************************************************          
         TITLE 'SPGOL03 - SPOTPAK GOALS ADD/DELETE'                             
T20203   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T20203                                                         
*                                                                               
         LA    R9,2048(RB)                                                      
         LA    R9,2048(R9)                                                      
         USING T20203+4096,R9                                                   
*                                                                               
         L     RC,0(R1)                                                         
         LA    R7,2048(RC)                                                      
         LA    R7,2048(R7)                                                      
         USING GENOLD,RC,R7                                                     
*                                                                               
         L     RA,4(R1)                                                         
         USING T202FFD,RA                                                       
*                                                                               
         XC    GKEY,GKEY                                                        
         XC    BDATA,BDATA         CLEAR OLD DATA VALUES                        
* CLEAR ALL MKT NAMES WITH CHANGED MARKETS                                      
         LA    R4,GOLACT1H                                                      
*                                                                               
GL1A     LR    R2,R4               SAVE PREVIOUS HDR ADDRESS                    
         SR    R5,R5                                                            
         IC    R5,0(R4)                                                         
         AR    R4,R5                                                            
         CLI   0(R4),0                                                          
         BE    GL1X                                                             
         TM    1(R4),X'20'         TEST PROTECTED (MKTNAME)                     
         BZ    GL1A                NO - SKIP                                    
         CLI   5(R2),0             IF MKT # LENGTH IS 0 THEN IT'S OK            
         BE    *+12                                                             
         TM    4(R2),X'20'         IS MARKET VALID                              
         BO    GL1A                YES - NEXT                                   
         IC    R5,0(R4)            GET LENGTH                                   
         SH    R5,=H'9'            SET FOR EX                                   
         EX    R5,GLOC             IS ANYTHING THERE                            
         BZ    GL1A                NO                                           
         EX    R5,GLXC                                                          
         FOUT  (R4)                                                             
         B     GL1A                                                             
*                                                                               
GLOC     OC    8(0,R4),8(R4)                                                    
GLXC     XC    8(0,R4),8(R4)                                                    
         EJECT                                                                  
GL1X     DS    0H                                                               
         LA    R2,GOLACT1H                                                      
         MVI   FLDNUM,1            SET FIRST INPUT FIELD                        
*                                                                               
GL2      LH    RE,LINNUM                                                        
         LA    RE,1(RE)                                                         
         STH   RE,LINNUM                                                        
*                                                                               
         ST    R2,BLNADDR          SAVE START OF LINE ADDRESS                   
         MVI   ERRCD,MSSNGERR                                                   
         CLI   5(R2),0                                                          
         BE    GLERR                                                            
         MVC   BACT,8(R2)          SAVE ACTION                                  
         BAS   RE,CHKPC                                                         
         MVC   BSUBACT,9(R2)       AND SUB-ACTION                               
         CLI   8(R2),C'A'          TEST ADD                                     
         BE    GL4                                                              
         CLI   BSUBACT,C'T'        TOTAL SUBACT ONLY FOR ADD                    
         BE    GLERR                                                            
*                                                                               
         XC    BDOLS,BDOLS         FORCE $ AND PTS NEXT ADD                     
         XC    BPTS,BPTS                                                        
         MVI   BCPPSW,0                                                         
*                                                                               
GL4      DS    0H                                                               
         CLI   8(R2),C'*'                                                       
         BNE   GL6                                                              
*                                                                               
* NO CHANGED DATA FIELDS ALLOWED ON * LINE                                      
*                                                                               
         MVI   ERRCD,CHDTAERR                                                   
         LR    R4,R2                                                            
         SR    R5,R5                                                            
         LA    R0,7                TEST MKT/NAME/DPT/DOLS/PTS/PER               
         B     GL4B                                                             
*                                                                               
GL4A     TM    1(R4),X'20'         TEST PROTECTED                               
         BO    GL4B                YES - SKIP                                   
         TM    4(R4),X'20'         TEST VALIDATED                               
         BZ    GLERR                                                            
*                                                                               
GL4B     IC    R5,0(R4)                                                         
         AR    R4,R5                                                            
         BCT   R0,GL4A                                                          
*                                                                               
         B     GLMKT                                                            
         EJECT                                                                  
GL6      CLI   8(R2),C'A'                                                       
         BE    GL8                                                              
         CLI   8(R2),C'+'                                                       
         BE    GL8                                                              
         CLI   8(R2),C'-'                                                       
         BE    GL8                                                              
         CLI   SVPRD,X'FF'         NO DELETE FOR CPP DATA                       
         BE    *+12                                                             
         CLI   8(R2),C'D'                                                       
         BE    GLMKT                                                            
         MVI   ERRCD,INVERR                                                     
         B     GLERR                                                            
*                                                                               
GL8      CLC   QPRD,QPRD2        MAKE SURE PRDS IN ALPHA SEQ ON ADD             
         BL    GLMKT                                                            
         OC    QPRD2,QPRD2         OR IF ONLY ONE PRD                           
         BZ    GLMKT                                                            
         MVI   ERRCD,INVERR                                                     
         LA    R2,GOLPRH                                                        
         B     GLERR                                                            
         SPACE 2                                                                
GLMKT    SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         MVI   FLDNUM,2                                                         
*                                                                               
         CLI   5(R2),0             TEST FOR DATA                                
         BNE   GLMKT2              OK                                           
         MVI   ERRCD,MSSNGERR                                                   
         OC    BMKT,BMKT           DID WE HAVE A MARKET ABOVE                   
         BZ    GLERR                                                            
* CLEAR DATA IN MKTNAME (IF ANY)                                                
         B     GLMKTX                                                           
*                                                                               
GLMKT2   NI    4(R2),X'FF'-X'20'   SO REDISPLAY MARKET NAME                     
         GOTO1 USER1               'EDTMKT'                                     
*                                                                               
         TM    4(R2),X'20'                                                      
         BO    GLMKTX                                                           
         SR    R4,R4                                                            
         IC    R4,0(R2)                                                         
         AR    R4,R2                                                            
         FOUT  (R4),SVMKTNAM,16                                                 
*                                                                               
GLMKTX   OI    4(R2),X'20'                                                      
         CLI   BACT,C'+'                                                        
         BE    GLSTAT                                                           
         CLI   BACT,C'-'                                                        
         BE    GLSTAT                                                           
         EJECT                                                                  
GLDPT    SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         IC    R0,0(R2)            SKIP MKT NAME                                
         AR    R2,R0                                                            
         MVI   FLDNUM,3            DPT/SLN=3                                    
*                                                                               
         CLI   5(R2),0                                                          
         BNE   GLDPT2                                                           
         CLI   BDPT,C'*'          ANOTHER * DPT LINE?                           
         BNE   GLDPT1                                                           
         BAS   RE,RDSPLT          YES - REREAD SPLIT RECORD                     
         B     GLDPTX                                                           
*                                                                               
GLDPT1   CLI   BDPT,0                                                           
         BNZ   GLDPTX                                                           
         MVI   ERRCD,MSSNGERR                                                   
         B     GLERR                                                            
*                                                                               
GLDPT2   CLI   SVPRD,X'FF'                                                      
         BE    GLDPT3                                                           
         CLI   8(R2),C'*'          TEST SPLIT DATA                              
         BNE   GLDPT3                                                           
         MVI   BDPT,C'*'           SET FOR FUTURE REFERENCE                     
         BAS   RE,RDSPLT           READ SPLIT RECORD                            
*                                                                               
GLDPT2A  SR    R0,R0                                                            
         LA    R3,REC2+24                                                       
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         CLI   0(R3),2                                                          
         BNE   *-10                                                             
         LA    R3,2(R3)                                                         
*                                                                               
GLDPT2B  OC    1(2,R3),1(R3)       TEST DPT HAS WEIGHT                          
         BZ    GLDPT2D                                                          
* NOW MAKE SURE DPT IS IN DPT MENU                                              
         LA    R1,SVMENU                                                        
GLDPT2C  CLC   0(1,R3),0(R1)                                                    
         BE    GLDPT2D                                                          
         LA    R1,1(R1)                                                         
         CLI   0(R1),0                                                          
         BNE   GLDPT2C                                                          
         MVI   ERRCD,NOSPLDPT                                                   
         B     GLERR                                                            
*                                                                               
GLDPT2D  LA    R3,3(R3)                                                         
         CLI   0(R3),0                                                          
         BNE   GLDPT2B                                                          
*                                                                               
GLDPT3   CLI   BDPT,C'$'           NEVER CARRY $ FORWARD                        
         BH    GLDPT3A                                                          
         MVI   BDPT,0                                                           
         MVI   BSLN,0                                                           
         MVI   BTLN,0                                                           
*                                                                               
GLDPT3A  GOTO1 USER2               'EDTDPTLN'                                   
*                                                                               
         CLI   SVPRD,X'FF'         TEST CPP                                     
         BE    GLDPT4                                                           
* NOT CPP                                                                       
         MVI   ERRCD,SLNERR                                                     
         CLI   BSLN,0                                                           
         BE    GLERR                                                            
         B     GLDPTX                                                           
* CPP                                                                           
GLDPT4   DS    0H                                                               
         CLI   BSLN,30             ** THIS IS FOR COMPATIBILITY **              
         BE    GLDPTX                                                           
*                                                                               
         MVI   ERRCD,INVERR                                                     
         CLI   BSLN,0              SPTLN MUST BE OMITTED                        
         BNE   GLERR                                                            
         MVI   BSLN,30             FORCE 30 SEC LEN                             
         MVI   BTLN,30             FORCE 30 SEC LEN                             
*                                                                               
GLDPTX   OI    4(R2),X'20'                                                      
         B     GLDOLS                                                           
         EJECT                                                                  
RDSPLT   NTR1                                                                   
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D40'                                                  
         MVC   KEY+2(1),SVAGYMD                                                 
         MVC   KEY+3(2),SVCLT                                                   
         MVC   KEY+5(3),QPRD                                                    
         MVC   KEY+8(1),SVEST                                                   
         GOTO1 HIGH                                                             
         MVI   ERRCD,NOSPLIT                                                    
         CLC   KEY(13),KEYSAVE                                                  
         BNE   GLERR                                                            
         LA    R1,REC2                                                          
         ST    R1,AREC                                                          
         GOTO1 GETREC                                                           
         LA    R1,REC                                                           
         ST    R1,AREC                                                          
         XIT1                                                                   
         EJECT                                                                  
GLDOLS   SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         ST    R2,SVADOLS          SAVE FIELD ADDRESS                           
         MVI   FLDNUM,5            COST=FLD5                                    
*                                                                               
         CLI   SVCXTRA+8,C'P'      TEST P&G GOALS                               
         BNE   GLDOL2                                                           
         CLI   5(R2),0             TEST DOLLARS INPUT                           
         BNE   GLDOL2                                                           
         MVI   ERRCD,MSSNGERR                                                   
         B     GLERR                                                            
*                                                                               
GLDOL2   CLI   BACT,C'*'                                                        
         BE    *+12                                                             
         CLI   BACT,C'A'                                                        
         BNE   GLDOLX                                                           
*                                                                               
         CLI   SVPRD,X'FF'                                                      
         BE    GLCPP                                                            
*                                                                               
         CLI   5(R2),0                                                          
         BE    GLDOLX                                                           
*                                                                               
GLDOL4   MVI   BDOLSIGN,0          CLEAR INCREMENTAL IND                        
         CLI   BCPPSW,0            WAS LAST TIME $/CPP                          
         BE    *+10                NO                                           
         XC    BPTS,BPTS           CLEAR POINTS                                 
         MVI   BCPPSW,0            RESET SWITCH                                 
         SR    R5,R5                                                            
         IC    R5,5(R2)                                                         
         LA    R4,8(R2)                                                         
         CLI   0(R4),C'+'                                                       
         BE    GLDOL10                                                          
         CLI   0(R4),C'-'                                                       
         BE    GLDOL10                                                          
         CLI   0(R4),C'N'                                                       
         BE    GLDOL10                                                          
         CLI   BSUBACT,C'T'          TEST TOTAL DOLLARS INPUT                   
         BE    GLDOL12                                                          
*                                                                               
* SCAN INPUT FIELD FOR A '/'                                                    
* IF FOUND, ENTRY MUST BE DOLLARS/CPP                                           
* ELSE JUST EDIT DOLLARS                                                        
*                                                                               
GLDOL6   LA    R4,8(R2)                                                         
         SR    R5,R5                                                            
         IC    R5,5(R2)                                                         
         STM   R4,R5,WORK          SAVE ADDRESS/LEN                             
         CLI   0(R4),C'B'          TEST BUDGET                                  
         BNE   GLDOL6A                                                          
         LA    R4,1(R4)                                                         
         BCTR  R5,0                                                             
         B     GLDOL12                                                          
GLDOL6A  CLI   0(R4),C'/'          SEARCH FOR SEPARATOR                         
         BE    GLDOL6B                                                          
         LA    R4,1(R4)                                                         
         BCT   R5,GLDOL6A                                                       
         LM    R4,R5,WORK          RESTORE ADDRESS/LENGTH                       
         B     GLDOL12                                                          
*                                                                               
GLDOL6B  L     R1,WORK             GET DATA START                               
         LR    R0,R1                                                            
         SR    R0,R4               START-END                                    
         LPR   R0,R0               GIVES LEN                                    
         BAS   RE,TSTNUM           CHECK FIELD NUMERIC                          
*                                                                               
         L     R1,WORK             DATA START                                   
         L     RE,WORK+4           ORIGINAL LEN                                 
         SR    RE,R5               GIVES DATA LEN                               
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,0(0,R1) ** EXECUTED **                                       
         CVB   R0,DUB                                                           
         MH    R0,=H'100'          X 100                                        
         ST    R0,BDOLS                                                         
*                                                                               
         LA    R4,1(R4)            POINT TO CPP DATA                            
         BCTR  R5,0                ADJUST LEN                                   
         MVI   ERRCD,INVERR                                                     
         LTR   R5,R5                                                            
         BZ    GLERR                                                            
*                                                                               
         GOTO1 VCASHVAL,DMCB,(2,(R4)),(R5)                                      
*                                                                               
         MVI   ERRCD,INVERR                                                     
         CLI   0(R1),X'FF'                                                      
         BE    GLERR                                                            
         OC    4(4,R1),4(R1)       TEST ZERO CPP                                
         BZ    GLERR                                                            
* CALCULATE POINTS                                                              
         L     RE,BDOLS                                                         
         AR    RE,RE               X 2                                          
         MHI   RE,10                                                            
*                                                                               
         IF  (CLI,TWODEC,EQ,C'Y')  IF POINTS ARE TO 2 DECIMAL                   
           MHI RE,10                 X 10 FOR MORE PRECISION                    
         ENDIF                                                                  
*                                                                               
****     SRDA  RE,32                                                            
         SRDL  RE,32                                                            
         D     RE,4(R1)                                                         
         AHI   RF,1                                                             
         SRL   RF,1                DIVIDE BY 2                                  
         ST    RF,BPTS                                                          
         MVI   BCPPSW,C'Y'         INDICATE CPP DATA WAS ENTERED                
         B     GLCPPX                                                           
         EJECT                                                                  
GLDOL10  MVC   BDOLSIGN,0(R4)      SAVE SIGN                                    
         LA    R4,1(R4)                                                         
         BCTR  R5,0                                                             
         MVI   ERRCD,INVERR                                                     
         LTR   R5,R5                                                            
         BZ    GLERR                                                            
*                                                                               
GLDOL12  GOTO1 VCASHVAL,DMCB,(2,(R4)),(R5)                                      
*                                                                               
         MVI   ERRCD,INVERR                                                     
         CLI   0(R1),X'FF'                                                      
         BE    GLERR                                                            
         L     R0,4(R1)                                                         
         CLI   BDPT,C'-'          TEST SPECIAL CCUSA NEGATIVE DPT               
         BNE   *+6                                                              
         LNR   R0,R0                                                            
         ST    R0,BDOLS                                                         
*                                                                               
         CLI   BDOLSIGN,C'N'      FOR NET AMOUNT                                
         BNE   GLDOL20                                                          
         SR    R0,R0                                                            
         ICM   R1,15,BDOLS                                                      
         BZ    GLERR              ZERO MAKES NO SENSE                           
         M     R0,=F'200'         X100X2                                        
         D     R0,=F'85'                                                        
         LTR   R1,R1                                                            
         BNP   *+8                                                              
         A     R1,=F'1'                                                         
         SRA   R1,1                                                             
         ST    R1,BDOLS                                                         
         MVI   BDOLSIGN,0         RESET DOLLAR SIGN                             
*                                                                               
GLDOL20  CLI   BSUBACT,C'T'        TEST TOTAL INPUT                             
         BNE   GLDOLX                                                           
         OC    BDOLS,BDOLS                                                      
         BNZ   GLDOLX                                                           
         MVC   BDOLS,=F'-1'                                                     
*                                                                               
GLDOLX   OI    4(R2),X'20'                                                      
*                                                                               
         MVI   ERRCD,INVERR                                                     
         CLI   BDPT,C'+'           TEST SPECIAL CCUSA DPTS                      
         BE    *+12                                                             
         CLI   BDPT,C'-'                                                        
         BNE   GLDOLX2                                                          
         ZIC   R0,0(R2)                                                         
         AR    R2,R0               POINT TO POINTS                              
         CLI   5(R2),0                                                          
         BNE   GLERR               SHOULD HAVE NO POINTS                        
         B     GLPER               BUT SHOULD HAVE PERIOD DATA                  
*                                                                               
GLDOLX2  CLI   8(R2),C'B'          TEST BUDGET                                  
         BNE   GLPTS                                                            
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   5(R2),0             SHOULD HAVE NO POINTS DATA                   
         BNE   GLERR                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   5(R2),0               OR PERIOD DATA                             
         BNE   GLERR                                                            
*                                                                               
         CLI   BACT,C'A'                                                        
         BNE   GLNEXT                                                           
         MVI   BACT,C'B'           SET 'BUDGET' FLAG IN ACTN                    
         B     GLKEY                                                            
         EJECT                                                                  
* PRODUCT = POL, EDIT CPP ENTRY                                                 
*                                                                               
GLCPP    SR    R5,R5                                                            
         IC    R5,5(R2)                                                         
         GOTO1 VCASHVAL,DMCB,(2,8(R2)),(R5)                                     
*                                                                               
         MVI   ERRCD,INVERR                                                     
         CLI   0(R1),X'FF'                                                      
         BE    GLERR                                                            
         OC    4(4,R1),4(R1)                                                    
         BZ    GLERR                                                            
         LA    R0,1000             SET POINTS=100X10                            
         ST    R0,BPTS                                                          
         L     R0,4(R1)                                                         
         MHI   R0,100              DOLLARS = CPP X 100 POINTS                   
         ST    R0,BDOLS                                                         
*                                                                               
GLCPPX   OI    4(R2),X'20'                                                      
*                                                                               
         LR    RF,R2               SAVE A(DOLLARS/WEEKS FIELD)                  
         SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0               POINTS ENTRY                                 
         CLI   5(R2),0             SHOULD HAVE NO DATA                          
         BE    MAXCPP                                                           
         MVI   ERRCD,CPPERR                                                     
         B     GLERR                                                            
         EJECT                                                                  
GLPTS    SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         MVI   FLDNUM,6            POINTS=6                                     
*                                                                               
         CLI   BACT,C'*'                                                        
         BE    *+12                                                             
         CLI   BACT,C'A'                                                        
         BNE   GLPTSX                                                           
         CLI   5(R2),0                                                          
         BE    GLPTSX                                                           
         MVI   ERRCD,INVERR                                                     
         CLI   BSUBACT,C'T'        TEST TOTAL INPUT                             
         BE    GLERR               SHOULD NOT ENTER POINTS                      
*                                                                               
GLPTS2   MVI   BPTSSIGN,0          CLEAR INCREMENTAL IND                        
         MVI   BCPPSW,0            RESET CPP DATA SWITCH                        
*                                                                               
         LA    R4,8(R2)                                                         
         ZIC   R5,5(R2)                                                         
         CLI   8(R2),C'+'          CHECK FOR INCREMENTAL INPUT                  
         BE    GLPTS4                                                           
         CLI   8(R2),C'-'                                                       
         BE    GLPTS4                                                           
         B     GLPTS6                                                           
*                                                                               
GLPTS4   MVC   BPTSSIGN,8(R2)      SAVE SIGN                                    
         LA    R4,1(R4)                                                         
         BCTR  R5,0                                                             
*                                                                               
GLPTS6   DS    0H                                                               
         GOTO1 VCASHVAL,DMCB,(2,(R4)),(R5)                                      
         MVI   ERRCD,INVERR                                                     
         CLI   0(R1),X'FF'                                                      
         BE    GLERR                                                            
         L     R0,4(R1)            GET POINTS X 100                             
         ST    R0,BPTS             SAVE                                         
*                                                                               
         CLI   SVDEMOS+3,C'R'      RATINGS?                                     
         JE    *+12                                                             
         CLI   SVDEMOS+3,C'E'        EXTENDED RATINGS?                          
         JNE   GLPTS6A                                                          
         CLI   TWODEC,C'Y'         TEST 2-DECIMAL GOAL RTGS?                    
         JE    GLPTSX              YES, LEAVE THE NUMBER ALONE                  
         J     GLPTS6O             NO, ADJUST TO ONE DECIMAL                    
*                                                                               
GLPTS6A  CLI   TWODCIMPS,C'Y'      IF NOT RATINGS, THEN IMPS                    
         JE    GLPTSX              WE HAVE 2 DECIMAL GOAL IMPS                  
*                                                                               
* ELSE SET GOALS TO 1-DEC                                                       
GLPTS6O  AR    R0,R0               X 2                                          
         SRDA  R0,32                                                            
         D     R0,=F'10'                                                        
         AHI   R1,1                                                             
         SRL   R1,1                                                             
         ST    R1,BPTS             SET POINTS X 10 IN RECORD                    
         CLI   BPTS,0              LIMIT POINTS TO 3 BYTES                      
         JNE   GLERR                                                            
         EJECT                                                                  
*                                                                               
         MVI   ERRCD,INCGLERR                                                   
         CLC   BPTSSIGN,BDOLSIGN   CHECK POINTS/DOLLARS HAVE SAME SIGN          
         BNE   GLERR                                                            
*                                                                               
GLPTSX   OI    4(R2),X'20'                                                      
*                                                                               
* MUST HAVE POINTS OR DOLLARS UNLESS PROFILE SAYS TO TREAT AS DELETE            
*                                                                               
         MVI   ERRCD,MSSNGERR                                                   
         CLI   BACT,C'A'                                                        
         BNE   MAXCPP                                                           
         OC    BPTS,BPTS                                                        
         BNZ   MAXCPP                                                           
         OC    BDOLS,BDOLS                                                      
         BNZ   MAXCPP                                                           
         CLI   SVCXTRA+8,C'P'      TEST P&G GOALS                               
         BNE   *+12                                                             
         L     R2,SVADOLS                                                       
         B     GLERR                                                            
*                                                                               
         CLI   SVPPROF6,C'Y'                                                    
         BNE   GLERR                                                            
         MVI   BACT,C'H'           SET 'HIATUS'                                 
*                                                                               
MAXCPP   XR    R0,R0               CLEAR R0                                     
         L     R1,BDOLS            DOLLARS                                      
         ICM   R4,15,BPTS          ANY POINTS?                                  
         BZ    GLPER               NO - DON'T DIVIDE BY 0!                      
*                                                                               
         CLI   SVPRD,X'FF'         TEST DOING CPP                               
         JE    MAXCPP5             SO PTS ARE NEVER 2-DEC                       
         CLI   TWODCIMPS,C'Y'      TEST 2-DECIMAL IMPS                          
         JE    MAXCPP5             YES                                          
         CLI   TWODEC,C'Y'         TEST 2-DECIMAL RTGS                          
         JNE   *+8                 NO                                           
MAXCPP5  M     R0,=F'10'           THEN SCALE UP DOLLARS FOR DIVIDE             
         DR    R0,R4               GET CPP                                      
*                                                                               
         CLI   BCPPSW,C'Y'         CPP DATA WAS ENTERED?                        
         BNE   *+8                 NO                                           
         L     R1,BPTS             YES - CPP ALREADY CALCULATED                 
*                                                                               
         C     R1,=F'10000000'     CPP > 1 MILLION PER POINT?                   
         BL    GLPER               NO - LESS                                    
         BH    *+10                YES - ERROR                                  
         LTR   R0,R0               EQUAL - ANY REMAINDER?                       
         BZ    GLPER               NO                                           
*        MVC   GOLMSG(35),=C'* ERROR * CPP > 1 MILLION PER POINT'               
         MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=AL2(GMILLION)                                            
         CLI   BCPPSW,C'Y'         CPP DATA WAS ENTERED?                        
         BNE   *+6                 NO                                           
         LR    R2,RF               YES - CURSOR TO DOLLAR/WEEKS FIELD           
         OI    6(R2),X'40'         SET CURSOR TO POINTS/WEEK FIELD              
****     MVI   ERRAREA,X'FF'       SET ERROR POSTED FLAG                        
****     B     EXXMOD              GO REPORT ERROR MESSAGE                      
         J     GLERR                                                            
*                                                                               
GLPER    SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         MVI   FLDNUM,7            PERIOD=FLD7                                  
*                                                                               
         CLI   5(R2),0                                                          
         BNE   GLPER2                                                           
         OC    BWEEKS,BWEEKS                                                    
         BNZ   GLPERX                                                           
         MVI   ERRCD,MSSNGERR                                                   
         B     GLERR                                                            
*                                                                               
GLPER2   DS    0H                                                               
         GOTO1 USER3               'EDTPER'                                     
*                                                                               
         MVI   BPERSW,0                                                         
         CLC   =C'S-E',8(R2)       TEST PERIOD IS S-E                           
         BNE   *+8                                                              
         MVI   BPERSW,C'E'                                                      
*                                                                               
GLPERX   OI    4(R2),X'20'                                                      
         EJECT                                                                  
         CLI   BACT,C'*'           TEST NO ACTION THIS LINE                     
         BE    GLNEXT                                                           
*                                                                               
         CLI   BACT,C'A'                                                        
         BNE   GLKEY                                                            
*                                                                               
         CLI   SVPRD,X'FF'         FOR CPP GUIDE, NO CPP LOOK-UP                
         BE    GLKEY                                                            
*                                                                               
         CLI   SVPROF+8,C'0'       OR IF NO CPP REQUIRED                        
         BH    GLKEY                                                            
*                                                                               
         CLI   BPTSSIGN,0          TEST INCREMENTAL GOALS                       
         BNE   GLKEY                                                            
         OC    BDOLS,BDOLS         TEST ZERO DOLS                               
         BZ    GLRDCPP                                                          
         OC    BPTS,BPTS           OR POINTS                                    
         BZ    GLRDCPP                                                          
         B     GLKEY                                                            
         SPACE 2                                                                
GLRDCPP  CLI   BSUBACT,C'T'        TEST TOTAL INPUT                             
         BE    GLKEY                                                            
         CLI   BDPT,C'+'           TEST SPECIAL CCUSA DPTS                      
         BE    GLKEY                                                            
         CLI   BDPT,C'-'                                                        
         BE    GLKEY                                                            
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,2                                                            
         MVC   KEY+1(1),SVAGYMD                                                 
         MVC   KEY+2(2),SVCLT                                                   
         MVI   KEY+4,X'FF'                                                      
         MVC   KEY+5(2),BMKT                                                    
         MVC   KEY+7(1),SVEST                                                   
         MVC   KEY+8(1),BDPT                                                    
         MVI   KEY+9,30                                                         
         CLI   SVCPPES,0                                                        
         BE    *+16                                                             
         MVC   KEY+2(2),SVCPPCL                                                 
         MVC   KEY+7(1),SVCPPES                                                 
*                                                                               
         CLC   KEY(10),BCPPKEY     DO WE HAVE IT ALREADY                        
         BNE   GLRDCPP2            NO                                           
         B     GLKEY                                                            
*                                                                               
GLRDCPP2 CLI   BDPT,C'*'                                                        
         BE    GLRDCPP4                                                         
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         MVC   BCPPKEY,KEY                                                      
         CLC   KEY(10),KEYSAVE     DID WE FIND IT                               
         BNE   GLNOCPP                                                          
         CLI   SVCPPES2,0                                                       
         BE    GLKEY                                                            
         MVC   KEY+7(1),SVCPPES2                                                
         GOTO1 HIGH                                                             
         CLC   KEY(10),KEYSAVE                                                  
         BE    GLKEY                                                            
         SPACE 1                                                                
*********************************************************************           
*  WARNING -- ERRCD ACTS AS MISSING CPP FLAG FROM HERE ON           *           
*                                                                   *           
GLNOCPP  MVI   ERRCD,NOCPPERR                                       *           
         OC    BPTS,BPTS           TEST POINTS INPUT                *           
         BNZ   GLKEY               YES - MAY HAVE TOTAL ELEM        *           
         L     R2,BLNADDR                                           *           
         B     GLERR               ELSE HAVE ERROR NOW              *           
*********************************************************************           
         SPACE 2                                                                
* FOR SPLIT,CHECK ALL ACTIVE DPTS HAVE CPP'S                                    
*                                                                               
GLRDCPP4 LA    R3,REC2+24                                                       
         SR    R0,R0                                                            
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         CLI   0(R3),2                                                          
         BNE   *-10                                                             
         LA    R3,2(R3)            POINT TO FIRST DPT                           
*                                                                               
         MVC   BCPPKEY,KEY         SAVE KEY JUST BUILT                          
*                                                                               
GLRDCPP6 CLI   0(R3),0                                                          
         BE    GLKEY                                                            
         OC    1(2,R3),1(R3)       TEST ACTIVE                                  
         BZ    GLRDCPP7                                                         
         MVC   KEY+8(1),0(R3)      MOVE DPT                                     
         CLI   SVCPPES,0                                                        
         BE    *+10                                                             
         MVC   KEY+7(1),SVCPPES    RESTORE EST                                  
GLRDCP6A GOTO1 HIGH                                                             
         CLC   KEY(10),KEYSAVE                                                  
         BNE   GLRDCPP8                                                         
         CLI   SVCPPES2,0                                                       
         BE    GLRDCPP7                                                         
         MVC   KEY+7(1),SVCPPES2                                                
         GOTO1 HIGH                                                             
         CLC   KEY(10),KEYSAVE                                                  
         BNE   GLRDCPP8                                                         
GLRDCPP7 LA    R3,3(R3)                                                         
         B     GLRDCPP6                                                         
*                                                                               
GLRDCPP8 MVC   GOLMSG(28),=C'* ERROR * NO CPP FOR DPT X *'                      
         MVC   GOLMSG+25(1),0(R3)                                               
         L     R2,BLNADDR                                                       
         OI    6(R2),X'40'         SET CURSOR TO ACTN                           
         MVI   ERRAREA,X'FF'       SET ERROR POSTED FLAG                        
         B     EXXMOD                                                           
         EJECT                                                                  
*============================================================                   
* MAKE SURE PURPOSE CODE PRESENT IF REQUIRED                                    
*============================================================                   
         SPACE 1                                                                
CHKPC    CLI   SVB0PROF+9,C'Y'                                                  
         BNER  RE                                                               
         OC    SVIDR,SVIDR         TEST IDR PRESENT                             
         BNZR  RE                  YES                                          
* PURPOSE CODE NEEDS TO BE THERE                                                
         L     R2,BLNADDR          PURPOSE CODE NOT PRESENT                     
         MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=Y(NOPURCOD)                                              
         B     GLERR                                                            
         EJECT                                                                  
* BUILD GOAL KEY                                                                
GLKEY    DS    0H                                                               
*                                                                               
         CLI   BDPT,C'*'           TEST SPLIT                                   
         BE    GLREQ                                                            
*                                                                               
         LA    RE,GOALREC          SET RECORD ADDRESS                           
         ST    RE,AREC                                                          
*                                                                               
         XC    GKEY,GKEY                                                        
         MVI   GKEYTYPE,2                                                       
         MVC   GKEYAM,SVAGYMD                                                   
         MVC   GKEYCLT,SVCLT                                                    
         MVC   GKEYPRD,SVPRD                                                    
         MVC   GKEYMKT,BMKT                                                     
         MVC   GKEYEST,SVEST                                                    
         MVC   GKEYDPT,BDPT                                                     
         MVC   GKEYSLN,BSLN                                                     
         MVC   GKEYSEC,BTLN        SET TOTAL LENGTH                             
         MVC   GKEYPRD2,SVPRD2     SET PARTNER PRD CODE                         
         OC    SVIDR,SVIDR         ID ENTERED?                                  
         BZ    *+12                                                             
         OI    GKEYAGY,X'40'       NOT A PRODUCT IN GKEYPRD2                    
         MVI   IDCODE,0            CLEAR ID CODE                                
*                                                                               
         MVC   KEY(13),GKEY                                                     
*                                                                               
GLKEY1   CLI   BACT,C'A'                                                        
         BNE   GLKEY2                                                           
*                                                                               
         CLI   BPTSSIGN,0          TEST INCR GOALS                              
         BE    GLKEY2              NO                                           
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     IF INCR GOALS, MUST FIND REC                 
         BE    GLADD1                                                           
         OC    SVIDR,SVIDR         TEST ID ENTERED                              
         BZ    *+14                                                             
         CLC   KEY(11),KEYSAVE     IF PARTIAL MATCH                             
         BE    GLADD1              GO DO GETREC                                 
         MVI   ERRCD,NODTAERR                                                   
         B     GLADD2ER                                                         
*                                                                               
GLKEY2   CLI   BACT,C'A'                                                        
         BE    GLADD                                                            
         CLI   BACT,C'B'           TEST BUDGET                                  
         BE    GLADD                                                            
         CLI   BACT,C'D'                                                        
         BE    GLDEL                                                            
         CLI   BACT,C'H'                                                        
         BE    GLDEL                                                            
         DC    H'0'                                                             
*                                                                               
GLADD    OI    DMINBTS,X'08'       PASS DELETES                                 
         GOTO1 HIGH                                                             
*                                                                               
         OC    SVIDR,SVIDR         ID ENTERED?                                  
         BNZ   GLADDC                                                           
         CLC   KEY(13),KEYSAVE                                                  
         BNE   GLADD2              BUILD NEW REC                                
         B     GLADD1                                                           
*                                                                               
GLADDA   MVC   IDCODE,KEY+12       SAVE LAST ID CODE ENCOUNTERED                
*                                                                               
GLADDB   GOTO1 SEQ                                                              
*                                                                               
GLADDC   CLC   KEY(11),KEYSAVE                                                  
         BNE   GLADD2                                                           
         TM    KEY+11,X'40'        ID CODE IN GKEYPRD2                          
         BNO   GLADDB              NO - CONTINUE                                
         EJECT                                                                  
* ALWAYS RE-READ SPTFILE REC TO REBUILD GETREC TABLE                            
*                                                                               
GLADD1   GOTO1 GETREC                                                           
*                                                                               
         TM    VCALLBAS,X'80'      TEST DDLINK UPLOAD                           
         JO    GLADD1A                                                          
         CLI   GDDDLINK,C'Y'       TEST RECORD WAS UPLOADED                     
         JNE   GLADD1A                                                          
****     J     GLADD1A             TEMPORARILY ALLOW CHANGES ON MF              
*****                                                                           
         CLC   =C'LESLIEHO',GOLPLNR  SPECIAL OVERRIDE                           
         JE    GLADD1A                                                          
         MVI   ERRCD,NEWERRS       IF SO, CANT CHANGE FROM M/F                  
         MVC   NERRCD,=Y(NOTOWNER) YOU DON'T OWN THIS RECORD                    
         J     GLERR                                                            
                                                                                
GLADD1A  OC    SVIDR,SVIDR         ID ENTERED?                                  
         BZ    GLADD1B                                                          
         CLC   GDIDR,SVIDR         YES, SAME ID?                                
         BNE   GLADDA                                                           
         MVC   KEYSAVE(13),KEY                                                  
*                                                                               
GLADD1B  MVC   GBUYNAME,GOLPLNR    UPDATE NAME IN RECORD                        
         BAS   RE,CHKELDT                                                       
         TM    GDSTAT,X'80'        TEST STATUS                                  
         BZ    GLADDT                                                           
         MVI   ERRCD,BADSTAT                                                    
         L     R2,BLNADDR                                                       
         B     GLERR                                                            
*                                                                               
GLADDT   CLI   ERRCD,NOCPPERR      TEST CPP ERR PENDING                         
         BNE   GLADDTX             NO                                           
* MUST CHECK FOR TOTAL ELEM NOW                                                 
         LA    R8,BWEEKS           POINT TO DATE LIST ITEM                      
GLADDT2  BAS   RE,TOTCHK                                                        
         BNE   GLADD2ER            IF NO TOTAL ELEM, ERROR                      
         LA    R8,2(R8)            NEXT WEEK                                    
         CLI   0(R8),0             TEST E-O-L                                   
         BNE   GLADDT2                                                          
*                                                                               
GLADDTX  B     GLADD5                                                           
         SPACE 2                                                                
* BUILD NEW RECORD                                                              
*                                                                               
GLADD2   CLI   BPTSSIGN,0          TEST INCREMENTAL INPUT                       
         BZ    GLADD2A             YES - ERROR                                  
         MVI   ERRCD,NODTAERR                                                   
         B     GLADD2ER                                                         
*                                                                               
GLADD2A  CLI   ERRCD,NOCPPERR      TEST HAD CPP ERR PENDING                     
         BNE   GLADD2B             NO                                           
*                                                                               
GLADD2ER L     R2,BLNADDR          DEFINITELY NO TOTAL ELEM                     
         B     GLERR               SO GIVE ERROR NOW                            
*                                                                               
*                                                                               
GLADD2B  CLI   SVPRD,X'FF'         TEST ADDING CPP GUIDE                        
         BNE   GLADD3                                                           
*                                                                               
         MVI   ERRCD,CPPDTERR                                                   
         CLI   BPERSW,C'E'         TEST ENTIRE ESTIMATE PERIOD                  
         BNE   GLERR                                                            
*                                                                               
GLADD3   LA    R0,GOALREC                                                       
         LHI   R1,REC2-REC                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         MVC   GKEY(13),KEYSAVE                                                 
         MVC   GLENGTH,=H'100'                                                  
         MVC   GAGYALPH,AGYALPHA                                                
         MVC   GDELEM(2),=X'204C'                                               
         MVC   GBUYNAME,GOLPLNR                                                 
         CLI   SVCPPES,0                                                        
         BE    GLADD4                                                           
         MVC   GDCPPCL,SVCPPCL                                                  
         MVC   GDCPPES(2),SVCPPES                                               
*                                                                               
GLADD4   OC    SVIDR,SVIDR         ID ENTERED?                                  
         BZ    GLADD5                                                           
         MVC   GDIDR,SVIDR                                                      
         ZIC   R1,IDCODE                                                        
         LA    R1,1(R1)                                                         
         STC   R1,GKEY+12          INCREMENT ID CODE                            
         CLI   GKEY+12,0           ID CODE CAN'T BE ZERO                        
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
GLADD5   CLI   BACT,C'B'                                                        
         BE    GLBUD                                                            
         CLI   BSUBACT,C'T'        TEST TOTAL INPUT                             
         BE    GLTOT                                                            
*                                                                               
         LA    R8,BWEEKS                                                        
*                                                                               
GLADD6   XC    ELEM,ELEM                                                        
         MVC   ELEM(2),=X'210C'                                                 
         CLI   SVOPT1,C'G'         TEST LOCKED GOALS                            
         BNE   *+8                                                              
         MVI   ELEM,X'A1'                                                       
         MVC   ELEM+2(2),0(R8)                                                  
         MVC   ELEM+8(4),BDOLS                                                  
         MVC   ELEM+4(4),BPTS                                                   
*                                                                               
         CLI   SVPRD,X'FF'                                                      
         JE    GLADD7              POINTS IN CPP REC ALWAYS 1-DEC               
*                                                                               
         CLI   SVDEMOS+3,C'R'      PRIMARY DEMO IS RATINGS?                     
         JE    *+12                                                             
         CLI   SVDEMOS+3,C'E'                                                   
         JNE   GLADD6A                                                          
         CLI   TWODEC,C'Y'         TEST 2-DEC GOAL RTGS                         
         JNE   GLADD7                                                           
         OI    ELEM+4,GLGRP2DEC    SET FLAG                                     
         J     GLADD7                                                           
*                                                                               
GLADD6A  CLI   TWODCIMPS,C'Y'      IF NOT RATINGS, THEN IMPS                    
         JNE   GLADD7                                                           
         OI    ELEM+4,GLGRP2DEC    SET FLAG                                     
*                                                                               
GLADD7   LA    R6,GDELEM                                                        
         MVI   ELCODE,X'21'                                                     
         CLI   SVOPT1,C'G'                                                      
         BNE   *+8                                                              
         MVI   ELCODE,X'A1'                                                     
         CLI   BPTSSIGN,0          TEST INCR INPUT                              
         BNE   GLADD12                                                          
         SPACE 2                                                                
GLADD8   SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BE    GLADD10                                                          
         CLC   0(1,R6),ELCODE                                                   
         BL    GLADD8                                                           
         BH    GLADD10                                                          
         CLC   2(2,R6),0(R8)       TEST SAME WEEK                               
         BL    GLADD8                                                           
         BH    GLADD10                                                          
* DELETE EXISTING ELEMENT                                                       
         GOTO1 VRECUP,DMCB,GOALREC,(R6),0                                       
         SPACE 2                                                                
* ADD NEW ELEMENT                                                               
GLADD10  GOTO1 VRECUP,DMCB,GOALREC,ELEM,(R6)                                    
         B     GLADD20                                                          
         EJECT                                                                  
* SUBROUTINE TO CHECK FOR TOTAL ELEM FOR DATE AT 0(R3) *                        
         SPACE 1                                                                
TOTCHK   DS    0H                                                               
         LA    R6,GDELEM                                                        
         SR    R0,R0                                                            
TOTCHK2  ICM   R0,1,1(R6)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BE    TOTCHKX                                                          
         CLI   0(R6),X'42'         TEST TOTAL ELEMENT                           
         BNE   TOTCHK2                                                          
         CLC   0(2,R8),2(R6)       DATE PRIOR TO EL START                       
         BL    TOTCHK2                                                          
         CLC   0(2,R8),4(R6)       DATE AFTER EL END                            
         BH    TOTCHK2                                                          
         CR    RE,RE               EXIT WITH CC EQ                              
         BR    RE                                                               
TOTCHKX  LTR   RE,RE               EXIT WITH CC NOT EQ                          
         BR    RE                                                               
         EJECT                                                                  
* CREATE BUDGET ELEMENT                                                         
*                                                                               
GLBUD    DS    0H                                                               
         XC    ELEM,ELEM                                                        
         MVC   ELEM(2),=X'400C'                                                 
         GOTO1 VDATCON,DMCB,SVSTART,(2,ELEM+2)                                  
         GOTO1 (RF),(R1),SVEND,(2,ELEM+4)                                       
         MVC   ELEM+8(4),BDOLS                                                  
*                                                                               
         LA    R6,GDELEM                                                        
         MVI   ELCODE,X'40'                                                     
         BAS   RE,NEXTEL                                                        
         BNE   GLBUD2                                                           
* DELETE EXISTING ELEM                                                          
         GOTO1 VRECUP,DMCB,GOALREC,(R6)                                         
*                                                                               
* ADD NEW ELEMENT                                                               
*                                                                               
GLBUD2   DS    0H                                                               
         GOTO1 VRECUP,DMCB,GOALREC,ELEM,(R6)                                    
         B     GLSET                                                            
         EJECT                                                                  
* INCREMENTAL INPUT                                                             
*                                                                               
GLADD12  BAS   RE,NEXTEL                                                        
         BNE   GLADD12X                                                         
         CLC   2(2,R6),0(R8)       COMPARE WEEKS                                
         BL    GLADD12                                                          
         BE    GLADD14B                                                         
GLADD12X MVI   ERRCD,NOWKERR       MUST FIND FIRST WEEK                         
         B     GLERR                                                            
*                                                                               
GLADD14A BAS   RE,NEXTEL                                                        
         BNE   GLADD20                                                          
GLADD14B CLC   2(2,R6),0(R8)       MATCH WEEK                                   
         BNE   GLADD14A                                                         
*                                                                               
         MVC   DUB(8),4(R6)        ALIGN POINTS/DOLS                            
         NI    DUB,X'3F'           DROP 2-DEC FLAG                              
         LM    RE,RF,DUB                                                        
*                                                                               
         CLI   TWODEC,C'Y'         TEST 2-DEC GOAL RTGS?                        
         JE    *+12                                                             
         CLI   TWODEC,C'Y'                    GOAL IMPS?                        
         JNE   GLADD15A            YES                                          
         TM    4(R6),GLGRP2DEC     NO - TEST GOAL IN ELEM IS 2-DEC              
         JZ    GLADD15X            NO                                           
         LR    R1,RE               GET 2-DEC POINTS                             
         M     R0,=F'2'            DOUBLE                                       
         D     R0,=F'10'                                                        
         AHI   R1,1                                                             
         SRL   R1,1                                                             
         LR    RE,R1               GIVES 1-DECIMAL GOAL PTS                     
         J     GLADD15X                                                         
                                                                                
* 2-DECIMAL FEATURE IS ON                                                       
                                                                                
GLADD15A TM    4(R6),GLGRP2DEC     TEST GOAL IN ELEM IS 2-DEC                   
         JO    GLADD15X            YES                                          
         MHI   RE,10               ELSE SCALE UP TO 2-DEC                       
*                                                                               
GLADD15X CLI   BPTSSIGN,C'+'                                                    
*                                                                               
GLADD16A MVI   FLDNUM,6                                                         
         S     RE,BPTS                                                          
         BNP   GLADD16X                                                         
         MVI   FLDNUM,7                                                         
         S     RF,BDOLS                                                         
         BNP   GLADD16X                                                         
*                                                                               
GLADD16B STM   RE,RF,DUB                                                        
         MVC   4(8,R6),DUB                                                      
         CLI   SVDEMOS+3,C'R'      PRIMARY DEMO IS RATINGS?                     
         JE    *+12                                                             
         CLI   SVDEMOS+3,C'E'                                                   
         JNE   GLADD16C                                                         
         CLI   TWODEC,C'Y'         TEST 2-DEC GOAL RTGS                         
         JNE   *+8                                                              
         OI    4(R6),GLGRP2DEC     SET 2-DEC FLAG                               
         B     GLADD20                                                          
*                                                                               
GLADD16C CLI   TWODCIMPS,C'Y'      TEST 2-DEC GOAL IMPS                         
         JNE   *+8                                                              
         OI    4(R6),GLGRP2DEC     SET 2-DEC FLAG                               
*                                                                               
GLADD16X MVI   ERRCD,NEGERR                                                     
         L     R2,BLNADDR                                                       
         B     GLERR                                                            
*                                                                               
GLADD20  LA    R8,2(R8)            NEXT WEEK                                    
         CLI   0(R8),0                                                          
         BE    GLSET                                                            
*                                                                               
         CLI   BPTSSIGN,0          TEST INCR INPUT                              
         BE    GLADD6              NO                                           
         LA    R6,GDELEM                                                        
         B     GLADD14A                                                         
         EJECT                                                                  
* CREATE TOTAL ELEMENT FOR REQUESTED PERIOD OR                                  
* OR DELETE ELEMENT IF BDOLS = -1                                               
         SPACE 1                                                                
GLTOT    XC    ELEM,ELEM                                                        
         MVC   ELEM(2),=X'420C'                                                 
         MVC   ELEM+8(4),BDOLS                                                  
         MVC   ELEM+2(2),BWEEKS    SET START DATE                               
         LA    R8,BWEEKS                                                        
GLT2     MVC   ELEM+4(2),0(R8)     SET END DATE                                 
         LA    R8,2(R8)                                                         
         CLI   0(R8),0                                                          
         BNE   GLT2                                                             
* CHECK FOR OVERLAP WITH OTHER ELEMENTS                                         
         MVI   ELCODE,X'42'                                                     
         LA    R6,GDELEM                                                        
         MVI   BYTE,C'N'           RESET FLAG                                   
         MVI   ERRCD,ELOVLAP                                                    
*                                                                               
GLT4     BAS   RE,NEXTEL                                                        
         BNE   GLT10                                                            
*                                                                               
GLT5     CLC   =F'-1',BDOLS        TEST DELETE                                  
         BNE   GLT6                                                             
         CLI   BPERSW,C'E'         TEST ENTIRE ESTIMATE PERIOD                  
         BE    GLT7                                                             
*                                                                               
GLT6     CLC   ELEM+4(2),2(R6)     END PRIOR TO OTHER START                     
         BL    GLT4                                                             
         CLC   ELEM+2(2),4(R6)     START AFTER OTHER ENDS                       
         BH    GLT4                                                             
         CLC   ELEM+2(4),2(R6)     SAME DATES EXACTLY                           
         BNE   GLERR               NO                                           
*                                                                               
GLT7     MVI   BYTE,C'Y'           SET MATCH FLAG                               
         GOTO1 VRECUP,DMCB,REC,(R6)                                             
         BAS   RE,NEXTEL2                                                       
         BE    GLT5                                                             
*                                                                               
GLT10    CLC   =F'-1',BDOLS        TEST DELETE FUNCTION                         
         BNE   GLT12                                                            
         MVI   ERRCD,NOTOTEL                                                    
         CLI   BYTE,C'Y'                                                        
         BNE   GLERR                                                            
         B     GLTX                                                             
*                                                                               
GLT12    LA    R6,GDELEM           MAKE SURE TO ADD IT IN SEQUENCE              
*                                                                               
GLT14    LLC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BE    GLT16                                                            
         CLI   0(R6),X'42'                                                      
         BNE   GLT14                                                            
         CLC   ELEM+2(2),2(R6)     COMPARE DATES                                
         BH    GLT14                                                            
*                                                                               
GLT16    GOTO1 VRECUP,DMCB,GOALREC,ELEM,(R6)                                    
*                                                                               
GLTX     B     GLSET                                                            
         EJECT                                                                  
*                                                                               
* SET ACTIVITY DATE IN REC                                                      
*                                                                               
GLSET    DS    0H                                                               
         GOTO1 VDATCON,DMCB,(5,0),(2,GACTDATE)                                  
*                                                                               
         TM    VCALLBAS,X'80'      TEST DDLINK UPLOAD                           
         JZ    GLSET50                                                          
                                                                                
         L     R1,VCOMFACS                                                      
         L     RF,CGETFACT-COMFACSD(R1)                                         
         GOTO1 (RF),DMCB,(X'80',0),F#UTLD                                       
         L     R1,0(R1)                                                         
         USING F@UTLD,R1                                                        
         XC    WORK,WORK                                                        
         MVC   WORK,F@TXPINF          COPY EXTERNAL PROGRAM INFO                
         LA    R1,WORK                                                          
         USING TXPINFO,R1                                                       
         LA    RE,ALLWEXPG            A(ALLOWED EXTERNAL PROGRAMS)              
GLSET10  OC    0(L'ALLWEXPG,RE),0(RE)                                           
         JZ    GLSET50                NOT ALLOWED, DO MARK IT AS SUCH           
*                                                                               
         CLC   TXPNUM,0(RE)           MATCH ON EXTERNAL PROGRAM #?              
         JE    GLSET20                YES                                       
         LA    RE,L'ALLWEXPG(RE)      NO, CHECK NEXT PROGRAM                    
         J     GLSET10                                                          
*                                                                               
GLSET20  MVC   HALF(1),2(RE)          SAVE OFF THE CHARACTER                    
         J     GLSET30                                                          
         DROP  R1                                                               
         LTORG                                                                  
*                                     SEE FAXPEQUS & FAXPTAB                    
ALLWEXPG DS    0XL3                   ALLOWED EXTERNAL PROGRAMS                 
         DC    AL2(81),C'P'           - PRISMA                                  
         DC    AL2(90),C'L'           - LUMINA                                  
         DC    3X'00'                                                           
*                                                                               
GLSET30  MVI   GDDDLINK,C'Y'       SET UPLOAD FLAG                              
                                                                                
* UPDATE PRISMA UID ELEMENT IN RECORD                                           
                                                                                
         XC    ELEM,ELEM                                                        
         LA    RE,ELEM                                                          
         USING GLUPLDEL,RE                                                      
         MVI   GLUPCOD,X'61'                                                    
         MVI   GLUPLEN,L'SVUID+3                                                
         MVC   GLUPTYPE(1),HALF       USED SAVED CHAR OF PROGRAM                
         MVC   GLUPUID(20),SVUID                                                
         OC    GLUPUID(20),SPACES                                               
         DROP  RE                                                               
*                                                                               
         MVI   ELCODE,X'61'                                                     
         LA    R6,GDELEM                                                        
         BRAS  RE,NEXTEL                                                        
         JNE   GLSET2                                                           
* DELETE EXISTING ELEMENT                                                       
         GOTO1 VRECUP,DMCB,GOALREC,(R6),0                                       
* ADD NEW ELEMENT                                                               
GLSET2   GOTO1 VRECUP,DMCB,GOALREC,ELEM,(R6)                                    
*                                                                               
GLSET50  MVC   GDCPPCL,SVCPPCL     SET CPP EST DATA IN CASE ADDED LATE          
         MVC   GDCPPES(2),SVCPPES                                               
*                                                                               
         OC    GREDATE,GREDATE                                                  
         BNZ   *+10                                                             
         MVC   GREDATE,GACTDATE                                                 
*                                                                               
         TM    GCNTRLS,X'80'       TEST REC IS DELETED                          
         BZ    GLADD30             NO                                           
*                                                                               
         NI    GCNTRLS,X'7F'       RESET 'DELETE'                               
*                                                                               
         BAS   RE,GLDOTOT                                                       
         GOTO1 PUTREC                                                           
         SPACE 2                                                                
* UNSET DIRECTORY CONTROL BIT TOO                                               
         NI    KEY+13,X'7F'                                                     
         MVC   COMMAND,=C'DMWRT'                                                
         GOTO1 DIR                                                              
*                                                                               
         CLI   SVPRD2,0                                                         
         BE    *+14                                                             
         MVC   COMMAND,=C'UND'                                                  
         BAS   RE,PASSIVE                                                       
         B     GLREQ                                                            
         EJECT                                                                  
GLADD30  CLC   KEY(13),KEYSAVE     TEST REC ON FILE                             
         BE    GLADD40             YES                                          
         GOTO1 ADDREC                                                           
*                                                                               
         MVC   KEY(13),GKEY        DISK ADDRESS IS IN KEY+14                    
         MVI   KEY+13,0                                                         
         CLI   SVPRD2,0            TEST NEED PIGGYBACK                          
         BE    GLREQ               NO                                           
         MVC   COMMAND,=C'DMADD'                                                
         BAS   RE,PASSIVE                                                       
         B     GLREQ                                                            
*                                                                               
GLADD40  BAS   RE,GLDOTOT                                                       
         GOTO1 PUTREC                                                           
*                                                                               
         B     GLREQ                                                            
         EJECT                                                                  
*                                                                               
*                                                                               
GLDEL    DS    0H                                                               
         GOTO1 HIGH                                                             
         OC    SVIDR,SVIDR         ID ENTERED?                                  
         BZ    GLDEL6                                                           
         B     GLDEL4                                                           
*                                                                               
GLDEL2   GOTO1 SEQ                                                              
*                                                                               
GLDEL4   CLC   KEY(11),KEYSAVE                                                  
         BNE   GLDEL10             DOESN'T EXIST                                
         TM    KEY+11,X'40'        ID CODE IN GKEYPRD2?                         
         BNO   GLDEL2                                                           
         B     GLDEL12                                                          
*                                                                               
GLDEL6   CLC   KEY(13),KEYSAVE                                                  
         BE    GLDEL12                                                          
*                                                                               
GLDEL10  MVI   ERRCD,NODTAERR                                                   
         CLI   SVPPROF6,C'Y'       IGNORE ALL DELETE ERRORS                     
         BNE   GLERR               IF HIATUS OPTION ON                          
         L     R4,BLNADDR                                                       
         MVI   8(R4),C'*'          INDICATE ACTION COMPLETED                    
         MVC   9(1,R4),BACT                                                     
         MVI   10(R4),C' '                                                      
         OI    6(R4),X'80'         FORCE TO XMT                                 
         B     GLNEXT              AND IGNORE ERROR                             
*                                                                               
GLDEL12  LA    RE,GOALREC                                                       
         ST    RE,AREC                                                          
         GOTO1 GETREC                                                           
*                                                                               
         TM    VCALLBAS,X'80'      TEST DDLINK UPLOAD                           
         JO    GLDEL13                                                          
         CLI   GDDDLINK,C'Y'       TEST RECORD WAS UPLOADED                     
         JNE   GLDEL13                                                          
****     J     GLDEL13             TEMPORARILY ALLOW DELETES ON MF              
*****                                                                           
         CLC   =C'LESLIEHO',GOLPLNR  SPECIAL OVERRIDE                           
         JE    GLDEL13                                                          
         MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=Y(NOTOWNER) YOU DON'T OWN THIS RECORD                    
         J     GLERR                                                            
*                                                                               
GLDEL13  OC    SVIDR,SVIDR         ID ENTERED?                                  
         BZ    *+14                                                             
         CLC   SVIDR,GDIDR         SAME ID?                                     
         BNE   GLDEL2              NO, GET NEXT RECORD                          
         BAS   RE,CHKELDT                                                       
* SET ACTIVITY DATE                                                             
         GOTO1 VDATCON,DMCB,(5,0),(2,GACTDATE)                                  
         MVC   GBUYNAME,GOLPLNR    UPDATE NAME IN RECORD                        
*                                                                               
GLDEL14  LA    R8,BWEEKS                                                        
*                                                                               
GLDEL15  MVI   ELCODE,X'21'                                                     
         CLI   SVOPT1,C'G'         TEST LOCKED GOALS                            
         BNE   *+8                                                              
         MVI   ELCODE,X'A1'                                                     
         LA    R6,GDELEM                                                        
*                                                                               
         CLC   =C'S-',8(R2)        IF PERIOD INPUT IS 'S-', MISSING             
         BE    GLDEL18              FIRST ELEMENT IS OK                         
*                                                                               
GLDEL16  BAS   RE,NEXTEL                                                        
         BNE   GLDEL17                                                          
*                                                                               
         CLI   BPERSW,C'E'         TEST ENTIRE ESTIMATE PERIOD                  
         BE    GLDEL19             YES - SKIP DATE TEST                         
         CLC   2(2,R6),0(R8)                                                    
         BL    GLDEL16                                                          
         BE    GLDEL19                                                          
*                                                                               
GLDEL17  CLI   SVPPROF6,C'Y'       TEST DOING HIATUS                            
         BE    GLDEL10             YES - SET TO IGNORE ERROR                    
         MVI   ERRCD,NOWKERR                                                    
         B     GLERR                                                            
*                                                                               
GLDEL18  BAS   RE,NEXTEL                                                        
         BNE   GLDEL20                                                          
         CLI   BPERSW,C'E'         TEST ENTIRE ESTIMATE PERIOD                  
         BE    GLDEL19             YES - SKIP DATE TEST                         
         CLC   2(2,R6),0(R8)                                                    
         BL    GLDEL18                                                          
         BH    GLDEL20                                                          
* WE HAVE THE ELEMENT - GET RID OF IT                                           
GLDEL19  GOTO1 VRECUP,DMCB,GOALREC,(R6),0                                       
*                                                                               
GLDEL20  LA    R8,2(R8)                                                         
         CLI   0(R8),0                                                          
         BE    GLDEL22                                                          
         LA    R6,GDELEM                                                        
         B     GLDEL18                                                          
*                                                                               
* TEST ANY ELEMENTS LEFT                                                        
*                                                                               
GLDEL22  DS    0H                                                               
         LA    R6,GDELEM                                                        
         SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BNE   GLDEL30                                                          
* DELETE RECORD                                                                 
         OI    GCNTRLS,X'80'                                                    
         GOTO1 PUTREC                                                           
*                                                                               
         OI    KEY+13,X'80'                                                     
         MVC   COMMAND,=C'DMWRT'                                                
         GOTO1 DIR                                                              
*                                                                               
         CLI   SVPRD2,0            TEST PIGGYBACK                               
         BE    *+14                NO                                           
         MVC   COMMAND,=C'DEL'                                                  
         BAS   RE,PASSIVE                                                       
*                                                                               
         B     GLDEL40                                                          
*                                                                               
GLDEL30  GOTO1 PUTREC                                                           
*                                                                               
GLDEL40  B     GLREQ                                                            
         EJECT                                                                  
* ALLOCATE DOLLARS IN TOTAL ELEMENTS TO WEEKLY GOAL ELEMENTS *                  
         SPACE 1                                                                
GLDOTOT  NTR1                                                                   
         LA    R3,GDELEM                                                        
GLDOT2   ZIC   R0,1(R3)                                                         
         AR    R3,R0                                                            
         CLI   0(R3),0             TEST TOTAL ELEMENT                           
         BE    EXIT                EXIT WITH BDOLS AND BPTS UNTOUCHED           
         CLI   0(R3),X'42'                                                      
         BE    GLDOT6                                                           
         B     GLDOT2                                                           
*                                                                               
GLDOT4   ZIC   R0,1(R3)                                                         
         AR    R3,R0                                                            
         CLI   0(R3),0             TEST TOTAL ELEMENT                           
         BE    GLDOTOTX                                                         
         CLI   0(R3),X'42'                                                      
         BNE   GLDOT4                                                           
* FIND ELEMENTS IN THIS PERIOD *                                                
GLDOT6   LA    R6,GDELEM                                                        
         BAS   RE,GLDNEXT                                                       
         BNE   GLDOT4              NO ELEMENTS IN PERIOD - IGNORE               
         SR    R4,R4               CLEAR ACCUM                                  
*                                                                               
         A     R4,4(R6)            ACCUMULATE TOTAL POINTS                      
         BAS   RE,GLDNEXT                                                       
         BE    *-8                                                              
         LTR   R4,R4                                                            
         BNZ   GLDOT10                                                          
         L     R2,BLNADDR                GET START OF LINE ADDRESS              
         AH    R2,=Y(GOLPTS1H-GOLACT1H)  POINT TO POINTS                        
         MVI   ERRCD,MSSNGERR                                                   
         B     GLERR                                                            
*                                                                               
GLDOT10  ICM   R1,15,8(R3)         GET TOTAL DOLLARS                            
         M     R0,=F'200'          X 100 X 2                                    
         DR    R0,R4                                                            
         SRL   R1,1                                                             
         ST    R1,FULL             SAVE COST PER POINT                          
*                                                                               
         LA    R6,GDELEM                                                        
         SR    R4,R4                                                            
GLDOT14  BAS   RE,GLDNEXT                                                       
         BNE   GLDOT16                                                          
         L     R1,4(R6)            GET POINTS                                   
         N     R1,=X'3FFFFFFF'                                                  
         M     R0,FULL             X DOLLARS/POINT                              
         D     R0,=F'100'          SCALE                                        
         SR    R0,R0                                                            
         A     R1,=F'50'           GET WHOLE DOLLARS                            
         D     R0,=F'100'                                                       
         M     R0,=F'100'                                                       
         ST    R1,8(R6)            SET IN ELEMENT                               
         AR    R4,R1               ACCUMULATE TOTAL DOLLARS ALLOCATED           
         B     GLDOT14                                                          
*                                                                               
GLDOT16  ICM   R0,15,8(R3)         GET ORIGINAL TOTAL                           
         SR    R4,R0               LESS ACTUAL TOTAL                            
         LCR   R4,R4               AND REVERSE SIGN                             
         BZ    GLDOT4              IF EQUAL, DONE                               
* FIND LARGEST AMOUNT AND ADJUST                                                
         LA    R6,GDELEM                                                        
         BAS   RE,GLDNEXT                                                       
         LR    R1,R6               SAVE FIRST ELEMENT ADDRESS                   
*                                                                               
GLDOT18  BAS   RE,GLDNEXT                                                       
         BNE   GLDOT20                                                          
         CLC   8(4,R1),8(R6)       COMPARE DOLLARS                              
         BH    *+6                                                              
         LR    R1,R6                                                            
         B     GLDOT18                                                          
*                                                                               
GLDOT20  A     R4,8(R1)            ADJUST DOLLARS                               
         ST    R4,8(R1)                                                         
         B     GLDOT4                                                           
*                                                                               
GLDOTOTX XC    BDOLS,BDOLS         FORCE INPUT NEXT TIME                        
         XC    BPTS,BPTS                                                        
         MVI   BCPPSW,0                                                         
         B     EXIT                                                             
*                                                                               
GLDNEXT  SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BE    NEXTELX             TO EXIT WITH CC NOT EQ                       
         CLI   0(R6),X'21'         SEARCH FOR WEEKLY ELEMS                      
         BE    GLDNEXT2                                                         
         CLI   SVOPT1,C'G'                                                      
         BNE   GLDNEXT                                                          
         CLI   0(R6),X'A1'                                                      
         BNE   GLDNEXT                                                          
GLDNEXT2 CLC   2(2,R6),2(R3)       TEST PRIOR TO TOTEL START                    
         BL    GLDNEXT                                                          
         CLC   2(2,R6),4(R3)       TEST AFTER TOTEL END                         
         BH    GLDNEXT                                                          
         CR    RE,RE               SET CC EQUAL                                 
         BR    RE                                                               
         EJECT                                                                  
* SUBROUTINES TO ADD AND DELETE PASSIVE PIGGYBACK POINTERS                      
* ACTIVE KEY IS IN 'KEY'                                                        
         SPACE 1                                                                
PASSIVE  NTR1                                                                   
         MVC   WORK2(20),KEY                                                    
         MVC   KEY+4(1),KEYSAVE+12   MOVE PRD                                   
         ZIC   R1,KEYSAVE+10         GET TLN                                    
         ZIC   R0,KEYSAVE+9          GET ACTIVE SLN                             
         SR    R1,R0                                                            
         STC   R1,KEY+9              SET PASSIVE SLN                            
         OI    KEY+11,X'80'          INDICATE PASSIVE POINTER                   
         MVC   KEY+12(1),KEYSAVE+4   MOVE ACTIVE PRD                            
*                                                                               
         CLC   =C'DMADD',COMMAND                                                
         BNE   DELPSSV                                                          
* ADD NEW PASSIVE POINTER *                                                     
         GOTO1 DIR                                                              
         B     PASSIVEX                                                         
*                                                                               
DELPSSV  CLC   =C'DEL',COMMAND                                                  
         BNE   UNDPSSV                                                          
* DELETE PASSIVE POINTER *                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    KEY+13,X'80'                                                     
         MVC   COMMAND,=C'DMWRT'                                                
         GOTO1 DIR                                                              
         B     PASSIVEX                                                         
*                                                                               
UNDPSSV  CLC   =C'UND',COMMAND                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
* UNDELETE PASSIVE                                                              
         OI    DMINBTS,X'08'       SET TO PASS DELETES                          
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         NI    KEY+13,X'7F'                                                     
         MVC   COMMAND,=C'DMWRT'                                                
         GOTO1 DIR                                                              
*                                                                               
PASSIVEX MVC   KEY,WORK2           RESTORE KEY                                  
         B     EXIT                                                             
         EJECT                                                                  
GLREQ    DS    0H                                                               
         CLI   BSUBACT,C'T'        TEST TOTAL INPUT                             
         BE    GLNEXT                                                           
*                                                                               
         CLC   =X'0D40',REC2       TEST SPLIT REC PRESENT                       
         BNE   GLREQ0X             NO                                           
         CLI   BDPT,C'*'           TEST FIRST TIME                              
         BNE   GLREQ0A             NO                                           
*                                                                               
         BAS   RE,GLSPLT           COMPUTE SPLIT AMOUNTS                        
         LA    R3,REC2+24-12                                                    
*                                                                               
         MVC   TPDOLS,BDOLS       SAVE DOLLARS                                  
         MVC   TPPTS,BPTS         SAVE POINTS                                   
*                                                                               
GLREQ0A  LA    R3,12(R3)                                                        
         CLI   0(R3),0             TEST E-O-L                                   
         BE    GLREQ0X                                                          
         OC    4(8,R3),4(R3)       TEST NON-ZERO AMOUNT                         
         BZ    GLREQ0A                                                          
* PROCESS THIS DAYPART                                                          
         MVC   BDPT,0(R3)          SET DAYPART                                  
         MVC   BPTS,4(R3)          MOVE POINTS (INPUT WAS SCALED UP)            
         MVC   BDOLS,8(R3)         AND DOLLARS                                  
         B     GLKEY                                                            
*                                                                               
GLREQ0X  DS    0H                                                               
         L     R4,BLNADDR                                                       
         MVI   8(R4),C'*'          INDICATE ACTION COMPLETED                    
         MVC   9(1,R4),BACT                                                     
         MVI   10(R4),C' '                                                      
*                                                                               
         CLC   =X'0D40',REC2       TEST SPLIT REC PROCESSING                    
         BNE   GLREQ0Z                                                          
         MVC   BDOLS,TPDOLS        RESTORE DOLLARS                              
         MVC   BPTS,TPPTS          RESTORE POINTS                               
         MVI   BDPT,C'*'           YES - RESTORE SPLIT RECORD ACTION            
         MVI   REC2,0              AND MUST READ SPLIT REC NEXT TIME            
*                                                                               
GLREQ0Z  BAS   RE,GLAUTH           ADD AUTHORIZATION RECORDS                    
*                                                                               
         FOUT  (R4)                                                             
*                                                                               
         CLI   BACT,C'B'           TEST 'BUDGET' FLAG                           
         BNE   GLREQ1                                                           
         MVI   9(R4),C'A'          SET PROPER ACTN CODE                         
         B     GLNEXT                                                           
*                                                                               
GLREQ1   MVC   WORK(5),GOLPLNR                                                  
         OC    WORK(5),SPACES                                                   
         CLC   =C'SPOT ',WORK                                                   
         BE    GLREQX                                                           
         CLI   SVPPROF1,C'N'       TEST SUPPRESS T/A                            
         BE    GLREQX                                                           
*                                                                               
         CLI   SVPRD,X'FF'         NO CPP TURNAROUNDS                           
         BE    GLREQX                                                           
         CLC   BMKT,SVMKT          TEST THIS MARKET REQUESTED YET               
         BE    GLREQX                                                           
*                                                                               
         OC    SVMKT,SVMKT         TEST ANY REQUESTS THESE HEADLINES            
         BZ    GLREQ2              NO                                           
*                                                                               
         CLI   SVPPROF1,C'M'       TEST T/A BY MARKET                           
         BNE   GLREQX                                                           
*                                                                               
GLREQ2   MVC   SVMKT,BMKT          INDICATE MARKET REQUESTED                    
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R4,ELEM+26                                                       
         MVI   0(R4),C' '                                                       
         MVC   1(79,R4),0(R4)                                                   
*                                                                               
         MVC   0(2,R4),=C'U9'                                                   
         CLI   SVPPROF4,C'8'                                                    
         BNE   *+8                                                              
         MVI   1(R4),C'8'                                                       
         MVC   2(2,R4),AGYALPHA                                                 
         MVC   4(2,R4),GOLMD                                                    
         MVC   5(3,R4),QCLT                                                     
         MVC   11(3,R4),QPRD                                                    
         CLI   SVPPROF4,C'8'                                                    
         BNE   *+10                                                             
         MVC   11(3,R4),=C'POL'                                                 
*                                                                               
         LH    R0,BMKT                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  14(4,R4),DUB                                                     
*                                                                               
         CLI   SVPPROF1,C'M'       TEST T/A BY MKT                              
         BE    *+10                                                             
         MVC   14(4,R4),=CL4'ALL'                                               
*                                                                               
         SR    R0,R0                                                            
         IC    R0,SVEST                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  23(3,R4),DUB                                                     
*                                                                               
         MVC   37(12,R4),SVSTART   EST START/END                                
*                                                                               
         MVI   62(R4),C'G'                                                      
         MVC   68(12,R4),GOLPLNR                                                
         OC    68(12,R4),SPACES                                                 
*                                                                               
GLREQ10  GOTO1 VDATAMGR,DMCB,=C'DMADD',=C'REQUEST',ELEM,ELEM                    
*                                                                               
         CLC   SVRFPGRP,SPACES                                                  
         BNH   GLREQX                                                           
         CLC   =C'G7',AGYALPHA                                                  
         BNE   *+12                                                             
         TM    SVEFLAG1,X'80'      TEST GENERATE RFP REQ                        
         BZ    GLREQX              NO                                           
         MVC   0(2,R4),=C'RF'                                                   
         MVC   49(8,R4),SVRFPGRP                                                
         GOTO1 (RF),(R1)                                                        
*                                                                               
GLREQX   DS    0H                                                               
         EJECT                                                                  
GLNEXT   SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BE    EXXMOD                                                           
* CHECK FOR NO INPUT FIELDS ON NEXT LINE                                        
         LA    RE,7                                                             
         LR    R4,R2                                                            
*                                                                               
GLN2     TM    1(R4),X'20'         TEST PROTECTED                               
         BO    GLN4                                                             
         CLI   5(R4),0                                                          
         BNE   GL2                 EDIT NEXT LINE                               
*                                                                               
GLN4     IC    R0,0(R4)                                                         
         AR    R4,R0                                                            
         BCT   RE,GLN2                                                          
*                                                                               
* CLEAR PROTECTED FIELDS ON REMAINING LINES                                     
*                                                                               
GLX2     TM    1(R2),X'20'         TEST PROTECTED                               
         BZ    GLX4                NO                                           
         SR    R5,R5                                                            
         IC    R5,0(R2)                                                         
         SH    R5,=H'9'                                                         
         EX    R5,*+8                                                           
         B     *+10                                                             
         OC    8(0,R2),8(R2)  ** EXECUTED **                                    
*                                                                               
         BZ    GLX4                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         XC    8(0,R2),8(R2)  ** EXECUTED **                                    
*                                                                               
         FOUT  (R2)                                                             
*                                                                               
GLX4     IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BNE   GLX2                                                             
         B     EXXMOD                                                           
         EJECT                                                                  
* SET/RESET STATUS BIT FOR ALLOCATION PROGRAM                                   
* TO IGNORE GOAL RECORDS FOR THIS PRD/EST/MKT                                   
*                                                                               
GLSTAT   DS    0H                                                               
         OI    DMINBTS,X'08'       SET TO PASS DELETES                          
         XC    KEY,KEY                                                          
         MVI   KEY,2                                                            
         MVC   KEY+1(1),SVAGYMD                                                 
         MVC   KEY+2(2),SVCLT                                                   
         MVC   KEY+4(1),SVPRD                                                   
         MVC   KEY+5(2),BMKT                                                    
         MVC   KEY+7(1),SVEST                                                   
         MVI   ERRCD,NODTAERR                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(8),KEYSAVE                                                   
         BNE   GLERR                                                            
         B     GLSTAT4                                                          
GLSTAT2  DS    0H                                                               
         GOTO1 SEQ                                                              
         CLC   KEY(8),KEYSAVE      02/A-M/CLT/PRD/MKT/EST                       
         BNE   GLSTATX                                                          
GLSTAT4  DS    0H                                                               
         LA    RE,GOALREC                                                       
         ST    RE,AREC                                                          
         GOTO1 GETREC                                                           
         OI    GDSTAT,X'80'                                                     
         CLI   BACT,C'-'                                                        
         BE    *+8                                                              
         NI    GDSTAT,X'7F'                                                     
         GOTO1 PUTREC                                                           
         B     GLSTAT2                                                          
*                                                                               
GLSTATX  DS    0H                                                               
         L     R4,BLNADDR                                                       
         MVI   8(R4),C'*'                                                       
         MVC   9(1,R4),BACT                                                     
         MVI   10(R4),C' '                                                      
         FOUT  (R4)                                                             
* POSTITION CURSOR TO PERIOD FIELD                                              
         LA    R0,5                                                             
GLSTATX2 ZIC   RE,0(R2)                                                         
         AR    R2,RE                                                            
         BCT   R0,GLSTATX2                                                      
         B     GLNEXT                                                           
         EJECT                                                                  
NEXTEL   SR    R0,R0                                                            
         CLI   0(R6),0             TEST E-O-R                                   
         JE    NEXTELX                                                          
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
NEXTEL2  CLC   ELCODE,0(R6)                                                     
         JNE   NEXTEL                                                           
         BR    RE                                                               
NEXTELX  LTR   RE,RE                                                            
         BR    RE                                                               
*                                                                               
TSTNUM   MVI   ERRCD,INVERR                                                     
TSTNUM2  CLI   0(R1),C'0'                                                       
         BL    GLERR                                                            
         CLI   0(R1),C'9'                                                       
         BH    GLERR                                                            
         LA    R1,1(R1)                                                         
         BCT   R0,TSTNUM2                                                       
         BR    RE                                                               
*                                                                               
GLERR    GOTO1 ERROR                                                            
*                                                                               
EXIT     XIT1                                                                   
EXXMOD   EQU   EXIT                                                             
         EJECT                                                                  
*                                                                               
********************************************************************            
* CALL SPAUTH TO ADD OR UPDATE SUPERDESK AUTH MKT LEVEL RECORDS    *            
********************************************************************            
GLAUTH   NTR1                                                                   
         TM    SVEFLAG1,EF1SDE           IS THIS A SUPERDESK EST?               
         BNO   GLAUTHX                                                          
*                                                                               
         PUSH  USING                                                            
         USING SPAUTHD,WORK                                                     
         XC    SPAUTHD(SPAUTHLQ),SPAUTHD                                        
         MVC   SPACOM,VCOMFACS                                                  
         MVC   SPAKAM,SVAGYMD                                                   
         MVC   SPAKCLT,SVCLT                                                    
         MVC   SPAKPRD,SVPRD                                                    
         MVC   SPAKPRD2,SVPRD2                                                  
         MVC   SPAKEST,SVEST                                                    
         MVC   SPAKMKT,BMKT                                                     
         LA    RF,REC2                                                          
         ST    RF,SPAIO                                                         
         LA    RF,BWEEKS                                                        
         MVC   SPASDTE,0(RF)       GOAL START DATE                              
         CLI   2(RF),0                                                          
         BE    *+12                                                             
         LA    RF,2(RF)                                                         
         B     *-12                                                             
         MVC   SPAEDTE,0(RF)       GOAL END DATE (WEEK)                         
         GOTO1 VDATCON,(R1),(2,SPAEDTE),(0,WORK2) END DATE (CHAR)               
         GOTO1 VADDAY,(R1),(C'D',WORK2),WORK2+6,6 +6 FOR END OF WEEK            
         GOTO1 VDATCON,(R1),WORK2+6,(2,SPAEDTE)   END DATE (COMPRESSED)         
*                                                                               
         MVI   SPAUPDT,SPAUPGOL    UPDATE GOAL DATES                            
*                                                                               
         MVC   DMCB+4(3),=X'D9000A'                                             
         MVI   DMCB+7,QSPAUTH                                                   
         GOTO1 VCALLOV,DMCB,0      GET SPAUTH ADDR                              
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,0(R1)                                                         
         GOTO1 (RF),WORK                                                        
         CLI   SPAERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
GLAUTHX  XIT1  ,                                                                
         POP   USING                                                            
         EJECT                                                                  
********************************************************************            
* SUBROUTINE TO APPORTION INPUT AMONG DAYPARTS IN SPLIT RECORD     *            
* OUTPUT IS IN REC2+24 IS SERIES OF 12 BYTE ENTRIES                *            
*           DPT(1)/PCTG(2)/SPARE(1)/PTS(4)/DOLS(4)                 *            
* ROUNDING DIFFERENCES ARE APPORTIONED TO LARGEST VALUES IN        *            
* DESCENDING ORDER                                                 *            
********************************************************************            
         SPACE 1                                                                
GLSPLT   NTR1                                                                   
         LA    R3,REC2+24          MOVE NON-ZERO ENTRIES                        
GLSPLT2  ZIC   R0,1(R3)                                                         
         AR    R3,R0                                                            
         CLI   0(R3),2                                                          
         BNE   GLSPLT2                                                          
         LA    R3,2(R3)            POINT TO FIRST DPT                           
         LA    R8,ELEM                                                          
         XC    0(256,R8),0(R8)     CLEAR WORK AREA                              
         SR    RF,RF               CLEAR COUNTER                                
*                                                                               
GLSPLT4  OC    1(2,R3),1(R3)       TEST NON-ZERO PCT                            
         BZ    GLSPLT6                                                          
         MVC   0(3,R8),0(R3)       MOVE DPT/PCTG                                
         LA    RF,1(RF)            BUMP COUNTER                                 
         SPACE 1                                                                
* COMPUTE VALUE FOR THIS DPT *                                                  
         SPACE 1                                                                
         CLI   BACT,C'D'           TEST ACTUAL DELETE                           
         BE    GLSPLT5A            YES                                          
         CLI   BACT,C'H'           TEST VIRTUAL DELETE (0 PTS/DOLS)             
         BNE   GLSPLT5X            NO                                           
*                                                                               
GLSPLT5A LHI   R0,-1                                                            
         ST    R0,4(R8)                                                         
         ST    R0,8(R8)                                                         
         LA    R8,12(R8)                                                        
         B     GLSPLT6                                                          
*                                                                               
GLSPLT5X MVC   DUB(2),1(R3)                                                     
         LH    R1,DUB                                                           
         AR    R1,R1               X 2                                          
         M     R0,BPTS                                                          
         D     R0,=F'10000'                                                     
         AHI   R1,1                                                             
         SRL   R1,1                                                             
         ST    R1,4(R8)                                                         
*                                                                               
         L     R0,BDOLS                                                         
         LH    R1,DUB                                                           
         MR    R0,R0                                                            
         A     R1,=F'5000'                                                      
         D     R0,=F'10000'                                                     
         ST    R1,8(R8)                                                         
         LA    R8,12(R8)                                                        
*                                                                               
GLSPLT6  LA    R3,3(R3)                                                         
         CLI   0(R3),0             TEST DPT PRESENT                             
         BNZ   GLSPLT4                                                          
*                                                                               
         ST    RF,DUB              SAVE NUMBER OF ENTRIES                       
*                                                                               
         GOTO1 VCALLOV,DMCB,0,X'D9000A12'  GET XSORT ADDRESS                    
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   DUB+4(4),DMCB       AND SAVE IT                                  
* FIRST TIME ONLY SORT ON PCTGS                                                 
         DS    0H                                                               
         L     RF,DUB+4            GET XSORT ADDRESS                            
         L     R0,DUB              GET RECORD COUNT                             
         GOTO1 (RF),DMCB,(1,ELEM),(R0),12,2,1                                   
         B     GLSPLT12                                                         
* OTHERWISE SORT ON  POINTS                                                     
GLSPLT10 DS    0H                                                               
         L     RF,DUB+4            GET XSORT ADDRESS                            
         L     R0,DUB              GET RECORD COUNT                             
         GOTO1 (RF),DMCB,(1,ELEM),(R0),12,4,4                                   
         SPACE 1                                                                
* SUM THE ALLOCATIONS AND COMPARE TO ORIGINAL INPUT *                           
         SPACE 1                                                                
GLSPLT12 CLI   BACT,C'H'           TEST VIRTUAL DELETE                          
         BE    GLSPLTX                                                          
         CLI   BACT,C'D'           TEST ACTUAL DELETE                           
         BE    GLSPLTX                                                          
*                                                                               
         LA    R8,ELEM                                                          
         SR    R0,R0               CLEAR POINTS ACCUM                           
         L     RF,DUB              GET RECORD COUNT                             
*                                                                               
GLSPLT14 A     R0,4(R8)            ADD TO POINTS                                
         LA    R8,12(R8)                                                        
         BCT   RF,GLSPLT14                                                      
*                                                                               
         S     R0,BPTS                                                          
         BZ    GLSPLT20                                                         
         LA    RF,1                POINTS ARE IN TENTHS                         
         BM    *+6                 IF SUM TOO LOW, NEED TO ADD                  
         LNR   RF,RF               AND VICE VERSA                               
*                                                                               
         LA    R8,ELEM                                                          
         L     RE,4(R8)                                                         
         AR    RE,RF                                                            
         ST    RE,4(R8)                                                         
         B     GLSPLT10                                                         
         SPACE 1                                                                
* RE-SORT TO ORIGINAL SEQUENCE ON DESCENDING PERCENTAGES                        
         SPACE 1                                                                
GLSPLT20 L     RF,DUB+4            GET XSORT ADDRESS                            
         L     R0,DUB              GET RECORD COUNT                             
         GOTO1 (RF),DMCB,(1,ELEM),(R0),12,2,1                                   
         B     GLSPLT24                                                         
* OTHERWISE SORT ON DOLLARS                                                     
GLSPLT22 L     RF,DUB+4            GET XSORT ADDRESS                            
         L     R0,DUB              GET RECORD COUNT                             
         GOTO1 (RF),DMCB,(1,ELEM),(R0),12,4,8                                   
         SPACE 1                                                                
* SUM THE DOLLARS AND COMPARE TO ORIGINAL INPUT *                               
         SPACE 1                                                                
GLSPLT24 LA    R8,ELEM                                                          
         SR    R0,R0               CLEAR ACCUM                                  
         L     RF,DUB              GET RECORD COUNT                             
*                                                                               
GLSPLT26 A     R0,8(R8)            ADD TO DOLLARS                               
         LA    R8,12(R8)                                                        
         BCT   RF,GLSPLT26                                                      
*                                                                               
         S     R0,BDOLS                                                         
         BZ    GLSPLTX                                                          
         LA    RF,1                DOLLARS ARE IN PENNIES                       
         BM    *+6                 IF SUM TOO LOW, NEED TO ADD                  
         LNR   RF,RF               AND VICE VERSA                               
*                                                                               
         LA    R8,ELEM                                                          
         L     RE,8(R8)                                                         
         AR    RE,RF                                                            
         ST    RE,8(R8)                                                         
         B     GLSPLT22                                                         
*                                                                               
GLSPLTX  MVC   REC2+24(256),ELEM   MOVE OUTPUT OVER RECORD DATA                 
         B     EXIT                                                             
         EJECT                                                                  
* THIS ROUTINE CHECKS THAT FIRST ELEMENT IS EST START DATE OR                   
* MONDAY AND CHANGES DATE IF NECESSARY.                                         
*                                                                               
CHKELDT  NTR1                                                                   
         CLI   SVEDAILY,C'Y'       NOT FOR DAILY !                              
         BE    CHKELX                                                           
         LA    R6,GDELEM                                                        
*                                                                               
CHKEL2   ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BE    CHKELX                                                           
         CLI   0(R6),X'21'                                                      
         BE    CHKEL4                                                           
         CLI   SVOPT1,C'G'                                                      
         BNE   CHKEL2                                                           
         CLI   0(R6),X'A1'                                                      
         BNE   CHKEL2                                                           
*                                                                               
CHKEL4   GOTO1 VDATCON,DMCB,(2,2(R6)),WORK  GET 6-BYTE DATE                     
*                                                                               
         CLC   SVSTART,WORK        TEST EST START DATE                          
         BE    CHKELX                                                           
*                                                                               
         CLI   SVEOWSDY,0         TEST OUT-OF-WEEK DATA                         
         BE    CHKEL5                                                           
         GOTO1 USER4              YES -- FIX ELEMENT DATES IN RECORD            
         B     CHKELX                                                           
*                                                                               
CHKEL5   GOTO1 VGETDAY,DMCB,WORK,WORK+6                                         
*                                                                               
         CLI   0(R1),1             TEST MONDAY                                  
         BE    CHKELX                                                           
* BACK UP TO PREVIOUS MONDAY OR ESTART                                          
         ZIC   R0,0(R1)                                                         
         BCTR  R0,0                                                             
         LCR   R0,R0                                                            
         GOTO1 VADDAY,DMCB,WORK,WORK+6,(R0)                                     
*                                                                               
         CLC   SVSTART,WORK+6                                                   
         BL    *+10                                                             
         MVC   WORK+6(6),SVSTART                                                
*                                                                               
         GOTO1 VDATCON,DMCB,WORK+6,(2,2(R6))                                    
*                                                                               
CHKELX   XIT1                                                                   
         LTORG                                                                  
         PRINT OFF                                                              
       ++INCLUDE SPGOLWRK                                                       
       ++INCLUDE SPGENAUTH                                                      
       ++INCLUDE DDCOREQUS                                                      
       ++INCLUDE SPAUTHD                                                        
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE FAUTL                                                          
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'043SPGOL03   08/24/20'                                      
         END                                                                    
