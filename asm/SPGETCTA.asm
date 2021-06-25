*          DATA SET SPGETCTA   AT LEVEL 006 AS OF 05/01/02                      
*PHASE T00A7CA                                                                  
         SPACE 1                                                                
*=================================================================*             
* 17NOV98 MHER IF BDCIND2 = X'20' TBS BRAND POL RULES APPLY       *             
*              AND A BUY IS EITHER A TRADE BUY (PRD ENDS IN T)    *             
*              OR NOT (PRD ENDS IN C)                             *             
*              IF BDSTAT2 = X'20' WT TRUE POL RULES APPLY AND     *             
*              TRADE STATUS IS DETERMINED AT THE SPOT LEVEL       *             
*              X'80' IN PRD CODE MEANS TRADE PRD (ENDS IN $)      *             
*=================================================================*             
SPGETCTA TITLE 'MAINTAIN TBS CONTRACT ANALYSIS DOLLARS'                         
SPGETCTA CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORKX-WORKD,SPGETCTA,CLEAR=YES,RR=R8                             
         USING WORKD,RC                                                         
         ST    R8,RELO                                                          
         LR    RA,R1                                                            
         USING CIBBLKD,RA                                                       
         MVI   CIBERR,0                                                         
         MVC   CIBBLKID,=C'*CIBBLK*'  SET EYECATCHERS                           
         MVC   BUCKETID,=C'*BUCKETS'                                            
         MVC   KEYID,=C'*KEY'                                                   
*                                                                               
         L     RF,CIBACOMF                                                      
         USING COMFACSD,RF                                                      
         L     RF,CCALLOV                                                       
         DROP  RF                                                               
         MVC   DMCB+4(3),=X'D9000A'                                             
         MVI   DMCB+7,QGETRATE                                                  
         GOTO1 (RF),DMCB,0                                                      
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   VGETRATE,0(R1)                                                   
*                                                                               
         L     R8,CIBABUY          GET BUY RECORD ADDRESS                       
         USING BUYRECD,R8                                                       
*                                                                               
         CLI   CIBIAM,CIBPAYQ      TEST PAY PROGRAM CALL                        
         BE    *+12                                                             
         CLI   CIBACT,CIBUPDQ                                                   
         BNE   GCTA05                                                           
         BAS   RE,UPDATE                                                        
         B     EXIT                                                             
*                                                                               
GCTA05   TM    BDSTAT2,X'20'       NON-TBS TRADE BUY                            
         BO    GCTA10                                                           
         TM    BDCIND2,X'02'       TBS TRADE BUY                                
         BO    GCTA10              YES                                          
         CLI   CIBACT,CIBCHGQ      TEST CHANGE                                  
         BE    GCTA20              YES - REMOVE OLD CON DOLLARS                 
         B     EXIT                ELSE EXIT                                    
*                                                                               
GCTA10   LA    R7,CIBOLD           COPY DATA GOES IN OLD                        
         CLI   CIBACT,CIBCPYQ                                                   
         BE    *+8                                                              
         LA    R7,CIBNEW           ADD/CHG DATA GOES IN NEW                     
*                                                                               
         USING CIBDATA,R7                                                       
*                                                                               
         BAS   RE,BUYVALS          EXTRACT BUY VALUES                           
         CLI   CIBERR,0                                                         
         BNE   EXIT                                                             
*                                                                               
         BAS   RE,GETCON           GET PWOS CONTRACT (OR NONE)                  
         CLI   CIBERR,0                                                         
         BNE   EXIT                                                             
*                                                                               
GCTA20   TM    CIBFLAGS,CIBNUPDQ   TEST 'DO NOT UPDATE'                         
         BO    EXIT                                                             
         CLI   CIBACT,CIBCPYQ      TEST PROCESS COPY                            
         BE    EXIT                YES - DEFER UPDATE TILL CHANGE               
*                                                                               
         BAS   RE,UPDATE           UPDATE CTA RECORD                            
         BAS   RE,UPDBUY           INSERT NEW CONTRACT ELEM IN BUY              
         CLI   CIBERR,0                                                         
         BNE   EXIT                                                             
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
RELO     DS    A                                                                
         EJECT                                                                  
BUYVALS  NTR1                                                                   
         USING BUYRECD,R8                                                       
*                                                                               
         TM    BUYREC+15,X'80'     TEST BUYREC DELETED                          
         BO    EXIT                YES - NO NEW VALUES                          
*                                                                               
BV10     LA    R6,BDELEM                                                        
*                                                                               
         MVI   ELCDLO,11                                                        
         MVI   ELCDHI,13                                                        
         CLI   BUYKEY+3,X'FF'      TEST POL BUY                                 
         BE    BV12                                                             
         MVI   ELCDLO,6                                                         
         MVI   ELCDHI,8                                                         
*                                                                               
BV12     DS    0H                  SET UP FOR MULTIPLE GETRATE CALLS            
         L     RF,VGETRATE                                                      
         LA    R1,DMCB                                                          
         LA    RE,SPOTS                                                         
         ST    RE,0(R1)                                                         
         MVC   0(1,R1),BUYKPRD                                                  
         ST    R8,4(R1)            SET A(BUYREC)                                
         EJECT                                                                  
         USING REGELEM,R6                                                       
BV20     BAS   RE,NEXTEL                                                        
         BNE   EXIT                                                             
         TM    BDSTAT2,X'20'       TEST NON-TBS TRADE BUY                       
         BZ    BV22                NO                                           
* NON-TBS TRADE BUY STUPIDO                                                     
         CLI   1(R6),10            TEST ALLOCATED                               
         BE    BV20                NO - IGNORE                                  
         TM    10(R6),X'80'        TEST TRADE PRODUCT                           
         BZ    BV20                NO - IGNORE                                  
         OI    CIBDFLAG,X'40'      SET FLAG FOR TRADE SPOT FOUND                
* CALL GETRATE -- NOTE RF, R1 ASSUMED TO BE INTACT                              
BV22     GOTO1 (RF),(R1),,,(R6)                                                 
*                                                                               
         L     R0,CIBDORD                                                       
         A     R0,GROSS                                                         
         ST    R0,CIBDORD                                                       
*                                                                               
         OC    RPAY,RPAY           TEST REGEL PAID                              
         BZ    BV20                NO                                           
*                                                                               
         L     R0,CIBDPAID                                                      
         A     R0,GROSS                                                         
         ST    R0,CIBDPAID                                                      
         B     BV20                                                             
         DROP  R6,R8                                                            
         EJECT                                                                  
*==========================================================*                    
* UPDATE CTA RECORDS                                       *                    
* DO NEW RECORD FIRST SO IF ERROR, DATA REMAINS IN OLD     *                    
*==========================================================*                    
         SPACE                                                                  
UPDATE   NTR1                                                                   
         USING BUYRECD,R8                                                       
*                                                                               
         LA    R7,CIBNEW           POINT TO NEW DATA                            
         USING CIBDATA,R7                                                       
*                                                                               
         CLI   CIBIAM,CIBPAYQ      TEST PAY PROGRAM CALL                        
         BE    UPDATE2                                                          
*                                                                               
         OC    CIBNEW,CIBNEW       TEST ANY NEW DATA                            
         BZ    UPDATE4             NO                                           
         CLC   CIBNCON,CIBOCON     TEST CHANGE OF CONTRACT                      
         BNE   UPDATE2             YES                                          
         LM    R0,R1,CIBNORD       GET NEW ORD/PAID DOLLARS                     
         S     R0,CIBOORD          LESS OLD DOLLARS                             
         S     R1,CIBOPAID                                                      
         STM   R0,R1,CIBNORD       SET DIFFERENCE IN NEW                        
         XC    CIBOLD,CIBOLD       THEN CLEAR OLD                               
         OC    CIBNORD(8),CIBNORD  TEST ANY CHANGE IN VALUES                    
         BZ    EXIT                NO - DONE                                    
**NOP    TM    BDSTAT2,X'20'       TEST NON-TBS TRADE BUY                       
**NOP    BZ    UPDATE2                                                          
**NOP    OC    CIBOCON,CIBOCON     TEST ANY OLD CONTRACT                        
**NOP    BZ    EXIT                NO - EXIT                                    
*                                                                               
UPDATE2  BAS   RE,UPDFILE                                                       
         CLI   CIBERR,0                                                         
         BNE   EXIT                                                             
*                                                                               
UPDATE4  LA    R7,CIBOLD           POINT TO OLD DATA                            
         OC    CIBOLD,CIBOLD       TEST ANY OLD DATA                            
         BZ    EXIT                                                             
         LM    R0,R1,CIBOORD       GET OLD ORD/PAID DOLLARS                     
         LCR   R0,R0               AND COMPLEMENT THEM                          
         LCR   R1,R1                                                            
         STM   R0,R1,CIBOORD                                                    
         BAS   RE,UPDFILE                                                       
         B     EXIT                                                             
         EJECT                                                                  
UPDFILE  NTR1                                                                   
         USING CIBDATA,R7                                                       
* FIRST BUILD KEY OF CONTRACT RECORD                                            
         XC    KEY,KEY                                                          
         LA    R1,KEY                                                           
         USING CTAKEY,R1                                                        
         XC    KEY,KEY                                                          
         MVI   CTAKTYP,CTAKTYPQ                                                 
         MVI   CTAKSUB,CTAKSUBQ                                                 
         MVC   CTAKAGMD,BUYKAM                                                  
         MVC   CTAKCNUM,CIBDCON    CONTRACT NUMBER                              
         DROP  R1                                                               
         EJECT                                                                  
         MVC   KEYSAVE,KEY                                                      
         L     RF,CIBACOMF                                                      
         USING COMFACSD,RF                                                      
         L     RF,CDATAMGR                                                      
         DROP  RF                                                               
         GOTO1 (RF),DMCB,=C'DMRDHI',=C'SPTDIR',KEYSAVE,KEY                      
         CLC   KEY(13),KEYSAVE                                                  
         BNE   BADCON                                                           
*                                                                               
         GOTO1 (RF),DMCB,(X'80',=C'GETREC'),=C'SPTFILE',KEY+14,CIBAIO, X        
               DMWORK                                                           
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
* BUILD CTAUSEL                                                                 
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING CTAUSELD,R6                                                      
*                                                                               
         MVI   CTAUSEL,CTAUSELQ                                                 
         MVI   CTAUSELN,CTAUSLNQ                                                
         MVC   CTAUSSTA,BUYMSTA+2  STATION                                      
         MVC   CTAUSCLT,BUYKCLT    CLIENT                                       
         MVC   CTAUSPRD,BUYKPRD    PRODUCT                                      
         MVC   CTAUSEST,BUYKEST    ESTIMATE                                     
         MVC   CTAUSOGR,CIBDORD    NEW ORD                                      
         MVC   CTAUSPGR,CIBDPAID   NEW PAID                                     
         DROP  R6                                                               
*                                                                               
         L     R8,CIBAIO                                                        
         USING CTARECD,R8                                                       
*                                                                               
         TM    CTARCNTL,CTARCBAL                                                
         BZ    UPD8                                                             
         MVI   CIBERR,CIBGCIQ      CONTRACT OUT OF BALANCE                      
         OI    CIBDFLAG,X'80'      SET 'THIS CONTRACT IN ERR'                   
         B     EXIT                                                             
*                                                                               
UPD8     TM    CTARCNTL,CTARCACC                                                
         BO    UPD10                                                            
         MVI   CIBERR,CIBNOACC     NO ACC CONTRACT                              
         OI    CIBDFLAG,X'80'      SET 'THIS CONTRACT IN ERR'                   
         B     EXIT                                                             
         EJECT                                                                  
* SEARCH RECORD FOR MATCHING ELEMENT                                            
*                                                                               
UPD10    LA    R6,CTAEL                                                         
         USING CTAUSELD,R6                                                      
         SR    R0,R0                                                            
*                                                                               
UPD12    ICM   R0,1,1(R6)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BE    UPD15                                                            
*                                                                               
         CLC   0(L'CTAUSKEY,R6),ELEM   NEW ELEMENT TO RECORD ELEM               
         BL    UPD12                   IF LOW, KEEP GOING                       
         BH    UPD15                   HIGH - INSERT ELEM BEFORE                
*                                                                               
* ADD VALUES IN CURRENT ELEMENT TO EXISTING ELEMENT                             
*                                                                               
         ICM   R0,15,CTAUSOGR      ORD FROM OLD ELEMENT                         
         A     R0,CIBDORD          NEW ORD                                      
         STCM  R0,15,CTAUSOGR                                                   
*                                                                               
         ICM   R0,15,CTAUSPGR      PAID FROM OLD ELEMENT                        
         A     R0,CIBDPAID         NEW PAID                                     
         STCM  R0,15,CTAUSPGR                                                   
*                                                                               
         OC    CTAUSOGR(8),CTAUSOGR  TEST ANY DOLLARS LEFT                      
         BNZ   UPD20                                                            
         GOTO1 CIBARCUP,DMCB,(C'S',(R8)),(R6)                                   
         B     UPD20                                                            
*                                                                               
UPD15    DS    0H                  ADD ELEMENT TO RECORD                        
         GOTO1 CIBARCUP,DMCB,(C'S',(R8)),(C'R',ELEM),(R6)                       
         CLI   4(R1),0             TEST OVERFLOW                                
         BNE   *+6                 NO                                           
         DC    H'0'                                                             
*                                                                               
UPD20    TM    CIBFLAGS,CIBDOITQ   TEST ALLOW OVER BUDGET                       
         BO    UPD40               YES - DON'T BOTHER ADDING IT UP              
         CLI   CIBIAM,CIBPAYQ      TEST PAY PROGRAM CALL                        
         BE    UPD40                                                            
         L     R0,CIBDORD          GET CHANGE AMOUNT                            
         LTR   R0,R0                                                            
         BM    UPD40               IF NEGATIVE, DON'T BOTHER CHECKING           
*                                                                               
* ELSE NEED TO CHECK IF OVER BUDGET                                             
*                                                                               
         MVI   ELCDLO,7            FIRST FIND TRANSFER ELEMENTS                 
         MVI   ELCDHI,7                                                         
         LA    R6,CTAEL                                                         
         USING CTAXFELD,R6                                                      
*                                                                               
         XC    SVXFROGR,SVXFROGR                                                
UPD22    BAS   RE,NEXTEL                                                        
         BNE   UPD24                                                            
         ICM   R0,15,CTAXFOGR                                                   
         TM    CTAXSTAT,CTAXSTIN   TEST USAGE FROM ANOTHER CONTRACT             
         BZ    *+6                 NO                                           
         LCR   R0,R0               USAGE FROM REDUCES GCI                       
         A     R0,SVXFROGR                                                      
         ST    R0,SVXFROGR                                                      
         B     UPD22                                                            
*                                                                               
UPD24    LA    R6,CTAEL                                                         
         USING CTAUSELD,R6                                                      
         ICM   RF,15,CTDSCGCI      GET GROSS CONTRACT VALUE                     
         A     RF,SVXFROGR         ADJUST FOR TRANSFERS                         
*                                                                               
         TM    CTDSCTYP,X'84'      TEST DOLLARS BY PARTICIPANT                  
         BZ    UPD30               NO - GO ADD UP USAGE                         
* NEED TO FIND ELEMENT FOR THIS PARTICIPANT                                     
         MVI   ELCDLO,5                                                         
         MVI   ELCDHI,5                                                         
*                                                                               
UPD26    BAS   RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         USING CTPARD,R6                                                        
         L     RE,CIBABUY                                                       
         CLC   CTPARSTA,BUYMSTA+2-BUYRECD(RE)                                   
         BNE   UPD26                                                            
         ICM   RF,15,CTPARAMT      GET DOLLARS FOR THIS STATION                 
*                                                                               
UPD30    MVI   ELCDLO,6                                                         
         MVI   ELCDHI,6                                                         
         LA    R6,CTAEL                                                         
         USING CTAUSELD,R6                                                      
*                                                                               
UPD32    BAS   RE,NEXTEL                                                        
         BNE   UPD40                                                            
         TM    CTDSCTYP,X'84'      TEST DOLLARS BY PARTICIPANT                  
         BZ    UPD34                                                            
         L     RE,CIBABUY                                                       
         CLC   CTAUSSTA,BUYMSTA+2-BUYRECD(RE)  TEST RIGHT STATION               
         BNE   UPD32                                                            
UPD34    S     RF,CTAUSOGR                                                      
         BNM   UPD32                                                            
UPD36    MVI   CIBERR,CIBMAX$Q                                                  
         OI    CIBDFLAG,X'80'      SET 'THIS CON IN ERR'                        
         B     EXIT                                                             
*                                                                               
UPD40    L     RF,CIBACOMF         ALL DONE - WRITE RECORD                      
         USING COMFACSD,RF                                                      
         L     RF,CDATAMGR                                                      
         DROP  RF                                                               
         TM    CIBFLAGS,CIBNOWRT                                                
         BO    EXIT                                                             
         GOTO1 (RF),DMCB,(X'80',=C'PUTREC'),=C'SPTFILE',KEY+14,CIBAIO, X        
               DMWORK                                                           
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         B     EXIT                                                             
         DROP  R6,R8                                                            
         EJECT                                                                  
*============================================================*                  
* SUBROUTINE SEARCHES BUYREC FOR CONTRACT NUMBER             *                  
* PWOS CONTRACT IS RETURNED IN CIBDCON                       *                  
* IF ACTION = ADD AND THERE IS NO CONTRACT IN THE BUY, THE   *                  
* OLDEST CONTRACT WITH UNRESERVED DOLLARS IS USED            *                  
*============================================================*                  
         SPACE 1                                                                
GETCON   NTR1                                                                   
         USING BUYREC,R8                                                        
         TM    BDSTAT2,X'20'       NON TBS TRADE BUY                            
         BO    *+12                                                             
         TM    BDCIND2,X'02'       TBS TRADE BUY                                
         BZ    EXIT                NO                                           
*                                                                               
         MVI   ELCDLO,X'70'        FIND CONTRACT ID ELEMENT IN BUY              
         MVI   ELCDHI,X'70'                                                     
         LA    R6,BDELEM                                                        
         BAS   RE,NEXTEL                                                        
         BNE   GETCON4                                                          
*                                                                               
         LA    R1,3(R6)            TEST CONTRACT NUMERIC                        
         LA    R0,5                                                             
*                                                                               
GETCON2  CLI   0(R1),C'0'                                                       
         BL    BADCON                                                           
         CLI   0(R1),C'9'                                                       
         BH    BADCON                                                           
         LA    R1,1(R1)                                                         
         BCT   R0,GETCON2                                                       
*                                                                               
         ZAP   DUB(4),=P'999999'                                                
         PACK  DUB+4(4),3(5,R6)    PACK CONTRACT NUMBER                         
         SP    DUB(4),DUB+4(4)     GIVES 9'S COMPLEMENT                         
         L     R0,DUB                                                           
         SRL   R0,4                                                             
         STCM  R0,7,CIBDCON        STORE PWOS CONTRACT NUM                      
         B     EXIT                                                             
         EJECT                                                                  
* NO CONTRACT IN BUY - NEED TO FIND ONE                                         
*                                                                               
GETCON4  CLI   CIBIAM,CIBPAYQ      IS THIS A PAY PROGRAM CALL                   
         BE    BADCON              YES - CONTRACT SHOULD BE THERE               
*                                                                               
         CLI   CIBACT,CIBCPYQ      IS THIS A COPY                               
         BNE   GETCON10            NO - GO GET A CONTRACT                       
         TM    BDSTAT2,X'20'       TEST NON-TBS TRADE BUY                       
         BZ    BADCON              NO - SHOULD HAVE A CONTRACT                  
         TM    CIBOFLAG,X'40'      TEST ANY OLD TRADE SPOT                      
         BO    BADCON              YES - ERROR                                  
         B     EXIT                                                             
*                                                                               
GETCON10 TM    BDSTAT2,X'20'       TEST NON-TBS TRADE BUY                       
         BZ    GETCON12            NO                                           
         TM    CIBNFLAG,X'40'      TEST ANY NEW TRADE SPOTS                     
         BZ    EXIT                NO TRADE - EXIT                              
*                                                                               
GETCON12 L     R8,CIBABUY                                                       
         LA    R1,KEY                                                           
         USING CTAKEY,R1                                                        
         XC    KEY,KEY                                                          
         MVI   CTAPTYP,CTAPTYPQ                                                 
         MVI   CTAPSUB,CTAPSUBQ                                                 
         MVC   CTAPAGMD,BUYKAM                                                  
         MVC   CTAPMKT,BUYMSTA     MARKET                                       
         OC    DFLTMKT,DFLTMKT     IF LOOKED UP DEFAULT, USE IT                 
         BZ    *+10                                                             
         MVC   CTAPMKT,DFLTMKT                                                  
         MVC   CTAPSTA,BUYMSTA+2   STATION                                      
         DROP  R1                                                               
*                                                                               
         MVC   KEYSAVE,KEY                                                      
         LA    RE,=C'DMRDHI'                                                    
         B     *+8                                                              
*                                                                               
GETCON14 LA    RE,=C'DMRSEQ'                                                    
         ST    RE,DMCB                                                          
         L     R8,CIBABUY                                                       
*                                                                               
         L     RF,CIBACOMF                                                      
         USING COMFACSD,RF                                                      
         L     RF,CDATAMGR                                                      
         DROP  RF                                                               
         GOTO1 (RF),DMCB,,=C'SPTDIR',KEYSAVE,KEY                                
*                                                                               
         CLC   KEY(10),KEYSAVE     TYPE/A-M/MKT/STA                             
         BE    GETCON16                                                         
         OC    DFLTMKT,DFLTMKT     TEST TRIED FOR DEFAULT MKT                   
         BNZ   BADCON              YES - THEN THERE IS NO CONTRACT              
         BAS   RE,GETMKT                                                        
         CLC   DFLTMKT,BUYMSTA     TEST DEFAULT = ORIGINAL                      
         BE    BADCON                                                           
         B     GETCON12            GO TRY FOR DEFAULT MARKET                    
*                                                                               
GETCON16 GOTO1 (RF),DMCB,(X'80',=C'GETREC'),=C'SPTFILE',KEY+14,CIBAIO, X        
               DMWORK                                                           
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         EJECT                                                                  
         L     R8,CIBAIO                                                        
         USING CTARECD,R8                                                       
         TM    CTARCNTL,CTARCLOC+CTARCCLS   TEST LOCKED/CLOSED                  
         BNZ   GETCON14            YES - NO MORE BUYS - GET NEXT                
*                                                                               
         TM    CTDSCTYP,X'40'      TEST BACK-TO-BACK                            
         BZ    GETCON17            NO                                           
         LA    R1,CTDSCUS1         CHECK OK FOR THIS CLT/PRD/PERIOD             
         BAS   RE,CHKBB                                                         
         BE    GETCON17                                                         
         LA    R1,CTDSCUS2                                                      
         BAS   RE,CHKBB                                                         
         BE    GETCON17                                                         
         B     GETCON14                                                         
*                                                                               
CHKBB    L     RF,CIBABUY                                                       
         USING BUYRECD,RF                                                       
         CLC   BUYKCLT,0(R1)       TEST MATCHES CLIENT                          
         BNER  RE                                                               
         CLI   2(R1),0             TEST PRODUCT ENTERED                         
         BE    CHKBB2              NO - ALLOW ANY PRODUCT                       
         CLC   BDMASPRD(1),2(R1)   MATCH PRODUCT                                
         BNER  RE                                                               
*                                                                               
CHKBB2   OC    3(6,R1),3(R1)       TEST LIMITED DATE RANGE                      
         BZR   RE                  NO - USE THIS ONE                            
         CLC   BDSTART,3(R1)       TEST STARTS BEFORE PERIOD STARTS             
         BLR   RE                                                               
         CLC   BDEND,6(R1)         TEST ENDS AFTER PERIOD STARTS                
         BHR   RE                                                               
         CR    RE,RE               SET CC EQ                                    
         BR    RE                                                               
         DROP  RF                                                               
*                                                                               
* NEED TO TEST IF ENOUGH UNRESERVED DOLLARS IN THIS CONTRACT                    
*                                                                               
GETCON17 LA    R6,CTAEL                                                         
         ICM   RF,15,CTDSCGCI      GET GROSS CONTRACT VALUE                     
         MVI   ELCDLO,6                                                         
         MVI   ELCDHI,6                                                         
*                                                                               
GETCON19 BAS   RE,NEXTEL                                                        
         BNE   GETCON20                                                         
         USING CTAUSELD,R6                                                      
         TM    CTAUSTAT,CTAUSTBB   TEST BAL FWD ELEMENT                         
         BZ    GETCON19                                                         
         ICM   R0,15,CTAUSPGR      ADJUST VALUE BY PAID GROSS                   
         SR    RF,R0                                                            
         DROP  R6                                                               
*                                                                               
GETCON20 LA    R6,CTAEL                                                         
         TM    CTDSCTYP,X'84'      TEST DOLLARS BY PARTICIPANT                  
         BZ    GETCON24                                                         
* NEED TO FIND ELEMENT FOR THIS PARTICIPANT                                     
         MVI   ELCDLO,5                                                         
         MVI   ELCDHI,5                                                         
*                                                                               
GETCON22 BAS   RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         USING CTPARD,R6                                                        
         L     RE,CIBABUY                                                       
         CLC   CTPARSTA,BUYMSTA+2-BUYRECD(RE)                                   
         BNE   GETCON22                                                         
         ICM   RF,15,CTPARAMT      GET DOLLARS FOR THIS STATION                 
*                                                                               
GETCON24 MVI   ELCDLO,6                                                         
         MVI   ELCDHI,6                                                         
         LA    R6,CTAEL                                                         
         USING CTAUSELD,R6                                                      
*                                                                               
GETCON30 BAS   RE,NEXTEL                                                        
         BNE   GETCON40                                                         
         TM    CTAUSTAT,CTAUSTBB   TEST BAL FWD ELEMENT                         
         BO    GETCON30                                                         
         TM    CTDSCTYP,X'84'      TEST DOLLARS BY PARTICIPANT                  
         BZ    GETCON32                                                         
         L     RE,CIBABUY                                                       
         CLC   CTAUSSTA,BUYMSTA+2-BUYRECD(RE)  TEST RIGHT STATION               
         BNE   GETCON30                                                         
GETCON32 S     RF,CTAUSOGR                                                      
         BNM   GETCON30                                                         
*                                                                               
GETCON40 S     RF,CIBDORD          LESS CURRENT ORDERED DOLLARS                 
         BM    GETCON14            IF NEGATIVE, TRY ANOTHER CONTRACT            
* USE THIS CONTRACT                                                             
         MVC   CIBDCON,CTAKCNUM    SET CONTRACT IN BLOCK                        
         B     EXIT                AND EXIT                                     
         DROP  R6                                                               
*                                                                               
BADCON   MVI   CIBERR,CIBCONTQ     SET INVALID CONTRACT                         
         B     EXIT                                                             
         DROP  R8                                                               
         EJECT                                                                  
*===========================================================*                   
* ADD OR REPLACE EXISTING CONTRACT ELEMENT IN BUY RECORD    *                   
*===========================================================*                   
         SPACE 1                                                                
UPDBUY   NTR1                                                                   
         L     R8,CIBABUY                                                       
         USING BUYRECD,R8                                                       
*                                                                               
         TM    BDSTAT2,X'20'       NON-TBS TRADE BUY                            
         BO    *+12                                                             
         TM    BDCIND2,X'02'       TBS TRADE BUY                                
         BZ    UPDB8               NO - DELETE CONTRACT ELEMENT                 
         OC    CIBNEW(3),CIBNEW    TEST NEW CONTRACT ASSIGNED                   
         BZ    EXIT                NO                                           
*                                                                               
         XC    DUB,DUB             BUILD NEW ID ELEMENT                         
         XC    ELEM,ELEM                                                        
         MVI   ELEM,X'70'                                                       
         MVI   ELEM+1,15                                                        
         MVI   ELEM+2,C' '                                                      
         MVC   ELEM+3(12),ELEM+2                                                
         SR    R0,R0                                                            
         ICM   R0,7,CIBNEW         GET NEW CONTRACT NUMBER                      
         SLL   R0,4                                                             
         AH    R0,=H'12'           ADD ZONE BITS                                
         ST    R0,ELEM+16          ELEM IS ALIGNED ON DBLWD                     
         ZAP   DUB,=P'999999'                                                   
         SP    DUB,ELEM+16(4)                                                   
         OI    DUB+7,X'0F'                                                      
         UNPK  ELEM+3(5),DUB                                                    
* FIND ID ELEMENT IN BUY RECORD AND CHANGE IT                                   
UPDB8    LA    R6,BDELEM                                                        
         MVI   ELCDLO,X'70'                                                     
         MVI   ELCDHI,X'70'                                                     
         BAS   RE,NEXTEL                                                        
         BNE   UPDB10                                                           
* DELETE EXISTING ID ELEMENT                                                    
         DS    0H                                                               
         GOTO1 CIBARCUP,DMCB,(R8),(R6)                                          
*                                                                               
UPDB10   TM    BDSTAT2,X'20'       NON-TBS TRADE BUY                            
         BO    *+12                                                             
         TM    BDCIND2,X'02'       TBS TRADE BUY                                
         BZ    EXIT                NO - EXIT                                    
* INSERT NEW ID ELEMENT                                                         
         DS    0H                                                               
         GOTO1 CIBARCUP,DMCB,(R8),ELEM,(R6)                                     
         B     EXIT                                                             
         DROP  R8                                                               
         SPACE                                                                  
NEXTEL   CLI   0(R6),0                                                          
         BE    NEXTELX                                                          
         SR    R0,R0                                                            
         ICM   R0,1,1(R6)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,R0                                                            
NEXTEL2  CLC   0(1,R6),ELCDLO                                                   
         BL    NEXTEL                                                           
         CLC   0(1,R6),ELCDHI                                                   
         BH    NEXTEL                                                           
         CR    RE,RE               SET CC EQ                                    
         BR    RE                                                               
NEXTELX  LTR   RE,RE                                                            
         BR    RE                                                               
         EJECT                                                                  
*---------------------------------------------------------------*               
* WHEN NO CONTRACT IS FOUND, READ STATION MASTER RECORD TO FIND *               
* WHAT THE DEFAULT MARKET NUMBER IS. THE CONTRACT IS NEVER IN   *               
* A CLIENT OVERRIDE MARKET                                      *               
*---------------------------------------------------------------*               
         SPACE 1                                                                
GETMKT   NTR1                                                                   
         L     R8,CIBABUY                                                       
         USING BUYRECD,R8                                                       
         XC    DMCB(8),DMCB        GET STAPACK ADDRESS                          
         MVC   DMCB+4(3),=X'D900A00'                                            
         MVI   DMCB+7,QSTAPACK                                                  
         L     RF,CIBACOMF                                                      
         USING COMFACSD,RF                                                      
         L     RF,CCALLOV                                                       
         DROP  RF                                                               
         GOTO1 (RF),DMCB,0                                                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB             SAVE STAPACK ADDRESS                         
* BUILD PARAM LIST FOR STAPACK                                                  
         LA    R1,ELEM                                                          
         USING STAPACKD,R1                                                      
         MVI   STAPACT,C'U'        SET FOR UNPACK                               
         MVC   STAPACOM,CIBACOMF                                                
         MVC   STAPAGY,BUYALPHA                                                 
         MVI   STAPMED,C'T'                                                     
         TM    BUYKAM,X'01'        TEST TV                                      
         BO    *+8                                                              
         MVI   STAPMED,C'R'                                                     
         MVI   STAPCTRY,C'U'                                                    
         MVC   STAPMKST,BUYMSTA                                                 
         GOTO1 (RF),(R1)                                                        
         CLI   STAPERR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
* READ STATION MASTER RECORD                                                    
         MVI   KEY,C'0'                                                         
         MVC   KEY+1(14),KEY                                                    
         MVI   KEY,C'S'                                                         
         MVC   KEY+1(1),STAPMED                                                 
         MVC   KEY+2(5),STAPQSTA                                                
         CLI   KEY+6,C' '                                                       
         BNE   *+8                                                              
         MVI   KEY+6,C'T'                                                       
         MVC   KEY+7(2),STAPAGY                                                 
         DROP  R1                                                               
         L     RF,CIBACOMF                                                      
         USING COMFACSD,RF                                                      
         L     RF,CDATAMGR                                                      
         DROP  RF                                                               
         GOTO1 (RF),DMCB,=C'DMRDHI',=C'STATION',KEY,CIBAIO                      
*                                                                               
         L     RE,CIBAIO                                                        
         USING STARECD,RE                                                       
         CLC   KEY(9),0(RE)                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
         PACK  DUB,SMKT                                                         
         CVB   R0,DUB                                                           
         STH   R0,DFLTMKT                                                       
         B     EXIT                                                             
         DROP  RE                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
WORKD    DSECT                                                                  
DUB      DS    D                                                                
VGETRATE DS    A                                                                
DMCB     DS    6A                                                               
DMWORK   DS    16F                                                              
*                                                                               
KEYID    DS    CL4                                                              
KEY      DS    XL20                                                             
KEYSAVE  DS    XL20                                                             
*                                                                               
SVXFROGR DS    F                                                                
SPOTS    DS    F                                                                
GROSS    DS    F                                                                
NET      DS    F                                                                
ADJ      DS    F                                                                
*                                                                               
         DS    0D                                                               
ELEM     DS    XL64                                                             
WORK     DS    XL64                                                             
ELCDLO   DS    C                                                                
ELCDHI   DS    C                                                                
DFLTMKT  DS    XL2                                                              
         DS    XL4                 SPARE                                        
*                                                                               
         DS    0D                                                               
BUCKETID DS    D                                                                
BUCKETS  DS    0XL16                                                            
         DS    XL256                                                            
WORKX    EQU   *                                                                
*                                                                               
         EJECT                                                                  
       ++INCLUDE SPCIBBLK                                                       
         EJECT                                                                  
       ++INCLUDE SPGENCTA                                                       
         EJECT                                                                  
       ++INCLUDE SPSTAPACKD                                                     
*        EJECT                                                                  
BUYRECD  DSECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE SPGENBUY                                                       
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDCOREQUS                                                      
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006SPGETCTA  05/01/02'                                      
         END                                                                    
