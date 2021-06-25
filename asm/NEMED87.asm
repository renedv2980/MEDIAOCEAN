*          DATA SET NEMED87    AT LEVEL 032 AS OF 05/01/02                      
*PHASE T31E87,*                                                                 
         TITLE 'T31E87 - BRAND BILLING ALLOCATION'                              
T31E87   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**BBAL**,RR=R2                                                 
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         LA    R6,2048(RB)                                                      
         LA    R6,2048(R6)                                                      
         USING T31E87+4096,R6                                                   
         L     RA,ATWA                                                          
         USING T31EFFD,RA                                                       
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
         L     R7,ANETWS2                                                       
         LA    R7,1500(R7)                                                      
         USING WORKD,R7                                                         
         ST    R2,RELO                                                          
         EJECT                                                                  
*HIPO******************************************************************         
*  TITLE: NEMED87 (T31E87)                                            *         
*                                                                     *         
*  COMMENTS: READ UNIT RECORDS - PRINTS GROSS/NET DOLLARS FOR THREE   *         
*            MONTHS FOR EACH PRODUCT                                  *         
*                                                                     *         
*  CALLS TO: NETIO                                                    *         
*                                                                     *         
*                                                                     *         
*  LOCALS: ANETWS4 USED FOR PRODUCT DOLLARS 220 X 12                  *         
*          ANETWS3 USED TO SAVE CLIENT RECORD                         *         
*          ANETWS2+1500 USED FOR MYWORKD                              *         
*          R7-MYWORKD                                                 *         
*                                                                     *         
***********************************************************************         
*  LOGIC:  FIRST, GET 3 MONTHS DATES- INPUT PRD/DOLLARS LIST          *         
*                                                                     *         
*          SECOND, READS UNIT RECS THROUGH NETIO AND ADDS UNIT        *         
*          DATA TO MONTH AND FINAL BUCKETS                            *         
*                                                                     *         
*          THIRD, CALCULATES % EACH PRODUCT REPRESENTS OF THE TOTAL   *         
*          DOLLLARS AND PRINTS THEM                                   *         
*                                                                     *         
*          FOURTH, PRINTS A NETWORK RECAP                             *         
*                                                                     *         
*ENDHIPO***************************************************************         
         SPACE 3                                                                
         CLI   MODE,VALKEY                                                      
         BE    VK                                                               
         CLI   MODE,PRINTREP                                                    
         BE    LR                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
* VALIDATE KEY *                                                                
         SPACE                                                                  
VK       DS    0H                                                               
         MVI   NBSEQ,C'N'          NETWORK ORDER                                
         SPACE                                                                  
* VALIDATE SCREEN INPUT FIELDS                                                  
*                                                                               
         MVI   FTERMFLG,0          SET REQUIRED FLAG                            
         MVC   NBACLI,ANETWS3      SAVE CLIENT IN 3RD IO AREA                   
         LA    R2,SPLCLIH                                                       
         NETGO NVCLI,DMCB,SPLCLIN                                               
         OI    SPLCLINH+6,X'80'                                                 
*                                                                               
VK100    LA    R2,SPLESTH                                                       
         MVI   FTERMFLG,0          SET REQUIRED FLAG                            
         NETGO NVESTRNG,DMCB,SPLESTN,0                                          
         OI    SPLESTNH+6,X'80'                                                 
*        MVI   ERROR,NBINVEST                                                   
*        CLC   =C'ALL',SPLEST                                                   
*        BE    TRAPERR                                                          
*        CLI   NBSELEST,0                                                       
*        BE    TRAPERR                                                          
*        CLI   NBSELESE,0                                                       
*        BNE   TRAPERR                                                          
*                                                                               
         MVI   FTERMFLG,1          SET OPTIONAL FLAG                            
         LA    R2,SPLNETH                                                       
         NETGO NVNET,DMCB,SPLNETN                                               
         OI    SPLNETNH+6,X'80'                                                 
*                                                                               
         MVI   FTERMFLG,1          SET REQUIRED FLAG                            
         LA    R2,SPLPAKH          PACKAGE                                      
         NETGO NVPAK,DMCB,SPLPAKN                                               
         OI    SPLPAKNH+6,X'80'                                                 
*                                                                               
         LA    R2,SPLSTRTH         START MONTH                                  
         MVI   NBSELEND,X'FF'      IF NO END DATE ENTERED                       
         NETGO NVGETFLD,DMCB                                                    
         BNZ   VK200                                                            
VKDTER   MVI   ERROR,INVDATE                                                    
         B     TRAPERR                                                          
*                                                                               
VK200    GOTO1 DATVAL,DMCB,(2,FLD),STMTH                                        
         OC    DMCB(4),DMCB                                                     
         BZ    VKDTER                                                           
*                                                                               
VK300    LA    R2,SPLENDH          END MONTH                                    
         MVI   EDMTH,X'FF'                                                      
         NETGO NVGETFLD,DMCB                                                    
         BZ    VK320                                                            
         GOTO1 DATVAL,DMCB,(2,FLD),EDMTH                                        
         OC    DMCB(4),DMCB                                                     
         BZ    VKDTER                                                           
         CLC   STMTH,EDMTH                                                      
         BH    VKDTER                                                           
*                                                                               
VK320    BAS   RE,SETMTHDT                                                      
*                                                                               
         LA    R2,SPLMN1H           START/END MONTHS                            
         NETGO NVGETFLD,DMCB                                                    
         BZ    VK325                                                            
         XC    MNTH1S(36),MNTH1S                                                
         LA    R4,3                                                             
         SR    R5,R5                                                            
         LA    R3,MNTH1S                                                        
VK322    NETGO NVGETFLD,DMCB                                                    
         BZ    VK324                                                            
         GOTO1 DATVAL,DMCB,(0,FLD),0(R3)                                        
         OC    DMCB(4),DMCB                                                     
         BZ    VKDTER                                                           
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         LA    R3,6(R3)                                                         
         NETGO NVGETFLD,DMCB                                                    
         BZ    VKDTER                                                           
         GOTO1 DATVAL,DMCB,(0,FLD),0(R3)                                        
         OC    DMCB(4),DMCB                                                     
         BZ    VKDTER                                                           
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         LA    R3,6(R3)                                                         
         LA    R5,1(R5)                                                         
         BCT   R4,VK322                                                         
VK324    STC   R5,CNTMTHS                                                       
*                                                                               
VK325    MVC   NBSELSTR,MNTH1S                                                  
         MVC   NBSELEND,MNTH3E                                                  
         SPACE                                                                  
         BAS   RE,EDITPRDS                                                      
         SPACE                                                                  
VKEXIT   DS    0H                                                               
         B     EXIT                                                             
         SPACE 2                                                                
* SET UP 3 MONTHS START & END DATES                                             
SETMTHDT ST    RE,SAVERE                                                        
         LA    R4,3                                                             
         SR    R5,R5                                                            
         LA    R3,MNTH1S                                                        
         MVC   0(4,R3),STMTH                                                    
         MVC   4(2,R3),=C'01'                                                   
ST100    LA    R5,1(R5)                                                         
         LH    R0,=H'30'                                                        
         GOTO1 ADDAY,DMCB,0(R3),6(R3),(R0)                                      
ST120    CLC   0(4,R3),6(R3)                                                    
         BE    ST200                                                            
         LH    R0,=H'-1'                                                        
         GOTO1 ADDAY,(R1),6(R3),6(R3),(R0)                                      
         B     ST120                                                            
*                                                                               
ST200    MVC   ENDMTH,6(R3)        END MONTH                                    
         CLC   6(4,R3),EDMTH                                                    
         BE    ST300                                                            
         BCT   R4,*+8                                                           
         B     ST300                                                            
*                                                                               
         LH    R0,=H'1'            NEXT MONTH                                   
         GOTO1 ADDAY,(R1),6(R3),12(R3),(R0)                                     
         LA    R3,12(R3)                                                        
         B     ST100                                                            
*                                                                               
ST300    STC   R5,CNTMTHS          # OF MONTHS                                  
         MVC   HDMN1S,MNTH1S                                                    
         MVC   HDMN2S,MNTH2S                                                    
         MVC   HDMN3S,MNTH3S                                                    
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
         SPACE                                                                  
********************************************************************            
*                                                                  *            
* READS UNIT RECORDS : POSTS TO PRODUCT BUCKETS - WHEN DONE GETS   *            
*                      PERCENT OF EACH PRODUCTS DOLLARS            *            
*                      THAN ALLOCATES NEW DOLLARS BY MONTH         *            
*                                                                  *            
*  RTN USED : POSTACT - ADDS ACTUAL DOLLARS TO MONTH/FINAL TOTALS  *            
*             PRNTPRD - PRINTS PRDS FOR NETWORK                    *            
*             FINLTOT - PRINTS PRD RECAP & FINAL TOTALS            *            
*                                                                  *            
********************************************************************            
         SPACE 2                                                                
LR       DS    0H                                                               
*                                                                               
* NOW READ UNIT RECS                                                            
LR100    LA    R1,HEADING                                                       
         ST    R1,SPECS                                                         
         LA    R1,HDRTN                                                         
         ST    R1,HEADHOOK                                                      
         MVI   NBSPLOPT,C'Y'                                                    
*                                                                               
         MVI   NBDATA,C'U'         JUST UNITS                                   
         MVI   NBSELUOP,C'A'       JUST ACTUAL DOLLARS                          
         SPACE                                                                  
LR200    NETGO NSNETIO,DMCB,NETBLOCK                                            
         CLI   NBERROR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   NBMODE,NBREQLST                                                  
         BE    LR300                                                            
         CLI   NBMODE,NBPROCUN                                                  
         BNE   LR200                                                            
         SPACE                                                                  
         GOTO1 DATCON,(R1),(2,NBACTDAT),CURDTE                                  
*        CLC   CURDTE,ENDMTH       AIR DATE NOT WITHIN MONTH                    
         CLC   CURDTE,NBSELEND     AIR DATE NOT WITHIN MONTH                    
         BH    LR200                                                            
         CLC   CURDTE,NBSELSTR                                                  
         BL    LR200                                                            
         BAS   RE,POSTACT                                                       
         MVC   SVACTNET,NBACTNET                                                
         B     LR200                                                            
         SPACE                                                                  
* END OF READ - TEST FOR ANYTHING TO PRINT                                      
LR300    ICM   RE,1,NUNITS                                                      
         BNZ   LR320                                                            
         MVC   P(16),=C'NO DATA TO PRINT'                                       
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     EXIT                                                             
         SPACE                                                                  
* CALCULATE PERCENTS                                                            
LR320    L     R1,TTLDLRS                                                       
         AH    R1,=H'50'                                                        
         SR    R0,R0                                                            
         D     R0,=F'100'                                                       
         ST    R1,TTLDLRS                                                       
         L     RE,ANETWS4                                                       
         LA    R0,221                                                           
LR340    OC    0(PSIZE,RE),0(RE)                                                
         BZ    LR360                                                            
         L     R5,4(RE)                                                         
         M     R4,=F'10000000'     TO GET 7 DIGITS                              
         D     R4,TTLDLRS                                                       
         ST    R5,4(RE)                                                         
*                                                                               
         L     RF,TLPCT                                                         
         AR    RF,R5                                                            
         ST    RF,TLPCT                                                         
*                                                                               
         LA    RE,PSIZE(RE)                                                     
         BCT   R0,LR340                                                         
*                                                                               
LR360    SR    R0,R0                                                            
         L     R1,FINGTL                                                        
         AH    R1,=H'50'                                                        
         D     R0,=F'100'                                                       
         ST    R1,FINGTL           GET RID OF PENNIES                           
*                                                                               
         BAS   RE,PRNTPRD                                                       
         BAS   RE,FINLTOT                                                       
*                                                                               
LREXIT   B     EXIT                                                             
         EJECT                                                                  
* POST ACTUAL GROSS DOLLARS TO BUCKETS                                          
POSTACT  ST    RE,SAVERE                                                        
         LA    RF,MNTHTL1                                                       
*                                                                               
         CLC   CURDTE,MNTH1E       FIND WHICH MONTH IT GOES IN                  
         BNH   PS200                                                            
         LA    RF,4(RF)            NEXT MONTH TOTAL                             
         CLC   CURDTE,MNTH2E                                                    
         BNH   PS200                                                            
         LA    RF,4(RF)            GOES IN LAST MONTH TOTAL                     
*                                                                               
PS200    ICM   R1,15,NBACTUAL                                                   
         BZ    PSEXT                                                            
         L     R0,FINGTL                                                        
         AR    R0,R1               ADD ACTUAL $ FINAL TOTAL                     
         ST    R0,FINGTL                                                        
         L     R0,0(RF)                                                         
         AR    R0,R1               ADD ACTUAL $ MONTH FINAL TOTAL               
         ST    R0,0(RF)                                                         
*                                                                               
         ZIC   R1,NUNITS           ADD TO NUMBER OF UNITS                       
         LA    R1,1(R1)                                                         
         STC   R1,NUNITS                                                        
*                                                                               
PSEXT    L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
* SET UP PRINT LINE                                                             
PRNTPRD  NTR1                                                                   
         XC    NUMPRDS,NUMPRDS                                                  
         LA    R2,P                                                             
         USING LISTD,R2                                                         
         L     R4,ANETWS4                                                       
         L     R5,NPRDS                                                         
*                                                                               
PP100    OC    0(PSIZE,R4),0(R4)   ANY DOLLARS ?                                
         BZ    PP500               NO SKIP TO NEXT PRD                          
*                                                                               
         XC    KEY(13),KEY                                                      
         MVC   KEY+1(3),NBACTAM    AGY/MED/CLI                                  
         MVC   KEY+4(3),0(R4)      PRD CODE                                     
         ZIC   R0,NBDMGOPT                                                      
         L     R3,ANETWS2                                                       
         GOTO1 NBDM,DMCB,((R0),=CL8'DMREAD'),=CL8'SPTDIR',KEY,(R3),0            
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   KEY,0(R3)                                                        
         MVC   NBDTADSP,=H'24'                                                  
         ZIC   R0,NBDMGOPT                                                      
         GOTO1 NBDM,(R1),((R0),=CL8'GETREC'),=CL8'SPTFILE',KEY+14,     X        
               (R3),DMWORK                                                      
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING PRDHDR,R3                                                        
         MVC   LPRDC,0(R4)                                                      
         MVC   LPRDN,PNAME                                                      
         DROP  R3                                                               
         SPACE                                                                  
* OUTPUT 4 DIGIT PERCENT OF TOTAL & ROUND PENNIES                               
         BAS   RE,PPCT                                                          
         SPACE                                                                  
* PRINT THREE MONTHS PRODUCT GROSS/NET                                          
         BAS   RE,PDOLLRS                                                       
*                                                                               
         MVI   SPACING,2                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         SPACE                                                                  
* SET UP TO DO NEXT PRODUCT                                                     
         LA    R4,PSIZE(R4)                                                     
         BCT   R5,PP100                                                         
         SPACE                                                                  
* PRINT MONTH TOTALS (6)                                                        
PP500    MVC   LVPCT,=C'-----------'                                            
         MVC   LMON1G,=C'--------'                                              
         MVC   LMON1N,=C'--------'                                              
         CLI   CNTMTHS,1                                                        
         BE    PP600                                                            
         MVC   LMON2G,=C'--------'                                              
         MVC   LMON2N,=C'--------'                                              
         CLI   CNTMTHS,2                                                        
         BE    PP600                                                            
         MVC   LMON3G,=C'--------'                                              
         MVC   LMON3N,=C'--------'                                              
PP600    GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         MVC   P+6(14),=C'MONTHLY TOTAL:'                                       
         MVC   LVPCT(8),=C'100.0000'                                            
*                                                                               
         L     R5,MNTHTL1                                                       
         SR    R4,R4                                                            
         AH    R5,=H'50'                                                        
         D     R4,=F'100'          GET RID OF PENNIES                           
         EDIT  (R5),(8,LMON1G)                                                  
         SR    R4,R4                                                            
         M     R4,=F'85'                                                        
         AH    R5,=H'50'           ROUND UP                                     
         D     R4,=F'100'                                                       
         EDIT  (R5),(8,LMON1N)                                                  
         CLI   CNTMTHS,1                                                        
         BE    PP620                                                            
*                                                                               
         L     R5,MNTHTL2                                                       
         SR    R4,R4                                                            
         AH    R5,=H'50'                                                        
         D     R4,=F'100'          GET RID OF PENNIES                           
         EDIT  (R5),(8,LMON2G)                                                  
         SR    R4,R4                                                            
         M     R4,=F'85'                                                        
         AH    R5,=H'50'           ROUND UP                                     
         D     R4,=F'100'                                                       
         EDIT  (R5),(8,LMON2N)                                                  
         CLI   CNTMTHS,2                                                        
         BE    PP620                                                            
*                                                                               
         L     R5,MNTHTL3                                                       
         SR    R4,R4                                                            
         AH    R5,=H'50'                                                        
         D     R4,=F'100'          GET RID OF PENNIES                           
         EDIT  (R5),(8,LMON3G)                                                  
         SR    R4,R4                                                            
         M     R4,=F'85'                                                        
         AH    R5,=H'50'           ROUND UP                                     
         D     R4,=F'100'                                                       
         EDIT  (R5),(8,LMON3N)                                                  
*                                                                               
PP620    MVI   SPACING,1                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         SPACE                                                                  
         B     EXIT                                                             
         SPACE 2                                                                
********************************************************************            
* PRINT 4 DIGIT PERCENT                                            *            
* INPUT        R5 = PRODUCTS GROSS PERCENT                         *            
* OUTPUT       A 4 DIGIT PERCENT                                   *            
********************************************************************            
PPCT     NTR1                                                                   
         L     R5,4(R4)                                                         
         SR    R4,R4                                                            
         AH    R5,=H'500'                                                       
         D     R4,=F'1000'         JUST PRINT 3 DEC                             
         CVD   R5,DUB2                                                          
         MVC   WORK(17),=X'4040404040404040404020202120202020'                  
         ED    WORK(17),DUB2+4                                                  
         MVC   LVPCT+4(4),WORK+13                                               
         MVI   LVPCT+3,C'.'                                                     
         MVC   LVPCT(3),WORK+10                                                 
         B     EXIT                                                             
         SPACE 2                                                                
********************************************************************            
* PRINT ONE LINE OF GROSS/NET PRODUCT DOLLARS (ALLOCATE PRD $)     *            
* INPUT        4(R4) = PERCENT TO ALLOCATE FOR THIS PRD            *            
* OUTPUT       3 GROOS/NET PRODUCT TOTALS PRINTED                  *            
*              PRD BUCKETS 8(R4) = GROSS DOLLARS NO PENNIES        *            
********************************************************************            
PDOLLRS  NTR1                                                                   
         ZIC   R3,CNTMTHS          3 GROSS/NET DOLLARS TO PRINT                 
         LA    R5,LMON1G           START PRINT LINE                             
         LA    R2,MNTHTL1          1ST MONTH TOTAL                              
         XC    8(4,R4),8(R4)       CLEAR PRD TOTAL                              
*                                                                               
PD100    L     RF,0(R2)                                                         
         SR    RE,RE                                                            
         AH    RF,=H'50'                                                        
         D     RE,=F'100'          GET RID OF PENNIES                           
         M     RE,4(R4)            GET PRODUCT ALLOCATION                       
         D     RE,=F'1000000000'                                                
         L     R0,8(R4)                                                         
         AR    R0,RF                                                            
         ST    R0,8(R4)            NEW PRODUCT TOTAL TO PRINT                   
         EDIT  (RF),(8,(R5))                                                    
         LA    R5,10(R5)                                                        
*                                                                               
         M     RE,=F'85'                                                        
         AH    RF,=H'50'           ROUND UP                                     
         D     RE,=F'100'                                                       
         EDIT  (RF),(8,(R5))                                                    
         LA    R5,11(R5)           NEXT MONTHS GROSS                            
         LA    R2,4(R2)            MEXT MONTH                                   
         BCT   R3,PD100                                                         
         L     RF,NUMPRDS                                                       
         LA    RF,4(RF)                                                         
         ST    RF,NUMPRDS                                                       
         B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
*                                                                               
FINLTOT  NTR1                                                                   
         LA    R2,P                                                             
         LA    RE,22               PRINT ON SAME PAGE                           
         A     RE,NUMPRDS                                                       
         CH    RE,=H'64'                                                        
         BNL   FP200                                                            
*                                                                               
         L     R4,ABOX                                                          
         USING BOXD,R4                                                          
         LTR   R4,R4               IS ABOX ZEROS                                
         BZ    FP100               YES/ ON-LINE SKIP BOXES                      
         SPACE                                                                  
         MVI   BOXINIT,0                                                        
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXOFF,0           (ONLY FOR SPOOF)                              
         LA    R5,BOXROWS-1        PRINT LAST LINE                              
         ZIC   RE,LINE                                                          
         AR    R5,RE                                                            
         MVI   0(R5),C'B'                                                       
FP100    MVI   SPACING,4                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         LTR   R4,R4               IS ABOX ZEROS                                
         BZ    FP120               YES/ ON-LINE SKIP BOXES                      
         SPACE                                                                  
         MVC   BOXCOLS,SPACES                                                   
         MVC   BOXROWS,SPACES                                                   
         MVI   BOXINIT,0                                                        
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXOFF,0           (ONLY FOR SPOOF)                              
         LA    R5,BOXCOLS                                                       
         MVI   43(R5),C'L'                                                      
         MVI   82(R5),C'R'                                                      
         SPACE                                                                  
         LA    R5,BOXROWS-1                                                     
         ZIC   RE,LINE                                                          
         AR    R5,RE                                                            
         MVI   0(R5),C'T'                                                       
         MVI   3(R5),C'M'                                                       
         MVI   BOXROWS+63,C'B'                                                  
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         USING LISTT,R2                                                         
FP120    MVC   LTPRDC(11),=C'BRAND TOTAL'                                       
         GOTO1 DATCON,DMCB,(0,MNTH1S),(6,LTGRS+2)                               
         MVI   LTGRS+10,C'-'                                                    
         GOTO1 DATCON,(R1),ENDMTH,(6,LTNET+1)                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         MVC   LTGRS+1(5),=C'GROSS'                                             
         MVC   LTNET+2(3),=C'NET'                                               
         MVI   SPACING,2                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     FP220                                                            
         DROP  R4,R2                                                            
*                                                                               
FP200    MVI   FORCEHED,C'Y'                                                    
         MVI   HEDPRNT,C'Y'                                                     
         USING LISTT,R2                                                         
*                                                                               
FP220    LA    R3,220                                                           
         L     R4,ANETWS4                                                       
*                                                                               
FP300    OC    0(PSIZE,R4),0(R4)                                                
         BZ    FP600               NO SKIP TO NEXT PRD                          
*                                                                               
         MVC   LTPRDC+1(3),0(R4)                                                
*                                                                               
         SPACE                                                                  
* PRINT GROSS & NET                                                             
FP400    L     R1,8(R4)            PRINT GROSS THAN GET NET & PRINT             
         EDIT  (R1),(9,LTGRS)                                                   
         M     R0,=F'85'                                                        
         AH    R1,=H'50'           ROUND UP                                     
         D     R0,=F'100'                                                       
         EDIT  (R1),(9,LTNET)                                                   
*                                                                               
         MVI   SPACING,2                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         SPACE                                                                  
* SET UP TO DO NEXT PRODUCT                                                     
FP500    LA    R4,PSIZE(R4)                                                     
         BCT   R3,FP300                                                         
         SPACE                                                                  
* PRINT GROSS & NET TOTALS                                                      
FP600    MVC   LTGRS,=C'---------'                                              
         MVC   LTNET,=C'---------'                                              
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         MVC   LTPRDC+2(6),=C'TOTAL:'                                           
         L     R1,FINGTL                                                        
         EDIT  (R1),(9,LTGRS)                                                   
*                                                                               
         M     R0,=F'85'                                                        
         AH    R1,=H'50'           ROUND UP                                     
         D     R0,=F'100'                                                       
         EDIT  (R1),(9,LTNET)                                                   
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
         SPACE                                                                  
         MVI   FORCEHED,C'Y'                                                    
         MVI   HEDPRNT,0                                                        
         B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
* BUILD A TABLE OF VALID PRODUCT PERCENTS FROM INPUT PRODUCT DOLLARS            
EDITPRDS NTR1                                                                   
         MVI   ERROR,INVALID                                                    
         LA    R2,SPLPRDH                                                       
         L     R3,ANETWS4                                                       
         LA    R5,12                                                            
         XC    FTERM,FTERM                                                      
EP100    ST    R2,FADDR                                                         
         XC    FLAST,FLAST                                                      
*                                                                               
EP200    MVI   FTERM,C'='                                                       
         GOTO1 FVAL                                                             
         CLI   FLDH+5,0                                                         
         BE    EP500                                                            
*                                                                               
         LA    R0,220                                                           
         L     R4,NBACLI                                                        
         LA    R4,CLIST-CLTHDR(R4) R4 = PRODUCT CODES/#                         
*                                                                               
EP220    OC    0(4,R4),0(R4)       ANY PRDS LEFT                                
         BZ    TRAPERR             YES SKIP TO NEXT PRD                         
         CLC   FLD(3),0(R4)        PRD MATCH                                    
         BE    EP240                                                            
         LA    R4,4(R4)                                                         
         BCT   R0,EP220                                                         
         B     TRAPERR                                                          
*                                                                               
EP240    MVC   0(3,R3),FLD                                                      
*                                                                               
         MVI   FTERM,C','                                                       
         GOTO1 FVAL                                                             
         CLI   FLDH+5,0                                                         
         BE    TRAPERR                                                          
         TM    FLDH+4,X'08'                                                     
         BZ    TRAPERR                                                          
         MVC   4(4,R3),DUB                                                      
         LA    R3,PSIZE(R3)        NEXT PRODUCT ENTRY                           
*                                                                               
         L     RE,TTLDLRS          ADD TO TOTAL PRODUCT DOLLARS                 
         A     RE,DUB                                                           
         ST    RE,TTLDLRS                                                       
*                                                                               
         ZIC   RE,NPRDS                                                         
         LA    RE,1(RE)            COUNT # OF PRODS                             
         STC   RE,NPRDS                                                         
         B     EP200                                                            
*                                                                               
EP500    ZIC   RE,0(R2)            NEXT SCREEN LINE                             
         AR    R2,RE                                                            
         BCT   R5,EP100                                                         
*                                                                               
         OC    NPRDS,NPRDS                                                      
         BNZ   EXIT                                                             
         LA    R2,SPLPRDH                                                       
         ST    R2,FADDR                                                         
         B     TRAPERR                                                          
         EJECT                                                                  
* FVAL - FIELD VALIDATION AND SCANNING ROUTINE                                  
*                                                                               
* ON ENTRY                                                                      
*        FADDR = A(FIELD HEADER)                                                
*        FLAST = A(LAST STRING) SET BY FVAL                                     
*              = ZERO (FORCES EDIT TO START AT FADDR+8)                         
*        FLEN  = LENGTH OF LAST STRING - SET BY FVAL                            
*              = ZERO TO FORCE EDIT TO START AT FLAST                           
*        FTERM = LIST OF UP TO 6 SCAN TERMINATORS ENDED BY X'00'                
*                                                                               
* ON EXIT                                                                       
*        FLDH  = FIELD HEADER BUILT BY FVAL - CONTAINS DATA LENGTH              
*                AND VALIDITY BITS                                              
*        FLD   = EXTRACTED DATA STRING IN SPACE FILLED FIELD                    
*        FSTOP = STOP CHARACTER FOUND BY FVAL OR X'FF' FOR NO MORE DATA         
*        DUB   = CONTAINS VALUE OF NUMERIC FIELD WITH .00 ATTACHED              
*                                                                               
* PARAMETER LIST                                                                
*                                                                               
*        R1    = 0 FOR NO PARAMETER LIST                                        
*                                                                               
FVAL     NTR1                                                                   
         XC    FLDH,FLDH           CLEAR OUTPUT FIELD HEADER                    
         MVI   FLD,C' '                                                         
         MVC   FLD+1(L'FLD-1),FLD  FILL OUTPUT FIELD WITH SPACES                
         MVI   FLDH,L'FLDH+L'FLD   SET DUMMY HEADER LENGTH                      
         MVI   FSTOP,X'FF'         SET FSTOP TO NO DATA FOUND                   
         L     R2,FADDR                                                         
         ZIC   R3,0(R2)                                                         
         SH    R3,=H'8'            R3 CONTAINS FIELD DATA LENGTH                
         LA    R2,8(R2)            POINT TO DATA START                          
         OC    FLAST,FLAST         TEST FOR LAST STRING                         
         BNZ   FVAL2                                                            
*                                                                               
* CODE BELOW TO FVAL8 ASSUMES -                                                 
* R1 POINTS TO SCAN INPUT  R2 POINTS TO SCREEN FIELD START                      
* R3 CONTAINS BYTE REMAINING TO BE SCANNED                                      
*                                                                               
FVAL1    STCM  R2,7,FLAST          SAVE SCAN START POINT                        
         LR    R1,R2               SET R1 AS SCAN INPUT POINTER                 
         MVI   FLEN,0              CLEAR LAST LENGTH TO BE SAFE                 
         MVI   FNDX,0                                                           
         B     FVAL4                                                            
*                                                                               
FVAL2    SR    R1,R1                                                            
         ICM   R1,7,FLAST          SET R1 TO POINT TO SCAN START                
         ZIC   R0,FLEN             LENGTH OF LAST STRING                        
         AR    R1,R0               POINT TO LAST STOP CHARACTER                 
         CLI   FLEN,0              TEST IF RE-EDITING                           
         BE    *+8                 YES-NO NEED TO JUMP OVER STOP CHAR.          
         LA    R1,1(R1)            INCREMENT LENGTH FOR STOP CHARACTER          
         LR    R0,R1               START POINT FOR SCAN                         
         SR    R0,R2               BYTES ALREADY SCANNED                        
         SR    R3,R0               BYTES LEFT TO SCAN                           
         BNP   FVALX               NOTHING TO SCAN - EXIT                       
         STCM  R1,7,FLAST          SAVE SCAN START                              
         MVI   FLEN,0              CLEAR LAST LENGTH                            
*                                                                               
FVAL4    LA    R0,L'FTERM                                                       
         LA    RE,FTERM            POINT AT SCAN TERMINATORS                    
         SPACE 1                                                                
FVAL5    CLI   0(RE),X'00'         TEST FOR END-OF-LIST                         
         BE    FVAL6                                                            
         CLC   0(1,R1),0(RE)       TEST FOR TERMINATOR                          
         BE    FVAL7               FOUND ONE                                    
         LA    RE,1(RE)            NEXT LIST ENTRY                              
         BCT   R0,FVAL5                                                         
*                                                                               
FVAL6    LA    R1,1(R1)            NEXT DATA BYTE                               
         BCT   R3,FVAL4                                                         
         B     FVAL8               SEARCH WAS FRUITLESS                         
*                                                                               
FVAL7    MVC   FSTOP,0(R1)         SET STOP CHARACTER                           
*                                                                               
FVAL8    LR    R3,R1               COMPUTE DATA LENGTH                          
         SR    RE,RE                                                            
         ICM   RE,7,FLAST                                                       
         SR    R3,RE                                                            
         BZ    FVALX               ONLY FOUND A TERMINATOR                      
         STC   R3,FLEN                                                          
         STC   R3,FLDH+5                                                        
         BCTR  R3,0                SET TO EXECUTE                               
         EX    R3,*+8              EXTRACT DATA STRING                          
         B     FVAL10                                                           
         MVC   FLD(0),0(RE)                                                     
*                                                                               
FVAL10   LA    RE,FLD(R3)          ADJUST LENGTH FOR TRAILING BLANKS            
         LA    R3,1(R3)            COUNTER                                      
         LR    R0,R3                                                            
*                                                                               
FVAL11   CLI   0(RE),0             TEST FOR TRAILING ZERO                       
         BNE   *+8                                                              
         MVI   0(RE),C' '          CHANGE IT TO A BLANK                         
         CLI   0(RE),C' '          TEST FOR A BLANK                             
         BNE   *+10                NO                                           
         BCTR  RE,0                YES-BACK UP FIELD POINTER                    
         BCT   R0,FVAL11                                                        
         STC   R0,FLDH+5                                                        
         LTR   R3,R0               TEST FOR ZERO REAL LENGTH                    
         BZ    FVALX               EXIT FOR EMPTY FIELD                         
         LA    RE,FLD              POINT TO START OF DATA                       
         MVI   FLDH+4,X'0C'        VALID NUMERIC AND ALPHA DATA                 
*                                                                               
*                                                                               
FVAL14   CLI   0(RE),C'.'          TEST IF DECIMAL POINT                        
         BE    FVAL15                                                           
         CLI   0(RE),C'0'          TEST IF NUMERIC                              
         BL    *+12                                                             
         CLI   0(RE),C'9'                                                       
         BNH   FVAL15                                                           
         NI    FLDH+4,X'FF'-X'08'  NOT NUMERIC                                  
*                                                                               
FVAL15   LA    RE,1(RE)            NEXT BYTE IN DATA STRING                     
         BCT   R3,FVAL14                                                        
*                                                                               
         TM    FLDH+4,X'08'        TEST FOR NUMERIC DATA                        
         BZ    FVALX                                                            
         NI    FLDH+4,X'FF'-X'04'  NOT ALPHA                                    
         CLI   FLDH+5,15                                                        
         BNH   *+12                                                             
FVAL16   NI    FLDH+4,X'FF'-X'08'                                               
         B     FVALX                                                            
         ZIC   R0,FLDH+5                                                        
         GOTO1 CASHVAL,DMCB,FLD,(R0)                                            
         CLI   0(R1),X'FF'                                                      
         BE    FVAL16                                                           
         MVC   DUB(4),4(R1)        EDITED DOLLAR FIELD                          
*                                                                               
FVALX    B     EXIT                                                             
*                                                                               
TRAPERR  GOTO1 ERREX                                                            
         SPACE                                                                  
*                                                                               
         EJECT                                                                  
HDRTN    NTR1                                                                   
         MVC   H1(12),=C'NETWORK T.V.'                                          
         MVC   H1+14(4),SVACTNET                                                
         MVC   H4(6),=C'CLIENT'                                                 
         MVC   H4+10(3),SPLCLI                                                  
         MVC   H4+15(20),SPLCLIN                                                
         MVC   H5(8),=C'ESTIMATE'                                               
         MVC   H5+10(3),SPLEST                                                  
         MVC   H5+15(20),SPLESTN                                                
         MVC   H6(7),=C'PACKAGE'                                                
         MVC   H6+10(3),SPLPAK                                                  
         MVC   H6+15(34),SPLPAKN                                                
         SPACE                                                                  
         MVC   H3+49(6),=C'PERIOD'                                              
*        GOTO1 DATCON,DMCB,(0,MNTH1S),(5,H3+57)                                 
         GOTO1 DATCON,DMCB,(0,HDMN1S),(5,H3+57)                                 
         MVI   H3+66,C'-'                                                       
         GOTO1 DATCON,DMCB,(0,ENDMTH),(5,H3+68)                                 
         SPACE                                                                  
         CLI   HEDPRNT,C'Y'                                                     
         BE    HD120                                                            
         LA    R2,H9                                                            
         USING LISTD,R2                                                         
*        GOTO1 DATCON,DMCB,(0,MNTH1S),(6,LMON1G+5)                              
         GOTO1 DATCON,DMCB,(0,HDMN1S),(6,LMON1G+5)                              
         CLI   CNTMTHS,1                                                        
         BE    HD100                                                            
*        GOTO1 DATCON,(R1),(0,MNTH2S),(6,LMON2G+5)                              
         GOTO1 DATCON,(R1),(0,HDMN2S),(6,LMON2G+5)                              
         CLI   CNTMTHS,2                                                        
         BE    HD100                                                            
*        GOTO1 DATCON,(R1),(0,MNTH3S),(6,LMON3G+5)                              
         GOTO1 DATCON,(R1),(0,HDMN3S),(6,LMON3G+5)                              
         SPACE                                                                  
HD100    LA    R2,H10                                                           
         MVC   LPRDC(5),=C'BRAND'                                               
         MVC   LVPCT(7),=C'PERCENT'                                             
         MVC   LMON1G+1(5),=C'GROSS'                                            
         MVC   LMON1N+2(3),=C'NET'                                              
         CLI   CNTMTHS,1                                                        
         BE    HDBOXES                                                          
         MVC   LMON2G+1(5),=C'GROSS'                                            
         MVC   LMON2N+2(3),=C'NET'                                              
         CLI   CNTMTHS,2                                                        
         BE    HDBOXES                                                          
         MVC   LMON3G+1(5),=C'GROSS'                                            
         MVC   LMON3N+2(3),=C'NET'                                              
         B     HDBOXES                                                          
*                                                                               
HD120    LA    R2,H9                                                            
         USING LISTT,R2                                                         
         MVC   LTPRDC(11),=C'BRAND TOTAL'                                       
*        GOTO1 DATCON,DMCB,(0,MNTH1S),(6,LTGRS+2)                               
         GOTO1 DATCON,DMCB,(0,HDMN1S),(6,LTGRS+2)                               
         MVI   LTGRS+10,C'-'                                                    
         GOTO1 DATCON,(R1),ENDMTH,(6,LTNET+1)                                   
*                                                                               
         LA    R2,H10                                                           
         MVC   LTGRS+1(5),=C'GROSS'                                             
         MVC   LTNET+2(3),=C'NET'                                               
         DROP  R2                                                               
*  SET UP BOXES PARAMETERS *                                                    
         SPACE                                                                  
HDBOXES  L     R1,ABOX                                                          
         USING BOXD,R1                                                          
         LTR   R1,R1               IS ABOX ZEROS                                
         BZ    HDX                 YES/ ON-LINE SKIP BOXES                      
         MVC   BOXCOLS,SPACES                                                   
         MVC   BOXROWS,SPACES                                                   
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         MVI   BOXOFF,0           (ONLY FOR SPOOF)                              
         CLI   HEDPRNT,C'Y'                                                     
         BE    HD200                                                            
         SPACE                                                                  
         LA    R5,BOXCOLS                                                       
         MVI   0(R5),C'L'                                                       
         LA    R5,26(R5)           BRAND                                        
         MVI   0(R5),C'C'                                                       
         LA    R5,11(R5)           PERCENT                                      
         MVI   0(R5),C'C'                                                       
         LA    R5,21(R5)           1ST MONTH                                    
         MVI   0(R5),C'C'                                                       
         LA    R5,21(R5)           2ND MONTH                                    
         MVI   0(R5),C'C'                                                       
         LA    R5,21(R5)           3RD MONTH                                    
         MVI   0(R5),C'R'                                                       
         B     HD300                                                            
         SPACE                                                                  
HD200    LA    R5,BOXCOLS                                                       
         MVI   43(R5),C'L'                                                      
         MVI   82(R5),C'R'                                                      
         SPACE                                                                  
HD300    LA    R5,BOXROWS                                                       
         LA    R5,7(R5)                                                         
         MVI   0(R5),C'T'                                                       
         LA    R5,3(R5)                                                         
         MVI   0(R5),C'M'                                                       
         LA    R5,46(R5)                                                        
         MVI   0(R5),C'B'                                                       
         SPACE                                                                  
HDX      B     EXIT                 (XIT1)                                      
         DROP  R1                                                               
         EJECT                                                                  
*                                                                               
         SPACE 2                                                                
HEADING  SSPEC H2,1,REQUESTOR                                                   
         SSPEC H1,51,C'BRAND BILLING ALLOCATION'                                
         SSPEC H2,51,C'------------------------'                                
         SSPEC H1,93,AGYNAME                                                    
         SSPEC H2,93,AGYADD                                                     
         SSPEC H3,93,REPORT                                                     
         SSPEC H4,93,RUN                                                        
         SSPEC H5,103,PAGE                                                      
         DC    X'00'                                                            
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
WORKD    DSECT                     MYWORK AREA  ANETWS2+500                     
         DS    0F                                                               
*                                                                               
RELO     DS    F                                                                
FINGTL   DS    F                   TOTAL GROSS FOR ALL PRODS                    
TTLDLRS  DS    F                   TOTAL INPUT DOLLLARS                         
TLPCT    DS    F                   TOTAL PERCENT                                
SVMTHDLR DS    A                   A 3 MONTH WORTH OF DOLLARS                   
NUMPRDS  DS    F                   COUNT # OF PRODUCTS                          
NUNITS   DS    F                   NUMBER OF UNITS PROCESSED                    
SAVERE   DS    F                   SAVE RETURN REG                              
SVACTNET DS    CL4                 NEW NETWORK                                  
DUB2     DS    D                   SECOND DUB                                   
MNTH1S   DS    XL6                 FIRST START MONTH DATE                       
MNTH1E   DS    XL6                 FIRST END MONTH DATE                         
MNTH2S   DS    XL6                 SECOND START MONTH DATE                      
MNTH2E   DS    XL6                 SECOND END MONTH DATE                        
MNTH3S   DS    XL6                 THIRD START MONTH DATE                       
MNTH3E   DS    XL6                 THIRD END MONTH DATE                         
ENDMTH   DS    XL6                 LAST MONTH ON REPORT                         
CURDTE   DS    XL6                 UNITS 6 BYTE DATE                            
STMTH    DS    CL6                 REQUEST START MONTH                          
EDMTH    DS    CL6                 REQUEST END MONTH                            
NPRDS    DS    XL1                                                              
BOXSET   DS    CL1                 OUTPUT BOXES                                 
CNTMTHS  DS    XL1                 # OF MONTHS ON REPORT                        
HEDPRNT  DS    CL1                 PRINTING A TOTAL                             
FNDX     DS    XL1                                                              
HDMN1S   DS    XL6                                                              
HDMN2S   DS    XL6                                                              
HDMN3S   DS    XL6                                                              
FADDR    DS    F                                                                
SVREGE   DS    F                                                                
MNTHTL1  DS    F                   TOTAL MONTH ONE                              
MNTHTL2  DS    F                   TOTAL MONTH TWO                              
MNTHTL3  DS    F                   TOTAL MONTH THREE                            
MTHTLL   EQU   *-MNTHTL1                                                        
*                                                                               
PRDGRSTD DSECT                                                                  
PGPRDCDE DS    F                                                                
PGRSDLRS DS    F                                                                
PPERCENT DS    F                                                                
PSIZE    EQU   *-PGPRDCDE                                                       
PGRSM    DS    (PSIZE*220)XL1      # PRODUCTS TIMES 4 BYTES STORAGE             
MPRODS   EQU   220                                                              
         SPACE 2                                                                
*                                                                               
LISTD    DSECT                                                                  
         DS    CL1                                                              
LPRDC    DS    CL3                                                              
         DS    CL1                                                              
LPRDN    DS    CL20                                                             
         DS    CL3                                                              
LVPCT    DS    CL8 M                                                            
         DS    CL3                                                              
LMON1G   DS    CL8                                                              
         DS    CL2                                                              
LMON1N   DS    CL8                                                              
         DS    CL3                                                              
LMON2G   DS    CL8                                                              
         DS    CL2                                                              
LMON2N   DS    CL8                                                              
         DS    CL3                                                              
LMON3G   DS    CL8                                                              
         DS    CL2                                                              
LMON3N   DS    CL8                                                              
         DS    CL1                                                              
         SPACE 2                                                                
*                                                                               
LISTT    DSECT                                                                  
         DS    CL46                                                             
LTPRDC   DS    CL8                                                              
         DS    CL2                                                              
LTGRS    DS    CL9                                                              
         DS    CL3                                                              
LTNET    DS    CL9                                                              
         DS    CL1                                                              
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE NETINCLS                                                       
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE SPGENPRD                                                       
ESTD     DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
         PRINT ON                                                               
         EJECT                                                                  
       ++INCLUDE NEMEDFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE NEMEDEED                                                       
       ++INCLUDE DDBIGBOX                                                       
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'032NEMED87   05/01/02'                                      
         END                                                                    
