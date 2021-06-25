*          DATA SET NEMED2C    AT LEVEL 018 AS OF 05/19/11                      
*          DATA SET NEMED2C    AT LEVEL 036 AS OF 03/29/94                      
*PHASE T31E2CA,+0                                                               
*INCLUDE NETNET                                                                 
*INCLUDE NEPACC                                                                 
         TITLE 'T31E2C - FIS'                                                   
T31E2C   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**FIPR**,RR=R2                                                 
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING T31EFFD,RA                                                       
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
         L     R7,ANETWS1          USE AREA 1 FOR W/S                           
**       L     R7,ANETWS2          USE W/S AREA 2                               
         USING POSTD,R7                                                         
         ST    R2,RELO                                                          
         LA    R2,HOOK                                                          
         ST    R2,HEADHOOK                                                      
*                                                                               
         LA    R3,STACK            CLEAR ACCUMS 1ST TIME                        
         LA    R0,85               17 ROWS OF 5                                 
         SPACE 1                                                                
CLR2     ZAP   0(8,R3),=P'0'                                                    
         LA    R3,8(R3)                                                         
         BCT   R0,CLR2                                                          
         EJECT                                                                  
*              INITIALIZE NETBLOCK                                              
         SPACE 3                                                                
*                                                                               
         OI    NBSBKEND,NBNODPT2   DON'T READ FOR DPT                           
         MVI   NBDATA,C'U'         GET UNITS                                    
         MVI   NBSEQ,C'D'          GET IN DATE SEQUENCE                         
         MVI   NBUSER+13,C'N'      OVERRIDE PROFILE TO RETURN                   
*                                   PRE-EMPTED UNITS                            
         MVI   NREPTYP,C'A'        ID AS ACCOUNTING REPORT                      
         MVI   NBSPLOPT,X'80'      SUPPORT PIGGYBACKS                           
         SPACE 1                                                                
         XC    PERTYPE,PERTYPE                                                  
         MVI   PERTYPE,C'M'        USE MONTHS                                   
         LA    R4,MAXMONTS                                                      
         ST    R4,NUMMONS          MAX NUMBER OF MONTHS                         
*                                  NEUBILLRDR?                                  
****     OI    NBINDS2,NBNOBLRD    DEFAULT NOT TO READ                          
****     XC    WORK,WORK                                                        
****     MVC   WORK(4),=C'SB1S'                                                 
****     NI    WORK,X'BF'          LOWER CASE                                   
****     MVC   WORK+4(2),NBSELAGY                                               
****     GOTO1 NBGTPROF,DMCB,WORK,WORK+20,NBDM                                  
****     CLI   WORK+23,C'Y'        MUST READ NEW BILL RECS?                     
****     BNE   PROCDAT                                                          
**       OI    NBINDS2,NBBILLRD+NBLCMPR READ BILLS/COMPRESS                     
*******************************************************************             
* DON'T USE COMPRESS SINCE IT DOES NOT BREAK OUT SPECIAL CHARGES                
* BY PRODUCT - ONLY GIVES A TOTAL OF VARIOUS SPECIAL CHARGE TYPES               
* FIS NOW PROVIDES A LARGER I/O ARE TO BILL READER TO SEED UNIT                 
* WITH ALL BILL ELEMENTS                                                        
*******************************************************************             
         OI    NBINDS2,NBBILLRD         READ BILLS                              
         LA    R1,BLRDSCT                                                       
         USING NBLBILLD,R1                                                      
         ST    R1,NBABILRD                                                      
         XC    0(NBLLENQ,R1),0(R1)                                              
         LA    R2,MYIO                                                          
         ST    R2,NBLUNAIO                                                      
         ST    R2,NBAIO                                                         
         OI    NBLFUNC,NBLSEED    SEED UNIT REC WITH ALL BILL ELEMS             
         DROP  R1                                                               
********************************************************************            
         SPACE 1                                                                
PROCDAT  NETGO NSNETIO,DMCB,NETBLOCK   PROCESS DATE                             
         CLI   NBERROR,0                                                        
         BNE   PROCERR                                                          
         CLI   NBMODE,NBVALDAT                                                  
         BE    GOTDATE                                                          
         B     PROCDAT                                                          
         SPACE 1                                                                
*                                  FILL MONTHLIST                               
GOTDATE  NETGO NVWKLST,DMCB,NUMMONS,MONLIST,PERTYPE                             
         SPACE 1                                                                
GETUNIT  NETGO NSNETIO,DMCB,NETBLOCK    NOW DO UNIT RECORDS                     
         CLI   NBERROR,0                                                        
         BNE   PROCERR                                                          
         CLI   NBMODE,NBREQLST     LAST ONE                                     
         BE    LASTONE                                                          
         CLI   NBMODE,NBPROCUN     IF A UNIT                                    
         BE    GOTUNIT                                                          
         B     GETUNIT                                                          
         SPACE 1                                                                
GOTUNIT  CLI   OFFLINE,C'Y'           IF ONLINE                                 
         BE    *+8                                                              
         BAS   RE,CHKMAXIO            CHECK IO COUNT                            
         BAS   RE,POST                                                          
         B     GETUNIT                                                          
         SPACE 1                                                                
LASTONE  BAS   RE,MANUALS                                                       
         BAS   RE,TOTS                                                          
         XIT1                                                                   
         SPACE 1                                                                
PROCERR  DC    H'0'                                                             
         EJECT                                                                  
*              POSTING ROUTINES                                                 
         SPACE 3                                                                
POST     NTR1                                                                   
         SPACE 1                                                                
         TM    NBUNITST,X'42'      IF MISSED OR PRE-EMPT                        
         BZ    POST1                                                            
         XC    NBACTUAL,NBACTUAL      ZERO ACTUAL COST                          
         XC    NBASSIGN,NBASSIGN                                                
         XC    NBINTEG,NBINTEG                                                  
***** NOTE DO THIS INSTEAD OF SETTING NBSELUOP=A BECAUSE WE STILL               
*****   WANT TO REPORT BILLING, PAYING                                          
         SPACE 1                                                                
POST1    LA    R2,WORKAC                                                        
         USING ACCUMD,R2                                                        
         MVC   WORKAC,=5PL8'0'                                                  
         CLI   INTOPT,C'I'         FIGURE OUT ORDERED                           
         BE    POST6                                                            
         CLI   NBRTTYPE,0          DO WE HAVE RATETYPES                         
         BE    SKIPRATE                                                         
         GOTO1 =V(NETNET),DMCB,(NBRTTYPE,NBACTUAL),RATESG,RR=RELO               
         L     R1,RATESG                                                        
         CLI   NETOPT,C'Y'                                                      
         BNE   POSTB                                                            
         L     R1,RATESN                                                        
         B     POSTB                                                            
SKIPRATE L     R1,NBACTUAL                                                      
*****    LTR   R1,R1            ***** WANT FIS TO ALWAYS SHOW ACTUAL            
*****    BNZ   *+8                                                              
*****    L     R1,NBASSIGN                                                      
         CLI   NETOPT,C'Y'         FOR NET                                      
         BNE   POSTB                                                            
         LR    R0,R1                                                            
         SRDA  R0,32                                                            
         M     R0,=F'8500'                                                      
         SLDA  R0,1                PREPARE FOR ROUNDING                         
         D     R0,=F'10000'                                                     
         LTR   R1,R1                                                            
         BM    *+8                                                              
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
POSTB    CVD   R1,ORDERED                                                       
         AP    UNCLEAR,ORDERED                                                  
         AP    UNBILLED,ORDERED                                                 
         BAS   RE,SPECIALS              GET SPECIAL CHARGES                     
         OC    NBPAYTGR,NBPAYTGR        CLEARED                                 
         BZ    POST2                                                            
         L     R1,NBPAYTGR                                                      
         CLI   NETOPT,C'Y'                                                      
         BNE   POSTA                                                            
         L     R1,NBPAYTNT         OR NET                                       
POSTA    CVD   R1,DUB                                                           
         AP    CLEARED,DUB                                                      
         SP    UNCLEAR,DUB                                                      
         SPACE 1                                                                
POST2    OC    NBBILTGR,NBBILTGR   BILLED                                       
         BZ    POST4                                                            
         L     R1,NBBILTGR                                                      
         CLI   NETOPT,C'Y'                                                      
         BNE   POST3                                                            
         L     R1,NBBILTNT         OR NET                                       
POST3    CVD   R1,DUB                                                           
         AP    BILLED,DUB                                                       
         SP    UNBILLED,DUB                                                     
         SPACE 1                                                                
POST4    CLI   INTOPT,C'-'                                                      
         BE    POST10                                                           
         SPACE 1                                                                
POST6    CLI   NBRTTYPE,0          DO WE HAVE RATETYPES                         
         BE    POST6D                                                           
         CLI   NBSDRTCV,C'T'       TIME ONLY ?                                  
         BE    POST6D                                                           
         GOTO1 =V(NETNET),DMCB,(NBRTTYPE,NBINTEG),RATESG,RR=RELO                
         L     R1,RATESG                                                        
         CLI   NETOPT,C'Y'                                                      
         BNE   POST65                                                           
         L     R1,RATESN                                                        
         B     POST65                                                           
POST6D   L     R1,NBINTEG          INTEGRATION                                  
         CLI   NETOPT,C'Y'         FOR NET                                      
         BNE   POST65                                                           
         LR    R0,R1                                                            
         SRDA  R0,32                                                            
         M     R0,=F'8500'                                                      
         SLDA  R0,1                PREPARE FOR ROUNDING                         
         D     R0,=F'10000'                                                     
         LTR   R1,R1                                                            
         BM    *+8                                                              
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
POST65   CVD   R1,DUB                                                           
         AP    ORDERED,DUB                                                      
         AP    UNCLEAR,DUB                                                      
         AP    UNBILLED,DUB                                                     
         OC    NBPAYIGR,NBPAYIGR   CLEARED INT.                                 
         BZ    POST8                                                            
         L     R1,NBPAYIGR                                                      
         CLI   NETOPT,C'Y'                                                      
         BNE   POST7                                                            
         L     R1,NBPAYINT         OR NET                                       
POST7    CVD   R1,DUB                                                           
         AP    CLEARED,DUB                                                      
         SP    UNCLEAR,DUB                                                      
         SPACE 1                                                                
POST8    OC    NBBILIGR,NBBILIGR   BILLED INT.                                  
         BZ    POST10                                                           
         L     R1,NBBILIGR                                                      
         CLI   NETOPT,C'Y'                                                      
         BNE   POST9                                                            
         L     R1,NBBILINT         OR NET                                       
POST9    CVD   R1,DUB                                                           
         AP    BILLED,DUB                                                       
         SP    UNBILLED,DUB                                                     
         SPACE 1                                                                
POST10   BAS   RE,ADDLINES                                                      
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO ADD INTO ACCUMULATOR ARRAY                            
         SPACE 3                                                                
ADDLINES NTR1                                                                   
         L     R4,NUMMONS                                                       
         LA    R2,WORKAC                                                        
         LA    R3,STACK            ADD INTO TOTAL LINE                          
         BAS   RE,ADD6                                                          
         LA    R6,MONLIST          GET MONTH (WEEK) NUMBER IN LIST              
         SR    R1,R1                                                            
ADD2     LA    R1,1(R1)               INTO R1                                   
         CLC   NBACTDAT(2),2(R6)                                                
         BNH   ADD4                                                             
         LA    R6,4(R6)            NEXT DATE SET                                
         BCT   R4,ADD2                                                          
*                                                                               
*                                  OUT OF RANGE                                 
         S     R6,=F'4'            ADD TO LAST COLUMN                           
         SPACE 1                                                                
ADD4     MH    R1,=H'40'                                                        
         AR    R3,R1                                                            
         BAS   RE,ADD6                                                          
         B     XIT                                                              
         SPACE 1                                                                
ADD6     NTR1                                                                   
         LA    R0,5                                                             
         SPACE 1                                                                
ADD8     AP    0(8,R3),0(8,R2)                                                  
         LA    R2,8(R2)                                                         
         LA    R3,8(R3)                                                         
         BCT   R0,ADD8                                                          
         B     XIT                                                              
         EJECT                                                                  
*              CONTROL OF TOTALS                                                
         SPACE 3                                                                
TOTS     NTR1                                                                   
         LA    R2,MONLIST                                                       
         LA    R3,STACK+40                                                      
         L     R4,NUMMONS                                                       
         SPACE 1                                                                
TOTS2    CLC   0(40,R3),=5PL8'0'                                                
         BE    TOTS6                                                            
         GOTO1 DATCON,DMCB,(2,0(R2)),WORK                                       
         CLI   NBUSER+2,C'C'       CALENDAR OR BRODCAST MONTHS                  
         BE    TOTS4                                                            
         GOTO1 ADDAY,DMCB,WORK,WORK+6,7                                         
         MVC   WORK(6),WORK+6                                                   
         SPACE 1                                                                
TOTS4    GOTO1 DATCON,DMCB,WORK,(9,P)                                           
         MVC   P+3(3),P+4                                                       
         BAS   RE,TOTSPLAT                                                      
         SPACE 1                                                                
TOTS6    LA    R2,4(R2)                                                         
         LA    R3,40(R3)                                                        
         BCT   R4,TOTS2                                                         
*                                                                               
         LA    R3,STACK                                                         
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVC   P(6),=C'TOTALS'                                                  
         BAS   RE,TOTSPLAT                                                      
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO FORMAT A LINE AND PRINT                               
         SPACE 3                                                                
TOTSPLAT NTR1                                                                   
         LR    R2,R3                                                            
         USING ACCUMD,R2                                                        
         CP    ORDERED,=P'0'       SHOW PCT CLEARED                             
         BE    SPLAT2                                                           
         CP    CLEARED,=P'0'                                                    
         BE    SPLAT2                                                           
         ZAP   WORK(16),CLEARED                                                 
         MP    WORK(16),=P'2000'                                                
         DP    WORK(16),ORDERED                                                 
         CVB   R1,WORK                                                          
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         EDIT  (R1),(5,P+7),1                                                   
         MVI   P+12,C'%'                                                        
         SPACE 1                                                                
SPLAT2   LA    R3,P+13                                                          
         LA    R0,5                                                             
         SPACE 1                                                                
SPLAT4   EDIT  (P8,(R2)),(13,(R3)),2,FLOAT=-,ZERO=BLANK                         
         LA    R2,8(R2)                                                         
         LA    R3,13(R3)                                                        
         BCT   R0,SPLAT4                                                        
         MVI   SPACING,2                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
         EJECT                                                                  
*              HEADLINE ROUTINES                                                
         SPACE 3                                                                
HOOK     NTR1                                                                   
         MVC   H4+10(3),SPLCLI                                                  
         MVC   H5+10(3),SPLPRO                                                  
         MVC   H6+10(3),SPLEST                                                  
         MVC   H4+14(20),SPLCLIN                                                
         MVC   H5+14(20),SPLPRON                                                
         MVC   H6+14(24),SPLESTN                                                
         MVC   H7+10(4),=C'NONE'                                                
         CLI   SPLOPTH+5,0                                                      
         BE    *+10                                                             
         MVC   H7+10(8),SPLOPT                                                  
         MVC   H5+64(3),=C'ALL'                                                 
         CLI   SPLNETH+5,0                                                      
         BE    *+10                                                             
         MVC   H5+64(4),SPLNET                                                  
         MVC   H6+64(3),=C'ALL'                                                 
         CLI   SPLDPTH+5,0                                                      
         BE    *+10                                                             
         MVC   H6+64(8),SPLDPT                                                  
         MVC   H7+64(3),=C'ALL'                                                 
         CLI   SPLPAKH+5,0                                                      
         BE    *+10                                                             
         MVC   H7+64(4),SPLPAK                                                  
         B     XIT                                                              
         SPACE 1                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
*********************************************************                       
*  GETS SPECIAL RATES AND ADD TO ORDERED                                        
SPECIALS NTR1                                                                   
         DROP  R2                                                               
         LA    R4,WORKAC                                                        
         USING ACCUMD,R4                                                        
         L     R3,NBAIO            NOW GET ACTUAL UNIT                          
         USING NURECD,R3                                                        
         MVC   FULL,NBSPCHRG       SPECIAL CHARGE ORDERED                       
         TM    NBUNITST,X'42'      SKIP MISSED/PREEMPT SPECIAL RATES            
         BNZ   FL5                                                              
*                                                                               
* CHECK RATE TYPE VS COST WE ARE DEALING WITH                                   
         CLI   NBRTTYPE,0          IS THERE RATE TYPE?                          
         BE    FL2                                                              
         CLI   NBSDRTCV,C'A'       A=APPLY TO ALL COSTS                         
         BE    FL1                                                              
         CLI   NBSDRTCV,0          0=APPLY TO ALL COSTS                         
         BNE   FL2                                                              
FL1      GOTO1 =V(NETNET),DMCB,(NBRTTYPE,NBSPCHRG),WORK,RR=RELO                 
         ICM   R0,15,WORK+4          NET                                        
         CLI   NETOPT,C'Y'                                                      
         BE    FL3                                                              
         ICM   R0,15,WORK                                                       
         B     FL3                                                              
*                                                                               
FL2      TM    NBUNITST,X'42'      SKIP MISSED/PREEMPT SPECIAL RATES            
         BZ    *+10                                                             
         XC    FULL,FULL                                                        
         L     R0,FULL                                                          
         CLI   NETOPT,C'Y'          IS IT NET OPTION                            
         BNE   FL3                                                              
*                                                                               
         LA    R2,NUMAINEL         SET UP R2 FOR GETEL                          
         MVI   SRCHEL,X'03'        GET SPECIAL ELEM                             
         BAS   RE,NEXTEL                                                        
         BNE   FL5                                                              
         USING NUSPRD,R2                                                        
         CLI   NUSPRCOM,C'C'        IS IT COMMISION                             
         BE    *+12                                                             
         CLI   NUSPRCOM,C'Y'        IS IT COMMISION                             
         BNE   FL3                                                              
         BAS   RE,FINDNET                                                       
FL3      CVD   R0,DUB                                                           
         AP    ORDERED,DUB                                                      
         AP    UNCLEAR,DUB                                                      
         AP    UNBILLED,DUB                                                     
         DROP  R2                                                               
*                                                                               
FL5      L     R3,NBAIO            NOW GET PAID SPECIAL                         
         USING NURECD,R3                                                        
         LA    R2,NUMAINEL         SET UP R2 FOR GETEL                          
         MVI   SRCHEL,X'12'        GET PAID ELEMENTS                            
FL7      BAS   RE,NEXTEL                                                        
         USING NUPAYD,R2                                                        
         BNE   FL10                                                             
         CLI   NUPAYTYP,C'T'       SKIP TIME                                    
         BE    FL7                                                              
         CLI   NUPAYTYP,C'I'       AND INTEGRATION                              
         BE    FL7                                                              
         CLI   NBSPLPRN,0                                                       
         BE    FL8                                                              
FL8      MVC   FULL,NUPAYGRS       ASSUME ALL ELSE IS SPECIAL                   
         CLI   NETOPT,C'Y'                                                      
         BNE   *+10                                                             
         MVC   FULL,NUPAYNET                                                    
         BAS   RE,SPLIT                                                         
         L     R0,FULL                                                          
         CVD   R0,DUB                                                           
         AP    CLEARED,DUB                                                      
         SP    UNCLEAR,DUB                                                      
         B     FL7                                                              
*                                                                               
FL10     L     R3,NBAIO            NOW GET SPECIAL BILLING                      
         USING NURECD,R3                                                        
         LA    R2,NUMAINEL         SET UP R2 FOR GETEL                          
         MVI   SRCHEL,X'10'        GET BILL ELEMENTS                            
FL12     BAS   RE,NEXTEL                                                        
         USING NUBILD,R2                                                        
         BNE   FLX                                                              
         CLI   NUBILTYP,C'T'       SKIP TIME                                    
         BE    FL12                                                             
         CLI   NUBILTYP,C'I'       AND INTEGRATION                              
         BE    FL12                                                             
         TM    NUBILST,X'20'       SKIP UNBILLED                                
         BO    FL12                                                             
         CLI   NBSPLPRN,0                                                       
         BE    FL14                                                             
         CLC   NBSPLPRN,NUBILPRD   CHECK PRODUCT                                
         BNE   FL12                                                             
FL14     MVC   FULL,NUBILGRS       ASSUME ALL ELSE IS SPECIAL                   
         CLI   NETOPT,C'Y'                                                      
         BNE   *+10                                                             
         MVC   FULL,NUBILNET                                                    
         L     R0,FULL                                                          
         CVD   R0,DUB                                                           
         AP    BILLED,DUB                                                       
         SP    UNBILLED,DUB                                                     
         B     FL12                                                             
FLX      XIT1                                                                   
         DROP  R2,R3,R4                                                         
*                                                                               
* FINDNET - COMPUTES NET COST - AT ENTRY R0 CONTAINS GROSS COST                 
*                                                                               
FINDNET  SRDA  R0,32               PREPARE MULTIPLICAND                         
         M     R0,=F'8500'                                                      
         SLDA  R0,1                DOUBLE FOR ROUNDING                          
         D     R0,=F'10000'                                                     
         LTR   R1,R1                                                            
         BM    *+8                                                              
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         LR    R0,R1               REPLACE COST WITH NET                        
         BR    RE                                                               
*                                                                               
         EJECT                                                                  
*          DATA SET NENETACC   AT LEVEL 012 AS OF 03/14/95                      
* - FULL HAS VALUE TO BE SPLIT                                                  
SPLIT    NTR1                                                                   
         CLI   NBPRDNO,0           ,,IF MULT PRODS                              
         BE    SPLIT0                                                           
         CLI   NBSPLPRN,0                                                       
         BE    XIT                                                              
         XC    OUTAREA,OUTAREA     ,,DO SPECIAL PROD PROCESSING                 
         ICM   R2,15,FULL            R2 = VALUE TO SPLIT                        
         MVC   DMCB,NBAPROD                 ADDR OF MULT PROD ELEM              
         MVC   DMCB(1),NBSPLCNT             RETURN SPECIFIC PROD DATA           
         L     RF,=V(NEPACC)                                                    
         GOTO1 (RF),DMCB,,(R2),(1,OUTAREA),NETBLOCK,RR=RELO                     
         MVC   FULL,OUTAREA                                                     
         B     XIT                                                              
*                                                                               
SPLIT0   CLI   NBSPLPRN,0                                                       
         BE    XIT                                                              
         CLI   NBPRD2,0            ONLY NEEDED IF 2 PRODUCTS                    
         BE    XIT                                                              
         L     R1,=F'5000'         MAY BE 50/50                                 
         OC    NBP1SHR,NBP1SHR                                                  
         BZ    SPLIT2                                                           
         MVC   DUB(2),NBP1SHR                                                   
         LH    R1,DUB                                                           
         SPACE 1                                                                
SPLIT2   M     R0,FULL             FIGURE OUT FIRST PROD SHARE                  
         D     R0,=F'10000'                                                     
         CLC   NBSPLPRN,NBPRD      .IF NOT FIRST PRODUCT                        
         BNE   SPLIT3              .CALCULATE 2ND PROD SHARE                    
         CLC   NBPRD,NBPRD2        ..IS IT BOOKENDS                             
         BNE   SPLIT4              ..NO                                         
         CLI   NBSPLTYP,C'F'       YES/FIRST                                    
         BE    SPLIT4                                                           
SPLIT3   LR    R0,R1                                                            
         L     R1,FULL                                                          
         SR    R1,R0               REST FOR SECOND PRODUCT                      
         SPACE 1                                                                
SPLIT4   ST    R1,FULL                                                          
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
* READS STATION BUCKETS FOR MANUAL BILLS                                        
*                                                                               
MANUALS  NTR1  WORK=(R2,300)                                                    
         LR    R5,R2               SAVE POINTER TO WORK AREA                    
         NETGO NVSETSPT,DMCB                                                    
         MVC   FILENAME,=C'SPTDIR  '                                            
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0E01'                                                  
         MVC   KEY+2(3),NBKEYLST+1        AM/CLT                                
         GOTO1 HIGH                                                             
         B     MAN5                                                             
MANSEQ   DS    0H                                                               
         MVC   FILENAME,=C'SPTDIR  '                                            
         GOTO1 SEQ                                                              
MAN5     CLC   KEY(5),KEYSAVE             ID/AM/CLT                             
         BNE   MANX                                                             
*                                                                               
         CLI   NBSELPRD,0          CHECK PRODUCT                                
         BE    MAN20                                                            
         CLC   NBSELPRD,=C'POL'                                                 
         BE    MAN20                                                            
         CLC   NBSELPRD,=C'ALL'                                                 
         BE    MAN20                                                            
***      L     R3,ANETWS4          POINT TO CLIST                               
MAN7     CLC   0(3,R3),NBSELPRD                                                 
***      BE    MAN10                                                            
***      LA    R3,4(R3)                                                         
***      CLI   0(R3),0                                                          
***      BNE   MAN7                                                             
***      DC    H'0'                                                             
**MAN10    CLC   KEY+5(1),3(R3)                                                 
         CLC   KEY+5(1),NBEFFPNM                                                
         BNE   MANSEQ                                                           
*                                                                               
MAN20    CLI   NBSELEST,0          CHECK ESTIMATE                               
         BE    MAN30                                                            
         CLC   NBSELEST,KEY+6                                                   
         BNE   MANSEQ                                                           
*                                                                               
MAN30    DS    0H                                                               
         LR    R2,R5               RESET TO START OF WORK AREA                  
         LR    RE,R2                                                            
         L     RF,=F'800'                                                       
         XCEF                                                                   
         MVC   FILENAME,=C'SPTFIL  '                                            
         GOTO1 DATAMGR,DMCB,=C'GETREC',FILENAME,KEY+14,(R2),(0,DMWORK)          
         USING STABELEM,R2                                                      
         MVI   SRCHEL,X'0E'                                                     
         LA    R2,24(R2)                                                        
         CLI   0(R2),X'0E'                                                      
         BE    *+12                                                             
NXTELEM  BAS   RE,NEXTEL                                                        
         BNE   MANSEQ                                                           
*                             CONVERT BINARY(YYMM) TO COMPRESSED                
* NOTE THAT STAB RECS HAS STABPER=YYMM, STABBDT=2 BYTE COMPRESSED               
*                                                                               
DATCONVT MVI   WORK+2,X'01'                                                     
         MVC   WORK(2),STABPER                                                  
         GOTO1 DATCON,DMCB,(3,WORK),(2,WORK+50)    WORK+50=BILLPERIOD           
*                                                                               
         CLC   NBCMPSTR(2),WORK+50      CHK DATE RANGE                          
         BH    NXTELEM                                                          
         CLC   NBCMPEND(2),WORK+50                                              
         BL    NXTELEM                                                          
         MVC   NBACTDAT,WORK+50    FUDGE NBACTDAT FOR ADDLINE ROUTINE           
*                                                                               
         MVC   FULL,STABGRS                                                     
         CLI   NETOPT,C'Y'                                                      
         BNE   *+10                                                             
         MVC   FULL,STABNET                                                     
         L     R0,FULL                                                          
         CVD   R0,DUB                                                           
         LA    R4,WORKAC                                                        
         USING ACCUMD,R4                                                        
         MVC   WORKAC,=5PL8'0'                                                  
         AP    BILLED,DUB                                                       
         SP    UNBILLED,DUB                                                     
         BAS   RE,ADDLINES                                                      
         B     NXTELEM                                                          
*                                                                               
MANX     DS    0H                                                               
         NETGO NVSETUNT,DMCB                                                    
         XC    FILENAME,FILENAME                                                
         MVI   NBFUNCT,NBFRDHI                                                  
         XIT1                                                                   
         DROP  R4                                                               
         EJECT                                                                  
         GETEL R2,NBDTADSP,SRCHEL                                               
*                                                                               
         EJECT                                                                  
* SET MAXIOCTR TO 90% OF MAX IO'S SHOWN BY GETFACT                              
* CHECK IF I/O OVER                                                             
CHKMAXIO NTR1                                                                   
                                                                                
         L     R5,ACOMFACS         GET MAX IO                                   
         USING COMFACSD,R5                                                      
         GOTO1 CGETFACT,DMCB,0                                                  
         L     R1,0(R1)                                                         
                                                                                
         CLI   MAXIOCTR,0          HAVE WE SET MAXIO ?                          
         BNE   MAX20                           YES                              
         MVC   MAXIOCTR,FATMAXIO-FACTSD(R1)    NO                               
         SR    R2,R2                                                            
         LH    R3,MAXIOCTR                                                      
         LA    R4,9                MULTIPLY MAX IO BY 9                         
         MR    R2,R4                                                            
         LA    R4,10               DIVIDE MAXIMUM BY 10                         
         DR    R2,R4                                                            
         STH   R3,MAXIOCTR                                                      
         B     MAXOK                                                            
MAX20    DS    0H                                                               
         CLC   MAXIOCTR,FATIOCNT-FACTSD(R1)   MAXED OUT ?                       
         BH    MAXOK                                                            
                                                                                
         DROP  R5                                                               
******   L     R5,ATWA                                                          
******   USING T31EFFD,R5                                                       
         MVC   CONHEAD(38),=C'*-IO TIMEOUT REDUCE REQUEST PARAMETERS'           
         OI    CONHEADH+6,X'80'                                                 
         GOTO1 ERREX2,DMCB                                                      
         B     MAXOK                                                            
                                                                                
MAXOK    XIT1                                                                   
         DROP  R5                                                               
                                                                                
MAXIOCTR DS    H                                                                
*                                                                               
         EJECT                                                                  
                                                                                
         LTORG                                                                  
BLRDSCT  DS    CL200                                                            
MYIO     DS    CL5000                                                           
         EJECT                                                                  
*              DSECTS FOR FIS                                                   
         SPACE 3                                                                
POSTD    DSECT                                                                  
*                                  COMMON WITH EDIT                             
DPFILT   DS    CL1                                                              
NETOPT   DS    CL1                                                              
INTOPT   DS    CL1                                                              
         SPACE 1                                                                
SAVEPACK DS    CL1                                                              
         SPACE 1                                                                
PERTYPE  DS    CL3                 FIRST BYTE IS PERIOD TYPE                    
MAXMONTS EQU   16                  MAX DATE SETS IN LIST                        
NUMMONS  DS    F                   NUMBER OF MONTHS IN LIST                     
MONLIST  DS    CL(4*MAXMONTS)      MONTH (WEEK) LIST                            
         SPACE 1                                                                
WORKAC   DS    CL40                                                             
STACK    DS    17CL40                                                           
SRCHEL   DS    CL1                                                              
RATESG   DS    F                   GROSS RATE FROM NETNET                       
RATESN   DS    F                   NET RATE FROM NETNET                         
RELO     DS    F                                                                
*                                                                               
OUTAREA  DS    6F                                                               
         SPACE 3                                                                
*              ACCUMULATOR LINE                                                 
         SPACE 1                                                                
ACCUMD   DSECT                                                                  
ORDERED  DS    PL8                                                              
CLEARED  DS    PL8                                                              
UNCLEAR  DS    PL8                                                              
BILLED   DS    PL8                                                              
UNBILLED DS    PL8                                                              
         SPACE 1                                                                
*              NETINCLS HERE                                                    
       ++INCLUDE NETINCLS                                                       
       ++INCLUDE NEGENUNIT                                                      
       ++INCLUDE NETBILLRD                                                      
       ++INCLUDE SPGENSTAB                                                      
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
         EJECT                                                                  
       ++INCLUDE NEMEDFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NEMEDFCD                                                       
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'018NEMED2C   05/19/11'                                      
         END                                                                    
