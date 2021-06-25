*          DATA SET RECNT68    AT LEVEL 041 AS OF 02/02/05                      
*PHASE T80268A,+0                                                               
         TITLE 'T80268 - ENTERPRISE/MARKETRON FORMAT'                           
*                                                                               
***********************************************************************         
*                                                                     *         
*        RECNT68 (T80268) --- ENTERPRISE/MARKETRON FORMAT             *         
*                                                                     *         
* ------------------------------------------------------------------- *         
* REFER TO RECNTHIST FOR PAST HISTORY                                 *         
*                                                                     *         
*                                                                     *         
* 02FEB05 BU  WIDE ORBIT(N TRAFFIC) WORKSHEET                         *         
* 18OCT04 BU  MARKETRON (L TRAFFIC) WORKSHEET                         *         
* 10FEB04 BU  VSS TO RECEIVE KAMAN FORMAT WORKSHEET                   *         
* 07JAN04 BU  OSI TO RECEIVE KAMAN FORMAT WORKSHEET                   *         
* 09JAN01 RHV SPORT BUYS                                              *         
* 21OCT96 RHV PRINT K ORD CMT AFTER BUYLINES                          *         
* 08OCT96 SKU LOW POWER STATION                                       *         
* 07JUN96 SKU ADD EOP CODES                                           *         
* 06MAY96 RHV CONTYPE RECORD CONTROLLED WORKSHEET FORMATTING          *         
* 09APR96 RHV SUPPORT 34 BYTE AGY ADDRESS FIELDS FOR ALL USERS        *         
* 02APR96 RHV NEW DISPLAY OF LAST VER# & MOD#                         *         
* 06FEB96 RHV SUPPRESS PRINTING OF EI FIELDS IF EMPTY                 *         
* 01MAR96 RHV SUPPORT PETRY 34 BYTE AGY ADDR FIELDS                   *         
* 26FEB96 SKU KATZ CONVERTED ORDERS PRINT AGY ADDRESS IN X'71,72,73'  *         
* 06JAN96 SKU PROFILE 24 FOR TYPE D AND PROFILE 20 FOR TYPES N/X      *         
*             PRINT PTP NAME/PHONE OVER SALESPERSON'S                 *         
* 22NOV95 SKU 2K MAKEGOOD SUPPORT                                     *         
*                                                                     *         
***********************************************************************         
*                                                                               
T80268   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T80268,R9                                                      
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         L     R7,4(R1)                                                         
         USING MYD,R7                                                           
         ST    RB,MYRB                                                          
         ST    R9,MYR9                                                          
         L     RA,VTWA                                                          
         USING TWAD,RA                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         LA    RF,HOOK                                                          
         ST    RF,HEADHOOK                                                      
         LA    RF,HEDSPECS                                                      
         ST    RF,SPECS                                                         
         SPACE 2                                                                
         CLI   8(R1),1             ARE WE SETTING ADDRESSES ONLY                
         BE    EXXMOD                                                           
         SPACE 2                                                                
         CLI   FORMAT,C'K'         ENTERPRISE FORMAT                            
         BE    KAMAN                                                            
         CLI   FORMAT,C'N'         WIDE ORBIT FORMAT                            
         BE    KAMAN                                                            
         CLI   FORMAT,C'M'         MARKETRON FORMAT                             
         BE    MARKET                                                           
         CLI   FORMAT,C'I'         OSI       FORMAT                             
         BE    KAMAN                                                            
         CLI   FORMAT,C'L'         MARKETRON L-TRAFFIC FORMAT                   
         BE    KAMAN                                                            
         CLI   FORMAT,C'Z'         VSS       FORMAT                             
         BE    KAMAN                                                            
         DC    H'0'                                                             
         EJECT                                                                  
*           ROUTINE TO FORMAT BUY LINE TO KAMAN SPECIFICATIONS                  
         SPACE 1                                                                
KAMAN    DS    0H                                                               
*                                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF                                                       
         TM    TWAWSFLG,X'40'      K ORD COMMENT PRINTED YET?                   
         BO    KAM1G               YES - SKIP                                   
         OI    TWAWSFLG,X'40'      NO - IT IS NOW                               
*                                                                               
         L     R4,ASVRCOC          REP CONTRACT ORDER COMMENT                   
         LA    R5,10               FOR BCT                                      
         SPACE 1                                                                
         OC    0(60,R4),0(R4)      IS THERE A COMMENT                           
         BZ    KAM1D                                                            
         MVC   P+6(19),=C'*REP ORDER COMMENT*'                                  
*                                                                               
KAM1B    OC    0(60,R4),0(R4)      IS THERE A COMMENT                           
         BZ    KAM1C                                                            
*                                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         OI    TWAWSFLG,X'20'      SINGLE SPACE THIS SECTION                    
         DROP  RF                                                               
         GOTO1 PSTCMT,DMCB,(3,0(R4))  PRINT ORD CMTS                            
*                                                                               
         LA    R4,60(R4)           POINT TO NEXT PART OF COMMENT                
         BCT   R5,KAM1B                                                         
*                                                                               
KAM1C    BAS   RE,GOSPOOL                                                       
         SPACE 1                                                                
*                                                                               
KAM1D    LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF                                                       
         L     R4,ASVSCOC          STA CONTRACT ORDER COMMENT                   
         LA    R5,10               FOR BCT                                      
         OC    0(60,R4),0(R4)      IS THERE A COMMENT                           
         BZ    KAM1G                                                            
         MVC   P+6(23),=C'*STATION ORDER COMMENT*'                              
*                                                                               
KAM1E    OC    0(60,R4),0(R4)      IS THERE A COMMENT                           
         BZ    KAM1F                                                            
*                                                                               
         OI    TWAWSFLG,X'20'      SINGLE SPACE THIS SECTION                    
         DROP  RF                                                               
         MVC   P+30(60),0(R4)                                                   
         OC    P+30(60),SPACES                                                  
         BAS   RE,GOSPOOL                                                       
*                                                                               
         LA    R4,60(R4)           POINT TO NEXT PART OF COMMENT                
         BCT   R5,KAM1E                                                         
*                                                                               
KAM1F    BAS   RE,GOSPOOL                                                       
*                                                                               
KAM1G    DS    0H                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF                                                       
         NI    TWAWSFLG,X'FF'-X'20'                                             
         L     R4,AWORK4           POINT TO OUTPUT                              
         USING PD,R4                                                            
         LA    R3,P                POINT TO PRINT LINE                          
         USING PKAMD,R3                                                         
         ZIC   R5,BUYLIN                                                        
         LR    R2,R5               SET UP ALLOWLIN                              
         CLI   SVSTAOP2,C'Y'       TRIPLE SPACING                               
         BE    K2                                                               
         SLL   R2,1                DOUBLE SPACING                               
         B     *+8                                                              
K2       MH    R2,=H'3'                                                         
         TM    TWAWSFLG,X'02'      PRINT BUYLIN HEADER ALSO?                    
         BZ    *+8                 NO                                           
         AH    R2,=H'4'            YES - ALLOW ANOTHER 4 LINES                  
         STC   R2,ALLOWLIN                                                      
         DROP  RF                                                               
K5       DS    0H                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF                                                       
         TM    TWAWSFLG,X'02'      DO WE NEED TO PRINT BUYLINE HEADER?          
         BZ    K5A                 NO - SKIP                                    
         CLC   CONACT,=C'CONF'     FOR CONFIRM, NEVER PRINT HEADER              
         BE    K5A                                                              
         DROP  RF                                                               
         LA    R6,P                PUT HEADER ON P                              
         BAS   RE,KAMBLH           GENERATE BUYLINE HEADER                      
         BAS   RE,GOSPOOL          AND PRINT IT                                 
         SPACE 1                                                                
K5A      CLC   =C'ALT',PDAT        ALTERNATE WEEKS LINE                         
         BNE   K5D                                                              
         MVC   PKDAT-1(13),=C'ALTERNATE WKS'                                    
         MVC   PKDAY,PDAY          DAY                                          
         MVC   PKTIM,PTIM          TIME                                         
         B     K50                                                              
         SPACE 1                                                                
K5D      CLC   37(11,R4),=C'WEEKLY RATE'                                        
         BNE   *+14                                                             
         MVC   PKTCOS-28(39),PDAT+1   'WEEKLY RATE FOR PLAN XXX IS'             
         B     K50                                                              
         SPACE 1                                                                
         OC    PDAY,PDAY           IF NOTHING THERE, IT'S MORE DATES            
         BZ    K6                  USE EXISTING CODE TO FILL IN                 
         SPACE 1                                                                
K5G      DS    0H                                                               
         CLI   PDAY,X'FF'         SPORTS BUY DESCRIPTIVE?                       
         BNE   K5H                                                              
         MVC   PKTIM(L'PSDESC),PSDESC   SPORTS DESCRIPTIVE                      
         B     K50                                                              
K5H      OC    PDAY(5),PDAY       IF NOTHING HERE, IT'S A COMMENT               
         BNZ   K6                                                               
         CLC   =C'ROC',PDAY+6      REP ORDER COMMENT                            
         BNE   K5L                                                              
         MVC   PK+6(13),=C'*REP ORD CMT*'                                       
         B     K5P                                                              
K5L      CLC   =C'SOC',PDAY+6      STATION ORDER COMMENT                        
         BNE   K5S                                                              
         MVC   PK+6(13),=C'*STA ORD CMT*'                                       
K5P      MVC   21(60,R3),PDAY+10                                                
         B     K50                                                              
         SPACE 1                                                                
*   IT'S A BUY COMMENT, BUT THEY DON'T WANT TO SEE THE LABEL                    
K5S      DS    0H                                                               
         CLC   =C'P=',PDAY+6                                                    
         BNE   K5T                                                              
         MVC   21(12,R3),=C'PROGRAMMING='                                       
         MVC   33(64,R3),PDAY+8                                                 
         B     K50                                                              
*                                                                               
K5T      MVC   21(66,R3),PDAY+6                                                 
         B     K50                                                              
         SPACE 1                                                                
K6       MVC   PKCHG,PCHG          REVISION CODE                                
         OC    PKCHG,PKCHG         ONLY PRINT REV CODE LISTING                  
         BZ    *+8                 AT BOTTOM IF THERE ARE REVISIONS             
         MVI   CODESW,C'Y'                                                      
         SPACE 1                                                                
         MVC   PKLIN,PLIN          LINE NUMBER                                  
*                                                                               
         L     RF,AIO2             BUYREC                                       
         USING RBUYREC,RF                                                       
         TM    RBUYSFG,X'40'       SPORTS BUY?                                  
         BZ    K7                  NO                                           
         DROP  RF                                                               
         MVC   PKTIM(L'PSTYPE),PSTYPE   SPORTS TYPE                             
         B     K8                                                               
K7       MVC   PKDAY,PDAY          DAY                                          
         MVC   PKTIM,PTIM          TIME                                         
         MVC   PKSEC,PSEC          SECTION                                      
         MVC   PKCLS,PCLS          CLASS                                        
         MVC   PKPLN,PPLN          PLAN                                         
         MVC   PKLEN,PLEN          LEN                                          
K8       MVC   PKDAT-1(1),PDAT-1   * FOR ALTERNATE WEEKS                        
         SPACE 1                                                                
         GOTO1 DATVAL,DMCB,(1,PDAT),WORK    DATES                               
         OC    DMCB(4),DMCB                                                     
         BZ    K10                                                              
         MVC   PKDAT(2),WORK+2                                                  
         MVI   PKDAT+2,C'/'                                                     
         MVC   PKDAT+3(2),WORK+4                                                
         SPACE 1                                                                
K10      GOTO1 DATVAL,DMCB,(1,PDAT+6),WORK                                      
         OC    DMCB(4),DMCB                                                     
         BZ    K15                                                              
         MVI   PKDAT+5,C'-'                                                     
         MVC   PKDAT+6(2),WORK+2                                                
         MVI   PKDAT+8,C'/'                                                     
         MVC   PKDAT+9(2),WORK+4                                                
         SPACE 1                                                                
K15      CLI   PDAT+11,C'A'        ALTERNATE WEEKS                              
         BNE   *+8                                                              
         MVI   PKDAT+11,C'A'                                                    
         SPACE 1                                                                
         MVC   PKNPW,PNPW          NUMBER PER WEEK                              
         MVC   PKRAT,PRAT          RATE                                         
         MVC   PKTSPOT,PTOT        TOTAL SPOTS                                  
         SPACE 1                                                                
         OC    PDAY,PDAY           PRINT TOTAL COST ONLY ON FIRST LINE          
         BZ    K50                                                              
         L     R6,AIO2             GET TOTAL BUYLINE COST                       
         USING RBUYKEY,R6                                                       
         TM    RBUYCNTL,X'80'      IF DELETED                                   
         BO    K50                 DON'T SHOW TOTAL COST                        
         CLI   RBUYCHGI,C'C'       CANCELLED?                                   
         BE    K50                 DON'T SHOW TOTAL COST                        
         SPACE 1                                                                
         LA    R2,RBUYELEM                                                      
         USING RBUYELEM,R2                                                      
         EDIT  (4,RBUYTCOS),(12,PKTCOS),2,COMMAS=YES,FLOAT=-                    
         DROP  R2,R6                                                            
         SPACE 1                                                                
K50      BAS   RE,GOSPOOL                                                       
         MVI   RCSUBPRG,2          FOR CONTINUATION PAGES                       
*                                  ONLY INCLUDE A FEW HEADLINES                 
         LA    R4,L'PRTLN(R4)                                                   
         BCT   R5,K5                                                            
         SPACE 1                                                                
         B     EXXMOD                                                           
         DROP  R4                                                               
         EJECT                                                                  
*           ROUTINE TO FORMAT BUY LINE TO MARKETRON SPECIFICATIONS              
         SPACE 1                                                                
MARKET   DS    0H                                                               
*                                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF                                                       
         TM    TWAWSFLG,X'40'      K ORD COMMENT PRINTED YET?                   
         BO    MAR1G               YES - SKIP                                   
         OI    TWAWSFLG,X'40'      NO - IT IS NOW                               
*                                                                               
         L     R4,ASVRCOC          REP CONTRACT ORDER COMMENT                   
         LA    R5,10               FOR BCT                                      
         SPACE 1                                                                
         OC    0(60,R4),0(R4)      IS THERE A COMMENT                           
         BZ    MAR1D                                                            
         MVC   P+6(19),=C'*REP ORDER COMMENT*'                                  
*                                                                               
MAR1B    OC    0(60,R4),0(R4)      IS THERE A COMMENT                           
         BZ    MAR1C                                                            
*                                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         OI    TWAWSFLG,X'20'      SINGLE SPACE THIS SECTION                    
         DROP  RF                                                               
         GOTO1 PSTCMT,DMCB,(3,0(R4))  PRINT ORD CMTS                            
*                                                                               
         LA    R4,60(R4)           POINT TO NEXT PART OF COMMENT                
         BCT   R5,MAR1B                                                         
*                                                                               
MAR1C    BAS   RE,GOSPOOL                                                       
         SPACE 1                                                                
*                                                                               
MAR1D    LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF                                                       
         L     R4,ASVSCOC          STA CONTRACT ORDER COMMENT                   
         LA    R5,10               FOR BCT                                      
         OC    0(60,R4),0(R4)      IS THERE A COMMENT                           
         BZ    MAR1G                                                            
         MVC   P+6(23),=C'*STATION ORDER COMMENT*'                              
*                                                                               
MAR1E    OC    0(60,R4),0(R4)      IS THERE A COMMENT                           
         BZ    MAR1F                                                            
*                                                                               
         OI    TWAWSFLG,X'20'      SINGLE SPACE THIS SECTION                    
         DROP  RF                                                               
         MVC   P+30(60),0(R4)                                                   
         OC    P+30(60),SPACES                                                  
         BAS   RE,GOSPOOL                                                       
*                                                                               
         LA    R4,60(R4)           POINT TO NEXT PART OF COMMENT                
         BCT   R5,MAR1E                                                         
*                                                                               
MAR1F    BAS   RE,GOSPOOL                                                       
*                                                                               
MAR1G    DS    0H                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF                                                       
         NI    TWAWSFLG,X'FF'-X'20'                                             
         L     R4,AWORK4           POINT TO OUTPUT                              
         USING PD,R4                                                            
         LA    R3,P                POINT TO PRINT LINE                          
         USING PMARD,R3                                                         
         ZIC   R5,BUYLIN                                                        
         LR    R2,R5               SET UP ALLOWLIN                              
         CLI   SVSTAOP2,C'Y'       TRIPLE SPACING                               
         BE    M2                                                               
         SLL   R2,1                DOUBLE SPACING                               
         B     *+8                                                              
M2       MH    R2,=H'3'                                                         
         TM    TWAWSFLG,X'02'      PRINT BUYLIN HEADER ALSO?                    
         BZ    *+8                 NO                                           
         AH    R2,=H'4'            YES - ALLOW ANOTHER 4 LINES                  
         STC   R2,ALLOWLIN                                                      
         DROP  RF                                                               
         SPACE 1                                                                
*                                                                               
M5       DS    0H                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF                                                       
         TM    TWAWSFLG,X'02'      DO WE NEED TO PRINT BUYLINE HEADER?          
         BZ    M5A                 NO - SKIP                                    
         CLC   CONACT,=C'CONF'     FOR CONFIRM, NEVER PRINT HEADER              
         BE    M5A                                                              
         DROP  RF                                                               
         LA    R6,P                PUT HEADER ON P                              
         BAS   RE,MARBLH           GENERATE BUYLINE HEADER                      
         BAS   RE,GOSPOOL          AND PRINT IT                                 
M5A      DS    0H                                                               
         CLC   =C'ALT',PDAT        ALTERNATE WEEKS LINE                         
         BNE   M5D                                                              
         MVC   PMDAT-1(13),=C'ALTERNATE WKS'                                    
         MVC   PMDAY,PDAY          DAY                                          
         MVC   PMTIM,PTIM          TIME                                         
         B     M50                                                              
         SPACE 1                                                                
M5D      CLC   =C'WEEKLY RATE',PDAT+1                                           
         BNE   *+14                                                             
         MVC   PMRAT-28(39),PDAT+1   'WEEKLY RATE FOR PLAN XXX IS'              
         B     M50                                                              
         SPACE 1                                                                
         OC    PDAY,PDAY           IF NOTHING THERE, IT'S MORE DATES            
         BZ    M6                  USE EXISTING CODE TO FILL IN                 
         SPACE 1                                                                
M5G      DS    0H                                                               
         CLI   PDAY,X'FF'         SPORTS BUY DESCRIPTIVE?                       
         BNE   M5H                                                              
         MVC   PMDAT(L'PSDESC),PSDESC   SPORTS DESCRIPTIVE                      
         B     M50                                                              
M5H      OC    PDAY(5),PDAY       IF NOTHING HERE, IT'S A COMMENT               
         BNZ   M6                                                               
         CLC   =C'ROC',PDAY+6      REP ORDER COMMENT                            
         BNE   M5L                                                              
         MVC   PM+6(13),=C'*REP ORD CMT*'                                       
         B     M5P                                                              
M5L      CLC   =C'SOC',PDAY+6      STATION ORDER COMMENT                        
         BNE   M5S                                                              
         MVC   PM+6(13),=C'*STA ORD CMT*'                                       
M5P      MVC   21(60,R3),PDAY+10                                                
         B     M50                                                              
         SPACE 1                                                                
*   IT'S A BUY COMMENT, BUT THEY DON'T WANT TO SEE THE LABEL                    
M5S      DS    0H                                                               
         CLC   =C'P=',PDAY+6                                                    
         BNE   M5T                                                              
         MVC   21(12,R3),=C'PROGRAMMING='                                       
         MVC   33(64,R3),PDAY+8                                                 
         B     M50                                                              
*                                                                               
M5T      MVC   21(66,R3),PDAY+6                                                 
         B     M50                                                              
         SPACE 1                                                                
M6       MVC   PMCHG,PCHG          REVISION CODE                                
         OC    PMCHG,PMCHG         ONLY PRINT REV CODE LISTING                  
         BZ    *+8                 AT BOTTOM IF THERE ARE REVISIONS             
         MVI   CODESW,C'Y'                                                      
         SPACE 1                                                                
         MVC   PMLIN,PLIN          LINE NUMBER                                  
*                                                                               
         L     RF,AIO2             BUYREC                                       
         USING RBUYREC,RF                                                       
         TM    RBUYSFG,X'40'       SPORTS BUY?                                  
         BZ    M6A                 NO                                           
         DROP  RF                                                               
         MVC   PMTIM(L'PSTYPE),PSTYPE   SPORTS TYPE                             
         B     M9A                                                              
M6A      MVC   PMDAY,PDAY          DAY                                          
         MVC   PMTIM,PTIM          TIME                                         
         OC    SVSWE,SVSWE         IS THERE  UNI SUPPLEMENTARY ELEMENT?         
         BZ    M7                  NOT UNIVISION                                
         MVC   PMPTY,PSEC          WANT SECTION TO PRINT IN PTY                 
         CLI   PCLS+2,C'W'         IS THERE A REAL CLASS                        
         BE    M9                  NOT REAL CLASS DON'T PRNT CLS OR AVA         
         MVC   PMAVA+2(3),PCLS     YES REAL CLASS - PRINT IN AVA                
         B     M9                  BYPASS SECTION AND CLASS                     
*                                                                               
M7       MVC   PMSEC,PSEC          SECTION                                      
         MVC   PMCLS,PCLS          CLASS                                        
M9       MVC   PMPLN,PPLN          PLAN                                         
         MVC   PMLEN,PLEN          LEN                                          
         MVC   PMNPW,PNPW          NUMBER PER WEEK                              
M9A      MVC   PMDAT-1(1),PDAT-1   * FOR ALTERNATE WEEKS                        
         SPACE 1                                                                
         GOTO1 DATVAL,DMCB,(1,PDAT),WORK    DATES                               
         OC    DMCB(4),DMCB                                                     
         BZ    M10                                                              
         MVC   PMDAT(2),WORK+2                                                  
         MVI   PMDAT+2,C'/'                                                     
         MVC   PMDAT+3(2),WORK+4                                                
         SPACE 1                                                                
M10      GOTO1 DATVAL,DMCB,(1,PDAT+6),WORK                                      
         OC    DMCB(4),DMCB                                                     
         BZ    M15                                                              
         MVI   PMDAT+5,C'-'                                                     
         MVC   PMDAT+6(2),WORK+2                                                
         MVI   PMDAT+8,C'/'                                                     
         MVC   PMDAT+9(2),WORK+4                                                
         SPACE 1                                                                
M15      CLI   PDAT+11,C'A'        ALTERNATE WEEKS                              
         BNE   *+8                                                              
         MVI   PMDAT+11,C'A'                                                    
         SPACE 1                                                                
         MVC   PMRAT,PRAT          RATE                                         
         MVC   PMTSPOT,PTOT        TOTAL SPOTS                                  
         B     M50                                                              
         SPACE 1                                                                
M50      BAS   RE,GOSPOOL                                                       
         MVI   RCSUBPRG,4          FOR CONTINUATION PAGES                       
*                                  ONLY INCLUDE A FEW HEADLINES                 
         LA    R4,L'PRTLN(R4)                                                   
         BCT   R5,M5                                                            
         SPACE 1                                                                
         B     EXXMOD                                                           
         DROP  R4                                                               
         EJECT                                                                  
*              ROUTINE TO HANDLE SPOOL INTERFACE                                
         SPACE 1                                                                
GOSPOOL  NTR1                                                                   
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF                                                       
         MVI   SPACING,2                                                        
         CLI   SVSTAOP2,C'Y'       TRIPLE SPACING                               
         BNE   *+8                                                              
         MVI   SPACING,3                                                        
         TM    TWAWSFLG,X'20'                                                   
         BZ    GOSPL2                                                           
         MVI   SPACING,1                                                        
GOSPL2   GOTO1 SPOOL,DMCB,(R8)                                                  
         DROP  RF                                                               
         CLI   MYHEDSW,C'Y'        DID WE JUST PRINT HEADLINES                  
         BNE   GOSPLX                                                           
         LA    R4,H15                                                           
         ZIC   R3,XTRHED                                                        
GOSPL5   MVC   P,0(R4)             PRINT THE EXTRA HEADLINES                    
         MVI   SPACING,1           NO DOUBLE SPACE BWTN HEADS                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         LA    R4,132(R4)                                                       
         BCT   R3,GOSPL5                                                        
         SPACE 1                                                                
         GOTO1 SPOOL,DMCB,(R8)     PRINT BLANK LINE FOR SPACING                 
         SPACE 1                                                                
         MVC   P,SVPRNT            NOW PRINT THE LINE OF DATA                   
         MVI   SPACING,2                                                        
         CLI   SVSTAOP2,C'Y'       TRIPLE SPACING                               
         BNE   *+8                                                              
         MVI   SPACING,3                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
GOSPLX   MVI   MYHEDSW,C'N'                                                     
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* PURPOSE:                                                                      
*     PRINT STORED COMMENTS IF ANY, ELSE IT WILL PRINT FREE                     
*     FORM COMMENTS                                                             
*                                                                               
* INPUT: PARAMETER 1: BYTE 1    = MODE                                          
*                     BYTE 2-4  = A(COMMENT CODE)                               
*                                                                               
* OUTPUT: NONE                                                                  
***********************************************************************         
PSTCMT   NTR1                                                                   
         L     R3,0(R1)                                                         
*                                                                               
         MVC   WORK2X(L'KEY),KEY     SAVE OFF KEY FOR RESTORE                   
         LA    R2,IOAREA                                                        
         GOTO1 VREGENSC,DMCB,(3,0(R3)),(R2),DATAMGR,RCONREC,GETTXT              
         BNZ   PSTCMTX             COMMENT NOT FOUND, PRINT NOTHING             
         CLI   0(R2),X'FF'         IF X'FF', PRINT NOTHING                      
         BE    PSTCMTX                                                          
         CLI   0(R2),0             IF NULL, PRINT FREE FORM COMMENT             
         BE    PSTCMT20                                                         
*                                                                               
PSTCMT10 ZIC   R4,0(R2)            GET LENGTH OF COMMENT                        
         BCTR  R4,0                                                             
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   P+30(0),1(R2)                                                    
         BAS   RE,GOSPOOL                                                       
*                                                                               
         ZIC   R4,0(R2)            BUMP TO NEXT COMMENT ENTRY                   
         AR    R2,R4                                                            
         CLI   0(R2),X'FF'         IF X'FF', DONE                               
         BE    PSTCMTX                                                          
*                                                                               
         LR    R4,RC               BOUNDARY CHECK FOR R2                        
         A     R4,=AL4(IOAREA-GENOLD+1001)                                      
         CR    R4,R2                                                            
         BH    PSTCMT10                                                         
         B     PSTCMTX                                                          
*                                                                               
PSTCMT20 DS    0H                  PRINT FREE FORM COMMENTS                     
         MVC   P+30(60),0(R3)                                                   
         OC    P+30(60),SPACES                                                  
*                                                                               
         BAS   RE,GOSPOOL                                                       
*                                                                               
PSTCMTX  DS    0H                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         TM    TWAWSFLG,X'04'           NEED TO RESTORE SEQ LOOP?               
         BZ    XIT                                                              
         DROP  RF                                                               
         MVC   KEY,WORK2X               RESTORE BYREC SEQ LOOP                  
         OI    DMINBTS,X'08'       PASS DELETES                                 
         GOTO1 VHIGH                                                            
         CLC   KEY(L'RBUYKEY),KEYSAVE                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO GENERATE HEADLINES (HEADHOOK)                         
         SPACE 2                                                                
HOOK     NTR1  BASE=MYRB                                                        
         L     R9,MYR9                                                          
         CLI   FORMAT,C'K'         KAMAN FORMAT                                 
         BE    HD10                                                             
         CLI   FORMAT,C'N'         WIDE ORBIT FORMAT                            
         BE    HD10                                                             
         CLI   FORMAT,C'M'         MARKETRON FORMAT                             
         BE    HD200                                                            
         CLI   FORMAT,C'I'         OSI       FORMAT                             
         BE    HD10                                                             
         CLI   FORMAT,C'L'         MARKETRON L-TRAFFIC FORMAT                   
         BE    HD10                                                             
         CLI   FORMAT,C'Z'         VSS       FORMAT                             
         BE    HD10                                                             
         DC    H'0'                                                             
         SPACE 1                                                                
HD10     DC    0H'0'    ******KAMAN HEADLINES*********                          
         CLI   RCSUBPRG,1                                                       
         BNE   *+8                                                              
         MVI   RCSUBPRG,2          FOR CONTINUATION PAGES                       
*                                  ONLY INCLUDE A FEW HEADLINES                 
         CLC   =C'MGS',CONACT                                                   
         BNE   HD15                                                             
         MVC   H1+41(24),=C'MAKEGOOD OFFER WORKSHEET'                           
*                                                                               
HD15     DS    0H                                                               
         MVC   H2(5),ACTSTAT       STATION                                      
         CLI   ACTSTAT+4,C' '      TV                                           
         BNE   *+10                                                             
         MVC   H2+4(3),=C'-TV'                                                  
         CLI   ACTSTAT+4,C'L'      TV                                           
         BNE   *+10                                                             
         MVC   H2+4(3),=C'-L '                                                  
         MVC   H2+9(20),SVSTAMKT   MARKET                                       
         SPACE 1                                                                
*                                                                               
*   CALL ROUTINE TO DISPLAY MOD & VERSION NUMBER                                
         GOTO1 VGENDMV,DMCB,RCONREC,H1,GENOLD                                   
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),(8,H2+87)                                      
         MVC   H2+100(2),=C'AT'                                                 
         UNPK  DUB,SENDTIME                                                     
         MVC   H2+103(2),DUB+1     TIME                                         
         MVI   H2+105,C'.'                                                      
         MVC   H2+106(2),DUB+3                                                  
         CLC   CONACT,=C'LAST'     FOR ACTION LAST                              
         BNE   *+14                IDENTIFY AS DUPLICATE COPY                   
         MVC   H3+99(9),=C'DUPLICATE'                                           
         B     HD50                                                             
*                                                                               
         CLC   =C'RSND',CONACT    FOR ACTION RESEND                             
         BNE   *+14                IDENTIFY AS A DUPLICATE COPY                 
         MVC   H3+102(6),=C'RESENT'                                             
         B     HD50                                                             
*                                                                               
         MVC   H1+87(8),=C'FROM REP'                                            
         CLI   TWAACCS,C'$'        STATION                                      
         BNE   *+10                                                             
         MVC   H1+87(12),=C'FROM STATION'                                       
         SPACE 1                                                                
HD50     DS    0H                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF                                                       
         CLC   PAGE,=H'1'          FIRST PAGE?                                  
         BE    HD60                                                             
         NI    SPOOLIND,X'7F'      TURN OFF 'SKIP LINE' INDICATOR               
         MVC   H2+50(8),CONCNUM    ACE NUMBER                                   
         TM    TWAWSFLG,X'80'           SHOULD WE PRINT BUYLINE HEADER?         
         BZ    XIT                                                              
         MVI   H3,0                                                             
         LA    R6,H4                                                            
         BAS   RE,KAMBLH                                                        
         B     XIT                                                              
         SPACE 1                                                                
HD60     MVI   MYHEDSW,C'Y'        MORE THAN 14 HEADLINES                       
         MVC   SVPRNT,P            SAVE PRINT LINE                              
         MVC   P,SPACES            BLANK OUT PRINT LINE                         
         MVI   XTRHED,3            MAXIMUM NUMBER OF EXTRA HEADLINES            
         OI    SPOOLIND,X'80'  DON'T SKIP LINES AFTER 1ST SET OF HEADS          
         SPACE 1                                                                
         ZIC   R0,XTRHED           CLEAR OUT EXTRA HEADLINES                    
         LA    R1,H15                                                           
HD70     MVC   0(132,R1),SPACES                                                 
         LA    R1,132(R1)                                                       
         BCT   R0,HD70                                                          
         SPACE 1                                                                
         TM    SVCONF,X'80'        IF UNCONFIRMED                               
         BZ    HD80                                                             
         CLI   SVVER,1             AND NOT VERSION 1                            
         BE    HD80                                                             
         TM    SVSTAT,X'04'        AND PRINT CHANGES ONLY                       
         BZ    HD80                                                             
         MVC   H2+46(12),=C'CHANGES ONLY'  THEN SAY SO IN HEADLINE              
         SPACE 1                                                                
HD80     MVC   H4+8(10),SVADVC     ADVERTISER CODE                              
         MVC   H6+8(10),SVAGYC     AGENCY CODE                                  
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF                                                       
         MVC   H4+28(20),TWAADVNM  ADVERTISER NAME                              
         TM    TWAFMTFL,X'08'      CARE OF AGENCY?                              
         BZ    HD084               NO                                           
*                                                                               
         LA    RE,H6+28                                                         
         MVC   0(L'RCONKADV,RE),RCONKADV                                        
         LA    RE,L'RCONKADV+1(RE)                                              
         MVC   0(4,RE),=C'C/O '                                                 
         LA    RE,4(RE)                                                         
         MVC   0(L'TWAAGNM1,RE),TWAAGNM1  AGENCY NAME FROM SCREEN               
         B     HD086                                                            
*                                                                               
HD084    DS    0H                                                               
         MVC   H6+28(33),TWAAGNM2  FULL AGENCY NAME                             
*                                                                               
HD086    DS    0H                                                               
         MVC   H8+60(6),TWAEOPSL   EOP SL#                                      
         MVC   H8+81(20),TWASALNM  SALESPERSON NAME                             
         DROP  RF                                                               
         MVC   H6+88(20),CONOFFN   OFFICE NAME                                  
         GOTO1 DATCON,DMCB,(3,SVCONDTE),(5,H8+13)                               
         GOTO1 DATCON,DMCB,(3,SVCONDTE+3),(5,H8+35)                             
*                                                                               
         MVC   H12+18(20),CONPRD   PRODUCT NAME                                 
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF                                                       
         MVC   H10+65(20),TWABUYER BUYER                                        
         CLC   SVCONPRD,SPACES                                                  
         BE    *+10                                                             
         MVC   H12+18(20),TWAPRDNM     PRODUCT NAME                             
         DROP  RF                                                               
         SPACE 1                                                                
         OC    SVTRAF,SVTRAF       TRAFFIC NUMBER                               
         BZ    *+10                                                             
         MVC   H12+65(10),SVTRAF                                                
         SPACE 1                                                                
         MVC   H12+89(8),CONCNUM   ACE NUMBER                                   
         MVC   H14+70(9),SVSASST   SALES ASSISTANT                              
*                                                                               
* DISPLAY EASI CODES (IF EXIST)                                                 
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'A2'                                                     
         BAS   RE,GETEL                                                         
         BNE   HD90                                                             
         USING RCONIEL,R6                                                       
         MVC   H14+81(6),=C'EI ADV'                                             
         MVC   H14+93(3),=C'PRD'                                                
         MVC   H15+84(3),=C'PRD'                                                
         MVC   H15+93(3),=C'EST'                                                
         MVC   H14+88(4),RCONIADV                                               
         MVC   H14+97(4),RCONIPRD                                               
         MVC   H15+88(4),RCONIPR2                                               
         MVC   H15+97(10),RCONXEST                                              
         OC    H15+97(10),MYSPACES                                              
         CLC   H15+97(10),MYSPACES                                              
         BNE   *+10                                                             
         MVC   H15+97(4),RCONIEST                                               
         DROP  R6                                                               
*                                                                               
HD90     DS    0H                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         MVC   H14+11(34),TWAAGAD1                                              
         MVC   H15+11(36),TWAAGAD2                                              
         MVC   H16+11(36),TWAAGAD3                                              
         DROP  RF                                                               
*                                                                               
* DEAL WITH HEADLINES THAT DON'T FIT IN HEDSPECS                                
*                                                                               
HD115    DS    0H                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         LA    R1,H17                                                           
         OC    TWAAGYPH,TWAAGYPH                                                
         BZ    HD120                                                            
         MVC   0(7,R1),=C'PHONE #'   AGENCY PHONE NUMBER                        
         MVC   8(3,R1),TWAAGYPH                                                 
         MVI   11(R1),C'-'                                                      
         MVC   12(3,R1),TWAAGYPH+3                                              
         MVI   15(R1),C'-'                                                      
         MVC   16(4,R1),TWAAGYPH+6                                              
         LA    R1,25(R1)                                                        
*                                                                               
HD120    DS    0H                                                               
         OC    TWAAFAX,TWAAFAX                                                  
         BZ    HD130                                                            
         MVC   0(5,R1),=C'FAX #'  AGENCY FAX NUMBER                             
         MVC   6(3,R1),TWAAFAX                                                  
         MVI   9(R1),C'-'                                                       
         MVC   10(3,R1),TWAAFAX+3                                               
         MVI   13(R1),C'-'                                                      
         MVC   14(4,R1),TWAAFAX+6                                               
         DROP  RF                                                               
*                                                                               
HD130    DS    0H                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         MVC   H15(7),=C'ADDRESS'                                               
         MVC   H15+70(9),DASH      UNDERLINE SALES ASST.                        
*                                                                               
         MVC   H16+52(16),=C'SALESPERSON PH #'                                  
         MVC   H16+69(L'TWASALTL),TWASALTL                                      
*                                                                               
         OC    TWASALFX,TWASALFX   SALESPERSON FAX NUMBER                       
         BZ    HD140                                                            
         MVC   H17+52(16),=C'SALESPERSON FAX#'                                  
         MVC   H17+69(L'TWASALFX),TWASALFX                                      
         B     HD150                                                            
*                                                                               
HD140    DS    0H                  -OR- OFFICE FAX NUMBER                       
         OC    TWAOFFFX,TWAOFFFX                                                
         BZ    HD150                                                            
         MVC   H17+52(12),=C'OFFICE FAX# '                                      
         MVC   H17+64(3),TWAOFFFX                                               
         MVI   H17+67,C'-'                                                      
         MVC   H17+68(3),TWAOFFFX+3                                             
         MVI   H17+71,C'-'                                                      
         MVC   H17+72(4),TWAOFFFX+6                                             
HD150    DS    0H                                                               
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
HD200    DC    0H'0'     *****MARKETRON HEADLINES********                       
         CLI   RCSUBPRG,3                                                       
         BNE   *+8                                                              
         MVI   RCSUBPRG,4          FOR CONTINUATION PAGES                       
*                                  ONLY INCLUDE A FEW HEADLINES                 
         CLC   =C'MGS',CONACT                                                   
         BNE   HD205                                                            
         MVC   H1+41(24),=C'MAKEGOOD OFFER WORKSHEET'                           
*                                                                               
HD205    DS    0H                                                               
         MVC   H2(5),ACTSTAT       STATION                                      
         CLI   ACTSTAT+4,C' '      TV                                           
         BNE   *+10                                                             
         MVC   H2+4(3),=C'-TV'                                                  
         CLI   ACTSTAT+4,C'L'      TV                                           
         BNE   *+10                                                             
         MVC   H2+4(3),=C'-L '                                                  
         MVC   H2+9(20),SVSTAMKT   MARKET                                       
         MVC   H2+50(8),CONCNUM    ACE NUMBER                                   
         SPACE 1                                                                
*                                                                               
*   CALL ROUTINE TO DISPLAY MOD & VERSION NUMBER                                
         GOTO1 VGENDMV,DMCB,RCONREC,H1,GENOLD                                   
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),(8,H2+87)                                      
         MVC   H2+100(2),=C'AT'                                                 
         UNPK  DUB,SENDTIME                                                     
         MVC   H2+103(2),DUB+1     TIME                                         
         MVI   H2+105,C'.'                                                      
         MVC   H2+106(2),DUB+3                                                  
         CLC   CONACT,=C'LAST'     FOR ACTION LAST                              
         BNE   *+14                IDENTIFY AS DUPLICATE COPY                   
         MVC   H3+99(9),=C'DUPLICATE'                                           
         B     HD240                                                            
*                                                                               
         CLC   =C'RSND',CONACT    FOR ACTION RESEND                             
         BNE   *+14                IDENTIFY AS A DUPLICATE COPY                 
         MVC   H3+102(6),=C'RESENT'                                             
         B     HD240                                                            
*                                                                               
         MVC   H1+87(8),=C'FROM REP'                                            
         CLI   TWAACCS,C'$'        STATION                                      
         BNE   *+10                                                             
         MVC   H1+87(12),=C'FROM STATION'                                       
         SPACE 1                                                                
HD240    LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF                                                       
         CLC   PAGE,=H'1'          FIRST PAGE?                                  
         BE    HD250                                                            
         NI    SPOOLIND,X'7F'      TURN OFF 'SKIP LINE' INDICATOR               
         TM    TWAWSFLG,X'80'           SHOULD WE PRINT BUYLINE HEADER?         
         BZ    XIT                                                              
         MVI   H3,0                                                             
         LA    R6,H4                                                            
         BAS   RE,MARBLH                                                        
         B     XIT                                                              
         SPACE 1                                                                
HD250    MVI   MYHEDSW,C'Y'        MORE THAN 14 HEADLINES                       
         MVC   SVPRNT,P            SAVE PRINT LINE                              
         MVC   P,SPACES            BLANK OUT PRINT LINE                         
         MVI   XTRHED,14           MAXIMUM NUMBER OF EXTRA HEADLINES            
         OI    SPOOLIND,X'80'  DON'T SKIP LINES AFTER 1ST SET OF HEADS          
         SPACE 1                                                                
         ZIC   R0,XTRHED           CLEAR OUT EXTRA HEADLINES                    
         LA    R1,H15                                                           
HD260    MVC   0(132,R1),SPACES                                                 
         LA    R1,132(R1)                                                       
         BCT   R0,HD260                                                         
         SPACE 1                                                                
         TM    SVCONF,X'80'        IF UNCONFIRMED                               
         BZ    HD270                                                            
         CLI   SVVER,1             AND NOT VERSION 1                            
         BE    HD270                                                            
         TM    SVSTAT,X'04'        AND PRINT CHANGES ONLY                       
         BZ    HD270                                                            
         MVC   H3+87(12),=C'CHANGES ONLY'  THEN SAY SO IN HEADLINE              
         SPACE 1                                                                
HD270    OC    SVTRAF,SVTRAF       TRAFFIC NUMBER                               
         BZ    *+10                                                             
         MVC   H4+12(10),SVTRAF                                                 
         SPACE 1                                                                
         GOTO1 DATCON,DMCB,(3,SVCONDTE),(5,H6+100)                              
         GOTO1 DATCON,DMCB,(3,SVCONDTE+3),(5,H8+100)                            
         SPACE 1                                                                
         MVC   H11+14(10),SVADVC   ADVERTISER CODE                              
         MVC   H13+14(10),SVAGYC   AGENCY CODE                                  
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF                                                       
         MVC   H11+44(20),TWAADVNM  ADVERTISER NAME                             
         TM    TWAFMTFL,X'08'      CARE OF AGENCY?                              
         BZ    HD094               NO                                           
*                                                                               
         LA    RE,H13+44                                                        
         MVC   0(L'RCONKADV,RE),RCONKADV                                        
         LA    RE,L'RCONKADV+1(RE)                                              
         MVC   0(4,RE),=C'C/O '                                                 
         LA    RE,4(RE)                                                         
         MVC   0(L'TWAAGNM1,RE),TWAAGNM1  AGENCY NAME FROM SCREEN               
         B     HD096                                                            
*                                                                               
HD094    DS    0H                                                               
         MVC   H13+44(33),TWAAGNM2  AGENCY NAME FOR CONTRACTS                   
*                                                                               
HD096    DS    0H                                                               
         MVC   H15+27(20),TWASALNM  SALESPERSON NAME                            
         MVC   H15+9(3),SVSALC      SALESPERSON CODE                            
         MVC   H15+57(20),CONOFFN   OFFICE NAME                                 
         MVC   H19+14(20),TWABUYER BUYER                                        
         MVC   H19+61(9),SVSASST   SALES ASSISTANT                              
         MVC   H17+14(20),CONPRD   PRODUCT NAME                                 
         CLC   SVCONPRD,SPACES                                                  
         BE    *+10                                                             
         MVC   H17+14(20),TWAPRDNM     PRODUCT NAME                             
         DROP  RF                                                               
         SPACE 1                                                                
*  CHECK FOR ANY CASH/TRADE/NON-CM                                              
         CLI   SVSWE,0            ANY INPUT?                                    
         BE    HD290              NO                                            
         CLI   SVSWE,C'Y'         OLD WAY Y/N?                                  
         BNE   HD280                                                            
         MVI   H6+7,C'Y'                                                        
         B     HD330                                                            
*                                                                               
HD280    ZIC   R1,SVSWE           NEW WAY - NUMERIC                             
         EDIT  (R1),(2,H6+7),ALIGN=LEFT,ZERO=NOBLANK                            
         B     HD330                                                            
*                                                                               
HD290    CLI   SVSWE+1,0          ANY TRADE INPUT?                              
         BE    HD310              NO                                            
         CLI   SVSWE+1,C'Y'       TRADE - OLD WAY                               
         BNE   HD300              MUST BE NEW WAY - NUMERIC                     
         MVI   H6+22,C'Y'                                                       
         B     HD330                                                            
*                                                                               
HD300    ZIC   R1,SVSWE+1                                                       
         EDIT  (R1),(2,H6+7),ALIGN=LEFT,ZERO=NOBLANK                            
         B     HD330                                                            
*                                                                               
HD310    CLI   SVSWE+2,0          ANY NON-CM INPUT?                             
         BE    HD330                                                            
         CLI   SVSWE+2,C'Y'       NON-CM - OLD WAY                              
         BNE   HD320                                                            
         MVI   H6+37,C'Y'                                                       
         B     HD330                                                            
         SPACE                                                                  
HD320    ZIC   R1,SVSWE+2                                                       
         EDIT  (R1),(2,H6+7),ALIGN=LEFT,ZERO=NOBLANK                            
         B     HD330                                                            
* SHOW IF ANY LOCAL/NAT/LOPOL/NAPOL                                             
HD330    DS    0H                                                               
         CLI   SVSWE+10,1          LOCAL                                        
         BNE   HD340                                                            
         MVI   H6+50,C'Y'                                                       
         B     HD370                                                            
HD340    CLI   SVSWE+10,2          NATIONAL                                     
         BNE   HD350                                                            
         MVI   H6+62,C'Y'                                                       
         B     HD370                                                            
HD350    CLI   SVSWE+10,3          LOPOL                                        
         BNE   HD360                                                            
         MVI   H8+50,C'Y'                                                       
         B     HD370                                                            
HD360    CLI   SVSWE+10,4          NAPOL                                        
         BNE   HD370                                                            
         MVI   H8+62,C'Y'                                                       
         B     HD370                                                            
         SPACE                                                                  
HD370    CLC   SVSWE+3(3),SPACES                                                
         BE    *+10                                                             
         MVC   H17+43(3),SVSWE+3       PPC                                      
         SPACE 1                                                                
         CLC   SVSWE+6(3),SPACES                                                
         BE    *+10                                                             
         MVC   H17+55(3),SVSWE+6       SPC                                      
         SPACE                                                                  
         LA    R6,H21+92                                                        
         LA    R4,SVSWE+14        REMARK                                        
         LA    R5,4                                                             
HD380    CLI   0(R4),0            ANY REMARK #?                                 
         BE    HD400                                                            
         CH    R5,=H'4'                                                         
         BE    HD390                                                            
         MVI   0(R6),C','                                                       
         LA    R6,1(R6)                                                         
HD390    EDIT  (B1,0(R4)),(2,0(R6)),ALIGN=LEFT                                  
         AR    R6,R0                                                            
         LA    R4,1(R4)                                                         
         BCT   R5,HD380                                                         
*                                                                               
HD400    DS    0H                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF                                                       
         MVC   H21+10(34),TWAAGAD1                                              
         MVC   H22+10(36),TWAAGAD2                                              
         MVC   H23+10(36),TWAAGAD3                                              
         DROP  RF                                                               
*                                                                               
* DEAL WITH HEADLINES THAT DON'T FIT IN HEDSPECS                                
*                                                                               
HD425    DS    0H                                                               
         MVC   H15(7),=C'SALES #'                                               
         MVC   H15+21(4),=C'NAME'                                               
         MVC   H15+49(6),=C'OFFICE'                                             
         MVC   H15+78(26),=C'*  STANDARD       CALENDAR'                        
         MVC   H16+9(10),DASH                                                   
         MVC   H16+27(20),DASH                                                  
         MVC   H16+57(20),DASH                                                  
         MVI   H16+78,C'*'                                                      
         MVC   H16+91(3),DASH                                                   
         MVC   H16+105(3),DASH                                                  
         SPACE 1                                                                
         MVC   H17(12),=C'PRODUCT NAME'                                         
         MVC   H17+38(3),=C'PPC'                                                
         MVC   H17+50(3),=C'SPC'                                                
         MVC   H17+62(3),=C'APC'                                                
         MVC   H17+78(25),=C'*  WEEKLY         SPECIAL'                         
         MVC   H18+14(20),DASH                                                  
         MVC   H18+43(3),DASH                                                   
         MVC   H18+55(3),DASH                                                   
         MVC   H18+67(3),DASH                                                   
         MVI   H18+78,C'*'                                                      
         MVC   H18+91(3),DASH                                                   
         MVC   H18+105(3),DASH                                                  
         SPACE 1                                                                
         MVC   H19(10),=C'BUYER NAME'                                           
         MVC   H19+44(15),=C'SALES ASSISTANT'                                   
         MVC   H19+78(25),=C'* MANY PRDS       SEP INV'                         
         MVC   H20+14(20),DASH                                                  
         MVC   H20+61(9),DASH                                                   
         MVI   H20+78,C'*'                                                      
         MVC   H20+91(3),DASH                                                   
         MVC   H20+105(3),DASH                                                  
         SPACE 2                                                                
*                                                                               
* DISPLAY EASI CODES (IF EXIST)                                                 
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'A2'                                                     
         BAS   RE,GETEL                                                         
         BNE   HD430                                                            
         USING RCONIEL,R6                                                       
         MVC   H21+45(16),=C'EI  ADV      EST'                                  
         MVC   H22+49(12),=C'PRD      PRD'                                      
         MVC   H21+53(4),RCONIADV                                               
         MVC   H21+62(10),RCONXEST                                              
         OC    H21+62(10),MYSPACES                                              
         CLC   H21+62(10),MYSPACES                                              
         BNE   *+10                                                             
         MVC   H21+62(4),RCONIEST                                               
         MVC   H21+53(4),RCONIPRD                                               
         MVC   H22+62(4),RCONIPR2                                               
         DROP  R6                                                               
*                                                                               
HD430    MVC   H21(6),=C'AGENCY'                                                
         MVC   H22(7),=C'ADDRESS'                                               
         MVC   H21+78(10),=C'* REMARK #'                                        
         MVC   H22+91(14),DASH                                                  
         MVI   H22+78,C'*'                                                      
         SPACE 1                                                                
         MVC   H23+78(10),=C'* PRIORITY'                                        
         MVC   H24+91(14),DASH                                                  
         SPACE 1                                                                
*                                                                               
HD435    DS    0H                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         LA    R1,H24                                                           
         OC    TWAAGYPH,TWAAGYPH                                                
         BZ    HD440                                                            
         MVC   0(7,R1),=C'PHONE #'   AGENCY PHONE NUMBER                        
         MVC   8(3,R1),TWAAGYPH                                                 
         MVI   11(R1),C'-'                                                      
         MVC   12(3,R1),TWAAGYPH+3                                              
         MVI   15(R1),C'-'                                                      
         MVC   16(4,R1),TWAAGYPH+6                                              
         LA    R1,23(R1)                                                        
*                                                                               
HD440    DS    0H                                                               
         OC    TWAAFAX,TWAAFAX                                                  
         BZ    HD450                                                            
         MVC   0(5,R1),=C'FAX #'  AGENCY FAX NUMBER                             
         MVC   6(3,R1),TWAAFAX                                                  
         MVI   9(R1),C'-'                                                       
         MVC   10(3,R1),TWAAFAX+3                                               
         MVI   13(R1),C'-'                                                      
         MVC   14(4,R1),TWAAFAX+6                                               
         DROP  RF                                                               
*                                                                               
HD450    DS    0H                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         MVC   H23+47(16),=C'SALESPERSON PH #'                                  
         MVC   H23+64(L'TWASALTL),TWASALTL                                      
*                                                                               
         OC    TWASALFX,TWASALFX   SALESPERSON FAX NUMBER                       
         BZ    HD460                                                            
         MVC   H24+47(16),=C'SALESPERSON FAX#'                                  
         MVC   H24+64(L'TWASALFX),TWASALFX                                      
         B     HD470                                                            
*                                                                               
HD460    DS    0H                  -OR- OFFICE FAX NUMBER                       
         OC    TWAOFFFX,TWAOFFFX                                                
         BZ    HD470                                                            
         MVC   H24+47(12),=C'OFFICE FAX# '                                      
         MVC   H24+59(3),TWAOFFFX                                               
         MVI   H24+62,C'-'                                                      
         MVC   H24+63(3),TWAOFFFX+3                                             
         MVI   H24+66,C'-'                                                      
         MVC   H24+67(4),TWAOFFFX+6                                             
*                                                                               
HD470    DS    0H                                                               
         MVC   H25(20),=C'SPECIAL INSTRUCTIONS'                                 
         MVC   H26+22(51),DASH                                                  
         MVC   H28+22(51),DASH                                                  
         SPACE 1                                                                
         MVC   H26+78(19),=C'* PRINT SPOT PRICES'                               
         MVC   H27+105(3),DASH                                                  
         SPACE 1                                                                
         CLI   SVSWE+9,C'S'       STANDARD?                                     
         BNE   *+12                                                             
         MVI   H15+92,C'Y'                                                      
         B     HD480                                                            
         SPACE                                                                  
         CLI   SVSWE+9,C'C'       CALANDER?                                     
         BNE   *+12                                                             
         MVI   H15+106,C'Y'                                                     
         B     HD480                                                            
         SPACE                                                                  
         CLI   SVSWE+9,C'W'       WEEKLY?                                       
         BNE   HD480                                                            
         MVI   H17+92,C'Y'                                                      
         SPACE                                                                  
HD480    DS   0H                                                                
         CLI   SVSWE+12,C'Y'      MULTI-PRODS?                                  
         BNE   *+8                                                              
         MVI   H19+92,C'Y'                                                      
         SPACE                                                                  
         CLI   SVSWE+13,C'Y'      SEPERATE INVOICES?                            
         BNE   *+8                                                              
         MVI   H19+106,C'Y'                                                     
         SPACE                                                                  
         CLI   SVSWE+11,C'Y'      PRINT SPOT PRICES?                            
         BNE   *+8                                                              
         MVI   H26+106,C'Y'                                                     
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
KAMBLH   NTR1                                                                   
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF                                                       
         NI    TWAWSFLG,X'FF'-X'02'  TURN OFF FLAG FOR FIRST TIME HEADR         
         DROP  RF                                                               
         MVC   1(30,R6),=C'L  M   TIME PERIOD/       RATE'                      
         MVC   44(22,R6),=C'    DATES          ALT'                             
         MVC   82(3,R6),=C'TOT'                                                 
         LA    R6,132(R6)                                                       
         MVC   1(29,R6),=C'N  C   SELL PATTERN  LEN  SEC'                       
         MVC   32(9,R6),=C'COST/SPOT'                                           
         MVC   44(28,R6),=C'START    END  NPW  WK   DAYS'                       
         MVC   82(27,R6),=C'SPT  TOTAL COST    CLA  PLA'                        
         LA    R6,132(R6)                                                       
         MVC   0(31,R6),=C'--- --  ------------  ---  ----'                     
         MVC   32(9,R6),=C'---------'                                           
         MVC   44(36,R6),=C'------------  ---  ---  ------------'               
         MVC   82(27,R6),=C'---  ------------  ---  ---'                        
KAMBLHX  B     XIT                                                              
*                                                                               
MARBLH   NTR1                                                                   
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF                                                       
         NI    TWAWSFLG,X'FF'-X'02'  TURN OFF FLAG FOR FIRST TIME HEADR         
         DROP  RF                                                               
         MVC   0(19,R6),=C'   LINE/      DATES'                                 
         MVC   52(4,R6),=C'TIME'                                                
         MVC   95(3,R6),=C'TOT'                                                 
         LA    R6,132(R6)                                                       
         MVC   0(32,R6),=C'LN MOD CD START    END AVAIL PTY'                    
         MVC   33(29,R6),=C'LEN COST       START  END NPW'                      
         MVC   63(20,R6),=C'DAYS         FLIGHTS'                               
         MVC   95(15,R6),=C'SPT CLA SEC PLA'                                    
         LA    R6,132(R6)                                                       
         MVC   0(32,R6),=C'-- ------ ------------ ----- ---'                    
         MVC   33(29,R6),=C'--- ---------- ---------- ---'                      
         MVC   63(31,R6),=C'------------ ------------------'                    
         MVC   95(15,R6),=C'--- --- --- ---'                                    
MARBLHX  B     XIT                                                              
*                                                                               
*        CONSTANTS, LITERAL POOL, ETC.                                          
         SPACE 1                                                                
DASH     DC    51C'-'                                                           
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*          HEADLINE SPECS FOR CONTRACTS                                         
         SPACE 1                                                                
HEDSPECS DS    0D                                                               
         SPACE 1                                                                
         SPROG 1                ***KAMAN - PAGE 1 ONLY***                       
         PSPEC H4,1,C'ADV. #'                                                   
         PSPEC H5,9,C'----------'                                               
         PSPEC H4,23,C'NAME'                                                    
         PSPEC H5,29,C'--------------------'                                    
         PSPEC H4,53,C'PRODUCT CODES'                                           
         PSPEC H5,68,C'----------   ----------'                                 
         PSPEC H6,1,C'AGY. #'                                                   
         PSPEC H7,9,C'----------'                                               
         PSPEC H6,23,C'NAME'                                                    
         PSPEC H7,29,C'---------------------------------'                       
         PSPEC H6,65,C'REP'                                                     
         PSPEC H7,70,C'--------'                                                
         PSPEC H6,81,C'OFFICE'                                                  
         PSPEC H7,89,C'--------------------'                                    
         PSPEC H8,1,C'START DATE'                                               
         PSPEC H9,14,C'--------'                                                
         PSPEC H8,25,C'END DATE'                                                
         PSPEC H9,36,C'--------'                                                
         PSPEC H8,53,C'SLS #'                                                   
         PSPEC H9,61,C'--------'                                                
         PSPEC H8,74,C'NAME'                                                    
         PSPEC H9,82,C'--------------------'                                    
         PSPEC H10,1,C'CLASS (L/N)'                                             
         PSPEC H11,14,C'--------'                                               
         PSPEC H10,25,C'SUB CLASS'                                              
         PSPEC H11,36,C'--------'                                               
         PSPEC H10,53,C'BUYER NAME'                                             
         PSPEC H11,66,C'--------------------'                                   
         PSPEC H12,1,C'SPONSOR/PRODUCT'                                         
         PSPEC H13,19,C'--------------------'                                   
         PSPEC H12,53,C'TRAFFIC #'                                              
         PSPEC H13,66,C'----------'                                             
         PSPEC H12,82,C'ACE #'                                                  
         PSPEC H13,90,C'--------'                                               
         PSPEC H14,1,C'AGENCY'                                                  
         PSPEC H14,53,C'SALES ASSISTANT'                                        
         SPACE 1                                                                
         SPROG 1,2                ***KAMAN - ALL PAGES***                       
         PSPEC H1,46,C'ORDER WORKSHEET'                                         
         PSPEC H1,103,PAGE                                                      
         SPACE 1                                                                
         SPROG 5                  ***KAMAN - STORED COMMENTS***                 
         PSPEC H1,46,C'ORDER WORKSHEET'                                         
         PSPEC H1,103,PAGE                                                      
         EJECT                                                                  
         SPROG 3          ***MARKETRON - PAGE 1 ONLY***                         
         PSPEC H4,1,C'CONTRACT #'                                               
         PSPEC H5,13,C'----------'                                              
         PSPEC H6,1,C'CASH         TRADE         NON-CM'                        
         PSPEC H7,7,C'-----         -----          -----'                       
         PSPEC H8,1,C'TOTAL COST'                                               
         PSPEC H9,13,C'----------'                                              
         PSPEC H4,43,C'*      CIRCLE ONE     *'                                 
         PSPEC H5,43,C'*      ----------     *'                                 
         PSPEC H6,43,C'* LOCAL    NATIONAL   *'                                 
         PSPEC H7,43,C'*                     *'                                 
         PSPEC H8,43,C'* LOPOL    NAPOL      *'                                 
         PSPEC H4,69,C'* CIRCLE ONE  *'                                         
         PSPEC H5,69,C'* ----------  *'                                         
         PSPEC H6,69,C'* NEW  DELETE *'                                         
         PSPEC H7,69,C'*             *'                                         
         PSPEC H8,69,C'* ADD  CHANGE *'                                         
         PSPEC H6,89,C'START DATE'                                              
         PSPEC H7,101,C'--------'                                               
         PSPEC H8,89,C'END DATE'                                                
         PSPEC H9,101,C'--------'                                               
         SPACE 1                                                                
         PSPEC H11,1,C'ADVERTISER #'                                            
         PSPEC H12,15,C'----------'                                             
         PSPEC H11,27,C'ADVERTISER NAME'                                        
         PSPEC H12,45,C'--------------------'                                   
         PSPEC H13,1,C'AGENCY #'                                                
         PSPEC H14,15,C'----------'                                             
         PSPEC H13,27,C'AGENCY NAME'                                            
         PSPEC H14,45,C'---------------------------------'                      
         SPACE 1                                                                
         PSPEC H11,79,C'* DIRECT'                                               
         PSPEC H12,92,C'---'                                                    
         PSPEC H12,79,C'*'                                                      
         PSPEC H11,99,C'REMIT'                                                  
         PSPEC H12,106,C'---'                                                   
         PSPEC H13,79,C'* BILLING-'                                             
         PSPEC H14,79,C'*'                                                      
         SPACE 1                                                                
         SPROG 3,4             ***MARKETRON - ALL PAGES***                      
         PSPEC H1,46,C'ORDER WORKSHEET'                                         
         PSPEC H1,103,PAGE                                                      
         SPACE 1                                                                
         SPROG 6               ***MARKETRON - STORED COMMENTS PAGES***          
         PSPEC H1,46,C'ORDER WORKSHEET'                                         
         PSPEC H1,103,PAGE                                                      
         DC    X'00'                                                            
         EJECT                                                                  
       ++INCLUDE RECNTWR2K                                                      
       ++INCLUDE RECNTFMTD                                                      
       ++INCLUDE REGENPBYD                                                      
*    FORMAT FOR KAMAN WORKSHEET BUYLINES                                        
         SPACE 1                                                                
PKAMD    DSECT                                                                  
PK       DS    0CL110                                                           
PKLIN    DS    CL3                 DDS LINE NUMBER                              
         DS    CL1                                                              
PKCHG    DS    CL2                 CHANGE CODE                                  
         DS    CL2                                                              
PKTIM    DS    CL10                TIME                                         
         DS    CL4                                                              
PKLEN    DS    CL3                 LENGTH                                       
         DS    CL2                                                              
PKSEC    DS    CL3                 SECTION                                      
         DS    CL2                                                              
PKRAT    DS    CL10                RATE                                         
         DS    CL2                                                              
PKDAT    DS    CL12                DATE                                         
         DS    CL2                                                              
PKNPW    DS    CL3                 NUMBER PER WEEK                              
         DS    CL7                                                              
PKDAY    DS    CL12                DAY                                          
         DS    CL2                                                              
PKTSPOT  DS    CL3                 TOTAL SPOTS                                  
         DS    CL2                                                              
PKTCOS   DS    CL12                TOTAL COST                                   
         DS    CL2                                                              
PKCLS    DS    CL3                 CLASS                                        
         DS    CL2                                                              
PKPLN    DS    CL3                 PLAN                                         
         DS    CL1                                                              
         EJECT                                                                  
*    FORMAT FOR MARKETRON WORKSHEET BUYLINES                                    
         SPACE 1                                                                
PMARD    DSECT                                                                  
PM       DS    0CL110                                                           
         DS    CL3                                                              
PMLIN    DS    CL3                 DDS LINE NUMBER                              
         DS    CL1                                                              
PMCHG    DS    CL2                 CHANGE CODE                                  
         DS    CL1                                                              
PMDAT    DS    CL12                DATE                                         
         DS    CL1                                                              
PMAVA    DS    CL5                 AVAIL                                        
         DS    CL1                                                              
PMPTY    DS    CL3                 PRIORTY                                      
         DS    CL2                                                              
PMLEN    DS    CL3                 LENGTH                                       
         DS    CL1                                                              
PMRAT    DS    CL10                RATE                                         
         DS    CL1                                                              
PMTIM    DS    CL10                TIME                                         
         DS    CL1                                                              
PMNPW    DS    CL3                 NUMBER PER WEEK                              
         DS    CL1                                                              
PMDAY    DS    CL12                DAY                                          
         DS    CL20                                                             
PMTSPOT  DS    CL3                 TOTAL SPOTS                                  
         DS    CL1                                                              
PMCLS    DS    CL3                 CLASS                                        
         DS    CL1                                                              
PMSEC    DS    CL3                 SECTION                                      
         DS    CL1                                                              
PMPLN    DS    CL3                 PLAN                                         
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'041RECNT68   02/02/05'                                      
         END                                                                    
