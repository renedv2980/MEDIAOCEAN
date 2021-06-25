*          DATA SET RECNT66    AT LEVEL 067 AS OF 05/17/05                      
*PHASE T80266A,+0                                                               
         TITLE 'T80266 - BIAS/COLUMBINE FORMATS'                                
*                                                                               
***********************************************************************         
*                                                                     *         
*        RECNT66 (T80266) --- BIAS/COLUMBINE FORMATS                  *         
*                                                                     *         
* ------------------------------------------------------------------- *         
* REFER TO RECNTHIST FOR PAST HISTORY                                 *         
*                                                                     *         
*                                                                     *         
*                                                                     *         
* 17MAY05 BU  WIDE ORBIT / COLUMBINE FORMAT GOES THRU HERE            *         
* 02SEP03 BU  OSI        GOES THRU HERE                               *         
* 14FEB03 BU  VCI BDE (TRAFFIC CODE 'T') GOES TO BIAS NOW             *         
* 28OCT02 SKU FORMAT L SUPPORT                                        *         
* 10JUL02 BU  VCI BDE    GOES THRU HERE                               *         
* 23JAN02 RHV WIDE ORBIT GOES THRU HERE                               *         
* 08JAN01 RHV SPORTS BUYS                                             *         
* 09MAY00 SKU MULTI-MAKEGOOD PRINT BUG FIX                            *         
* 06JUL98 SKU ALLOW RADIO TO HAVE OTHER FORMATS                       *         
* 24JUN98 SKU ADD FORMAT O FOR PAXON                                  *         
* 22DEC97 JRD CARE OF AGENCIES                                        *         
* 15OCT96 RHV PRINT K ORD CMT AFTER BUYLINES                          *         
* 08OCT96 SKU LOW POWER STATION                                       *         
* 07JUN96 SKU ADD EOP CODES                                           *         
* 06MAY96 RHV CONTYPE RECORD CONTROLLED WORKSHEET FORMATTING          *         
* 09APR96 RHV SUPPORT 34 BYTE AGY ADDRESS FIELDS FOR ALL USERS        *         
* 02APR96 RHV NEW DISPLAY OF LAST VER# & MOD#                         *         
* 06FEB96 RHV SUPPRESS PRINTING OF EI FIELDS IF EMPTY                 *         
* 01MAR96 RHV SUPPORT PETRY 34 BYTE AGY ADDR FIELDS                   *         
* 26FEB96 SKU KATZ CONVERTED ORDERS PRINT AGY ADDRESS IN X'71,72,73'  *         
* 06JAN96 SKU PROFILE 24 TO PRINT PTP OVER SALESPERSON FOR TYPE D     *         
*             PROFILE 20 "   "    "    "     "         "   TYPE N/X   *         
* 30NOV95 SKU 2K CONTRACT SUPPORT                                     *         
*                                                                     *         
***********************************************************************         
*                                                                               
T80266   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T80266,R9                                                      
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
         CLI   FORMAT,C'B'         BIAS FORMAT                                  
         BE    BIAS                                                             
         CLI   FORMAT,C'C'         COLUMBINE FORMAT                             
         BE    COLFMT                                                           
         CLI   FORMAT,C'E'         WIDE ORBIT / COLUMBINE FORMAT                
         BE    COLFMT                                                           
         CLI   FORMAT,C'O'         WIDE ORBIT                                   
         BE    BIAS                                                             
         CLI   FORMAT,C'L'         WIDE ORBIT                                   
         BE    BIAS                                                             
         CLI   FORMAT,C'I'         OSI                                          
         BE    BIAS                                                             
         CLI   FORMAT,C'V'         VCI TRAFFIC/COLUMBINE FORMAT                 
         BE    COLFMT                                                           
         CLI   FORMAT,C'T'         VCI/BDE TRAFFIC/BIAS FORMAT                  
         BE    BIAS                                                             
         DC    H'0'                                                             
         EJECT                                                                  
*           ROUTINE TO FORMAT BUY LINE TO BIAS SPECIFICATIONS                   
         SPACE 1                                                                
BIAS     DS    0H                                                               
*                                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF                                                       
         TM    TWAWSFLG,X'40'      K ORD COMMENT PRINTED YET?                   
         BO    BIA1G               YES - SKIP                                   
         OI    TWAWSFLG,X'40'      NO - IT IS NOW                               
*                                                                               
         L     R4,ASVRCOC          REP CONTRACT ORDER COMMENT                   
         LA    R5,10               FOR BCT                                      
         SPACE 1                                                                
         OC    0(60,R4),0(R4)      IS THERE A COMMENT                           
         BZ    BIA1D                                                            
         MVC   P+6(19),=C'*REP ORDER COMMENT*'                                  
*                                                                               
BIA1B    OC    0(60,R4),0(R4)      IS THERE A COMMENT                           
         BZ    BIA1C                                                            
*                                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         OI    TWAWSFLG,X'20'      SINGLE SPACE THIS SECTION                    
         DROP  RF                                                               
         GOTO1 PSTCMT,DMCB,(3,0(R4))  PRINT ORD CMTS                            
*                                                                               
         LA    R4,60(R4)           POINT TO NEXT PART OF COMMENT                
         BCT   R5,BIA1B                                                         
*                                                                               
BIA1C    BAS   RE,GOSPOOL                                                       
         SPACE 1                                                                
*                                                                               
BIA1D    LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF                                                       
         L     R4,ASVSCOC          STA CONTRACT ORDER COMMENT                   
         LA    R5,10               FOR BCT                                      
         OC    0(60,R4),0(R4)      IS THERE A COMMENT                           
         BZ    BIA1G                                                            
         MVC   P+6(23),=C'*STATION ORDER COMMENT*'                              
*                                                                               
BIA1E    OC    0(60,R4),0(R4)      IS THERE A COMMENT                           
         BZ    BIA1F                                                            
*                                                                               
         OI    TWAWSFLG,X'20'      SINGLE SPACE THIS SECTION                    
         DROP  RF                                                               
         MVC   P+30(60),0(R4)                                                   
         OC    P+30(60),SPACES                                                  
         BAS   RE,GOSPOOL                                                       
*                                                                               
         LA    R4,60(R4)           POINT TO NEXT PART OF COMMENT                
         BCT   R5,BIA1E                                                         
*                                                                               
BIA1F    BAS   RE,GOSPOOL                                                       
*                                                                               
BIA1G    DS    0H                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF                                                       
         NI    TWAWSFLG,X'FF'-X'20'                                             
         L     R4,AWORK4           POINT TO OUTPUT                              
         USING PD,R4                                                            
         LA    R3,P                POINT TO PRINT LINE                          
         USING PBIASD,R3                                                        
         ZIC   R5,BUYLIN                                                        
*                                                                               
         CLI   RCSUBPRG,1          DON'T SET ALLOWLIN FOR PAGE 1                
         BE    B4                                                               
*                                                                               
         LR    R2,R5               SET UP ALLOWLIN                              
         CLI   SVSTAOP2,C'Y'       TRIPLE SPACING                               
         BE    B2                                                               
         SLL   R2,1                DOUBLE SPACING                               
         B     *+8                                                              
B2       MH    R2,=H'3'                                                         
         TM    TWAWSFLG,X'02'      PRINT BUYLIN HEADER ALSO?                    
         BZ    *+8                 NO                                           
         AH    R2,=H'4'            YES - ALLOW ANOTHER 4 LINES                  
         STC   R2,ALLOWLIN                                                      
         DROP  RF                                                               
*                                                                               
B4       DS    0H                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF                                                       
         TM    TWAWSFLG,X'02'      DO WE NEED TO PRINT BUYLINE HEADER?          
         BZ    B5                  NO - SKIP                                    
         CLC   CONACT,=C'CONF'     FOR CONFIRM, NEVER PRINT HEADER              
         BE    B5                                                               
         DROP  RF                                                               
         LA    R6,P                PUT HEADER ON P                              
         BAS   RE,BIABLH           GENERATE BUYLINE HEADER                      
         BAS   RE,GOSPOOL          AND PRINT IT                                 
*                                                                               
B5       CLC   =C'WEEKLY RATE',PDAT+1                                           
         BNE   *+14                                                             
         MVC   PBDAY+2(66),PDAY+5  'WEEKLY RATE FOR PLAN XXX IS'                
         B     B50                                                              
*                                                                               
         CLC   =C'MAKE',PDAY+6     MAKEGOOD REFERENCE?                          
         BNE   B5D                 YES                                          
         MVC   18(100,R3),PDAY+6                                                
         B     B50                                                              
*                                                                               
B5D      DS    0H                                                               
         OC    PDAY,PDAY           IF NOTHING THERE, IT'S MORE DATES            
         BZ    B6                                                               
         SPACE 1                                                                
         CLI   PDAY,X'FF'         SPORTS BUY DESCRIPTIVE?                       
         BNE   B5E                                                              
         MVC   PBDAY(L'PSDESC),PSDESC   SPORTS DESCRIPTIVE                      
         B     B50                                                              
B5E      DS    0H                                                               
         OC    PDAY(5),PDAY       IF NOTHING HERE, IT'S A COMMENT               
         BNZ   B6                                                               
         CLC   =C'ROC',PDAY+6     REP ORDER COMMENT                             
         BNE   B5L                                                              
         MVC   PB+3(13),=C'*REP ORD CMT*'                                       
         B     B5P                                                              
B5L      CLC   =C'SOC',PDAY+6      STATION ORDER COMMENT                        
         BNE   B5S                                                              
         MVC   PB+3(13),=C'*STA ORD CMT*'                                       
B5P      MVC   18(60,R3),PDAY+10                                                
         B     B50                                                              
         SPACE 1                                                                
*    IT'S A BUY COMMENT, BUT THEY DON'T WANT TO PRINT THE LABEL                 
B5S      DS    0H                                                               
         CLC   =C'P=',PDAY+6                                                    
         BNE   B5T                                                              
         MVC   18(12,R3),=C'PROGRAMMING='                                       
         MVC   30(64,R3),PDAY+8                                                 
         B     B50                                                              
*                                                                               
B5T      MVC   PBDAY+6(66),PDAY+6                                               
         B     B50                                                              
         SPACE 1                                                                
B6       MVC   PBCHG,PCHG          REVISION CODE                                
         MVC   PBLIN,PLIN          LINE NUMBER                                  
*                                                                               
         L     RF,AIO2             BUYREC                                       
         USING RBUYREC,RF                                                       
         TM    RBUYSFG,X'40'       SPORTS BUY?                                  
         BZ    B6D                 NO                                           
         DROP  RF                                                               
         MVC   PBDAY(L'PSTYPE),PSTYPE   SPORTS TYPE                             
         B     B9                                                               
B6D      DS    0H                                                               
         LA    R6,DAYTAB           CONVERT SINGLE DAYS TO BIAS FORMAT           
B6E      CLC   0(3,R6),PDAY                                                     
         BE    B6G                                                              
         CLI   0(R6),X'FF'                                                      
         BE    B6I                 NOT A SINGLE DAY                             
         LA    R6,5(R6)                                                         
         B     B6E                                                              
B6G      MVC   PBDAY(2),3(R6)                                                   
         B     B6L                                                              
B6I      MVC   PBDAY,PDAY          SO USE EXPRESSION AS GIVEN                   
         SPACE 1                                                                
*  CONVERT TO MILITARY TIME                                                     
*         FIGURE OUT THE LENGTH OF THE TIME EXPRESSION FROM REGENPBY            
B6L      OC    PTIM,PTIM                                                        
         BZ    B8K                                                              
         LA    R6,11               11 IS THE MAXIMUM LENGTH                     
         LA    R1,PTIM+10          START AT END AND WORK BACKWARDS              
B7       CLI   0(R1),C'A'          AM                                           
         BE    B8                                                               
         CLI   0(R1),C'P'          PM                                           
         BE    B8                                                               
         CLI   0(R1),C'N'          NOON                                         
         BE    B8                                                               
         CLI   0(R1),C'M'          MIDNIGHT                                     
         BE    B8                                                               
         BCTR  R1,R0                                                            
         BCT   R6,B7                                                            
         DC    H'0'                SHOULD NEVER GET TO HERE                     
         SPACE 1                                                                
B8       GOTO1 TIMVAL,DMCB,((R6),PTIM),FULL                                     
         CLI   DMCB,X'FF'                                                       
         BNE   *+6                                                              
         DC    H'0'                                                             
         SPACE 1                                                                
         EDIT  (2,FULL),(4,PBTIM)                                               
         EDIT  (2,FULL+2),(4,PBTIM+6)                                           
         SPACE 1                                                                
         LA    R1,21(R6,R4)                                                     
         CLC   0(2,R1),=C'CC'      IF END TIME IS TO CONCLUSION                 
         BNE   *+10                                                             
         MVC   PBTIM+6(2),=C'CC'   PRINT CC                                     
         SPACE 1                                                                
         LA    R1,PBTIM            SHOW 1201A-1259A AS 2401-2459                
         LA    R6,2                                                             
B8C      CLC   0(2,R1),SPACES                                                   
         BNE   B8F                                                              
         MVC   0(2,R1),=C'24'                                                   
         CLC   2(1,R1),SPACES                                                   
         BNE   B8F                                                              
         MVI   2(R1),C'0'                                                       
         CLC   3(1,R1),SPACES      IF TIME 12-1A SHOW 12 AS 2400                
         BNE   B8F                                                              
         MVI   3(R1),C'0'                                                       
B8F      LA    R1,6(R1)            POINT TO END TIME                            
         CLC   0(4,R1),SPACES      IF NO END TIME                               
         BE    *+8                 DON'T BOTHER LOOPING BACK                    
         BCT   R6,B8C                                                           
         SPACE 1                                                                
B8K      MVC   PBLEN,PLEN          LENGTH                                       
*                                                                               
B9       DS    0H                                                               
         MVC   PBRAT,PRAT          RATE                                         
         MVC   PBTSPOT,PTOT        TOTAL SPOTS                                  
*                                                                               
         GOTO1 DATVAL,DMCB,(1,PDAT),WORK      DATES                             
         OC    DMCB(4),DMCB                                                     
         BZ    B9A                                                              
         SPACE 1                                                                
         MVC   PBDAT(2),WORK+2                                                  
         MVI   PBDAT+2,C'/'                                                     
         MVC   PBDAT+3(2),WORK+4                                                
         SPACE 1                                                                
B9A      GOTO1 DATVAL,DMCB,(1,PDAT+6),WORK                                      
         OC    DMCB(4),DMCB                                                     
         BZ    B9B                                                              
         SPACE 1                                                                
         MVC   PBDAT+6(2),WORK+2                                                
         MVI   PBDAT+8,C'/'                                                     
         MVC   PBDAT+9(2),WORK+4                                                
         SPACE 1                                                                
B9B      MVI   PBDAT+11,0          CLEAR IN CASE THERE IS NONE                  
         CLI   PDAT+11,C'A'                                                     
         BNE   PB20                                                             
         MVI   PBDAT+11,C'A'       ALTERNATE WEEKS INDICATOR                    
         MVC   PBDAT-1(1),PDAT-1        * FOR ALTERNATE WEEKS                   
         SPACE 1                                                                
PB20     DS    0H                                                               
         L     RF,AIO2             BUYREC                                       
         USING RBUYREC,RF                                                       
         TM    RBUYSFG,X'40'       SPORTS BUY?                                  
         BO    B50                 NO                                           
         DROP  RF                                                               
         MVC   PBCLS,PCLS          CLASS                                        
         MVC   PBSEC,PSEC          SECTION                                      
         MVC   PBPLN,PPLN          PLAN                                         
         MVC   PBNPW,PNPW          NUMBER PER WEEK                              
         SPACE 1                                                                
B50      OC    P,SPACES                                                         
         BAS   RE,GOSPOOL                                                       
         MVI   RCSUBPRG,2          FOR CONTINUATION PAGES                       
*                                  ONLY INCLUDE A FEW HEADLINES                 
         LA    R4,L'PRTLN(R4)                                                   
         BCT   R5,B5                                                            
         SPACE 1                                                                
         B     EXXMOD                                                           
         EJECT                                                                  
*  ROUTINE TO FORMAT BUY LINE TO COLUMBINE WORKSHEET SPECIFICATIONS             
         SPACE 2                                                                
COLFMT   DS    0H                                                               
*                                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF                                                       
         TM    TWAWSFLG,X'40'      K ORD COMMENT PRINTED YET?                   
         BO    COL1G               YES - SKIP                                   
         OI    TWAWSFLG,X'40'      NO - IT IS NOW                               
*                                                                               
         L     R4,ASVRCOC          REP CONTRACT ORDER COMMENT                   
         LA    R5,10               FOR BCT                                      
         SPACE 1                                                                
         OC    0(60,R4),0(R4)      IS THERE A COMMENT                           
         BZ    COL1D                                                            
         MVC   P+6(19),=C'*REP ORDER COMMENT*'                                  
*                                                                               
COL1B    OC    0(60,R4),0(R4)      IS THERE A COMMENT                           
         BZ    COL1C                                                            
*                                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         OI    TWAWSFLG,X'20'      SINGLE SPACE THIS SECTION                    
         DROP  RF                                                               
         GOTO1 PSTCMT,DMCB,(3,0(R4))  PRINT ORD CMTS                            
*                                                                               
         LA    R4,60(R4)           POINT TO NEXT PART OF COMMENT                
         BCT   R5,COL1B                                                         
*                                                                               
COL1C    BAS   RE,GOSPOOL                                                       
         SPACE 1                                                                
*                                                                               
COL1D    LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF                                                       
         L     R4,ASVSCOC          STA CONTRACT ORDER COMMENT                   
         LA    R5,10               FOR BCT                                      
         OC    0(60,R4),0(R4)      IS THERE A COMMENT                           
         BZ    COL1G                                                            
         MVC   P+6(23),=C'*STATION ORDER COMMENT*'                              
*                                                                               
COL1E    OC    0(60,R4),0(R4)      IS THERE A COMMENT                           
         BZ    COL1F                                                            
*                                                                               
         OI    TWAWSFLG,X'20'      SINGLE SPACE THIS SECTION                    
         DROP  RF                                                               
         MVC   P+30(60),0(R4)                                                   
         OC    P+30(60),SPACES                                                  
         BAS   RE,GOSPOOL                                                       
*                                                                               
         LA    R4,60(R4)           POINT TO NEXT PART OF COMMENT                
         BCT   R5,COL1E                                                         
*                                                                               
COL1F    BAS   RE,GOSPOOL                                                       
*                                                                               
COL1G    DS    0H                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF                                                       
         NI    TWAWSFLG,X'FF'-X'20'                                             
         L     R4,AWORK4           POINT TO OUTPUT                              
         USING PD,R4                                                            
         LA    R3,P                POINT TO PRINT LINE                          
         USING PCOLD,R3                                                         
         ZIC   R5,BUYLIN                                                        
         LR    R2,R5               SET UP ALLOWLIN                              
         CLI   SVSTAOP2,C'Y'       TRIPLE SPACING                               
         BE    C2                                                               
         SLL   R2,1                DOUBLE SPACING                               
         B     *+8                                                              
C2       MH    R2,=H'3'                                                         
         TM    TWAWSFLG,X'02'      PRINT BUYLIN HEADER ALSO?                    
         BZ    *+8                 NO                                           
         AH    R2,=H'4'            YES - ALLOW ANOTHER 4 LINES                  
         STC   R2,ALLOWLIN                                                      
         DROP  RF                                                               
*                                                                               
C4       DS    0H                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF                                                       
         TM    TWAWSFLG,X'02'      DO WE NEED TO PRINT BUYLINE HEADER?          
         BZ    C5                  NO - SKIP                                    
         CLC   CONACT,=C'CONF'     FOR CONFIRM, NEVER PRINT HEADER              
         BE    C5                                                               
         DROP  RF                                                               
         LA    R6,P                PUT HEADER ON P                              
         BAS   RE,COLBLH           GENERATE BUYLINE HEADER                      
         BAS   RE,GOSPOOL          AND PRINT IT                                 
*                                                                               
C5       CLC   =C'ALT',PDAT        ALTERNATE WEEKS LINE                         
         BNE   C5E                                                              
         MVC   PCDAT-1(13),=C'ALTERNATE WKS'                                    
         MVC   PCDAY,PDAY          DAY                                          
         MVC   PCTIM,PTIM          TIME                                         
         OC    PDAY,PDAY           ONLY PRINT SLASHES IF                        
         BZ    *+10                ADDITIONAL DAY/TIME INFO                     
         MVC   PCNPW+5(11),=C'/ / / / / /'  FOR BRDCST PATTERN                  
         B     C50                                                              
         SPACE 1                                                                
C5E      CLC   =C'WEEKLY RATE',PDAT+1                                           
         BNE   *+14                                                             
         MVC   PCRAT-28(39),PDAT+1    'WEEKLY RATE FOR PLAN XXX IS'             
         B     C50                                                              
         SPACE 1                                                                
         OC    PDAY,PDAY           IF NOTHING THERE, IT'S MORE DATES            
         BZ    C6                  USE EXISTING CODE TO FILL IN                 
*                                                                               
C5G      DS    0H                                                               
         CLI   PDAY,X'FF'         SPORTS BUY DESCRIPTIVE?                       
         BNE   C5H                                                              
         MVC   PCDAY(L'PSDESC),PSDESC   SPORTS DESCRIPTIVE                      
         B     C50                                                              
C5H      DS    0H                                                               
         OC    PDAY(5),PDAY       IF NOTHING HERE, IT'S A COMMENT               
         BNZ   C6                                                               
         CLC   =C'ROC',PDAT+6      REP ORDER COMMENT                            
         BNE   C5L                                                              
         MVC   PC+6(13),=C'*REP ORD CMT*'                                       
         B     C5P                                                              
C5L      CLC   =C'SOC',PDAT+6      STATION ORDER COMMENT                        
         BNE   C5S                                                              
         MVC   PC+6(13),=C'*STA ORD CMT*'                                       
C5P      MVC   21(60,R3),PDAY+4                                                 
         B     C50                                                              
         SPACE 1                                                                
*   IT'S A BUY COMMENT, BUT THEY DON'T WANT TO SEE THE LABEL                    
C5S      DS    0H                                                               
         CLC   =C'P=',PDAY+6                                                    
         BNE   C5T                                                              
         MVC   21(12,R3),=C'PROGRAMMING='                                       
         MVC   33(64,R3),PDAY+8                                                 
         B     C50                                                              
*                                                                               
C5T      MVC   21(66,R3),PDAY+6                                                 
         B     C50                                                              
         SPACE 1                                                                
C6       MVC   PCCHG,PCHG          REVISION CODE                                
         MVC   PCLIN,PLIN          LINE NUMBER                                  
*                                                                               
         L     RF,AIO2             BUYREC                                       
         USING RBUYREC,RF                                                       
         TM    RBUYSFG,X'40'       SPORTS BUY?                                  
         BO    C7                  NO                                           
         DROP  RF                                                               
*                                                                               
         MVC   PCDAY,PDAY          DAY                                          
         MVC   PCTIM,PTIM          TIME                                         
         MVC   PCSEC,PSEC          SECTION                                      
         MVC   PCCLS,PCLS          CLASS                                        
         MVC   PCPLN,PPLN          PLAN                                         
         MVC   PCLEN,PLEN          LEN                                          
*                                                                               
C7       DS    0H                                                               
         CLI   PDAT+11,C'A'        ALTERNATE WEEKS?                             
         BNE   *+14                                                             
         MVC   PCDAT-1(1),PDAT-1   * AND A ARE INDICTORS OF                     
         MVI   PCDAT+11,C'A'       ALTERNATE WEEKS                              
         SPACE 1                                                                
         GOTO1 DATVAL,DMCB,(1,PDAT),WORK    DATES                               
         OC    DMCB(4),DMCB                                                     
         BZ    C10                                                              
         MVC   PCDAT(2),WORK+2                                                  
         MVI   PCDAT+2,C'/'                                                     
         MVC   PCDAT+3(2),WORK+4                                                
         SPACE 1                                                                
C10      GOTO1 DATVAL,DMCB,(1,PDAT+6),WORK                                      
         OC    DMCB(4),DMCB                                                     
         BZ    C15                                                              
         MVI   PCDAT+5,C'-'                                                     
         MVC   PCDAT+6(2),WORK+2                                                
         MVI   PCDAT+8,C'/'                                                     
         MVC   PCDAT+9(2),WORK+4                                                
         SPACE 1                                                                
C15      MVC   PCNPW,PNPW          NUMBER PER WEEK                              
         MVC   PCRAT,PRAT          RATE                                         
         MVC   PCTSPOT,PTOT        TOTAL SPOTS                                  
         SPACE 1                                                                
         MVC   PCNPW+5(11),=C'/ / / / / /'  FOR BRDCST PATTERN                  
         SPACE 1                                                                
C40      B     C50                                                              
         SPACE 1                                                                
C50      BAS   RE,GOSPOOL                                                       
         MVI   RCSUBPRG,6          FOR CONTINUATION PAGES                       
*                                  ONLY INCLUDE A FEW HEADLINES                 
         LA    R4,L'PRTLN(R4)                                                   
         LTR   R5,R5                                                            
         BZ    EXXMOD                                                           
         BCT   R5,C5                                                            
         SPACE 1                                                                
         B     EXXMOD                                                           
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
         MVI   SPACING,1           NO DOUBLE SPACE BTWN HEADS                   
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
         CLI   FORMAT,C'B'         BIAS FORMAT                                  
         BE    HD30                                                             
         CLI   FORMAT,C'C'         COLUMBINE FORMAT                             
         BE    HD200                                                            
         CLI   FORMAT,C'E'         WIDE ORBIT / COLUMBINE FORMAT                
         BE    HD200                                                            
         CLI   FORMAT,C'L'         WIDE ORBIT                                   
         BE    HD30                                                             
         CLI   FORMAT,C'O'         WIDE ORBIT                                   
         BE    HD30                                                             
         CLI   FORMAT,C'I'         OSI                                          
         BE    HD30                                                             
         CLI   FORMAT,C'V'         VCI TRAFFIC/COLUMBINE FORMAT                 
         BE    HD200                                                            
         CLI   FORMAT,C'T'         VCI/BDE TRAFFIC/BIAS FORMAT                  
         BE    HD30                                                             
         DC    H'0'                                                             
         SPACE 1                                                                
HD30     DS    0H                                                               
         CLI   RCSUBPRG,1                                                       
         BNE   *+8                                                              
         MVI   RCSUBPRG,2          FOR CONTINUATION PAGES                       
*                                  ONLY INCLUDE A FEW HEADLINES                 
         CLC   =C'MGS',CONACT                                                   
         BNE   HD35                                                             
         MVC   H1+38(24),=C'MAKEGOOD OFFER WORKSHEET'                           
*                                                                               
HD35     DS    0H                                                               
         MVC   H1+5(8),CONCNUM                                                  
         CLC   CONACT,=C'LAST'     FOR ACTION LAST                              
         BNE   *+14                IDENTIFY AS A DUPLICATE COPY                 
         MVC   H1+78(9),=C'DUPLICATE'                                           
         B     HD40                                                             
*                                                                               
         CLC   =C'RSND',CONACT     FOR ACTION RESEND                            
         BNE   *+14                IDENTIFY AS A DUPLICATE COPY                 
         MVC   H1+81(6),=C'RESENT'                                              
         B     HD40                                                             
*                                                                               
         MVC   H1+79(8),=C'FROM REP'                                            
         CLI   TWAACCS,C'$'        STATION                                      
         BNE   *+10                                                             
         MVC   H1+75(12),=C'FROM STATION'                                       
HD40     GOTO1 DATCON,DMCB,(5,0),(8,H1+90)                                      
         MVC   H1+100(2),=C'AT'                                                 
         UNPK  DUB,SENDTIME        TIME                                         
         MVC   H1+103(2),DUB+1                                                  
         MVI   H1+105,C'.'                                                      
         MVC   H1+106(2),DUB+3                                                  
         SPACE 1                                                                
*                                                                               
*   CALL ROUTINE TO DISPLAY MOD & VERSION NUMBER                                
         GOTO1 VGENDMV,DMCB,RCONREC,H3+8,GENOLD                                 
*                                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF                                                       
         CLC   PAGE,=H'1'          FIRST PAGE?                                  
         BE    HD80                                                             
         MVC   H2(36),H30+8        ON CONTINUATION PAGES, PUT                   
*                                  VERSION & CONF. STATUS ON H2                 
         NI    SPOOLIND,X'7F'      TURN OFF 'SKIP LINE' INDICATOR               
         TM    TWAWSFLG,X'80'           SHOULD WE PRINT BUYLINE HEADER          
         BZ    XIT                                                              
         MVI   H4,0                                                             
         LA    R6,H5                                                            
         BAS   RE,BIABLH                                                        
         B     XIT                                                              
         SPACE 1                                                                
HD80     MVC   H5+8(33),H30+8    ON PAGE 1, PUT VERS/CONF STATUS ON H5          
         MVI   MYHEDSW,C'Y'        MORE THAN 14 HEADLINES                       
         MVC   SVPRNT,P            SAVE PRINT LINE                              
         MVC   P,SPACES            BLANK OUT PRINT LINE                         
         MVI   XTRHED,12           MAXIMUM NUMBER OF EXTRA HEADLINES            
         OI    SPOOLIND,X'80'  DON'T SKIP LINES AFTER 1ST SET OF HEADS          
         SPACE 1                                                                
         ZIC   R0,XTRHED           CLEAR OUT EXTRA HEADLINES                    
         LA    R1,H15                                                           
HD90     MVC   0(132,R1),SPACES                                                 
         LA    R1,132(R1)                                                       
         BCT   R0,HD90                                                          
         SPACE 1                                                                
         OC    SVTRAF,SVTRAF       TRAFFIC NUMBER                               
         BZ    *+10                                                             
         MVC   H3+61(10),SVTRAF                                                 
         MVC   H3+80(5),ACTSTAT    STATION                                      
         CLI   ACTSTAT+4,C' '                                                   
         BNE   *+10                                                             
         MVC   H3+84(3),=C'-TV'                                                 
         CLI   ACTSTAT+4,C'L'                                                   
         BNE   *+10                                                             
         MVC   H3+84(3),=C'-L '                                                 
         SPACE 1                                                                
         MVC   H5+69(10),SVAGYC    AGENCY CODE                                  
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF                                                       
         TM    TWAFMTFL,X'08'      CARE OF AGENCY?                              
         BZ    HD094               NO                                           
*                                                                               
         LA    RE,H5+80                                                         
         MVC   0(L'RCONKADV,RE),RCONKADV                                        
         LA    RE,L'RCONKADV+1(RE)                                              
         MVC   0(4,RE),=C'C/O '                                                 
         LA    RE,4(RE)                                                         
         MVC   0(L'TWAAGNM1,RE),TWAAGNM1  AGENCY NAME FROM SCREEN               
         B     HD096                                                            
*                                                                               
HD094    DS    0H                                                               
         MVC   H5+80(L'TWAAGNM2),TWAAGNM2  AGENCY NAME FROM SCREEN              
*                                                                               
HD096    DS    0H                                                               
         DROP  RF                                                               
         SPACE 1                                                                
         TM    SVCONF,X'80'        IF UNCONFIRMED                               
         BZ    HD100                                                            
         CLI   SVVER,1             AND NOT VERSION 1                            
         BE    HD100                                                            
         TM    SVSTAT,X'04'        AND PRINT CHANGES ONLY                       
         BZ    HD100                                                            
         MVC   H7+16(12),=C'CHANGES ONLY'  THEN SAY SO IN HEADLINE              
         SPACE 1                                                                
HD100    MVC   H7+69(10),SVADVC    ADVERTISER CODE                              
         MVC   H9+80(20),CONPRD                                                 
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF                                                       
         MVC   H7+80(20),TWAADVNM  ADVERTISER NAME                              
         CLC   SVCONPRD,SPACES                                                  
         BE    HD110                                                            
         MVC   H9+80(20),TWAPRDNM                                               
         DROP  RF                                                               
*                                                                               
HD110    DS    0H                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         MVC   H11+80(20),TWASALNM                                              
         OC    SVSASST,SVSASST                                                  
         BZ    HD120                                                            
         MVI   H11+100,C'/'                                                     
         MVC   H11+101(9),SVSASST  SALES ASSISTANT                              
HD120    MVI   H13+85,C'-'                                                      
         MVC   H13+87(16),CONOFFN                                               
         MVC   H19+89(20),TWABUYER                                              
         DROP  RF                                                               
         CLI   SVSRC,C'A'                                                       
         BNE   *+10                                                             
         MVC   H21+94(3),=C'ARB'                                                
         CLI   SVSRC,C'N'                                                       
         BNE   *+10                                                             
         MVC   H21+94(3),=C'NSI'                                                
         CLI   SVSRC,C'S'                                                       
         BNE   *+10                                                             
         MVC   H21+94(3),=C'SRC'                                                
*                                                                               
* CONFLICT CODES                                                                
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'13'                                                     
         BAS   RE,GETEL                                                         
         BNE   HD123                                                            
         USING RCONCCEL,R6                                                      
         MVC   H9+69(3),RCONCCPR   PRODUCT CODE                                 
         MVC   H17+69(3),RCONCCAD  ADVERTISER CODE                              
         OC    RCONCCAR,RCONCCAR                                                
         BZ    *+10                                                             
         MVC   H17+89(1),RCONCCAR  AREA                                         
         MVC   H21+69(1),RCONCCOT  ORDER TYPE                                   
         DROP  R6                                                               
*                                                                               
* GET EOP RECORD FOR OFFCIE AND SALESPERSON                                     
*                                                                               
HD123    DS    0H                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF                                                       
         MVC   H11+71(6),TWAEOPSL                                               
         MVC   H13+69(6),TWAEOPOF                                               
         DROP  RF                                                               
*                                                                               
HD125    DS    0H                                                               
*                                                                               
* DISPLAY EASI CODES (IF EXIST)                                                 
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'A2'                                                     
         BAS   RE,GETEL                                                         
         BNE   HD130                                                            
         USING RCONIEL,R6                                                       
         MVC   H23+69(4),RCONIADV                                               
         MVC   H23+78(4),RCONIPRD                                               
         MVC   H23+87(4),RCONIPR2                                               
         MVC   H23+96(10),RCONXEST                                              
         OC    H23+96(10),MYSPACES                                              
         CLC   H23+96(10),MYSPACES                                              
         BNE   *+10                                                             
         MVC   H23+96(4),RCONIEST                                               
         DROP  R6                                                               
*                                                                               
HD130    DS    0H                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         MVC   H20+11(34),TWAAGAD1                                              
         MVC   H21+11(36),TWAAGAD2                                              
         MVC   H22+11(36),TWAAGAD3                                              
         DROP  RF                                                               
*                                                                               
HD155    DS    0H                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         LA    R1,H23+2                                                         
         OC    TWAAGYPH,TWAAGYPH                                                
         BZ    HD160                                                            
         MVC   0(7,R1),=C'PHONE #'                                              
         MVC   9(3,R1),TWAAGYPH   AGENCY PHONE NUMBER                           
         MVI   12(R1),C'-'                                                      
         MVC   13(3,R1),TWAAGYPH+3                                              
         MVI   16(R1),C'-'                                                      
         MVC   17(4,R1),TWAAGYPH+6                                              
         LA    R1,27(R1)                                                        
*                                                                               
HD160    DS    0H                                                               
         OC    TWAAFAX,TWAAFAX                                                  
         BZ    HD170                                                            
         MVC   0(5,R1),=C'FAX #'                                                
         MVC   6(3,R1),TWAAFAX    AGENCY FAX NUMBER                             
         MVI   9(R1),C'-'                                                       
         MVC   10(3,R1),TWAAFAX+3                                               
         MVI   13(R1),C'-'                                                      
         MVC   14(4,R1),TWAAFAX+6                                               
         DROP  RF                                                               
*                                                                               
HD170    DS    0H                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         MVC   H25+55(18),=C'SALESPERSON PHONE#'                                
         MVC   H25+74(L'TWASALTL),TWASALTL                                      
*                                                                               
         OC    TWASALFX,TWASALFX   SALESPERSON FAX NUMBER                       
         BZ    HD180                                                            
         MVC   H26+55(18),=C'SALESPERSON FAX  #'                                
         MVC   H26+74(L'TWASALFX),TWASALFX                                      
         B     HD190                                                            
*                                                                               
HD180    DS    0H                  -OR-                                         
         OC    TWAOFFFX,TWAOFFFX   OFFICE FAX NUMBER                            
         BZ    HD190                                                            
         MVC   H26+55(12),=C'OFFICE FAX# '                                      
         MVC   H26+67(3),TWAOFFFX                                               
         MVI   H26+70,C'-'                                                      
         MVC   H26+71(3),TWAOFFFX+3                                             
         MVI   H26+74,C'-'                                                      
         MVC   H26+75(4),TWAOFFFX+6                                             
         DROP  RF                                                               
*                                                                               
HD190    DS    0H                                                               
         GOTO1 DATCON,DMCB,(3,SVCONDTE),(5,H24+13)                              
         GOTO1 DATCON,DMCB,(3,SVCONDTE+3),(5,H24+34)                            
         EDIT  (1,SVWKS),(2,H24+49),ALIGN=LEFT                                  
         SPACE 1                                                                
* DEAL WITH HEADLINES THAT DON'T FIT IN HEDSPECS                                
         SPACE 1                                                                
         MVC   H15+2(51),DASH                                                   
         MVC   H17+2(14),=C'PARTIAL LINE/S '                                    
         MVC   H17+17(36),DASH                                                  
         MVC   H19+2(51),DASH                                                   
         MVC   H20+2(8),=C'AGENCY - '                                           
         MVC   H21+2(7),=C'ADDRESS'                                             
         MVC   H24+2(11),=C'START DATE-'                                        
         MVC   H24+25(9),=C'END DATE-'                                          
         MVC   H24+46(3),=C'WK-'                                                
         SPACE 1                                                                
         MVC   H15+55(10),=C'8/BILL WK.'                                        
         MVC   H16+55(23),DASH                                                  
         MVC   H17+55(11),=C'33/CFLCT CD'                                       
         MVC   H18+55(23),DASH                                                  
         MVC   H19+55(12),=C'11/RATE CARD'                                      
         MVC   H20+55(23),DASH                                                  
         MVC   H21+55(13),=C'14/ORDER TYPE'                                     
         MVC   H22+55(23),DASH                                                  
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'A2'                                                     
         BAS   RE,GETEL                                                         
         BNE   HD195                    DON'T DISPLAY FIELDS IF EMPTY           
         MVC   H23+55(13),=C'EI CODES  ADV'                                     
         MVC   H23+74(03),=C'PRD'                                               
         MVC   H23+83(03),=C'PRD'                                               
         MVC   H23+92(03),=C'EST'                                               
HD195    MVC   H24+55(51),DASH                                                  
         SPACE 1                                                                
         MVC   H15+80(12),=C'10/BILL CODE'                                      
         MVC   H16+80(30),DASH                                                  
         MVC   H17+80(10),=C'9/AREA - 1'                                        
         MVC   H18+80(30),DASH                                                  
         MVC   H19+80(9),=C'16/BYR - '                                          
         MVC   H20+80(30),DASH                                                  
         MVC   H21+80(13),=C'17/RTG SVC - '                                     
         MVC   H22+80(30),DASH                                                  
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
*********************************************************************           
*              COLUMBINE FORMAT                                                 
*********************************************************************           
HD200    DC    0H'0'                                                            
         CLC   =C'MGS',CONACT                                                   
         BNE   HD210                                                            
         MVC   H1+46(24),=C'MAKEGOOD OFFER WORKSHEET'                           
*                                                                               
HD210    DS    0H                                                               
         MVC   H2+55(8),CONCNUM                                                 
         GOTO1 DATCON,DMCB,(5,0),(8,H1+89)                                      
         MVC   H1+100(2),=C'AT'                                                 
         UNPK  DUB,SENDTIME                                                     
         MVC   H1+103(2),DUB+1     TIME                                         
         MVI   H1+105,C'.'                                                      
         MVC   H1+106(2),DUB+3                                                  
         CLC   CONACT,=C'LAST'     FOR ACTION LAST                              
         BNE   *+14                IDENTIFY AS A DUPLICATE COPY                 
         MVC   H1+77(9),=C'DUPLICATE'                                           
         B     HD220                                                            
*                                                                               
         CLC   =C'RSND',CONACT     FOR ACTION RESEND                            
         BNE   *+14                IDENTIFY AS A DUPLICATE COPY                 
         MVC   H1+81(6),=C'RESENT'                                              
         B     HD220                                                            
*                                                                               
         MVC   H1+78(8),=C'FROM REP'                                            
         CLI   TWAACCS,C'$'        STATION                                      
         BNE   *+10                                                             
         MVC   H1+74(12),=C'FROM STATION'                                       
HD220    DS    0H                                                               
*                                                                               
*   CALL ROUTINE TO DISPLAY MOD & VERSION NUMBER                                
         GOTO1 VGENDMV,DMCB,RCONREC,H1,GENOLD                                   
*                                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF                                                       
         CLC   PAGE,=H'1'          FIRST PAGE?                                  
         BE    HD260                                                            
         NI    SPOOLIND,X'7F'      TURN OFF 'SKIP LINE' INDICATOR               
         MVI   H3,0                                                             
         TM    TWAWSFLG,X'80'           SHOULD WE PRINT BUYLINE HEADER          
         BZ    XIT                                                              
         LA    R6,H4                                                            
         BAS   RE,COLBLH                                                        
         B     XIT                                                              
         SPACE 1                                                                
HD260    EQU   *                                                                
         MVC   H3(7),=C'STATION'                                                
         MVC   H4(44),=C'--------------------------------------------'          
         MVC   H5(8),=C'AGENCY #'                                               
         MVC   H6(44),=C'--------------------------------------------'          
         MVC   H7(12),=C'ADVERTISER #'                                          
         MVC   H8(44),=C'--------------------------------------------'          
         MVC   H9(10),=C'CONTRACT #'                                            
         MVC   H10(44),=C'--------------------------------------------'         
         MVC   H11(10),=C'SALESMAN #'                                           
         MVC   H12(44),=C'--------------------------------------------'         
         MVC   H13(8),=C'SALESMAN'                                              
         MVC   H14(44),=C'--------------------------------------------'         
         SPACE 1                                                                
         MVC   H3+71(17),=C'AGENCY ESTIMATE #'                                  
         MVC   H4+71(37),=C'-------------------------------------'              
         MVC   H5+71(9),=C'RATE CARD'                                           
         MVC   H6+71(37),=C'-------------------------------------'              
         MVC   H7+71(30),=C'CO-OP INVOICE    YES  NO  SORT'                     
         MVC   H8+71(37),=C'-------------------------------------'              
         MVC   H9+71(24),=C'BILLING PERIOD   S  M  I'                           
         MVC   H10+71(37),=C'-------------------------------------'             
         MVC   H11+71(32),=C'ACCOUNT TYPE     L  R  N    P  T'                  
         MVC   H12+71(37),=C'-------------------------------------'             
         MVC   H13+71(6),=C'OFFICE'                                             
         MVC   H14+71(37),=C'-------------------------------------'             
*                                                                               
         MVI   MYHEDSW,C'Y'        MORE THAN 14 HEADLINES                       
         MVC   SVPRNT,P            SAVE PRINT LINE                              
         MVC   P,SPACES            BLANK OUT PRINT LINE                         
         MVI   XTRHED,10           MAXIMUM NUMBER OF EXTRA HEADLINES            
         OI    SPOOLIND,X'80'  DON'T SKIP LINES AFTER 1ST SET OF HEADS          
         SPACE 1                                                                
         ZIC   R0,XTRHED           CLEAR OUT EXTRA HEADLINES                    
         LA    R1,H15                                                           
HD270    MVC   0(132,R1),SPACES                                                 
         LA    R1,132(R1)                                                       
         BCT   R0,HD270                                                         
         SPACE 1                                                                
         MVC   H3+13(5),ACTSTAT       STATION                                   
         CLI   ACTSTAT+4,C' '      TV                                           
         BNE   *+10                                                             
         MVC   H3+17(3),=C'-TV'                                                 
         CLI   ACTSTAT+4,C'L'      TV                                           
         BNE   *+10                                                             
         MVC   H3+17(3),=C'-L '                                                 
         MVC   H3+24(20),SVSTAMKT   MARKET                                      
         MVC   H5+13(10),SVAGYC    AGENCY CODE                                  
         MVC   H7+13(10),SVADVC    ADVERTISER CODE                              
*                                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         MVC   H11+13(6),TWAEOPSL  SALESPERSON CODE                             
         DROP  RF                                                               
*                                                                               
         TM    SVCONF,X'80'        IF UNCONFIRMED                               
         BZ    HD280                                                            
         CLI   SVVER,1             AND NOT VERSION 1                            
         BE    HD280                                                            
         TM    SVSTAT,X'04'        AND PRINT CHANGES ONLY                       
         BZ    HD280                                                            
         MVC   H6+51(12),=C'CHANGES ONLY'  THEN SAY SO IN HEADLINE              
         SPACE 1                                                                
HD280    OC    SVTRAF,SVTRAF         TRAFFIC NUMBER                             
         BZ    *+10                                                             
         MVC   H9+13(10),SVTRAF                                                 
*                                                                               
* PICK UP EASI CODES (IF EXIST)                                                 
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'A2'                                                     
         BAS   RE,GETEL                                                         
         BNE   HD290                                                            
         USING RCONIEL,R6                                                       
         MVC   H9+58(4),RCONIADV                                                
         MVC   H11+58(4),RCONIPRD                                               
         MVC   H13+58(4),RCONIPR2                                               
         MVC   H15+54(10),RCONXEST                                              
         OC    H15+54(10),SPACES                                                
         CLC   H15+54(10),SPACES                                                
         BNE   *+10                                                             
         MVC   H15+54(4),RCONIEST                                               
         MVC   H7+51(2),=C'EI'                                                  
         MVC   H8+51(2),=C'--'                                                  
         MVC   H9+51(3),=C'ADV'                                                 
         MVC   H10+51(12),=C'------------'                                      
         MVC   H11+51(3),=C'PRD'                                                
         MVC   H12+51(12),=C'------------'                                      
         MVC   H13+51(3),=C'PRD'                                                
         MVC   H14+51(12),=C'------------'                                      
         MVC   H15+50(3),=C'EST'                                                
         MVC   H16+50(14),=C'--------------'                                    
         DROP  R6                                                               
*                                                                               
HD290    DS    0H                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         MVC   H13+13(20),TWASALNM SALESPERSON NAME                             
         DROP  RF                                                               
         OC    SVSASST,SVSASST                                                  
         BZ    HD300                                                            
         MVI   H13+33,C'/'                                                      
         MVC   H13+34(9),SVSASST   SALES ASSISTANT                              
*                                                                               
HD300    DS    0H                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         MVC   H15(7),=C'SAL PH#'                                               
         MVC   H15+8(L'TWASALTL),TWASALTL                                       
         OC    TWASALFX,TWASALFX   SALESPERSON FAX NUMBER                       
         BZ    HD340                                                            
         MVC   H15+23(8),=C'SAL FAX#'                                           
         MVC   H15+32(L'TWASALFX),TWASALFX                                      
         B     HD350                                                            
*                                                                               
HD340    DS    0H                  -OR-                                         
         OC    TWAOFFFX,TWAOFFFX   OFFICE FAX NUMBER                            
         BZ    HD350                                                            
         MVC   H15+23(8),=C'OFF FAX# '                                          
         MVC   H15+32(3),TWAOFFFX                                               
         MVI   H15+35,C'-'                                                      
         MVC   H15+36(3),TWAOFFFX+3                                             
         MVI   H15+39,C'-'                                                      
         MVC   H15+40(4),TWAOFFFX+6                                             
*                                                                               
HD350    MVC   H13+88(20),CONOFFN     OFFICE NAME                               
         MVC   H17+13(20),TWAADVNM    ADVERTISER                                
         MVC   H15+88(20),CONPRD      PRODUCT NAME                              
*                                                                               
         CLC   SVCONPRD,SPACES                                                  
         BE    *+10                                                             
         MVC   H15+88(20),TWAPRDNM                                              
         MVC   H17+88(20),TWABUYER BUYER NAME                                   
*                                                                               
         TM    TWAFMTFL,X'08'      CARE OF AGENCY?                              
         BZ    *+10                NO                                           
         MVC   H19+13(20),TWAADVNM                                              
*                                                                               
         LA    RE,H20+13                                                        
         TM    TWAFMTFL,X'08'      CARE OF AGENCY?                              
         BZ    *+14                NO                                           
         MVC   0(04,RE),=C'C/O '                                                
         LA    RE,4(RE)                                                         
*                                                                               
         MVC   0(33,RE),TWAAGNM2   AGENCY NAME FOR CONTRACTS                    
         DROP  RF                                                               
*                                                                               
         GOTO1 DATCON,DMCB,(3,SVCONDTE),(5,H19+88)   CONTRACT DATES             
         MVI   H19+97,C'-'                                                      
         GOTO1 DATCON,DMCB,(3,SVCONDTE+3),(5,H19+99)                            
*                                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         MVC   H21+13(34),TWAAGAD1     ADDRESS                                  
         MVC   H22+13(36),TWAAGAD2                                              
         MVC   H23+13(36),TWAAGAD3                                              
         DROP  RF                                                               
*                                                                               
HD375    DS    0H                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         LA    R1,H24                                                           
         OC    TWAAGYPH,TWAAGYPH                                                
         BZ    HD380                                                            
         MVC   0(7,R1),=C'PHONE #'                                              
         MVC   13(3,R1),TWAAGYPH   AGENCY PHONE NUMBER                          
         MVI   16(R1),C'-'                                                      
         MVC   17(3,R1),TWAAGYPH+3                                              
         MVI   20(R1),C'-'                                                      
         MVC   21(4,R1),TWAAGYPH+6                                              
         LA    R1,30(R1)                                                        
*                                                                               
HD380    DS    0H                                                               
         OC    TWAAFAX,TWAAFAX                                                  
         BZ    HD390                                                            
         MVC   0(5,R1),=C'FAX #'                                                
         MVC   6(3,R1),TWAAFAX    AGENCY FAX NUMBER                             
         MVI   9(R1),C'-'                                                       
         MVC   10(3,R1),TWAAFAX+3                                               
         MVI   13(R1),C'-'                                                      
         MVC   14(4,R1),TWAAFAX+6                                               
         DROP  RF                                                               
*                                                                               
*  DEAL WITH HEADLINES THAT DON'T FIT IN HEDSPECS                               
*                                                                               
HD390    DS    0H                                                               
         MVC   H16(44),DASH                                                     
         MVC   H17(10),=C'ADVERTISER'                                           
         MVC   H18(44),DASH                                                     
         MVC   H15+71(9),=C'PRODUCT #'                                          
         MVC   H16+71(37),DASH                                                  
         MVC   H17+71(5),=C'BUYER'                                              
         MVC   H18+71(37),DASH                                                  
         MVC   H20(6),=C'AGENCY'                                                
         MVC   H21(8),=C'NAME AND'                                              
         MVC   H22(7),=C'ADDRESS'                                               
         MVC   H19+71(5),=C'DATES'                                              
         MVC   H20+71(37),DASH                                                  
*                                                                               
         B     XIT                                                              
         SPACE 1                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
*                                                                               
BIABLH   NTR1                                                                   
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF                                                       
         NI    TWAWSFLG,X'FF'-X'02'  TURN OFF FLAG FOR FIRST TIME HEADR         
         DROP  RF                                                               
         MVC   68(4,R6),=C'PLAN'                                                
         MVC   84(4,R6),=C'RATE'                                                
         MVC   101(3,R6),=C'STS'                                                
         LA    R6,132(R6)                                                       
         MVC   0(34,R6),=C'TRAF REP                 BEGIN END'                  
         CLI   FORMAT,C'T'                                                      
         BE    BIAB0020                                                         
         CLI   FORMAT,C'B'                                                      
         BNE   *+10                                                             
BIAB0020 EQU   *                                                                
         MVC   0(34,R6),=C'BIAS REP                 BEGIN END'                  
         MVC   41(39,R6),=C'BEGIN  END   CLA  SEC      PRICE SPOTS/'            
         MVC   84(26,R6),=C'SPOT    CONFLICT CD. TOTAL'                         
         LA    R6,132(R6)                                                       
         MVC   0(35,R6),=C'LN#  LN# CD DAYS         TIME  TIME'                 
         MVC   36(36,R6),=C'LEN  DATE   DATE  13/  14/ PLAN  16/'               
         MVC   74(36,R6),=C'WEEK      18/     PROD ADV 23/ SPOTS'               
         LA    R6,132(R6)                                                       
         MVC   0(35,R6),=C'---- --- -- -----------  ----------'                 
         MVC   36(37,R6),=C'---  -----------  ---  --- ---- -----'              
         MVC   74(36,R6),=C'----- ----------- -------- --- -----'               
BIABLHX  B     XIT                                                              
*                                                                               
COLBLH   NTR1                                                                   
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF                                                       
         NI    TWAWSFLG,X'FF'-X'02'  TURN OFF FLAG FOR FIRST TIME HEADR         
         DROP  RF                                                               
         MVC   0(17,R6),=C'M  LN  G T  MINUM'                                   
         MVC   33(9,R6),=C'BROADCAST'                                           
         MVC   64(24,R6),=C'TIME      #    BROADCAST'                           
         LA    R6,132(R6)                                                       
         SPACE 1                                                                
         MVC   0(27,R6),=C'C  #   R Y  SEPAR      DAYS'                         
         MVC   23(4,R6),=C'DAYS'                                                
         MVC   35(25,R6),=C'DATES     SEC CLA PLA LEN'                          
         MVC   62(25,R6),=C'SCHEDULE   PER    PATTERN'                          
         MVC   95(13,R6),=C'RATE    TOTAL'                                      
         LA    R6,132(R6)                                                       
         SPACE 1                                                                
         MVC   7(11,R6),=C'P P PRD ADV'                                         
         MVC   32(12,R6),=C'START    END'                                       
         MVC   61(29,R6),=C'START   END WK  M/T/W/T/F/S/S'                      
         MVC   103(5,R6),=C'SPOTS'                                              
         LA    R6,132(R6)                                                       
         SPACE 1                                                                
         MVC   0(31,R6),=C'-- --- - - ------- ------------'                     
         MVC   32(28,R6),=C'------------ --- --- --- ---'                       
         MVC   61(15,R6),=C'----------- ---'                                    
         MVC   77(31,R6),=C'-------------- ---------- -----'                    
COLBLHX  B     XIT                                                              
*                                                                               
*        CONSTANTS, LITERAL POOL, ETC.                                          
         SPACE 1                                                                
SVKEY    DS    CL27                                                             
DASH     DC    51C'-'                                                           
         SPACE 1                                                                
DAYTAB   DS    0CL5                                                             
         DC    CL3'MON',CL2'M '                                                 
         DC    CL3'TUE',CL2'T '                                                 
         DC    CL3'WED',CL2'W '                                                 
         DC    CL3'THU',CL2'TH'                                                 
         DC    CL3'FRI',CL2'F '                                                 
         DC    CL3'SAT',CL2'SA'                                                 
         DC    CL3'SUN',CL2'SU'                                                 
         DC    X'FF'                                                            
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE RECNTWR2K                                                      
         EJECT                                                                  
T80266   CSECT                                                                  
*          HEADLINE SPECS FOR CONTRACTS                                         
         SPACE 2                                                                
HEDSPECS DS    0D                                                               
         SPACE 1                                                                
         SPROG 1,9              ***BIAS/WIDE ORBIT/OSI - PAGE 1 ONLY***         
         PSPEC H3,1,C'NEW'                                                      
         PSPEC H5,1,C'ADD'                                                      
         PSPEC H7,1,C'DELETE'                                                   
         PSPEC H9,3,C'ORDER '                                                   
         PSPEC H9,9,C'---------------------------------------------'            
         PSPEC H11,3,C'------------------------------------------------X        
               ---'                                                             
         PSPEC H13,3,C'LINE/S '                                                 
         PSPEC H13,10,C'--------------------------------------------'           
         SPACE 1                                                                
         PSPEC H5,56,C'1/AGENCY#'                                               
         PSPEC H6,56,C'------------------------'                                
         PSPEC H7,56,C'2/ADVERTISER#'                                           
         PSPEC H8,56,C'------------------------'                                
         PSPEC H9,56,C'3/PRODUCT#'                                              
         PSPEC H10,56,C'-----------------------'                                
         PSPEC H11,56,C'10/EOS   4/SAL#'                                        
         PSPEC H12,56,C'-----------------------'                                
         PSPEC H13,56,C'5/REP#'                                                 
         PSPEC H14,56,C'-----------------------'                                
         PSPEC H3,81,C'DATE'                                                    
         PSPEC H4,81,C'------------------------------'                          
         PSPEC H6,81,C'------------------------------'                          
         PSPEC H8,81,C'------------------------------'                          
         PSPEC H10,81,C'------------------------------'                         
         PSPEC H12,81,C'------------------------------'                         
         PSPEC H14,81,C'------------------------------'                         
         SPACE 1                                                                
         SPROG 1                ***BIAS ONLY - PAGE 1 ***                       
         PSPEC H3,56,C'BIAS#'                                                   
         PSPEC H4,56,C'-----------------------'                                 
         SPACE 1                                                                
         SPROG 9                ***WIDE ORBIT/OSI ONLY - PAGE 1 ***             
         PSPEC H3,56,C'TRAF#'                                                   
         PSPEC H4,56,C'-----------------------'                                 
         SPACE 1                                                                
         SPROG 1,2,3,9,10         ***BIAS/WIDE ORBIT/OSI - ALL PAGES***         
         PSPEC H1,1,C'ACE #'      ***INCLUDING COMMENT PAGES***                 
         PSPEC H1,43,C'ORDER WORKSHEET'                                         
         PSPEC H2,103,PAGE                                                      
         EJECT                                                                  
         SPROG 5,6,7      ***COLUMBINE WORKSHEET - ALL PAGES****                
         SPACE 1                                                                
         PSPEC H1,51,C'ORDER WORKSHEET'                                         
         PSPEC H2,103,PAGE                                                      
         DC    X'00'                                                            
         EJECT                                                                  
       ++INCLUDE RECNTFMTD                                                      
         EJECT                                                                  
       ++INCLUDE REGENPBYD                                                      
*    FORMAT FOR BIAS WORKSHEET BUYLINES                                         
         SPACE 1                                                                
PBIASD   DSECT                                                                  
PB       DS    0CL110                                                           
         DS    CL5                                                              
PBLIN    DS    CL3                 DDS LINE NUMBER                              
         DS    CL1                                                              
PBCHG    DS    CL2                 CHANGE CODE                                  
         DS    CL1                                                              
PBDAY    DS    CL12                DAY                                          
         DS    CL1                                                              
PBTIM    DS    CL10                TIME                                         
         DS    CL1                                                              
PBLEN    DS    CL3                 LENGTH                                       
         DS    CL2                                                              
PBDAT    DS    CL12                DATE                                         
         DS    CL1                                                              
PBCLS    DS    CL3                 CLASS                                        
         DS    CL2                                                              
PBSEC    DS    CL3                 SECTION                                      
         DS    CL2                                                              
PBPLN    DS    CL3                 PLAN                                         
         DS    CL1                                                              
PBPRAT   DS    CL5                 PLAN PRICE                                   
         DS    CL2                                                              
PBNPW    DS    CL3                 NUMBER PER WEEK                              
         DS    CL2                                                              
PBRAT    DS    CL10                RATE                                         
         DS    CL16                                                             
PBTSPOT  DS    CL3                 TOTAL SPOTS                                  
         DS    CL1                                                              
         SPACE 3                                                                
*           FORMAT FOR COLUMBINE BUY LINES                                      
PCOLD   DSECT                                                                   
PC       DS    0CL110                                                           
PCCHG    DS    CL2                 CHANGE CODE                                  
         DS    CL1                                                              
PCLIN    DS    CL3                 DDS LINE NUMBER                              
         DS    CL13                                                             
PCDAY    DS    CL12                DAY                                          
         DS    CL1                                                              
PCDAT    DS    CL12                DATE                                         
         DS    CL1                                                              
PCSEC    DS    CL3                 SECTION                                      
         DS    CL1                                                              
PCCLS    DS    CL3                 CLASS                                        
         DS    CL1                                                              
PCPLN    DS    CL3                 PLAN                                         
         DS    CL1                                                              
PCLEN    DS    CL3                 LENGTH                                       
         DS    CL1                                                              
PCTIM    DS    CL11                TIME                                         
         DS    CL1                                                              
PCNPW    DS    CL3                 NUMBER PER WEEK                              
         DS    CL16                                                             
PCRAT    DS    CL10                RATE                                         
         DS    CL2                                                              
PCTSPOT  DS    CL3                 TOTAL SPOTS                                  
         DS    CL3                                                              
         EJECT                                                                  
       ++INCLUDE DDSPOOLD                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'067RECNT66   05/17/05'                                      
         END                                                                    
