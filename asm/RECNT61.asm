*          DATA SET RECNT61    AT LEVEL 117 AS OF 11/14/03                      
*PHASE T80261A,+0                                                               
         TITLE 'T80261 - ENTERPRISE II / VCI FORMATS'                           
*                                                                               
***********************************************************************         
*                                                                     *         
*        RECNT61 (T80261) --- ENTERPRISE II & VCI FORMATS             *         
*                                                                     *         
* ------------------------------------------------------------------- *         
*                                                                     *         
*                                                                     *         
* 13JAN99 RHV CREATION DATE                                           *         
* 08APR99 RHV ADD VCI FORMAT                                          *         
* 09JAN01 RHV SPORTS BUYS                                             *         
*                                                                     *         
***********************************************************************         
*                                                                               
T80261   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T80261,R9                                                      
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
         CLI   FORMAT,C'H'         ENTERPRISE FORMAT                            
         BE    KAMAN                                                            
         CLI   FORMAT,C'S'         VCI FORMAT                                   
         BE    VCI                                                              
         DC    H'0'                                                             
         EJECT                                                                  
*           ROUTINE TO FORMAT BUY LINE TO VCI SPECIFICATIONS                    
         SPACE 1                                                                
VCI      DS    0H                                                               
*                                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF                                                       
         TM    TWAWSFLG,X'40'      K ORD COMMENT PRINTED YET?                   
         BO    VCI1G               YES - SKIP                                   
         OI    TWAWSFLG,X'40'      NO - IT IS NOW                               
*                                                                               
         L     R4,ASVRCOC          REP CONTRACT ORDER COMMENT                   
         LA    R5,10               FOR BCT                                      
         SPACE 1                                                                
         OC    0(60,R4),0(R4)      IS THERE A COMMENT                           
         BZ    VCI1D                                                            
         MVC   P+6(19),=C'*REP ORDER COMMENT*'                                  
*                                                                               
VCI1B    OC    0(60,R4),0(R4)      IS THERE A COMMENT                           
         BZ    VCI1C                                                            
*                                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         OI    TWAWSFLG,X'20'      SINGLE SPACE THIS SECTION                    
         DROP  RF                                                               
         GOTO1 PSTCMT,DMCB,(3,0(R4))  PRINT ORD CMTS                            
*                                                                               
         LA    R4,60(R4)           POINT TO NEXT PART OF COMMENT                
         BCT   R5,VCI1B                                                         
*                                                                               
VCI1C    BAS   RE,GOSPOOL                                                       
         SPACE 1                                                                
*                                                                               
VCI1D    LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF                                                       
         L     R4,ASVSCOC          STA CONTRACT ORDER COMMENT                   
         LA    R5,10               FOR BCT                                      
         OC    0(60,R4),0(R4)      IS THERE A COMMENT                           
         BZ    VCI1G                                                            
         MVC   P+6(23),=C'*STATION ORDER COMMENT*'                              
*                                                                               
VCI1E    OC    0(60,R4),0(R4)      IS THERE A COMMENT                           
         BZ    VCI1F                                                            
*                                                                               
         OI    TWAWSFLG,X'20'      SINGLE SPACE THIS SECTION                    
         DROP  RF                                                               
         MVC   P+30(60),0(R4)                                                   
         OC    P+30(60),SPACES                                                  
         BAS   RE,GOSPOOL                                                       
*                                                                               
         LA    R4,60(R4)           POINT TO NEXT PART OF COMMENT                
         BCT   R5,VCI1E                                                         
*                                                                               
VCI1F    BAS   RE,GOSPOOL                                                       
*                                                                               
VCI1G    DS    0H                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF                                                       
         NI    TWAWSFLG,X'FF'-X'20'                                             
         L     R4,AWORK4           POINT TO OUTPUT                              
         USING PD,R4                                                            
         LA    R3,P                POINT TO PRINT LINE                          
         USING PVCID,R3                                                         
         ZIC   R5,BUYLIN                                                        
         LR    R2,R5               SET UP ALLOWLIN                              
         CLI   SVSTAOP2,C'Y'       TRIPLE SPACING                               
         BE    V2                                                               
         SLL   R2,1                DOUBLE SPACING                               
         B     *+8                                                              
V2       MH    R2,=H'3'                                                         
         TM    TWAWSFLG,X'02'      PRINT BUYLIN HEADER ALSO?                    
         BZ    *+8                 NO                                           
         AH    R2,=H'3'            YES - ALLOW ANOTHER 3 LINES                  
         STC   R2,ALLOWLIN                                                      
         DROP  RF                                                               
V5       DS    0H                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF                                                       
         TM    TWAWSFLG,X'02'      DO WE NEED TO PRINT BUYLINE HEADER?          
         BZ    V5A                 NO - SKIP                                    
         CLC   CONACT,=C'CONF'     FOR CONFIRM, NEVER PRINT HEADER              
         BE    V5A                                                              
         DROP  RF                                                               
         LA    R6,P                PUT HEADER ON P                              
         BAS   RE,VCIBLH           GENERATE BUYLINE HEADER                      
         BAS   RE,GOSPOOL          AND PRINT IT                                 
         SPACE 1                                                                
V5A      CLC   =C'ALT',PDAT        ALTERNATE WEEKS LINE                         
         BNE   V5D                                                              
         MVC   PVDAT-1(13),=C'ALTERNATE WKS'                                    
         MVC   PVDAY,PDAY          DAY                                          
         MVC   PVTIM,PTIM          TIME                                         
         B     V50                                                              
         SPACE 1                                                                
V5D      CLC   37(11,R4),=C'WEEKLY RATE'                                        
         BNE   *+14                                                             
         MVC   PVTCOS-28(39),PDAT+1   'WEEKLY RATE FOR PLAN XXX IS'             
         B     V50                                                              
         SPACE 1                                                                
         OC    PDAY,PDAY           IF NOTHING THERE, IT'S MORE DATES            
         BZ    V6                  USE EXISTING CODE TO FILL IN                 
V5G      DS    0H                                                               
         CLI   PDAY,X'FF'         SPORTS BUY DESCRIPTIVE?                       
         BNE   V5H                                                              
         MVC   PVDAT(L'PSDESC),PSDESC   SPORTS DESCRIPTIVE                      
         B     V50                                                              
*                                                                               
V5H      OC    PDAY(5),PDAY       IF NOTHING HERE, IT'S A COMMENT               
         BNZ   V6                                                               
         CLC   =C'ROC',PDAY+6      REP ORDER COMMENT                            
         BNE   V5L                                                              
         MVC   PV+6(13),=C'*REP ORD CMT*'                                       
         B     V5P                                                              
V5L      CLC   =C'SOC',PDAY+6      STATION ORDER COMMENT                        
         BNE   V5S                                                              
         MVC   PV+6(13),=C'*STA ORD CMT*'                                       
V5P      MVC   21(60,R3),PDAY+10                                                
         B     V50                                                              
         SPACE 1                                                                
*   IT'S A BUY COMMENT, BUT THEY DON'T WANT TO SEE THE LABEL                    
V5S      DS    0H                                                               
         CLC   =C'P=',PDAY+6                                                    
         BNE   V5T                                                              
         MVC   21(12,R3),=C'PROGRAMMING='                                       
         MVC   33(64,R3),PDAY+8                                                 
         B     V50                                                              
*                                                                               
V5T      MVC   21(66,R3),PDAY+6                                                 
         B     V50                                                              
         SPACE 1                                                                
V6       MVC   PVCHG,PCHG          REVISION CODE                                
         OC    PVCHG,PVCHG         ONLY PRINT REV CODE LISTING                  
         BZ    *+8                 AT BOTTOM IF THERE ARE REVISIONS             
         MVI   CODESW,C'Y'                                                      
         SPACE 1                                                                
         MVC   PVLIN,PLIN          LINE NUMBER                                  
*                                                                               
         L     RF,AIO2             BUYREC                                       
         USING RBUYREC,RF                                                       
         TM    RBUYSFG,X'40'       SPORTS BUY?                                  
         BZ    V7                  NO                                           
         DROP  RF                                                               
         MVC   PVTIM(L'PSTYPE),PSTYPE   SPORTS TYPE                             
         B     V8                                                               
*                                                                               
V7       MVC   PVDAY,PDAY          DAY                                          
         MVC   PVTIM,PTIM          TIME                                         
***>     MVC   PVSEC,PSEC          SECTION                                      
***>     MVC   PVCLS,PCLS          CLASS                                        
***>     MVC   PVPLN,PPLN          PLAN                                         
         MVC   PVLEN,PLEN          LEN                                          
         MVC   PVDAT-1(1),PDAT-1   * FOR ALTERNATE WEEKS                        
         SPACE 1                                                                
V8       GOTO1 DATVAL,DMCB,(1,PDAT),WORK    DATES                               
         OC    DMCB(4),DMCB                                                     
         BZ    V10                                                              
         MVC   PVDAT(2),WORK+2                                                  
         MVI   PVDAT+2,C'/'                                                     
         MVC   PVDAT+3(2),WORK+4                                                
         SPACE 1                                                                
V10      GOTO1 DATVAL,DMCB,(1,PDAT+6),WORK                                      
         OC    DMCB(4),DMCB                                                     
         BZ    V15                                                              
         MVI   PVDAT+5,C'-'                                                     
         MVC   PVDAT+6(2),WORK+2                                                
         MVI   PVDAT+8,C'/'                                                     
         MVC   PVDAT+9(2),WORK+4                                                
         SPACE 1                                                                
V15      CLI   PDAT+11,C'A'        ALTERNATE WEEKS                              
         BNE   *+8                                                              
         MVI   PVDAT+11,C'A'                                                    
         SPACE 1                                                                
         MVC   PVNPW,PNPW          NUMBER PER WEEK                              
         MVC   PVRAT,PRAT          RATE                                         
         MVC   PVTSPOT,PTOT        TOTAL SPOTS                                  
         SPACE 1                                                                
         OC    PDAY,PDAY           PRINT TOTAL COST ONLY ON FIRST LINE          
         BZ    V50                                                              
         L     R6,AIO2             GET TOTAL BUYLINE COST                       
         USING RBUYKEY,R6                                                       
         TM    RBUYCNTL,X'80'      IF DELETED                                   
         BO    V50                 DON'T SHOW TOTAL COST                        
         CLI   RBUYCHGI,C'C'       CANCELLED?                                   
         BE    V50                 DON'T SHOW TOTAL COST                        
         SPACE 1                                                                
         LA    R2,RBUYELEM                                                      
         USING RBUYELEM,R2                                                      
         EDIT  RBUYTCOS,(12,PVTCOS),2,COMMAS=YES,FLOAT=-,ALIGN=LEFT             
         DROP  R2,R6                                                            
         SPACE 1                                                                
V50      BAS   RE,GOSPOOL                                                       
         MVI   RCSUBPRG,8          FOR CONTINUATION PAGES                       
*                                  ONLY INCLUDE A FEW HEADLINES                 
         LA    R4,L'PRTLN(R4)                                                   
         BCT   R5,V5                                                            
         SPACE 1                                                                
         B     EXXMOD                                                           
         DROP  R4                                                               
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
*                                                                               
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
*                                                                               
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
         CLI   FORMAT,C'H'         KAMAN FORMAT                                 
         BE    HD10                                                             
         CLI   FORMAT,C'S'         VCI FORMAT                                   
         BE    VH10                                                             
         DC    H'0'                                                             
*                                                                               
VH10     DS    0H       ******VCI HEADLINES*********                            
         CLC   =C'MGS',CONACT                                                   
         BNE   *+10                                                             
         MVC   H1+41(24),=C'MAKEGOOD OFFER WORKSHEET'                           
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
         B     VH50                                                             
*                                                                               
         CLC   =C'RSND',CONACT    FOR ACTION RESEND                             
         BNE   *+14                IDENTIFY AS A DUPLICATE COPY                 
         MVC   H3+102(6),=C'RESENT'                                             
         B     VH50                                                             
*                                                                               
         MVC   H1+87(8),=C'FROM REP'                                            
         CLI   TWAACCS,C'$'        STATION                                      
         BNE   *+10                                                             
         MVC   H1+87(12),=C'FROM STATION'                                       
*                                                                               
VH50     DS    0H                                                               
         CLI   RCSUBPRG,7                                                       
         BE    VH60                                                             
         NI    SPOOLIND,X'7F'      TURN OFF 'SKIP LINE' INDICATOR               
         MVC   DUB(4),ACOMFACS     RFBLOCK                                      
         MVC   DUB+4(2),REPALPHA                                                
         GOTO1 (RFADVOUT,VREPFACS),DMCB,RCONKADV,H3+41,0,DUB                    
         GOTO1 (RFSALOUT,VREPFACS),DMCB,RCONSAL,H3+76,0,DUB                     
         GOTO1 (RFCONNUM,VREPFACS),DMCB,(1,RCONKCON),(6,H3+26)                  
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF                                                       
         TM    TWAWSFLG,X'80'           SHOULD WE PRINT BUYLINE HEADER?         
         BZ    XIT                                                              
         MVI   H3,0                                                             
         LA    R6,H5                                                            
         BAS   RE,VCIBLH                                                        
         B     XIT                                                              
         DROP  RF                                                               
*                                                                               
VH60     MVI   MYHEDSW,C'Y'        MORE THAN 14 HEADLINES                       
         MVC   SVPRNT,P            SAVE PRINT LINE                              
         MVC   P,SPACES            BLANK OUT PRINT LINE                         
         MVI   XTRHED,6            MAXIMUM NUMBER OF EXTRA HEADLINES            
         OI    SPOOLIND,X'80'  DON'T SKIP LINES AFTER 1ST SET OF HEADS          
*                                                                               
         ZIC   R0,XTRHED           CLEAR OUT EXTRA HEADLINES                    
         LA    R1,H15                                                           
VH70     MVC   0(132,R1),SPACES                                                 
         LA    R1,132(R1)                                                       
         BCT   R0,VH70                                                          
*                                                                               
         TM    SVCONF,X'80'        IF UNCONFIRMED                               
         BZ    VH80                                                             
         CLI   SVVER,1             AND NOT VERSION 1                            
         BE    VH80                                                             
         TM    SVSTAT,X'04'        AND PRINT CHANGES ONLY                       
         BZ    VH80                                                             
         MVC   H2+46(12),=C'CHANGES ONLY'  THEN SAY SO IN HEADLINE              
*                                                                               
VH80     DS    0H                                                               
         GOTO1 (RFCONNUM,VREPFACS),DMCB,(1,RCONKCON),(6,H4+29)                  
         GOTO1 (RFSTAOUT,VREPFACS),DMCB,RCONKSTA,H10+48                         
*                                                                               
         GOTO1 DATCON,DMCB,(3,RCONCREA),(5,H6+14)                               
*                                                                               
         MVC   H8+12(4),RCONKADV   ADVERTISER CODE                              
         MVC   DUB(4),ACOMFACS                                                  
         MVC   DUB+4(2),REPALPHA                                                
         GOTO1 (RFADVOUT,VREPFACS),DMCB,RCONKADV,H8+17,0,DUB                    
*                                                                               
         GOTO1 (RFAGYOUT,VREPFACS),DMCB,RCONKAGY,H10+8,H10+16,DUB               
*                                                                               
         GOTO1 (RFSALOUT,VREPFACS),DMCB,RCONSAL,H12+13,0,DUB                    
*                                                                               
         GOTO1 DATCON,DMCB,(3,SVCONDTE),(5,H14+7)                               
         GOTO1 DATCON,DMCB,(3,SVCONDTE+3),(5,H14+22)                            
*                                                                               
         MVC   H12+69(1),RCONTYPE                                               
*                                                                               
         MVC   H18+53(20),CONPRD   PRODUCT NAME                                 
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF                                                       
         CLC   SVCONPRD,SPACES                                                  
         BE    *+10                                                             
         MVC   H18+53(20),TWAPRDNM     PRODUCT NAME                             
         DROP  RF                                                               
*                                                                               
         BAS   RE,CALCTOTS                                                      
         EDIT  FULL,(13,H16+9),2,ALIGN=LEFT,FLOAT=$,ZERO=NOBLANK,      +        
               COMMAS=YES                                                       
         EDIT  HALF,(3,H16+34),ALIGN=LEFT,ZERO=NOBLANK                          
*                                                                               
* DISPLAY EASI CODES (IF EXIST)                                                 
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'A2'                                                     
         BAS   RE,GETEL                                                         
         BNE   VH90                                                             
         USING RCONIEL,R6                                                       
         MVC   H4+44(10),RCONXEST                                               
         OC    H4+44(10),MYSPACES                                               
         CLC   H4+44(10),MYSPACES                                               
         BNE   *+10                                                             
         MVC   H4+44(4),RCONIEST                                                
         DROP  R6                                                               
*                                                                               
* DEAL WITH HEADLINES THAT DON'T FIT IN HEDSPECS                                
*                                                                               
VH90     DS    0H                                                               
         MVC   H15+07(08),=8C'-'                                                
         MVC   H15+22(08),=8C'-'                                                
         MVC   H15+46(31),=31C'-'                                               
         MVC   H16+00(08),=C'TTL REV:'                                          
         MVC   H17+09(13),=13C'-'                                               
         MVC   H16+23(10),=C'TTL SPOTS:'                                        
         MVC   H17+34(03),=3C'-'                                                
         MVC   H16+39(13),=C'REVENUE TYPE:'                                     
         MVC   H17+53(24),=24C'-'                                               
         MVC   H18+39(13),=C'PRODUCT CODE:'                                     
         MVC   H19+53(24),=24C'-'                                               
*                                                                               
         MVC   H13+79(17),=17C'-'                                               
         MVC   H14+79(26),=C'|BROADCAST  |   |---------'                        
         MVC   H15+79(26),=C'|DAY/WEEK   |   |DAY |   |'                        
         MVC   H16+79(26),=C'|SELECT DATE|   |DATE|   |'                        
         MVC   H17+79(26),=26C'-'                                               
VH150    DS    0H                                                               
         B     XIT                                                              
*                                                                               
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
         MVI   XTRHED,6            MAXIMUM NUMBER OF EXTRA HEADLINES            
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
         MVC   H6+28(20),TWAAGNM1  SHORT AGENCY NAME                            
*                                                                               
HD086    DS    0H                                                               
         MVC   H10+60(6),TWAEOPSL   EOP SL#                                     
         MVC   H10+81(20),TWASALNM  SALESPERSON NAME                            
         DROP  RF                                                               
         MVC   H8+77(16),CONOFFN   OFFICE NAME                                  
         GOTO1 DATCON,DMCB,(3,SVCONDTE),(5,H8+13)                               
         GOTO1 DATCON,DMCB,(3,SVCONDTE+3),(5,H8+35)                             
*                                                                               
         MVC   H12+18(20),CONPRD   PRODUCT NAME                                 
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF                                                       
         MVC   H12+65(20),TWABUYER BUYER                                        
         CLC   SVCONPRD,SPACES                                                  
         BE    *+10                                                             
         MVC   H12+18(20),TWAPRDNM     PRODUCT NAME                             
         DROP  RF                                                               
         SPACE 1                                                                
         OC    SVTRAF,SVTRAF       TRAFFIC NUMBER                               
         BZ    *+10                                                             
         MVC   H14+65(10),SVTRAF                                                
         SPACE 1                                                                
         MVC   H14+89(8),CONCNUM   ACE NUMBER                                   
         MVC   H16+70(9),SVSASST   SALES ASSISTANT                              
*                                                                               
* DISPLAY EASI CODES (IF EXIST)                                                 
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'A2'                                                     
         BAS   RE,GETEL                                                         
         BNE   HD90                                                             
         USING RCONIEL,R6                                                       
         MVC   H16+81(6),=C'EI ADV'                                             
         MVC   H16+93(3),=C'PRD'                                                
         MVC   H17+84(3),=C'PRD'                                                
         MVC   H17+93(3),=C'EST'                                                
         MVC   H16+88(4),RCONIADV                                               
         MVC   H16+97(4),RCONIPRD                                               
         MVC   H17+88(4),RCONIPR2                                               
         MVC   H17+97(10),RCONXEST                                              
         OC    H17+97(10),MYSPACES                                              
         CLC   H17+97(10),MYSPACES                                              
         BNE   *+10                                                             
         MVC   H17+97(4),RCONIEST                                               
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
         MVC   H15+65(10),DASH                                                  
         MVC   H15+89(08),DASH                                                  
         MVC   H16+52(15),=C'SALES ASSISTANT'                                   
         MVC   H17+70(9),DASH      UNDERLINE SALES ASST.                        
*                                                                               
         MVC   H18+52(16),=C'SALESPERSON PH #'                                  
         MVC   H18+69(L'TWASALTL),TWASALTL                                      
*                                                                               
         OC    TWASALFX,TWASALFX   SALESPERSON FAX NUMBER                       
         BZ    HD140                                                            
         MVC   H19+52(16),=C'SALESPERSON FAX#'                                  
         MVC   H19+69(L'TWASALFX),TWASALFX                                      
         B     HD150                                                            
*                                                                               
HD140    DS    0H                  -OR- OFFICE FAX NUMBER                       
         OC    TWAOFFFX,TWAOFFFX                                                
         BZ    HD150                                                            
         MVC   H19+52(12),=C'OFFICE FAX# '                                      
         MVC   H19+64(3),TWAOFFFX                                               
         MVI   H19+67,C'-'                                                      
         MVC   H19+68(3),TWAOFFFX+3                                             
         MVI   H19+71,C'-'                                                      
         MVC   H19+72(4),TWAOFFFX+6                                             
HD150    DS    0H                                                               
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
*                                                                               
VCIBLH   NTR1                                                                   
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF                                                       
         NI    TWAWSFLG,X'FF'-X'02'  TURN OFF FLAG FOR FIRST TIME HEADR         
         DROP  RF                                                               
         USING PVCID,R6                                                         
         MVC   PVCHG,=CL3'ACT'                                                  
         MVC   PVLIN,=CL4'LINE'                                                 
         MVC   PVDAT,=CL11'START   END'                                         
         MVC   PVDAY,=CL12'DAYS'                                                
         MVC   PVTIM,=CL12'TIME PERIOD'                                         
         MVC   PVPGM,=CL12'PGM/CLASS'                                           
         MVC   PVNPW,=CL4'SP/W'                                                 
         MVC   PVTYP,=CL6'SP TYP'                                               
         MVC   PVRAT,=CL10'RATE'                                                
         MVC   PVLEN,=CL3'LEN'                                                  
         MVC   PVTSPOT,=CL5'SPOTS'                                              
         MVC   PVTCOS,=CL12'CASH'                                               
         LA    R6,132(R6)                                                       
         MVC   PVCHG,DASH                                                       
         MVC   PVLIN,DASH                                                       
         MVC   PVDAT,DASH                                                       
         MVC   PVDAY,DASH                                                       
         MVC   PVTIM,DASH                                                       
         MVC   PVPGM,DASH                                                       
         MVC   PVNPW,DASH                                                       
         MVC   PVTYP,DASH                                                       
         MVC   PVRAT,DASH                                                       
         MVC   PVLEN,DASH                                                       
         MVC   PVBLANK,DASH                                                     
         MVC   PVTSPOT,DASH                                                     
         MVC   PVTCOS,DASH                                                      
         DROP  R6                                                               
VCIBLHX  B     XIT                                                              
*                                                                               
KAMBLH   NTR1                                                                   
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF                                                       
         NI    TWAWSFLG,X'FF'-X'02'  TURN OFF FLAG FOR FIRST TIME HEADR         
         DROP  RF                                                               
         MVC   0(39,R6),=C' L  M                      TIME PERIOD/'             
         MVC   41(17,R6),=C'         DATES   '                                  
         MVC   60(33,R6),=C'RATE TOT                         '                  
         LA    R6,132(R6)                                                       
         MVC   0(39,R6),=C' N  C   DAYS          NPW  SELL PATTERN'             
         MVC   41(17,R6),=C'LEN  START    END'                                  
         MVC   60(33,R6),=C'SEC  SPT  COST/SPOT  TOTAL COST  '                  
         MVC   95(8,R6),=C'CLA  PLA'                                            
         LA    R6,132(R6)                                                       
         MVC   0(39,R6),=C'--- --  ------------  ---  ------------'             
         MVC   41(17,R6),=C'---  ------------'                                  
         MVC   60(33,R6),=C'---- ---  ---------  ------------'                  
         MVC   95(8,R6),=C'---  ---'                                            
KAMBLHX  B     XIT                                                              
*                                                                               
*    THIS ROUTINE SCANS ALL THE BUY RECORDS, AND CALCULATES THE                 
*          TOTAL # OF SPOTS AND TOTAL DOLLARS OF THE ORDER FOR THE              
*          HEADER RECORD.  ***NOTE***  AS THESE ARE ORIGINAL ORDERS             
*          ONLY, THERE IS NO - REPEAT, NO - PROVISION TO BACK OUT               
*          MAKEGOOD MISSED SPOTS, BECAUSE THERE SHOULDN'T BE ANY!!              
*          THIS, OF COURSE, ONLY PERTAINS TO THOSE ORDERS CREATED               
*          UNDER THE OLD MAKEGOOD STRUCTURE, WHERE MISSED SPOTS WERE            
*          NOT SUBTRACTED FROM THE EFFECTIVE DATE ELEMENTS, AND THE             
*          DATES OF THE ELEMENTS ADJUSTED ACCORDINGLY.  FOR NEWER               
*          ORDERS, MAKEGOODS ARE NOT A CONSIDERATION AT ALL.  B.UHR.            
*                                                                               
CALCTOTS NTR1                                                                   
         XC    FULL,FULL      ***  RETURN TOTAL COST IN FULL  ***               
         XC    HALF,HALF      ***  RETURN TOTAL SPOTS IN HALF ***               
         LA    RF,KEY                                                           
K        USING RBUYREC,RF                                                       
         MVI   K.RBUYKEY,X'0B'       INSERT ID                                  
         MVC   K.RBUYKREP,REPALPHA                                              
         GOTO1 (RFCONNUM,VREPFACS),DMCB,(1,RCONKCON),(3,K.RBUYKCON)             
         DROP  K                                                                
         GOTO1 VHIGH                                                            
         B     CTOT0020                                                         
CTOT0010 EQU   *                                                                
         GOTO1 VSEQ                                                             
CTOT0020 EQU   *                                                                
         CLC   KEY(22),KEYSAVE     COMPARE THROUGH CONTRACT #                   
         BNE   XIT                 DONE                                         
         GOTO1 VGETREC,DMCB,RBUYREC                                             
         CLC   =X'FFFF',RBUYKMLN   PLAN RECORD OF BUYS?                         
         BE    CTOT0010            YES - SKIP IT                                
         CLI   RBUYCHGI,C'C'       CANCELLED?                                   
         BE    CTOT0010            DON'T ADD INTO TOTALS                        
         LA    R2,RBUYELEM         SET A(01 DESCRIPTOR ELEMENT)                 
CTOT0040 EQU   *                                                                
         ZIC   RF,1(R2)            BUMP TO NEXT ELEMENT                         
         AR    R2,RF                                                            
         CLI   0(R2),0             END OF RECORD?                               
         BE    CTOT0010            YES - GO BACK FOR ANOTHER RECORD             
         CLI   0(R2),3             EFFECTIVE DATE ELEMENT?                      
         BNE   CTOT0040            NO  - GO BACK FOR NEXT ELEMENT               
*                                                                               
         USING RBUYDTEL,R2                                                      
*                                                                               
         ZIC   RF,RBUYDTNW         SET NUMBER PER WEEK                          
         SR    RE,RE                                                            
         ZIC   R1,RBUYDTWK         SET NUMBER OF WEEKS                          
         MR    RE,R1               #/WK * # SPOTS/WK = # SPOTS                  
         LH    RE,HALF             ACCUMULATE NUMBER OF SPOTS                   
         AR    RE,RF                                                            
         STH   RE,HALF             REPLACE TOTAL SPOTS                          
         SR    RE,RE                                                            
         ZICM  R1,RBUYCOS,4        LOAD SPOT COST                               
         MR    RE,R1               # SPOTS * $/SPOT = TOTAL $$                  
         L     RE,FULL             ACCUMULATE TOTAL DOLLARS                     
         AR    RE,RF                                                            
         ST    RE,FULL             REPLACE TOTAL DOLLARS                        
         B     CTOT0040            GO BACK FOR NEXT X'03' ELT                   
         DROP  R2                                                               
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
         SPROG 7                ***VCI - PAGE 1 ONLY***                         
         PSPEC H4,1,C'NEW/REVISE/DELETE'                                        
         PSPEC H4,20,C'CONTRACT#'                                               
         PSPEC H5,30,8C'-'                                                      
         PSPEC H6,1,C'DATE WRITTEN:'                                            
         PSPEC H7,15,23C'-'                                                     
         PSPEC H8,1,C'ADVERTISER:'                                              
         PSPEC H9,13,25C'-'                                                     
         PSPEC H10,1,C'AGENCY:'                                                 
         PSPEC H11,9,29C'-'                                                     
         PSPEC H12,1,C'SALESPERSON:'                                            
         PSPEC H13,14,24C'-'                                                    
         PSPEC H14,1,C'START:           END:'                                   
*                                                                               
         PSPEC H4,40,C'EST#'                                                    
         PSPEC H5,45,13C'-'                                                     
         PSPEC H4,60,C'PROP#'                                                   
         PSPEC H5,66,12C'-'                                                     
         PSPEC H6,40,C'DESCRIPTION:'                                            
         PSPEC H7,53,25C'-'                                                     
         PSPEC H8,40,C'SPEC HANDLING:'                                          
         PSPEC H9,55,23C'-'                                                     
         PSPEC H10,40,C'STATION:'                                               
         PSPEC H11,49,29C'-'                                                    
         PSPEC H12,40,C'ORD STATUS:'                                            
         PSPEC H13,52,6C'-'                                                     
         PSPEC H12,60,C'ORD TYPE:'                                              
         PSPEC H13,70,8C'-'                                                     
         PSPEC H14,40,C'BRAND:'                                                 
*                                                                               
         PSPEC H4,80,C'----------------------'                                  
         PSPEC H5,80,C'|INVOICE BY BRAND|   |'                                  
         PSPEC H6,80,C'|PRINT SPOT RATES|   |'                                  
         PSPEC H7,80,C'|CO-OP CONTRACT  |   |'                                  
         PSPEC H8,80,C'|AGENCY COMM     |   |'                                  
         PSPEC H9,80,C'|MULTI VENDOR    |   |'                                  
         PSPEC H10,80,C'|LEVEL BILL      |   |'                                 
         PSPEC H11,80,C'|   AMOUNT $         |'                                 
         PSPEC H12,80,C'----------------------'                                 
*                                                                               
         PSPEC H4,110,C'----------------------'                                 
         PSPEC H5,110,C'|REVISION#      |    |'                                 
         PSPEC H6,110,C'|CANCEL CONTRACT|    |'                                 
         PSPEC H7,110,C'----------------------'                                 
         PSPEC H8,110,C'----------------------'                                 
         PSPEC H9,110,C'|VENDOR         | %  |'                                 
         PSPEC H10,110,C'|---------------|----|'                                
         PSPEC H11,110,C'|---------------|----|'                                
         PSPEC H12,110,C'|---------------|----|'                                
         PSPEC H13,110,C'|---------------|----|'                                
         PSPEC H14,110,C'----------------------'                                
         SPACE 1                                                                
         SPROG 7,8                ***VCI - ALL PAGES***                         
         PSPEC H1,46,C'ORDER WORKSHEET'                                         
         PSPEC H1,103,PAGE                                                      
         SPACE 1                                                                
         SPROG 8                  ***VCI - CONTINUATION PAGES***                
         PSPEC H3,1,C'DEAL:'                                                    
         PSPEC H4,7,8C'-'                                                       
         PSPEC H3,17,C'CONTRACT:'                                               
         PSPEC H4,27,8C'-'                                                      
         PSPEC H3,37,C'ADV:'                                                    
         PSPEC H4,42,20C'-'                                                     
         PSPEC H3,64,C'SALESPERSON:'                                            
         PSPEC H4,77,20C'-'                                                     
*                                                                               
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
         PSPEC H7,29,C'-------------------'                                     
         PSPEC H6,53,C'BRAND CODE'                                              
         PSPEC H7,65,C'----------'                                              
         PSPEC H6,79,C'CONFLICT CODE  1      2'                                 
         PSPEC H7,95,C'-----  -----'                                            
         PSPEC H8,53,C'REP'                                                     
         PSPEC H9,58,C'--------'                                                
         PSPEC H8,70,C'OFFICE'                                                  
         PSPEC H9,78,C'----------------'                                        
         PSPEC H8,1,C'START DATE'                                               
         PSPEC H9,14,C'--------'                                                
         PSPEC H8,25,C'END DATE'                                                
         PSPEC H9,36,C'--------'                                                
         PSPEC H10,53,C'SLS #'                                                  
         PSPEC H11,61,C'--------'                                               
         PSPEC H10,74,C'NAME'                                                   
         PSPEC H11,82,C'--------------------'                                   
         PSPEC H10,1,C'CLASS (L/N)'                                             
         PSPEC H11,14,C'--------'                                               
         PSPEC H10,25,C'SUB CLASS'                                              
         PSPEC H11,36,C'--------'                                               
         PSPEC H12,53,C'BUYER NAME'                                             
         PSPEC H13,66,C'--------------------'                                   
         PSPEC H12,1,C'SPONSOR/PRODUCT'                                         
         PSPEC H13,19,C'--------------------'                                   
         PSPEC H14,53,C'TRAFFIC #'                                              
         PSPEC H14,82,C'ACE #'                                                  
         PSPEC H14,1,C'AGENCY'                                                  
         SPACE 1                                                                
         SPROG 1,2                ***KAMAN - ALL PAGES***                       
         PSPEC H1,46,C'ORDER WORKSHEET'                                         
         PSPEC H1,103,PAGE                                                      
         SPACE 1                                                                
         SPROG 5                  ***KAMAN - STORED COMMENTS***                 
         PSPEC H1,46,C'ORDER WORKSHEET'                                         
         PSPEC H1,103,PAGE                                                      
         EJECT                                                                  
         DC    X'00'                                                            
         EJECT                                                                  
       ++INCLUDE RECNTWR2K                                                      
       ++INCLUDE RECNTFMTD                                                      
       ++INCLUDE REGENPBYD                                                      
*    FORMAT FOR VCI WORKSHEET BUYLINES                                          
         SPACE 1                                                                
PVCID    DSECT                                                                  
PV       DS    0CL132                                                           
PVCHG    DS    CL3                 CHANGE CODE                                  
         DS    CL1                                                              
PVLIN    DS    CL4                 DDS LINE NUMBER                              
         DS    CL2                                                              
PVDAT    DS    CL11                DATE                                         
         DS    CL2                                                              
PVDAY    DS    CL12                DAY                                          
         DS    CL2                                                              
PVTIM    DS    CL12                TIME                                         
         DS    CL2                                                              
PVPGM    DS    CL12                PGM/CLASS                                    
         DS    CL2                                                              
PVNPW    DS    CL4                 NUMBER PER WEEK                              
         DS    CL2                                                              
PVTYP    DS    CL6                 SP TYPE                                      
         DS    CL2                                                              
PVRAT    DS    CL10                RATE                                         
         DS    CL2                                                              
PVLEN    DS    CL3                 LENGTH                                       
         DS    CL2                                                              
PVBLANK  DS    CL12                BLANK COLUMN                                 
         DS    CL2                                                              
PVTSPOT  DS    CL5                 TOTAL SPOTS                                  
         DS    CL1                                                              
PVTCOS   DS    CL12                TOTAL COST                                   
         EJECT                                                                  
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
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'117RECNT61   11/14/03'                                      
         END                                                                    
