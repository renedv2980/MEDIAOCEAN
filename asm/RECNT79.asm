*          DATA SET RECNT79    AT LEVEL 050 AS OF 06/23/08                      
*PHASE T80279A,+0                                                               
         TITLE 'T80279 - VCI STAR III'                                          
*                                                                               
***********************************************************************         
*                                                                     *         
*        RECNT79 (T80279) --- VCI STAR III                            *         
*                                                                     *         
* ------------------------------------------------------------------- *         
*                                                                     *         
*                                                                     *         
* 23JUN08 KUI - FIX ORBIT PRINTING                                    *         
* 11APR06 HQI - PASS DELETED BUY RECORD BACK                          *         
* 28MAR05 BU  - ADD TRAFFIC FORMAT 'D'                                *         
* 10FEB05 HQI - FIX BUYLINE TOTAL SPOTS/DOLLAR CALCULATION            *         
* 19APR01 RHV - VOILA!                                                *         
*                                                                     *         
***********************************************************************         
*                                                                               
T80279   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T80279,R9                                                      
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
         CLI   FORMAT,C'S'         VCI III FORMAT                               
         BE    VCI                                                              
         CLI   FORMAT,C'U'         VCI III FORMAT                               
         BE    VCI                                                              
         CLI   FORMAT,C'D'         VCI III FORMAT                               
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
         AH    R2,=H'4'            YES - ALLOW ANOTHER 4 LINES                  
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
         SPACE 1                                                                
V5G      DS    0H                                                               
         CLI   PDAY,X'FF'         SPORTS BUY DESCRIPTIVE?                       
         BNE   V5H                                                              
         CLC   =C'PGM=',PSDESC                                                  
         BE    *+14                                                             
         MVC   PVTIM(L'PSDESC),PSDESC   SPORTS DESCRIPTIVE                      
         B     V50                                                              
         MVC   P+21(L'PSDESC),PSDESC    PGM                                     
         B     V50                                                              
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
DAYTAB   DS    0XL3                BIT START/END, CHAR DISPL, CHAR              
         DC    X'4000',C'M'        START                                        
         DC    X'2001',C'T'                                                     
         DC    X'1002',C'W'                                                     
         DC    X'0803',C'T'                                                     
         DC    X'0404',C'F'                                                     
         DC    X'0205',C'S'                                                     
         DC    X'0106',C'S'                                                     
         DC    X'FF'                                                            
*                                                                               
V7       DS    0H                                                               
         OC    PDAY,PDAY                                                        
         BZ    V7D                 MORE DATES                                   
         L     R6,AIO2                                                          
         USING RBUYREC,R6                                                       
         MVC   PVDPT+1(1),RBUYDPT  DAYPART                                      
*                                                                               
         SR    RF,RF                                                            
         MVI   ELCODE,2                                                         
         BAS   RE,GETEL                                                         
         BNE   V7D                                                              
*                                                                               
         ZIC   R1,BUYLIN                                                        
         SR    R1,R5                                                            
         LTR   R1,R1                                                            
         BZ    V7A                                                              
V7AA     BAS   RE,NEXTEL                                                        
         BNE   V7D                                                              
         BCT   R1,V7AA                                                          
*                                                                               
V7A      LA    RE,DAYTAB                                                        
V7B      IC    RF,0(RE)                                                         
         EX    RF,*+8                                                           
         B     *+8                                                              
         TM    3(R6),0                                                          
         BZ    V7C                                                              
         ZIC   RF,1(RE)                                                         
         LA    RF,PVDAY(RF)                                                     
         MVC   0(1,RF),2(RE)                                                    
V7C      LA    RE,L'DAYTAB(RE)                                                  
         CLI   0(RE),X'FF'                                                      
         BNE   V7B                                                              
*        BAS   RE,NEXTEL                                                        
*        BE    V7A                                                              
*                                                                               
V7D      MVC   PVTIM,PTIM          TIME                                         
***>     MVC   PVSEC,PSEC          SECTION                                      
***>     MVC   PVCLS,PCLS          CLASS                                        
***>     MVC   PVPLN,PPLN          PLAN                                         
         MVC   PVLEN,PLEN          LEN                                          
         MVC   PVWKS,PCLS                                                       
V8       MVC   PVDAT-1(1),PDAT-1   * FOR ALTERNATE WEEKS                        
         SPACE 1                                                                
         GOTO1 DATVAL,DMCB,(1,PDAT),WORK    DATES                               
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
         EDIT  (4,RBUYTCOS),(12,PVTCOS),2,COMMAS=YES,FLOAT=-                    
         DROP  R2,R6                                                            
         SPACE 1                                                                
V50      BAS   RE,GOSPOOL                                                       
         MVI   RCSUBPRG,2          FOR CONTINUATION PAGES                       
*                                  ONLY INCLUDE A FEW HEADLINES                 
         LA    R4,L'PRTLN(R4)                                                   
         BCT   R5,V5                                                            
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
         CLI   FORMAT,C'S'         VCI FORMAT                                   
         BE    HD10                                                             
         CLI   FORMAT,C'U'         VCI FORMAT                                   
         BE    HD10                                                             
         CLI   FORMAT,C'D'         VCI FORMAT/WO                                
         BE    HD10                                                             
         DC    H'0'                                                             
         SPACE 1                                                                
HD10     DC    0H'0'    ******VCI HEADLINES*********                            
         CLI   RCSUBPRG,1                                                       
         BNE   *+8                                                              
         MVI   RCSUBPRG,2          FOR CONTINUATION PAGES                       
*                                  ONLY INCLUDE A FEW HEADLINES                 
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
         MVC   H2+14(8),CONCNUM                                                 
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1F'                                                     
         BAS   RE,GETEL                                                         
         BNE   HD52                                                             
         USING RCONXEL,R6                                                       
         MVC   H3+18(10),RCONTRF                                                
         DROP  R6                                                               
*                                                                               
HD52     LR    RF,RA                                                            
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1D'                                                     
         BAS   RE,GETEL                                                         
         BNE   HD55                                                             
         USING RCONDREL,R6                                                      
         EDIT  RCONDRLK,(8,H3+39),ALIGN=LEFT,ZERO=NOBLANK                       
*                                                                               
HD55     LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF                                                       
         CLC   PAGE,=H'1'          FIRST PAGE?                                  
         BE    HD60                                                             
         NI    SPOOLIND,X'7F'      TURN OFF 'SKIP LINE' INDICATOR               
         TM    TWAWSFLG,X'80'           SHOULD WE PRINT BUYLINE HEADER?         
         BZ    XIT                                                              
         DROP  RF                                                               
         LA    R6,H4                                                            
         BAS   RE,VCIBLH                                                        
         B     XIT                                                              
         SPACE 1                                                                
HD60     MVI   MYHEDSW,C'Y'        MORE THAN 14 HEADLINES                       
         MVC   SVPRNT,P            SAVE PRINT LINE                              
         MVC   P,SPACES            BLANK OUT PRINT LINE                         
         MVI   XTRHED,9            MAXIMUM NUMBER OF EXTRA HEADLINES            
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
*                                                                               
HD80     DS    0H                                                               
         BAS   RE,CALCTOTS                                                      
         EDIT  HALF,(4,H4+11),ALIGN=LEFT,ZERO=NOBLANK                           
         EDIT  FULL,(13,H6+09),2,ALIGN=LEFT,FLOAT=$,ZERO=NOBLANK,      +        
               COMMAS=YES                                                       
*                                                                               
         MVC   H4+88(2),RCONKOFF                                                
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'9F'                                                     
         BAS   RE,GETEL                                                         
         BNE   *+10                                                             
         USING RCONXXEL,R6                                                      
         MVC   H6+48(L'RCONXAST),RCONXAST                                       
         DROP  R6                                                               
*                                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF                                                       
         MVC   H6+93(L'TWASALTL),TWASALTL                                       
         MVC   H8+93(L'TWASALFX),TWASALFX                                       
         DROP  RF                                                               
*                                                                               
         GOTO1 (RFSTAOUT,VREPFACS),DMCB,RCONKSTA,H8+14                          
*                                                                               
         MVC   H8+48(20),CONPRD   PRODUCT NAME                                  
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF                                                       
         CLC   SVCONPRD,SPACES                                                  
         BE    *+10                                                             
         MVC   H8+48(20),TWAPRDNM   PRODUCT NAME                                
*                                                                               
         MVC   H4+48(20),TWASALNM  SALESPERSON NAME                             
         MVC   H12+48(L'TWABUYER),TWABUYER                                      
*                                                                               
         MVC   H10+12(20),TWAADVNM                                              
         TM    TWAFMTFL,X'08'      CARE OF AGENCY?                              
         BZ    HD084               NO                                           
*                                                                               
         MVC   H12+8(4),=C'C/O '                                                
         MVC   H12+12(L'TWAAGNM1),TWAAGNM1  AGENCY NAME FROM SCREEN             
         B     HD086                                                            
*                                                                               
HD084    DS    0H                                                               
         MVC   H12+8(L'TWAAGNM1),TWAAGNM1                                       
         DROP  RF                                                               
*                                                                               
HD086    DS    0H                                                               
         GOTO1 DATCON,DMCB,(3,SVCONDTE),(5,H14+7)                               
         GOTO1 DATCON,DMCB,(3,SVCONDTE+3),(5,H14+22)                            
         MVC   H15+7(8),DASH                                                    
         MVC   H15+22(8),DASH                                                   
*                                                                               
         MVC   H14+42(05),=C'ADV#:'                                             
         MVC   H15+48(10),DASH                                                  
         MVC   H16+42(05),=C'AGY#:'                                             
         MVC   H17+48(10),DASH                                                  
         MVC   H18+42(05),=C'SAL#:'                                             
         MVC   H19+48(10),DASH                                                  
         MVC   H20+42(05),=C'OFF#:'                                             
         MVC   H21+48(10),DASH                                                  
*                                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)     4K                                           
         USING TWAWORK,RF                                                       
         MVC   H18+48(6),TWAEOPSL                                               
         MVC   H20+48(6),TWAEOPOF                                               
         DROP  RF                                                               
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'9F'                                                     
         BAS   RE,GETEL                                                         
         BNE   HD088                                                            
         USING RCONXXEL,R6                                                      
         MVC   H14+48(10),RCONXADV                                              
         MVC   H16+48(10),RCONXAGY                                              
         DROP  R6                                                               
*                                                                               
HD088    DS    0H                                                               
         MVC   H16+0(13),=C'ORDER STATUS:'                                      
         MVC   H17+14(07),DASH                                                  
*                                                                               
         MVC   H16+22(11),=C'ORDER TYPE:'                                       
         MVC   H17+34(03),DASH                                                  
         MVC   H16+34(1),RCONTYPE                                               
*                                                                               
         MVC   H18+00(13),=C'REVENUE TYPE:'                                     
         MVC   H19+14(24),DASH                                                  
*                                                                               
         MVC   H20+00(12),=C'DESCRIPTION:'                                      
         MVC   H21+13(24),DASH                                                  
*                                                                               
* DISPLAY EASI CODES (IF EXIST)                                                 
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'A2'                                                     
         BAS   RE,GETEL                                                         
         BNE   HD90                                                             
         USING RCONIEL,R6                                                       
         MVC   H22+00(8),=C'EI: ADV:'                                           
         MVC   H22+14(4),=C'PRD:'                                               
         MVC   H22+25(4),=C'PRD:'                                               
         MVC   H22+36(4),=C'EST:'                                               
         MVC   H23+09(4),DASH                                                   
         MVC   H23+19(4),DASH                                                   
         MVC   H23+30(4),DASH                                                   
         MVC   H23+41(10),DASH                                                  
         MVC   H22+09(4),RCONIADV                                               
         MVC   H22+19(4),RCONIPRD                                               
         MVC   H22+30(4),RCONIPR2                                               
         MVC   H22+41(10),RCONXEST                                              
         OC    H22+41(10),MYSPACES                                              
         CLC   H22+41(10),MYSPACES                                              
         BNE   *+10                                                             
         MVC   H22+41(4),RCONIEST                                               
         DROP  R6                                                               
*                                                                               
HD90     DS    0H                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         MVC   H14+74(34),TWAAGAD1                                              
         MVC   H16+74(36),TWAAGAD2                                              
         MVC   H18+74(36),TWAAGAD3                                              
         DROP  RF                                                               
*                                                                               
* DEAL WITH HEADLINES THAT DON'T FIT IN HEDSPECS                                
*                                                                               
HD115    DS    0H                                                               
         LR    RF,RA                                                            
         AH    RF,=Y(TWAWORKQ)                                                  
         USING TWAWORK,RF                                                       
         LA    R1,H20+74                                                        
         MVC   0(7,R1),=C'PHONE #'   AGENCY PHONE NUMBER                        
         MVC   8(3,R1),TWAAGYPH                                                 
         MVI   11(R1),C'-'                                                      
         MVC   12(3,R1),TWAAGYPH+3                                              
         MVI   15(R1),C'-'                                                      
         MVC   16(4,R1),TWAAGYPH+6                                              
*                                                                               
         LA    R1,H22+74                                                        
         MVC   0(5,R1),=C'FAX #'  AGENCY FAX NUMBER                             
         MVC   6(3,R1),TWAAFAX                                                  
         MVI   9(R1),C'-'                                                       
         MVC   10(3,R1),TWAAFAX+3                                               
         MVI   13(R1),C'-'                                                      
         MVC   14(4,R1),TWAAFAX+6                                               
         DROP  RF                                                               
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
         MVC   11(7,R6),=C'MTWTFSS'                                             
         MVC   36(3,R6),=C'#OF'                                                 
         MVC   59(6,R6),=C'SP/ CM'                                              
         LA    R6,132(R6)                                                       
         MVC   0(39,R6),=C'ACT DP LIN  U H AU      START - END WKS'             
***>>    MVC   0(39,R6),=C'ACT DP LIN MTWTFSS      START - END WKS'             
         MVC   40(36,R6),=C'TIME PERIOD  T/P/C WK  TYPE LEN RATE'               
         MVC   83(17,R6),=C'SP TYPE SPOT CASH'                                  
         LA    R6,132(R6)                                                       
         MVC   0(35,R6),=C'--- -- --- ------------ -----------'                 
         MVC   36(35,R6),=C'--- ------------- ---  --- ---- ---'                
         MVC   72(36,R6),=C'---------- ------- ---- ------------'               
VCIBLHX  B     XIT                                                              
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
         MVC   WORK2X(L'KEY),KEY                                                
         XC    FULL,FULL      ***  RETURN TOTAL COST IN FULL  ***               
         XC    HALF,HALF      ***  RETURN TOTAL SPOTS IN HALF ***               
         XC    KEY,KEY                                                          
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
         BNE   CTOT0100            DONE                                         
         OI    DMINBTS,X'08'                                                    
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
CTOT0100 DS    0H                                                               
         MVC   KEY,WORK2X               RESTORE BYREC SEQ LOOP                  
         OI    DMINBTS,X'08'       PASS DELETES                                 
         GOTO1 VHIGH                                                            
         CLC   KEY(L'RBUYKEY),KEYSAVE                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         B     XIT                                                              
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
         SPROG 1                ***VCI - PAGE 1 ONLY***                         
         PSPEC H4,1,C'TTL SPOTS:'                                               
         PSPEC H4,36,C'SALESPERSON:'                                            
         PSPEC H4,75,C'SALES OFFICE:'                                           
         PSPEC H5,12,C'----'                                                    
         PSPEC H5,49,C'--------------------'                                    
         PSPEC H5,89,C'--'                                                      
         PSPEC H6,1,C'TTL REV:'                                                 
         PSPEC H6,38,C'ASSISTANT:'                                              
         PSPEC H6,75,C'SALESPERSON PHONE#'                                      
         PSPEC H7,10,C'-------------'                                           
         PSPEC H7,49,C'--------------------'                                    
         PSPEC H8,1,C'CALL LETTERS:'                                            
         PSPEC H8,42,C'BRAND:'                                                  
         PSPEC H8,75,C'SALESPERSON FAX#'                                        
         PSPEC H9,15,C'-------'                                                 
         PSPEC H9,49,C'--------------------'                                    
         PSPEC H10,1,C'ADVERTISER:'                                             
         PSPEC H11,13,C'--------------------'                                   
         PSPEC H12,1,C'AGENCY:'                                                 
         PSPEC H12,36,C'BUYERS NAME:'                                           
         PSPEC H12,75,C'AGENCY ADDRESS:'                                        
         PSPEC H13,9,C'--------------------'                                    
         PSPEC H13,49,C'------------------'                                     
         PSPEC H14,1,C'START:           END:'                                   
         SPACE 1                                                                
         SPROG 1,2                ***VCI - ALL PAGES***                         
         PSPEC H1,46,C'ORDER WORKSHEET'                                         
         PSPEC H1,103,PAGE                                                      
         PSPEC H2,1,C'REP HEADLINE#'                                            
         PSPEC H3,1,C'STATION CONTRACT#'                                        
         PSPEC H3,37,C'DARE ORDER#'                                             
         SPACE 1                                                                
         SPROG 5                  ***VCI - STORED COMMENTS***                   
         PSPEC H1,46,C'ORDER WORKSHEET'                                         
         PSPEC H1,103,PAGE                                                      
         DC    X'00'                                                            
         EJECT                                                                  
       ++INCLUDE RECNTWR2K                                                      
       ++INCLUDE RECNTFMTD                                                      
       ++INCLUDE REGENPBYD                                                      
*    FORMAT FOR VCI WORKSHEET BUYLINES                                          
         SPACE 1                                                                
PVCID    DSECT                                                                  
PV       DS    0CL110                                                           
PVCHG    DS    CL2                 CHANGE CODE                                  
         DS    CL2                                                              
PVDPT    DS    CL2                 DAYPART CODE                                 
         DS    CL1                                                              
PVLIN    DS    CL3                 DDS LINE NUMBER                              
         DS    CL1                                                              
PVDAY    DS    CL12                DAY                                          
         DS    CL1                                                              
PVDAT    DS    CL11                DATE                                         
         DS    CL1                                                              
PVWKS    DS    CL3                 WEEKS                                        
         DS    CL1                                                              
PVTIM    DS    CL11                TIME                                         
         DS    CL3                                                              
PVTPC    DS    CL3                 TIME/PGM/CLASS                               
         DS    CL2                                                              
PVNPW    DS    CL3                 NUMBER PER WEEK                              
         DS    CL1                                                              
PVCMT    DS    CL4                 CM TYPE                                      
         DS    CL1                                                              
PVLEN    DS    CL3                 LENGTH                                       
         DS    CL1                                                              
PVRAT    DS    CL10                RATE                                         
         DS    CL1                                                              
PVSPT    DS    CL7                 SP TYPE                                      
         DS    CL1                                                              
PVTSPOT  DS    CL4                 TOTAL SPOTS                                  
         DS    CL1                                                              
PVTCOS   DS    CL12                TOTAL COST                                   
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'050RECNT79   06/23/08'                                      
         END                                                                    
