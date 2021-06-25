*          DATA SET PEMSG05    AT LEVEL 149 AS OF 05/01/02                      
*PHASE TE1D05A                                                                  
         TITLE 'TE1D05 - PROCESS MESSAGE'                                       
         MACRO                                                                  
&NAME    TKEY                                                                   
&NAME    L     R6,AIO                                                           
         CLC   KEY(36),0(R6)                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         MEND                                                                   
         MACRO                                                                  
&NAME    SUBNTR                                                                 
         USING &NAME,RB                                                         
&NAME    NTR1  BASE=(RF)                                                        
         MEND                                                                   
         MACRO                                                                  
&NAME    SUBXIT                                                                 
&NAME    XIT1                                                                   
         LTORG                                                                  
         DROP  RB                                                               
         MEND                                                                   
         MACRO                                                                  
&NAME    GOTOR &A                                                               
&NAME    L     RF,=A(&A)                                                        
         A     RF,RELO                                                          
         BASR  RE,RF                                                            
         MEND                                                                   
         MACRO                                                                  
         TBADLEN                                                                
         CLI   DMCB+12,5                                                        
         BNE   *+6                                                              
         DC    H'0'                                                             
         MEND                                                                   
         EJECT                                                                  
TE1D05   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 1000,**TE1D05,R7,RR=R4                                           
         ST    R4,RELO                                                          
         LR    R3,RC                                                            
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING TE1DFFD,RA      BASE SCREEN FOR SYSTEM + THIS PROG               
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R3,AMSGWORK                                                      
         LA    R4,KEY                                                           
         USING MSGKEY,R4                                                        
         EJECT                                                                  
*                              VECTOR ON COMMAND                                
         SPACE 3                                                                
         CLI   MSADFLG,0                                                        
         BE    V1                                                               
         MVI   MSADFLG,0                                                        
         MVC   PAGNAM1,=C'TEMPLIST'                                             
         MVI   PAGNAM1H+5,8                                                     
         MVC   KEY,KEYT                                                         
         GOTO1 HIGH                                                             
         MVC   SVMSGDA,MSGDA                                                    
         MVI   PAGCMMD,C'S'                                                     
         XC    SGNMSG,SGNMSG                                                    
         B     SREC                                                             
V1       CLI   PAGCMMDH+5,0                                                     
         BNE   VEC1                                                             
         CLI   PAGCMMD,C'D'        DISPLAY PAGE                                 
         BE    TOP                                                              
         CLI   PAGCMMD,C'A'        ADD NEW PAGE                                 
         BE    AREC                                                             
         CLI   PAGCMMD,C'G'                                                     
         BE    GREC                                                             
VEC1     CLI   PAGCMMD,C'D'        DELETE MESSAGE                               
         BE    XREC                                                             
*                                                                               
         BAS   RE,UPREC            UPDATE PAGE                                  
         OI    PAGCMMDH+6,X'40'                                                 
         CLI   SGNMSG,C'U'                                                      
         BE    XIT                                                              
         CLI   SGNMSG,C'D'                                                      
         BE    XIT                                                              
         NI    PAGCMMDH+6,X'BF'                                                 
*                                                                               
         CLI   PAGCMMD,C'T'        DISPLAY TOP                                  
         BE    TOP                                                              
         CLI   PAGCMMD,C'N'        DISPLAY NEXT                                 
         BE    NEXT                                                             
         CLI   PAGCMMD,C'P'        DISPLAY PREV                                 
         BE    PREV                                                             
         CLI   PAGCMMD,C'S'        SEND PAGE                                    
         BE    SREC                                                             
         CLI   PAGCMMD,C'F'        FILE PAGE                                    
         BE    FREC                                                             
         CLI   PAGCMMD,C'K'        KEEP PAGE                                    
         BE    XIT                                                              
         OI    PAGCMMDH+6,X'C0'                                                 
         MVC   SGNMSG(21),=C'PLEASE SELECT COMMAND'                             
         CLI   PAGCMMD,0                                                        
         BE    DREC                                                             
         MVC   SGNMSG(30),=C'COMMAND NOT RECOGNIZED        '                    
         OI    SGNMSGH+6,X'80'                                                  
         MVI   PAGCMMD,0                                                        
         B     DREC                                                             
         EJECT                                                                  
*                             DISPLAY TOP OF MESSAGE                            
         SPACE 3                                                                
TOP      MVC   MSGDA,SVMSGDA                                                    
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         MVC   KEY,0(R6)                                                        
         MVC   DKEY,KEY                                                         
         MVI   DCREC,0                                                          
         MVI   DCEL,1                                                           
         B     DREC                                                             
         EJECT                                                                  
*                              NEXT PAGE                                        
NEXT     ZIC   R3,DCEL    LOCATE NEXT PAGE START                                
         LA    R3,16(R3)                                                        
         STC   R3,DCEL                                                          
*                                                                               
         MVC   KEY,DKEY   GET CURRENT PAGE                                      
         MVC   MSGKPAGE,DCREC                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(36),KEYSAVE                                                  
         BNE   NEXTE1                                                           
         GOTO1 GETREC                                                           
*                                                                               
NEXT1    L     R6,AIO    LOOK FOR LINE                                          
         MVI   ELCODE,X'31'                                                     
         BAS   RE,GETEL                                                         
         BNE   NEXTE1                                                           
NEXT11   CLC   DCEL,2(R6)                                                       
         BE    DREC DONE                                                        
         BAS   RE,NEXTEL                                                        
         BE    NEXT11                                                           
*                                                                               
         MVI   DNEL,1    FIND LAST EL                                           
         L     R6,AIO                                                           
         BAS   RE,GETEL                                                         
NEXT2    CLC   DNEL,2(R6)                                                       
         BH    *+10                                                             
         MVC   DNEL,2(R6)                                                       
         BAS   RE,NEXTEL                                                        
         BE    NEXT2                                                            
*                                                                               
         ZIC   R1,DCREC  FIND NEXT REC                                          
         LA    R1,1(R1)                                                         
         STC   R1,DCREC                                                         
         STC   R1,MSGKPAGE                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(36),KEYSAVE                                                  
         BE    NEXT3                                                            
*                                                                               
NEXT21   ZIC   R1,DCREC  BACK TO LAST PG                                        
         BCTR  R1,0                                                             
         STC   R1,DCREC                                                         
         MVC   DCEL,DNEL USE LAST                                               
         B     DREC                                                             
*                                                                               
NEXT3    GOTO1 GETREC GET REC & LOOK FOR LINE                                   
         ZIC   R1,DCEL ADJUST EL PTR                                            
         ZIC   R0,DNEL                                                          
         SR    R1,R0                                                            
         STC   R1,DCEL                                                          
         L     R6,AIO    LOOK FOR LINE                                          
         MVI   ELCODE,X'31'                                                     
         BAS   RE,GETEL                                                         
         BNE   NEXT21                                                           
         B     NEXT11                                                           
*                                                                               
NEXTE1   MVC   DCREC(2),=X'0001'                                                
         B     DREC                                                             
         EJECT                                                                  
*                              PREVIOUS PAGE                                    
         SPACE 3                                                                
PREV     MVI   DNEL,16  LINES TO GO BACK                                        
*                                                                               
PREV0    CLC   DCEL,DNEL                                                        
         BNH   PREV1                                                            
*                                                                               
         ZIC   R1,DCEL                                                          
         ZIC   R0,DNEL                                                          
         SR    R1,R0                                                            
         STC   R1,DCEL                                                          
         B     DREC                                                             
*                                                                               
PREV1    CLI   DCREC,0                                                          
         BNE   PREV2                                                            
         MVI   DCEL,1                                                           
         B     DREC                                                             
*                                                                               
PREV2    ZIC   R1,DCEL                                                          
         ZIC   R0,DNEL                                                          
         SR    R0,R1                                                            
         STC   R0,DNEL                                                          
*                                                                               
         ZIC   R1,DCREC                                                         
         BCTR  R1,0                                                             
         STC   R1,DCREC                                                         
         MVC   KEY,DKEY                                                         
         STC   R1,MSGKPAGE                                                      
         GOTO1 HIGH                                                             
         GOTO1 GETREC                                                           
         TKEY                                                                   
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'31'                                                     
         MVI   DCEL,0                                                           
         BAS   RE,GETEL                                                         
*                                                                               
PREV3    CLC   DCEL,2(R6)                                                       
         BH    *+10                                                             
         MVC   DCEL,2(R6)                                                       
         BAS   RE,NEXTEL                                                        
         BE    PREV3                                                            
*                                                                               
         B     PREV0                                                            
         EJECT                                                                  
*                              DISPLAY RECORD                                   
         SPACE 3                                                                
DREC     CLI   PAGCMMD+1,C'D'  ALLOW XD CMMDS                                   
         BE    XREC                                                             
         CLI   PAGCMMD+1,C'K'                                                   
         BNE   *+12                                                             
         MVI   PAGCMMD,C'K'                                                     
         B     XIT                                                              
*                                  CHECK INBOX                                  
         MVC   KEY,DKEY                                                         
         MVC   PAGWTNG,SPACES                                                   
         OI    PAGWTNGH+6,X'80'                                                 
         CLC   MSGKFILE,=C'INBOX   '                                            
         BE    DREC0                                                            
         MVC   MSGKFILE,=C'INBOX   '                                            
         XC    KEY+21(15),KEY+21                                                
         GOTO1 HIGH                                                             
         CLC   KEY(21),KEYSAVE                                                  
         BNE   DREC0                                                            
         MVC   PAGWTNG,=C'MAIL WAITING'                                         
*                                                                               
DREC0    MVC   KEY,DKEY                                                         
         MVI   MSGKPAGE,0                                                       
         GOTO1 HIGH                                                             
         CLC   KEY(36),KEYSAVE                                                  
         BE    DREC1                                                            
*                   IF  BAD MSG-GET RID                                         
         CLC   KEY(35),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 READ                                                             
         OI    KEY+36,X'80'                                                     
         GOTO1 WRITE                                                            
         MVI   PAGCMMD,C'K'                                                     
         B     XIT                                                              
*                                                                               
DREC1    GOTO1 GETREC                                                           
         MVI   PAGCMMD,C' '                                                     
*                                                                               
         MVC   PAGFROM,SPACES                                                   
         LA    R3,PAGFROM                                                       
*                                  FROM                                         
         CLC   MSGKFROM,MSGNAM                                                  
         BNE   *+14                                                             
         CLC   MSGKFRAT,MSGAGY                                                  
         BE    OTFA1                                                            
         MVI   PAGFROM,C'F'                                                     
         MVC   PAGFROM+2(8),MSGKFROM                                            
*                                  AT                                           
         CLC   MSGKFRAT,TWAORIG                                                 
         BE    OTFA                                                             
         MVC   WORK+1(2),MSGKFRAT                                               
         MVI   WORK,1                                                           
         GOTO1 GETSGN                                                           
         MVC   PAGFROM+11(8),WORK                                               
         B     OTFA                                                             
*                           CHECK IF SENT                                       
OTFA1    MVI   ELCODE,X'33'                                                     
         L     R6,AIO                                                           
         BAS   RE,GETEL                                                         
         BNE   OTFA                                                             
         MVI   PAGFROM,C'T'                                                     
         MVC   PAGFROM+2(8),4(R6)   FROM                                        
         MVC   PAGFROM+11(8),12(R6) TO                                          
OTFA     LA    R3,PAGFROM+20                                                    
*                                  ADJUST DATE AND TIME                         
         MVC   WORK(1),SGNONOO                                                  
         CLC   OPNFILE,=C'INBOX   '                                             
         BNE   *+10                                                             
         MVC   WORK(1),SGNONOI                                                  
         CLI   WORK,C'F'                                                        
         BE    *+10                                                             
         XC    MSGKDATE(4),=X'FFFFFFFF'                                         
*                                  DATE                                         
         GOTO1 DATCON,DMCB,(2,MSGKDATE),(5,WORK)                                
         MVC   0(8,R3),WORK                                                     
         LA    R3,9(R3)                                                         
*                                  TIME                                         
         SR    R0,R0                                                            
         ST    R0,WORK                                                          
         MVC   WORK+2(2),MSGKTIME                                               
         L     R1,WORK                                                          
         D     R0,=F'30'                                                        
         SR    R0,R0                                                            
         D     R0,=F'60'                                                        
         LA    R1,8(R1)                                                         
         CVD   R1,WORK       HOURS                                              
         UNPK  WORK(4),WORK+6(3)                                                
         MVC   0(2,R3),WORK                                                     
         MVI   2(R3),C':'                                                       
         CVD   R0,WORK       MINUTES                                            
         UNPK  WORK(4),WORK+6(3)                                                
         MVC   3(2,R3),WORK                                                     
*                                                                               
         MVI   MAX,1           SUBJECT                                          
         LA    R2,PAGSUBJH                                                      
         MVI   ELCODE,X'30'                                                     
         GOTO1 DISPCHAT                                                         
         CLI   SGNONUC,C'Y'                                                     
         BNE   *+10                                                             
         OC    PAGSUBJ,SPACES                                                   
         CLC   DCREC(2),=X'0001'                                                
         BE    *+8                                                              
         OI    PAGSUBJH+6,X'20'                                                 
*                                                                               
         L     R6,AIO          CC                                               
         OI    PAGCOPYH+6,X'80'                                                 
         MVI   ELCODE,X'32'                                                     
         BAS   RE,GETEL                                                         
         MVC   PAGCOPY,SPACES                                                   
         BNE   *+10                                                             
         MVC   PAGCOPY,=C'COPY'                                                 
*                              LINES                                            
         MVC   DNEL,DCEL          ELEMENT PTR                                   
         LA    R3,17              LINE CT                                       
         LA    R2,PAGLINEH        LINE PTR                                      
         LA    R1,321                                                           
         STH   R1,DADD            DISPLAY ADDRESS                               
         MVI   ELCODE,X'31'                                                     
*                                                                               
         ZIC   R1,DCREC           GET FIRST REC                                 
         STC   R1,DNREC                                                         
         MVC   KEY,DKEY                                                         
         STC   R1,MSGKPAGE                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(36),KEYSAVE                                                  
         MVC   KEY(36),KEYSAVE                                                  
         BNE   DRLPCMD1                                                         
         GOTO1 GETREC                                                           
*                                                                               
DRLP     XC    8(78,R2),8(R2)     LOOK FOR EL                                   
         OI    6(R2),X'80'                                                      
         L     R6,AIO                                                           
         BAS   RE,GETEL                                                         
DRLP2    BNE   DRLP1                                                            
         CLC   DNEL,2(R6)                                                       
         BE    DRLPMV                                                           
         BAS   RE,NEXTEL                                                        
         B     DRLP2                                                            
*                                                                               
DRLP1    ZIC   R1,MSGKPAGE        LOOK FOR ANOTHER REC                          
         LA    R1,1(R1)                                                         
         STC   R1,MSGKPAGE                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(36),KEYSAVE                                                  
         MVC   KEY(36),KEYSAVE                                                  
         BNE   DRLPCMD1                                                         
*                                                                               
         MVI   DNEL,1             SET TO FIRST EL                               
         MVC   DNREC,MSGKPAGE                                                   
         GOTO1 GETREC                                                           
         B     DRLP                                                             
*                                                                               
DRLPMV   CLI   SGNONDA,C'Y'       SUPPRESS ADDRESSES?                           
         BE    *+12                                                             
         TM    3(R6),X'40'                                                      
         BNZ   DRLPMV2                                                          
*                                                                               
         XC    0(86,R2),0(R2)     MAKE FIELD                                    
         MVI   0(R2),86                                                         
         MVI   1(R2),X'40'                                                      
         LH    R1,DADD                                                          
         AH    R1,=H'80'                                                        
         STH   R1,DADD                                                          
         STH   R1,2(R2)                                                         
         OI    6(R2),X'80'                                                      
         ZIC   R1,1(R6)           MOVE IN LINE                                  
         SH    R1,=H'4'                                                         
         BZ    DRLPMV1                                                          
         CH    R1,=H'78'                                                        
         BNH   *+8                                                              
         LA    R1,78                                                            
         BCTR  R1,0                                                             
         EXMVC R1,8(R2),4(R6)                                                   
         CLI   SGNONUC,C'Y'                                                     
         BNE   DRLPMV1                                                          
         EX    R1,*+8                                                           
         B     DRLPMV1                                                          
         OC    8(0,R2),SPACES                                                   
*                                                                               
DRLPMV1  NI    6(R2),X'EF'       SET PROTECT STATUS                             
         TM    3(R6),X'80'           CHECK SENT                                 
         BO    DRLPMV1A                                                         
         TM    3(R6),X'20'           CHECK FORMATTED                            
         BZ    DRLPMV1B                                                         
DRLPMV1A OI    6(R2),X'20'                                                      
*                                                                               
DRLPMV1B TM    3(R6),X'20'    CHECK FORMATTED                                   
         BZ    DRLPCMD                                                          
         TM    3(R6),X'80'    EXPLODE IF NOT SENT                               
         BZ    DRLPMV1D                                                         
*                                                                               
         LA    R0,78          EDIT OUT # SIGNS  IF SENT                         
         LA    R1,8(R2)                                                         
DRLPMV1C CLI   0(R1),C'#'                                                       
         BNE   *+8                                                              
         MVI   0(R1),C' '                                                       
         LA    R1,1(R1)                                                         
         BCT   R0,DRLPMV1C                                                      
         B     DRLPCMD                                                          
*                                                                               
DRLPMV1D GOTO1 DRLPEXPL      EXPLODE FORMATTED LINE                             
*                                                                               
DRLPCMD  DS    0H  ***********TEMP  DISP / CMMD GOES HERE                       
*                                                                               
         BAS   RE,BUMP             BUMP TO NEXT LINE                            
*                                                                               
DRLPMV2  ZIC   R1,DNEL             BUMP TO NEXT EL                              
         LA    R1,1(R1)                                                         
         STC   R1,DNEL                                                          
*                                                                               
         BCT   R3,DRLP                                                          
         B     DRLPCMD2                                                         
*                                                                               
DRLPCMD1 XC    0(86,R2),0(R2)       CLEAR REST OF PAGE                          
         MVI   0(R2),86                                                         
         MVI   1(R2),X'40'                                                      
         LH    R1,DADD                                                          
         AH    R1,=H'80'                                                        
         STH   R1,DADD                                                          
         STH   R1,2(R2)                                                         
         OI    6(R2),X'80'                                                      
         BAS   RE,BUMP                                                          
         BCT   R3,DRLPCMD1                                                      
*                                                                               
DRLPCMD2 MVI   0(R2),37        BUILD LABEL LINES                                
         MVI   1(R2),X'20'                                                      
         MVC   2(2,R2),=H'1762'                                                 
         MVC   4(2,R2),=X'0000'                                                 
         MVC   6(2,R2),=X'8000'                                                 
         MVC   8(23,R2),=C'S-SEND F-FILE D-DELETE '                             
         MVC   31(6,R2),=C'K-KEEP'                                              
         CLC   OPNFILE,=C'INBOX   '                                             
         BNE   *+10                                                             
         MVC   31(6,R2),SPACES                                                  
         BAS   RE,BUMP                                                          
*                                                                               
         MVI   0(R2),12      MORE                                               
         MVI   1(R2),X'28'                                                      
         MVC   2(2,R2),=H'1824'                                                 
         MVC   4(2,R2),=X'0000'                                                 
         MVC   6(2,R2),=X'8000'                                                 
         MVC   KEY,DKEY                                                         
DRLP5    MVC   MSGKPAGE,DNREC                                                   
DRLP5A   MVC   8(4,R2),=C'LAST'                                                 
         GOTO1 HIGH                                                             
         CLC   KEY(36),KEYSAVE                                                  
         BNE   DISPD                                                            
         GOTO1 GETREC                                                           
         MVC   8(4,R2),=C'MORE'                                                 
         L     R6,AIO                                                           
         BAS   RE,GETEL                                                         
DRLP3    BNE   DRLP4                                                            
         CLC   DNREC,MSGKPAGE                                                   
         BNE   DISPD                                                            
         CLC   DNEL,2(R6)                                                       
         BE    DISPD                                                            
         BAS   RE,NEXTEL                                                        
         B     DRLP3                                                            
DRLP4    ZIC   R1,MSGKPAGE                                                      
         LA    R1,1(R1)                                                         
         STC   R1,MSGKPAGE                                                      
         B     DRLP5A                                                           
*                                                                               
DISPD    CLC   8(4,R2),=C'LAST'                                                 
         BNE   DISPD1                                                           
         CLC   DCREC(2),=X'0001'                                                
         BNE   DISPD1                                                           
         MVC   8(4,R2),SPACES                                                   
DISPD1   BAS   RE,BUMP                                                          
*                                                                               
         MVI   0(R2),27                                                         
         MVI   1(R2),X'20'                                                      
         MVC   2(2,R2),=H'1842'                                                 
         MVC   4(2,R2),=X'0000'                                                 
         MVC   6(2,R2),=X'8000'                                                 
         MVC   8(19,R2),=C'N-NEXT P-PREV T-TOP'                                 
         BAS   RE,BUMP                                                          
*                                                                               
         MVI   0(R2),14                                                         
         MVI   1(R2),X'20'                                                      
         MVC   2(2,R2),=H'1899'                                                 
         MVC   4(2,R2),=X'0000'                                                 
         MVC   6(2,R2),=X'8000'                                                 
         MVC   8(6,R2),=C'M-MENU'                                               
         BAS   RE,BUMP                                                          
*                                                                               
         MVC   0(3,R2),=X'000101'    INDICS                                     
*                                                                               
         OI    PAGCMMDH+6,X'40'                                                 
         B     XIT                                                              
         EJECT                                                                  
*                              EXPLOAD FORMATTED LINE                           
         SPACE 3                                                                
DRLPEXPL NTR1                                                                   
*                                                                               
*         R2 POINTS TO HEADER OF A 78 CHAR PROTECTED LINE TO                    
*         BE EXPLODED.                                                          
*                                                                               
         LA    R3,8(0,R2)     START OF DATA AREA                                
         LA    R4,78          COUNT OF CHARS TO CHECK                           
         LA    R5,0           LENGTH OF THIS FIELD                              
*                                                                               
DREXP1   CLI   0(R3),C'#'     CHECK FOR ENTRY FIELD                             
         BE    DREXP2                                                           
         LA    R3,1(0,R3)                                                       
         LA    R5,1(0,R5)                                                       
         BCT   R4,DREXP1                                                        
         LA    R5,8(0,R5)                                                       
         STC   R5,0(R2)                                                         
         B     DREXPEX                                                          
*                                                                               
DREXP2   LA    R6,8(0,R5)      COMPLETE PROTECTED FIELD                         
         STC   R6,0(R2)                                                         
         BCT   R4,*+8                                                           
         B     DREXPEX                                                          
*                                                                               
         MVC   WORK(2),2(R2)     SAVE SCREEN ADDR                               
         MVC   WORK+2(80),0(R3)  SAVE REST OF LINE                              
         LA    R2,0(R2,R6)                                                      
*                                                                               
         MVC   0(8,R2),=X'0048000000008000'  START ENTRY FIELD                  
         LH    R1,WORK                                                          
         LA    R1,1(R1,R5)                                                      
         STH   R1,WORK                                                          
         MVC   2(2,R2),WORK                                                     
         MVC   8(80,R2),WORK+3                                                  
*                                                                               
         LA    R3,8(0,R2)     START OF DATA AREA                                
         LA    R5,0           LENGTH OF THIS FIELD                              
*                                                                               
         CLI   0(R3),C'&&'    ERASE & IF FORCED INPUT FIELD                     
         BNE   *+8                                                              
         MVI   0(R3),C' '                                                       
*                                                                               
DREXP3   CLI   0(R3),C'#'     CHECK FOR END OF FIELD                            
         BE    DREXP4                                                           
         LA    R3,1(0,R3)                                                       
         LA    R5,1(0,R5)                                                       
         BCT   R4,DREXP3                                                        
         LA    R5,8(0,R5)                                                       
         STC   R5,0(R2)                                                         
         B     DREXPEX                                                          
*                                                                               
DREXP4   LA    R6,8(0,R5)      COMPLETE ENTRY FIELD                             
         STC   R6,0(R2)                                                         
         BCT   R4,*+8                                                           
         B     DREXPEX                                                          
*                                                                               
         MVC   WORK(2),2(R2)     SAVE SCREEN ADDR                               
         MVC   WORK+2(80),0(R3)  SAVE REST OF LINE                              
         LA    R2,0(R2,R6)                                                      
*                                                                               
         MVC   0(8,R2),=X'0060000000008000'  START PROTECTED FIELD              
         LH    R1,WORK                                                          
         LA    R1,1(R1,R5)                                                      
         STH   R1,WORK                                                          
         MVC   2(2,R2),WORK                                                     
         MVC   8(80,R2),WORK+3                                                  
*                                                                               
         LA    R3,8(0,R2)     START OF DATA AREA                                
         LA    R5,0           LENGTH OF THIS FIELD                              
         B     DREXP1                                                           
*                                                                               
DREXPEX  XIT1  REGS=(R2)                                                        
         EJECT                                                                  
*                              ADD RECORD                                       
         SPACE 3                                                                
AREC     XC    KEY,KEY                                                          
         MVC   MSGAGY,TWAORIG                                                   
         MVI   MSGSYS,C'M'                                                      
         MVC   MSGNAM,SGNONUN                                                   
         MVI   MSGKTYPE,MSGKTYPQ                                                
         MVC   MSGKFILE,OPNFILE                                                 
*                                                                               
         MVC   MSGKFROM,MSGNAM      FROM SELF                                   
         MVC   MSGKFRAT,MSGAGY                                                  
*                                   DATE                                        
         GOTO1 DATCON,DMCB,(5,WORK),(2,MSGKDATE)                                
*                                   TIME                                        
         GOTO1 GETFACT,DMCB,(2,0)                                               
         L     R1,DMCB                                                          
         L     R1,0(R1)                                                         
         SR    R0,R0                                                            
         D     R0,=F'200'                                                       
         ST    R1,WORK                                                          
         MVC   MSGKTIME,WORK+2                                                  
*                                   ADJUST ORDER                                
         MVC   WORK(1),SGNONOO                                                  
         CLC   MSGKFILE,=C'INBOX   '                                            
         BNE   *+10                                                             
         MVC   WORK(1),SGNONOI                                                  
         CLI   WORK,C'F'                                                        
         BE    *+10                                                             
         XC    MSGKDATE(4),=X'FFFFFFFF'                                         
*                                                                               
         MVC   KEYSAVE,KEY                                                      
         L     RE,AIO                                                           
         LA    RF,128                                                           
         XCEF                                                                   
         L     RE,AIO                                                           
         MVC   0(36,RE),KEY                                                     
*                                                                               
         MVC   ELEMENT(4),=X'31040100'                                          
         GOTO1 ADDELEM                                                          
*                                                                               
         GOTO1 ADDREC                                                           
*                                                                               
         MVC   SVMSGDA,KEY                                                      
         MVI   PAGCMMD,0                                                        
         B     TOP                                                              
         EJECT                                                                  
*                              GET FORMAT RECORD                                
         SPACE 3                                                                
GREC     XC    KEY,KEY         BUILD TO KEY                                     
         MVC   MSGAGY,TWAORIG                                                   
         MVI   MSGSYS,C'M'                                                      
         MVC   MSGNAM,SGNONUN                                                   
         MVI   MSGKTYPE,MSGKTYPQ                                                
         MVC   MSGKFILE,OPNFILE                                                 
*                                                                               
         MVC   MSGKFROM,MSGNAM      FROM SELF                                   
         MVC   MSGKFRAT,MSGAGY                                                  
*                                   DATE                                        
         GOTO1 DATCON,DMCB,(5,WORK),(2,MSGKDATE)                                
*                                   TIME                                        
         GOTO1 GETFACT,DMCB,(2,0)                                               
         L     R1,DMCB                                                          
         L     R1,0(R1)                                                         
         SR    R0,R0                                                            
         D     R0,=F'200'                                                       
         ST    R1,WORK                                                          
         MVC   MSGKTIME,WORK+2                                                  
*                                   ADJUST ORDER                                
         CLI   SGNONOO,C'F'                                                     
         BE    *+10                                                             
         XC    MSGKDATE(4),=X'FFFFFFFF'                                         
*                                                                               
         MVC   KEYT,KEY              THIS IS 'TO' KEY                           
*                                                                               
*                              NOW LOOK FOR MESSAGE TO COPY                     
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(13),KEYT                                                     
         MVC   MSGKFILE,=C'FORMATS '                                            
         GOTO1 HIGH                                                             
         B     GREC1A                                                           
GREC1    GOTO1 SEQ                                                              
GREC1A   CLC   KEY(21),KEYSAVE                                                  
         BNE   GREC2                                                            
         CLI   MSGKPAGE,0                                                       
         BNE   GREC1                                                            
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         MVI   ELCODE,X'30'                                                     
         BAS   RE,GETEL   GET SUBJECT                                           
         ZIC   R1,1(R6)                                                         
         SH    R1,=H'5'                                                         
         BL    GREC1A1                                                          
         EX    R1,*+8                                                           
         B     GREC1A1                                                          
         OC    4(0,R6),SPACES                                                   
GREC1A1  LA    R2,PAGNAM1H                                                      
         GOTO1 ANY                                                              
         ZIC   R1,1(R6)                                                         
         SH    R1,=H'5'                                                         
         EXCLC R1,WORK,4(R6)                                                    
         BNE   GREC1  NOT THIS ONE                                              
*                                                                               
GREC3    MVC   KEYF,KEY                                                         
         MVI   COPYFLG,C'N'                                                     
         MVI   COPYNO,0                                                         
         BAS   RE,MOVREC                                                        
*                                                                               
         MVC   KEY,KEYT                                                         
         MVI   MSGKPAGE,0                                                       
         GOTO1 HIGH                                                             
         MVC   SVMSGDA,MSGDA                                                    
         MVI   PAGCMMD,0                                                        
         B     TOP                                                              
*                                                                               
GREC2    XC    KEY,KEY           CHECK GENERIC                                  
         MVI   KEY,C'M'                                                         
         MVC   MSGAGY,TWAORIG                                                   
         MVC   MSGNAM,=C'SYS     '                                              
         MVI   USRKTYP,USRKTYPQ                                                 
         MVC   USRNAME,SGNONUN                                                  
         GOTO1 HIGH                                                             
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         MVI   ELCODE,X'24'                                                     
         BAS   RE,GETEL   GET PROFILE NAME                                      
         BNE   GREC4                                                            
*                                                                               
         MVC   WORK,SPACES CONVERT TO AGY CODE                                  
         ZIC   R1,1(R6)                                                         
         SH    R1,=H'5'                                                         
         EXMVC R1,WORK+1,4(R6)                                                  
         MVI   WORK,2                                                           
         GOTO1 GETSGN                                                           
*                                                                               
         XC    KEY,KEY       BUILD GENERIC KEY                                  
         MVI   KEY,C'M'                                                         
         MVC   MSGAGY,WORK                                                      
         MVC   MSGNAM,=C'ALL     '                                              
         MVI   MSGKTYPE,MSGKTYPQ                                                
         MVC   MSGKFILE,=C'FORMATS '                                            
         GOTO1 HIGH                                                             
         B     GREC2B                                                           
GREC2A   GOTO1 SEQ                                                              
GREC2B   CLC   KEY(21),KEYSAVE                                                  
         BNE   GREC4                                                            
         CLI   MSGKPAGE,0                                                       
         BNE   GREC2A                                                           
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         MVI   ELCODE,X'30'                                                     
         BAS   RE,GETEL   GET SUBJECT                                           
         ZIC   R1,1(R6)                                                         
         SH    R1,=H'5'                                                         
         BL    GREC2B1                                                          
         EX    R1,*+8                                                           
         B     GREC2B1                                                          
         OC    4(0,R6),SPACES                                                   
GREC2B1  LA    R2,PAGNAM1H                                                      
         GOTO1 ANY                                                              
         ZIC   R1,1(R6)                                                         
         SH    R1,=H'5'                                                         
         EXCLC R1,WORK,4(R6)                                                    
         BNE   GREC2A NOT THIS ONE                                              
         B     GREC3  FOUND                                                     
*                                                                               
GREC4    MVC   SGNMSG(20),=C'GET FORMAT NOT FOUND'                              
         B     AREC                                                             
         EJECT                                                                  
*                              DELETE RECORD                                    
         SPACE 3                                                                
XREC     MVI   PAGCMMD,C'N'                                                     
         MVC   KEY,DKEY                                                         
         MVI   MSGKPAGE,0                                                       
*                                                                               
XREC1    GOTO1 HIGH                                                             
         CLC   KEY(36),KEYSAVE                                                  
         BNE   XIT                                                              
*                                                                               
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         OI    38(R6),X'80'                                                     
         GOTO1 PUTREC                                                           
*                                                                               
         GOTO1 READ                                                             
         OI    KEY+36,X'80'                                                     
         GOTO1 WRITE                                                            
*                                                                               
         ZIC   R1,MSGKPAGE                                                      
         LA    R1,1(R1)                                                         
         CH    R1,=H'255'                                                       
         BE    XIT                                                              
*                                                                               
         STC   R1,MSGKPAGE                                                      
         B     XREC1                                                            
         EJECT                                                                  
*                              UPDATE RECORD                                    
         SPACE 3                                                                
UPREC    NTR1                      GET RECORD                                   
         NI    PAGSUBJH+1,X'DF'                                                 
         MVC   KEY,DKEY                                                         
         MVI   MSGKPAGE,0                                                       
         GOTO1 HIGH                                                             
         CLC   KEY(36),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETREC                                                           
*                                                                               
         MVI   MAX,1               SUBJECT                                      
         LA    R2,PAGSUBJH                                                      
         MVI   ELCODE,X'30'                                                     
         GOTO1 VALICHAT                                                         
*                                                                               
UPREC1   GOTO1 PUTREC                                                           
*                                  LINES                                        
         LA    R3,17           LINE CT                                          
         LA    R2,PAGLINEH     LINE PTR                                         
         L     R5,AMSGWORK     WORKAREA PTR                                     
         XC    0(4,R5),0(R5)   INIT WORKAREA                                    
         MVC   MSGKPAGE,DCREC  CURRENT REC                                      
         MVC   ELPTR,DCEL      EL PTR                                           
         GOTO1 HIGH                                                             
         CLC   KEY(36),KEYSAVE                                                  
         MVC   KEY(36),KEYSAVE                                                  
         BNE   UPREC3                                                           
         GOTO1 GETREC                                                           
*                                                                               
UPREC1A  CLI   ELPTR,0   CHECK VALID REC                                        
         BE    UPREC3                                                           
         L     R6,AIO          LOCATE EL                                        
         MVI   ELCODE,X'31'    LINE EL                                          
         BAS   RE,GETEL                                                         
UPREC2   BE    UPREC4                                                           
         ZIC   R1,MSGKPAGE                                                      
         LA    R1,1(R1)                                                         
         STC   R1,MSGKPAGE                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(36),KEYSAVE                                                  
         MVC   KEY(36),KEYSAVE                                                  
         BNE   UPREC3                                                           
         GOTO1 GETREC                                                           
         MVI   ELPTR,1                                                          
         B     UPREC1A                                                          
*                                                                               
UPREC3   MVC   0(5,R5),=X'0004000000'  NO EL..MAKE DUMMY                        
         LA    R1,4                                                             
         MVI   ELPTR,0                                                          
         B     UPREC6                                                           
*                                                                               
UPREC4   CLC   ELPTR,2(R6)                                                      
         BE    UPREC5                                                           
         BAS   RE,NEXTEL                                                        
         B     UPREC2                                                           
*                                                                               
UPREC5   ZIC   R1,1(R6)  MOVE EL TO WORKAREA                                    
         EXMVC R1,0(R5),0(R6)                                                   
         MVI   0(R5),0  CLEAR CMMD                                              
*                                                                               
         CLI   SGNONDA,C'Y'    SKIP MERGE IF SUPPRESSED ADDRESS                 
         BE    UPREC5A                                                          
         TM    3(R5),X'40'                                                      
         BZ    UPREC5A                                                          
         LA    R5,0(R1,R5)                                                      
         GOTOR REMBLNK                                                          
         B     UPRECAA                                                          
*                                                                               
UPREC5A  TM    3(R5),X'20'     TEST FOR FORMATTED MESSAGE LINE                  
         BZ    UPREC6                                                           
         TM    3(R5),X'80'                                                      
         BO    UPREC9B    NO FORMATTING IF SENT                                 
         GOTO1 UPEXP      UPDATE EXPLOADED LINE                                 
         CLI   SGNMSG,C'D'                                                      
         BE    XIT                                                              
         ZIC   R1,1(R6)                                                         
         B     UPREC9B                                                          
*                                                                               
UPREC6   CLI   8(R2),C'.'      TEST FOR . CMMD                                  
         BNE   UPREC7                                                           
********************************** . CMMD PROCESSING GOES HERE                  
         B     UPREC9                                                           
*                                                                               
UPREC7   TM    3(R5),X'80'  PREVENT UPDATE OF LOCKED ELS                        
         BNZ   UPREC9                                                           
         CLI   8(R2),C'/'      TEST FOR / CMMD                                  
         BNE   UPREC8                                                           
         OC    9(2,R2),SPACES                                                   
         LA    R6,UPREC7A                                                       
UPREC71  CLC   9(2,R2),0(R6)                                                    
         BNE   *+14                                                             
         MVC   0(1,R5),2(R6)                                                    
         B     UPREC9                                                           
         LA    R6,3(R6)                                                         
         CLI   0(R6),C' '                                                       
         BNE   UPREC71                                                          
         LA    R6,UPREC7B                                                       
UPREC72  CLC   9(3,R2),0(R6)                                                    
         BNE   *+14                                                             
         MVC   0(1,R5),3(R6)                                                    
         B     UPREC9                                                           
         LA    R6,4(R6)                                                         
         CLI   0(R6),C' '                                                       
         BNE   UPREC72                                                          
         MVC   SGNMSG(25),=C'UNRECOGNIZED EDIT COMMAND'                         
         OI    6(R2),X'40'                                                      
         B     XIT                                                              
UPREC7A  DC    C'D 1A 2M 3C 4I 5 '                                              
UPREC7B  DC    C'DD 6MM 7CC 8 '                                                 
*                                                                               
UPREC8   ZIC   R1,5(R2)     UPDATE EL IN WORK FROM SCREEN                       
         LA    R1,4(R1) COULD BE 0                                              
         EXMVC R1,4(R5),8(R2)                                                   
         STC   R1,1(R5)                                                         
*                                                                               
UPREC9   CLI   0(R5),C'5'       DO /I CMMD                                      
         BNE   UPREC9A                                                          
         MVI   0(R5),0                                                          
         LA    R5,0(R1,R5)    UPDATE WORKA PTR                                  
         MVC   0(4,R5),=X'00040000'                                             
         LA    R5,4(R5)                                                         
         B     UPRECA                                                           
UPREC9A  CLI   0(R5),C'1'       DO /D CMMD                                      
         BE    UPRECA                                                           
UPREC9B  LA    R5,0(R1,R5)    UPDATE WORKA PTR                                  
         GOTOR REMBLNK                                                          
*                                                                               
UPRECA   BAS   RE,BUMP   NEXT LINE                                              
UPRECAA  CLI   ELPTR,0 NO MORE?                                                 
         BE    UPRECA1                                                          
         ZIC   R1,ELPTR  NEXT EL                                                
         LA    R1,1(R1)                                                         
         STC   R1,ELPTR                                                         
UPRECA1  BCT   R3,UPREC1A DO REST OF PAGE                                       
         MVI   0(R5),0                                                          
*                                                                               
*******************CHECK / COMMAND COMPATIBILITY HERE                           
*                                                                               
         MVC   RSREC(4),DCREC   PUT WORKAREA BACK IN RECS                       
         GOTOR RUNOUT                                                           
*************************** DD PROCESSING GOES HERE                             
*                                                                               
*************************** CM PROCESSING GOES HERE                             
*                                                                               
         MVC   MSGKPAGE,DCREC    CHECK FOR OFF END OF MSG                       
         GOTO1 HIGH                                                             
         CLC   KEY(36),KEYSAVE                                                  
         MVC   KEY(36),KEYSAVE                                                  
         BNE   UPRECE1                                                          
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         MVI   ELCODE,X'31'                                                     
         BAS   RE,GETEL                                                         
         BNE   UPRECE1                                                          
UPRECE2  CLC   DCEL,2(R6)                                                       
         BE    XIT                                                              
         BAS   RE,NEXTEL                                                        
         BE    UPRECE2                                                          
UPRECE1  MVC   DCREC(2),=X'0001'                                                
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
*                               UPDATE EXPLOADED LINE                           
         SPACE 3                                                                
UPEXP    NTR1                                                                   
*                                                                               
*        THIS MODULE UPDATES A FORMATTED MESSAGE LINE THAT                      
*        HAS BEEN EXPLOADED INTO A SERIES OF PROTECTED AND                      
*        UNPROTECTED FIELDS ON THE SCREEN.  ON ENTRY R5                         
*        POINTS TO THE ELEMENT TO BE UPDATED, IT'S LENGTH                       
*        IS ALLOWED TO BE CHANGED, AND R2 POINTS TO THE                         
*        HEADER OF THE FIRST SCREEN FIELD CORRESPONDING                         
*        TO THIS ELEMENT.  ON EXIT, R2 SHOULD BE RETURNED                       
*        POINTING TO THE HEADER OF THE LAST FIELD OF THE                        
*        ELEMENT                                                                
*                                                                               
         LA    R3,78                COUNT OF REMAINING CHARS                    
         LA    R5,4(R5)             START OF DATA                               
*                                                                               
UPEXP1   TM    1(R2),X'20'                                                      
         BO    UPEXP3               ONLY UPDATE UNPROTECTED FIELDS              
*                                                                               
         MVC   WORK,SPACES          GET INPUT DATA                              
         CLI   5(R2),0                                                          
         BE    UPEXP2                                                           
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EXMVC R1,WORK,8(R2)                                                    
*                                                                               
UPEXP2   CLI   0(R5),C'&&'           CHECK FORCED FIELD                         
         BNE   UPEXP2A                                                          
         CLI   WORK,C' '                                                        
         BNE   UPEXP2A                                                          
         ZIC   R1,0(R2)                                                         
         SH    R1,=H'10'                                                        
         EX    R1,UPCLC                                                         
         BNE   UPEXP2A                                                          
         OI    6(R2),X'40'                                                      
         MVC   SGNMSG(19),=C'DATA ENTRY REQUIRED'                               
         XIT1                                                                   
UPCLC    CLC   1(0,R5),WORK+1                                                   
*                                                                               
UPEXP2A  ZIC   R1,0(R2)                                                         
         SH    R1,=H'9'                                                         
         EX    R1,*+8                                                           
         B     UPEXP3                                                           
         MVC   0(0,R5),WORK                                                     
*                                                                               
UPEXP3   ZIC   R1,0(R2)                                                         
         SH    R1,=H'7'                                                         
         AR    R5,R1                UPDATE START OF REST OF DATA                
         SR    R3,R1                                                            
         BNH   UPEXPX               CHECK DONE                                  
         BAS   RE,BUMP                                                          
         B     UPEXP1                                                           
*                                                                               
UPEXPX   XIT1  REGS=(R2)                                                        
         EJECT                                                                  
*                              FILE THE MESSAGE                                 
         SPACE 3                                                                
FREC     LA    R2,PAGNAM1H                                                      
         CLI   5(R2),0                                                          
         BE    FRECE2                                                           
FREC1    MVC   SYSKEY,KEY                                                       
         MVC   KEYF,KEY                                                         
*                                   ADJUST ORDER                                
         MVC   WORK(1),SGNONOO                                                  
         CLC   MSGKFILE,=C'INBOX   '                                            
         BNE   *+10                                                             
         MVC   WORK(1),SGNONOI                                                  
         CLI   WORK,C'F'                                                        
         BE    *+10                                                             
         XC    MSGKDATE(4),=X'FFFFFFFF'                                         
*                                                                               
         MVI   MSGKPAGE,0                                                       
         LA    R2,PAGNAM1H                                                      
         GOTO1 ANY                                                              
         MVC   MSGKFILE,WORK                                                    
         CLC   MSGKFILE,=C'INBOX   '                                            
         BE    FRECE1                                                           
         CLC   MSGKFILE,=C'OUTBOX  '                                            
         BNE   *+12                                                             
         CLI   SGNONFO,C'Y'                                                     
         BE    FRECE4                                                           
         CLI   SGNONOO,C'F'                                                     
         BE    *+10                                                             
         XC    MSGKDATE(4),=X'FFFFFFFF'                                         
         GOTO1 HIGH                                                             
         CLC   KEY(36),KEYSAVE                                                  
         BE    FRECE3                                                           
         MVC   KEY,KEYSAVE                                                      
         MVC   KEYT,KEY                                                         
         MVI   COPYFLG,C'N'                                                     
         BAS   RE,MOVREC                                                        
         MVC   KEY,SYSKEY                                                       
         MVC   SGNMSG(13),=C'MESSAGE FILED'                                     
         MVC   PAGNAM1,SPACES                                                   
         OI    PAGNAM1H+6,X'80'                                                 
         MVI   PAGCMMD,C' '                                                     
         OI    PAGCMMDH+6,X'C0'                                                 
         B     DREC                                                             
FRECE1   MVC   SGNMSG(20),=C'CANNOT FILE TO INBOX'                              
         OI    6(R2),X'40'                                                      
         B     DREC                                                             
FRECE2   MVC   SGNMSG(21),=C'MUST SPECIFY FILENAME'                             
         OI    6(R2),X'40'                                                      
         B     DREC                                                             
FRECE3   MVC   SGNMSG(15),=C'ALREADY IN FILE'                                   
         OI    6(R2),X'40'                                                      
         B     DREC                                                             
FRECE4   MVC   SGNMSG(21),=C'CANNOT FILE TO OUTBOX'                             
         OI    6(R2),X'40'                                                      
         B     DREC                                                             
         EJECT                                                                  
         SPACE 3                                                                
*                              SEND THE MESSAGE                                 
SREC     MVC   SYSKEY,KEY                                                       
         CLI   PAGNAM1H+5,0       VALIDATE TO FIELD                             
         BE    SRECE1                                                           
         CLI   PAGNAM2H+5,0                                                     
         BNE   SRECSR1                                                          
         LA    R2,PAGNAM1H                                                      
         GOTO1 ANY                                                              
         XC    KEY+12(26),KEY+12                                                
         MVI   ADLKTYP,ADLKTYPQ                                                 
         MVC   ADLKNAM,WORK                                                     
         CLI   WORK,C'$'                                                        
         BNE   SRECSR0                                                          
         CLI   WORK+1,C'$'                                                      
         BE    SRECSS0                                                          
         MVC   ADLKNAM,WORK+1                                                   
         MVC   MSGNAM,=C'ALL     '                                              
         B     SRECSR0                                                          
SRECSS0  MVC   ADLKNAM,WORK+2                                                   
         MVC   MSGNAM,=C'SYSALL  '                                              
         MVC   MSGAGY,=X'005A' =DDS                                             
SRECSR0  GOTO1 HIGH                                                             
         CLC   KEY(36),KEYSAVE                                                  
         BE    SREC00                                                           
SRECSR1  XC    KEY,KEY                                                          
         MVI   MSGSYS,C'M'     READ RECIP RECORD                                
         MVC   MSGAGY,TWAORIG                                                   
         LA    R2,PAGNAM2H                                                      
         CLI   5(R2),0                                                          
         BE    SREC00A                                                          
         GOTO1 ANY                                                              
         MVC   WORK+15(8),WORK                                                  
         MVC   WORK+1(8),WORK+15                                                
         MVI   WORK,2                                                           
         GOTO1 GETSGN                                                           
         MVC   MSGAGY,WORK                                                      
SREC00A  MVC   MSGNAM,=C'SYS     '                                              
         MVI   USRKTYP,USRKTYPQ                                                 
         LA    R2,PAGNAM1H                                                      
         GOTO1 ANY                                                              
         MVC   USRNAME,WORK                                                     
         GOTO1 HIGH                                                             
         CLC   KEY(36),KEYSAVE                                                  
         MVC   KEY(36),KEYSAVE                                                  
         BE    SREC00                                                           
         MVI   USRPTYP,USRPTYPQ                                                 
         GOTO1 HIGH                                                             
         CLC   KEY(36),KEYSAVE                                                  
         BNE   SREC0X1                                                          
*                             TO OK,PROCESS REQUEST                             
SREC00   MVC   KEY,SYSKEY                                                       
         MVI   COPYNO,0  NO COPIES                                              
*                              ADD AUDIT TRAIL                                  
         MVI   MSGKPAGE,0                                                       
SRECAU0  GOTO1 HIGH                                                             
         CLC   KEY(35),KEYSAVE                                                  
         BNE   SRECAU1                                                          
         ZIC   R1,MSGKPAGE                                                      
         LA    R1,1(R1)                                                         
         STC   R1,MSGKPAGE                                                      
         B     SRECAU0                                                          
SRECAU1  MVC   KEY,KEYSAVE   GOT LAST REC+1                                     
         ZIC   R1,MSGKPAGE                                                      
         BCTR  R1,0                                                             
         STC   R1,MSGKPAGE   GOT LAST REC                                       
         STC   R1,RSREC                                                         
         GOTO1 HIGH                                                             
         GOTO1 GETREC                                                           
         MVI   RSEL,0    FIND LAST EL+1                                         
         L     R6,AIO                                                           
         MVI   ELCODE,X'31'                                                     
         BAS   RE,GETEL                                                         
         BNE   SRECAU2A                                                         
SRECAU2  CLC   RSEL,2(R6)                                                       
         BH    *+10                                                             
         MVC   RSEL,2(R6)                                                       
         BAS   RE,NEXTEL                                                        
         BE    SRECAU2                                                          
SRECAU2A ZIC   R1,RSEL                                                          
         LA    R1,1(R1)                                                         
         STC   R1,RSEL                                                          
         MVC   REREC(2),RSREC                                                   
*                                                                               
         XC    ELEMENT,ELEMENT    BUILD ELEMENT                                 
         MVI   ELEMENT,X'31'                                                    
         MVI   ELEMENT+1,80                                                     
         MVI   ELEMENT+4,C'.'                                                   
         MVC   ELEMENT+5(80),ELEMENT+4                                          
         L     R5,AMSGWORK                                                      
         ZIC   R1,ELEMENT+1                                                     
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),ELEMENT                                                  
         LA    R5,0(R1,R5)                                                      
*                                                                               
         MVC   ELEMENT+4(80),SPACES                                             
         MVC   ELEMENT+10(4),=C'FROM'                                           
         MVC   ELEMENT+15(8),SGNONUN                                            
         MVC   WORK+1(2),TWAORIG                                                
         MVI   WORK,1                                                           
         GOTO1 GETSGN                                                           
         MVI   ELEMENT+24,C'@'                                                  
         MVC   ELEMENT+25(8),WORK                                               
         MVC   ELEMENT+36(2),=C'TO'                                             
         LA    R2,PAGNAM1H                                                      
         GOTO1 ANY                                                              
         MVC   ELEMENT+39(8),WORK                                               
         LA    R2,PAGNAM2H                                                      
         CLI   5(R2),0                                                          
         BE    SRECAU3                                                          
         GOTO1 ANY                                                              
         MVI   ELEMENT+48,C'@'                                                  
         MVC   ELEMENT+49(8),WORK                                               
*                                  DATE                                         
SRECAU3  GOTO1 DATCON,DMCB,(5,WORK),(5,ELEMENT+60)                              
*                                  TIME                                         
         GOTO1 GETFACT,DMCB,(2,0)                                               
         L     R1,DMCB                                                          
         L     R1,0(R1)                                                         
         SR    R0,R0                                                            
         D     R0,=F'6000'                                                      
         SR    R0,R0                                                            
         D     R0,=F'60'                                                        
         LA    R1,8(R1)                                                         
         CVD   R1,WORK       HOURS                                              
         UNPK  WORK(4),WORK+6(3)                                                
         MVC   ELEMENT+69(2),WORK                                               
         MVI   ELEMENT+71,C':'                                                  
         CVD   R0,WORK       MINUTES                                            
         UNPK  WORK(4),WORK+6(3)                                                
         MVC   ELEMENT+72(2),WORK                                               
         ZIC   R1,ELEMENT+1                                                     
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),ELEMENT                                                  
         LA    R5,0(R1,R5)                                                      
*                           FORMAT ADDRESS LIST                                 
SRECA0   CLI   PAGNAM2H+5,0                                                     
         BNE   SRECAU4                                                          
         LA    R2,PAGNAM1H                                                      
         GOTO1 ANY                                                              
         XC    KEY+12(26),KEY+12                                                
         MVI   ADLKTYP,ADLKTYPQ                                                 
         MVC   ADLKNAM,WORK                                                     
         CLI   WORK,C'$'                                                        
         BNE   SRECASR                                                          
         CLI   WORK+1,C'$'                                                      
         BE    SRECASS                                                          
         MVC   ADLKNAM,WORK+1                                                   
         MVC   MSGNAM,=C'ALL     '                                              
         B     SRECASR                                                          
SRECASS  MVC   ADLKNAM,WORK+2                                                   
         MVC   MSGNAM,=C'SYSALL  '                                              
         MVC   MSGAGY,=X'005A' =DDS                                             
SRECASR  GOTO1 HIGH                                                             
         CLC   KEY(36),KEYSAVE                                                  
         BNE   SRECAU4                                                          
*                                                                               
SRECALP  GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         MVI   ELCODE,X'41'           DO TOS                                    
         BAS   RE,GETEL                                                         
         BNE   SRECA02                                                          
SRECA01  DS    0H                                                               
         MVC   ELEMENT,SPACES     BUILD ELEMENT                                 
         MVC   ELEMENT+4(03),=C'TO '                                            
         MVI   ELEMENT,X'31'                                                    
         MVI   ELEMENT+1,82                                                     
         MVI   ELEMENT+3,X'40'                                                  
SRECA010 MVI   ELEMENT+58,C'-'                                                  
         MVC   ELEMENT+60(8),2(R6)                                              
         MVC   ELEMENT+68(15),SPACES                                            
         CLC   10(2,R6),SYSKEY+MSGAGY-MSGKEYD                                   
         BE    SRECA011                                                         
         MVC   WORK+1(2),10(R6)                                                 
         MVI   WORK,1                                                           
         GOTO1 GETSGN                                                           
         MVC   ELEMENT+70(8),WORK                                               
         MVI   ELEMENT+69,C'@'                                                  
SRECA011 CLI   12(R6),0                                                         
         BE    SRECA012                                                         
         ZIC   R1,12(R6)                                                        
         LA    R2,ELEMENT+80                                                    
         EDIT  (R1),(2,(R2))                                                    
         MVI   ELEMENT+79,C'('                                                  
         MVI   ELEMENT+82,C')'                                                  
SRECA012 GOTO1 SQUASHER,DMCB,ELEMENT+4,79                                       
         CLC   ELEMENT+58(2),SPACES                                             
         BE    SRECA01A                                                         
         ZIC   R1,ELEMENT+1                                                     
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),ELEMENT                                                  
         LA    R5,0(R1,R5)                                                      
         MVC   ELEMENT+7(75),SPACES                                             
SRECA01A MVI   ELCODE,X'41'                                                     
         BAS   RE,NEXTEL                                                        
         BE    SRECA010                                                         
         CLI   ELEMENT+7,C' '                                                   
         BE    SRECA02                                                          
         ZIC   R1,ELEMENT+1                                                     
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),ELEMENT                                                  
         LA    R5,0(R1,R5)                                                      
SRECA02  L     R6,AIO                 DO COPYS                                  
         MVI   ELCODE,X'42'                                                     
         BAS   RE,GETEL                                                         
         BNE   SRECA04                                                          
SRECA03  DS    0H                                                               
         MVC   ELEMENT,SPACES     BUILD ELEMENT                                 
         MVC   ELEMENT+4(03),=C'CC '                                            
         MVI   ELEMENT,X'31'                                                    
         MVI   ELEMENT+1,82                                                     
         MVI   ELEMENT+3,X'40'                                                  
SRECA030 MVI   ELEMENT+58,C'-'                                                  
         MVC   ELEMENT+60(8),2(R6)                                              
         MVC   ELEMENT+68(15),SPACES                                            
         CLC   10(2,R6),SYSKEY+MSGAGY-MSGKEYD                                   
         BE    SRECA031                                                         
         MVC   WORK+1(2),10(R6)                                                 
         MVI   WORK,1                                                           
         GOTO1 GETSGN                                                           
         MVC   ELEMENT+70(8),WORK                                               
         MVI   ELEMENT+69,C'@'                                                  
SRECA031 CLI   12(R6),0                                                         
         BE    SRECA032                                                         
         ZIC   R1,12(R6)                                                        
         LA    R2,ELEMENT+80                                                    
         EDIT  (R1),(2,(R2))                                                    
         MVI   ELEMENT+79,C'('                                                  
         MVI   ELEMENT+82,C')'                                                  
SRECA032 GOTO1 SQUASHER,DMCB,ELEMENT+4,79                                       
         CLC   ELEMENT+58(2),SPACES                                             
         BNH   SRECA03A                                                         
         ZIC   R1,ELEMENT+1                                                     
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),ELEMENT                                                  
         LA    R5,0(R1,R5)                                                      
         MVC   ELEMENT+7(75),SPACES                                             
SRECA03A MVI   ELCODE,X'42'                                                     
         BAS   RE,NEXTEL                                                        
         BE    SRECA030                                                         
         CLI   ELEMENT+7,C' '                                                   
         BE    SRECA04                                                          
         ZIC   R1,ELEMENT+1                                                     
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),ELEMENT                                                  
         LA    R5,0(R1,R5)                                                      
SRECA04  GOTO1 SEQ                 DO NEXT LIST PAGE                            
         CLC   KEY(35),KEYSAVE                                                  
         BE    SRECALP                                                          
*                                                                               
SRECAU4  MVI   ELEMENT+3,0     BUILD END LINE                                   
         MVI   ELEMENT+1,80                                                     
         MVI   ELEMENT+4,C'*'                                                   
         MVC   ELEMENT+5(80),ELEMENT+4                                          
         ZIC   R1,ELEMENT+1                                                     
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),ELEMENT                                                  
         LA    R5,0(R1,R5)                                                      
*                                                                               
         GOTOR RUNOUT    INSERT LINES INTO MSG                                  
*                              FILE OUTBOX COPY                                 
*                                                                               
*                                                                               
*                                                                               
         OC    SGNMSG(5),SPACES         CHECK FOR INVALID                       
         CLC   SGNMSG(5),SPACES        CONDITION: RECORD TOO LONG               
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
*                                                                               
*                                                                               
*                                                                               
         CLI   SGNONFO,C'Y'                                                     
         BNE   SREC0                                                            
         MVC   KEY,SYSKEY                                                       
         MVC   KEYF,KEY                                                         
         MVC   MSGKFILE,=C'OUTBOX  '                                            
*                                  DATE                                         
         GOTO1 DATCON,DMCB,(5,WORK),(2,MSGKDATE)                                
*                                  TIME                                         
         GOTO1 GETFACT,DMCB,(2,0)                                               
         L     R1,DMCB                                                          
         L     R1,0(R1)                                                         
         SR    R0,R0                                                            
         D     R0,=F'200'                                                       
         ST    R1,WORK                                                          
         MVC   MSGKTIME,WORK+2                                                  
         CLI   SGNONOO,C'F'                                                     
         BE    *+10                                                             
         XC    MSGKDATE(4),=X'FFFFFFFF'                                         
         MVC   KEYT,KEY                                                         
         MVI   COPYFLG,C'Y'                                                     
         BAS   RE,MOVREC                                                        
         MVC   KEY,SYSKEY                                                       
*                              READ ADDRESS LIST                                
SREC0    CLI   PAGNAM2H+5,0                                                     
         BNE   SREC1                                                            
         LA    R2,PAGNAM1H                                                      
         GOTO1 ANY                                                              
         XC    KEY+12(26),KEY+12                                                
         MVI   ADLKTYP,ADLKTYPQ                                                 
         MVC   ADLKNAM,WORK                                                     
         CLI   WORK,C'$'                                                        
         BNE   SRECSR                                                           
         CLI   WORK+1,C'$'                                                      
         BE    SRECSS                                                           
         MVC   ADLKNAM,WORK+1                                                   
         MVC   MSGNAM,=C'ALL     '                                              
         B     SRECSR                                                           
SRECSS   MVC   ADLKNAM,WORK+2                                                   
         MVC   MSGNAM,=C'SYSALL  '                                              
         MVC   MSGAGY,=X'005A' =DDS                                             
SRECSR   GOTO1 HIGH                                                             
         CLC   KEY(36),KEYSAVE                                                  
         BNE   SREC1                                                            
*                                                                               
SRECLP   MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1                                                         
         L     R6,AIO2                                                          
         MVI   ELCODE,X'41'           DO TOS                                    
         BAS   RE,GETEL                                                         
         BNE   SREC02                                                           
SREC01   MVC   WORK(10),2(R6)                                                   
         MVC   COPYNO,12(R6) NUM OF COPIES                                      
         MVI   COPYFLG,C'N'                                                     
         GOTO1 SRECPUT                                                          
         CLI   WORK,0                                                           
         BE    *+4                                                              
         MVI   ELCODE,X'41'                                                     
         BAS   RE,NEXTEL                                                        
         BE    SREC01                                                           
SREC02   L     R6,AIO2                DO COPYS                                  
         MVI   ELCODE,X'42'                                                     
         BAS   RE,GETEL                                                         
         BNE   SREC04                                                           
SREC03   MVC   WORK(10),2(R6)                                                   
         MVC   COPYNO,12(R6) NUM OF COPIES                                      
         MVI   COPYFLG,C'Y'                                                     
         GOTO1 SRECPUT                                                          
         CLI   WORK,0                                                           
         BE    *+4                                                              
         MVI   ELCODE,X'42'                                                     
         BAS   RE,NEXTEL                                                        
         BE    SREC03                                                           
SREC04   L     R6,AIO2              DO NEXT LIST PAGE                           
         MVC   KEY,0(R6)                                                        
         GOTO1 HIGH                                                             
         CLC   KEY(36),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 SEQ                                                              
         CLC   KEY(35),KEYSAVE                                                  
         BE    SRECLP                                                           
*                                                                               
SREC0X   MVC   SGNMSG(12),=C'MESSAGE SENT'                                      
         CLI   WORK,0                                                           
         BNE   EX2                                                              
SREC0X1  MVC   SGNMSG(15),=C'USER NOT FOUND'                                    
EX2      OI    PAGNAM1H+6,X'80'                                                 
         XC    PAGNAM1,PAGNAM1                                                  
         MVI   PAGNAM1H+5,0                                                     
         OI    PAGNAM2H+6,X'80'                                                 
         XC    PAGNAM2,PAGNAM2                                                  
         MVI   PAGNAM2H+5,0                                                     
         MVI   PAGCMMD,0                                                        
         OI    PAGCMMDH+6,X'C0'                                                 
         B     DREC                                                             
*                                                                               
SREC1    MVI   COPYFLG,C'N'                                                     
         MVC   KEY,SYSKEY                                                       
         LA    R2,PAGNAM1H                                                      
         GOTO1 ANY                                                              
         MVC   WORK+8(2),MSGAGY                                                 
         CLI   PAGNAM2H+5,0                                                     
         BE    SREC1A                                                           
         MVC   ELEMENT(8),WORK                                                  
         LA    R2,PAGNAM2H                                                      
         GOTO1 ANY                                                              
         MVC   WORK+15(8),WORK                                                  
         MVC   WORK+1(8),WORK+15                                                
         MVI   WORK,2                                                           
         GOTO1 GETSGN                                                           
         MVC   WORK+8(2),WORK                                                   
         MVC   WORK(8),ELEMENT                                                  
SREC1A   MVI   WORK+25,C'T'                                                     
         GOTO1 SRECPUT                                                          
         B     SREC0X                                                           
*                                                                               
SRECPUT  NTR1               ***SEND ONE MESSAGE***                              
         MVC   KEYF,SYSKEY                                                      
         XC    KEY,KEY                                                          
         MVI   MSGSYS,C'M'     READ RECIP RECORD                                
         MVC   MSGAGY,WORK+8                                                    
         MVC   MSGNAM,=C'SYS     '                                              
         MVI   USRKTYP,USRKTYPQ                                                 
         MVC   USRNAME,WORK                                                     
         GOTO1 HIGH                                                             
         CLC   KEY(36),KEYSAVE                                                  
         BE    SRECP1            FOUND KEY                                      
         MVC   KEY,KEYSAVE                                                      
         MVI   USRPTYP,USRPTYPQ                                                 
         GOTO1 HIGH                                                             
         CLC   KEY(36),KEYSAVE                                                  
         BE    SRECP1            FOUND KEY (NICK)                               
         MVI   WORK,0                                                           
         B     XIT                                                              
*                                                                               
SRECP1   GOTO1 GETREC            GET PROFILE RECORD                             
*                                                                               
*                                                                               
*        CHECK HERE FOR VIA GRAPHNET,MCI,ETC                                    
*                                                                               
*                                                                               
         L     R6,AIO            CHECK FOR TWXAD EL                             
         MVI   ELCODE,X'25'                                                     
         BAS   RE,GETEL                                                         
SRECP2   BNE   SRECP4                                                           
         CLI   2(R6),1  MUST HAVE FIRST ONE                                     
         BE    SRECP3                                                           
         BAS   RE,NEXTEL                                                        
         B     SRECP2                                                           
*                                                                               
SRECP3   GOTOR SGRAPH            SEND VIA GRAPHNET                              
         B     SRECPE                                                           
*                                                                               
SRECP4   L     R6,AIO             GET TRUE NAME                                 
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   WORK(8),SPACES                                                   
         ZIC   R1,1(R6)                                                         
         SH    R1,=H'5'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),4(R6)                                                    
         MVI   ELCODE,X'23'      GET RECIP OPTIONS                              
         L     R6,AIO                                                           
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   WORK+8(16),0(R6)                                                 
*                                                                               
*        CHECK FOR AUTO PQ                                                      
*                                                                               
         CLI   WORK+8+OPTNAP-OPTNELD,C'Y'                                       
         BNE   SRECP5                                                           
         GOTOR SPQ                                                              
         B     SRECPE                                                           
*                                                                               
*        BUILD NEW MESSAGE                                                      
*                                                                               
SRECP5   XC    KEY+4(32),KEY+4   BUILD KEY                                      
         MVC   MSGNAM,WORK                                                      
         MVI   MSGKTYPE,MSGKTYPQ        BUILD NEW KEY                           
         MVC   MSGKFILE,=C'INBOX   '                                            
*                                  DATE                                         
         GOTO1 DATCON,DMCB,(5,WORK),(2,MSGKDATE)                                
*                                  TIME                                         
         GOTO1 GETFACT,DMCB,(2,0)                                               
         L     R1,DMCB                                                          
         L     R1,0(R1)                                                         
         SR    R0,R0                                                            
         D     R0,=F'200'                                                       
         ST    R1,WORK                                                          
         MVC   MSGKTIME,WORK+2                                                  
         CLI   WORK+8+OPTNOI-OPTNELD,C'F'                                       
         BE    *+10                                                             
         XC    MSGKDATE(4),=X'FFFFFFFF'                                         
         MVC   MSGKFROM,SYSKEY+MSGNAM-MSGKEYD                                   
         MVC   MSGKFRAT,SYSKEY+MSGAGY-MSGKEYD                                   
         MVC   KEYT,KEY                                                         
         BAS   RE,MOVREC                                                        
*                                                                               
SRECPE   MVI   WORK,C'X'     SIGNAL OK                                          
         B     XIT                                                              
*                                                                               
SRECE1   MVC   KEYT,KEY         SET UP FOR TEMP ADDLIST                         
         MVI   MSADFLG,X'FF'                                                    
         B     XIT                                                              
         EJECT                                                                  
*              COPY MESSAGE RECORDS                                             
         SPACE 3                                                                
MOVREC   NTR1                                                                   
         MVI   KEYT+35,0    INSURE FIRST REC START                              
         MVC   KEY,KEYF                 GET BACK RECORD                         
         MVI   MSGKPAGE,0                                                       
         GOTO1 HIGH                                                             
         CLC   KEY(36),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
MOVREC1  GOTO1 GETREC                                                           
         L     R2,AIO                                                           
         CLC   KEY(36),0(R2)                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   COPYFLG,C'N'   ADD COPY FLAG                                     
         BE    MOVREC11                                                         
         MVI   ELCODE,X'32'                                                     
         GOTO1 REMELEM                                                          
         MVC   ELEMENT(3),=X'320300'                                            
         GOTO1 ADDELEM                                                          
         TBADLEN                                                                
*                                                                               
MOVREC11 CLI   PAGCMMD,C'S'        SENT FLAG                                    
         BNE   MOVREC12                                                         
         MVI   ELCODE,X'33'                                                     
         GOTO1 REMELEM                                                          
         MVC   ELEMENT(4),=X'33140000'                                          
         LA    R2,PAGNAM1H                                                      
         GOTO1 ANY1                                                             
         MVC   ELEMENT+4(8),WORK                                                
         LA    R2,PAGNAM2H                                                      
         GOTO1 ANY1                                                             
         MVC   ELEMENT+12(8),WORK                                               
         GOTO1 ADDELEM                                                          
         TBADLEN                                                                
*                                                                               
MOVREC12 CLI   COPYNO,1     ADD NUMBER OF COPIES                                
         BNH   MOVREC13                                                         
         MVI   ELCODE,X'34'                                                     
         GOTO1 REMELEM                                                          
         MVC   ELEMENT(2),=X'3403'                                              
         MVC   ELEMENT+2(1),COPYNO                                              
         GOTO1 ADDELEM                                                          
         TBADLEN                                                                
*                                                                               
MOVREC13 CLI   PAGCMMD,C'S'                                                     
         BNE   MOVREC14                                                         
         L     R6,AIO                                                           
         MVI   ELCODE,X'31'                                                     
         BAS   RE,GETEL                                                         
         BNE   MOVREC14                                                         
MOVREC1A OI    3(R6),X'80'  MARK EL TO STOP UPDATE                              
         BAS   RE,NEXTEL                                                        
         BE    MOVREC1A                                                         
*                                                                               
MOVREC14 CLI   PAGCMMD,C'G'                                                     
         BNE   MOVREC2                                                          
         L     R6,AIO                                                           
         MVI   ELCODE,X'31'                                                     
         BAS   RE,GETEL                                                         
         BNE   MOVREC2                                                          
MOVREC1B CLI   1(R6),5                                                          
         BL    MOVREC1C                                                         
         CLI   4(R6),C'#'                                                       
         BE    MOVREC1X                                                         
MOVREC1C OI    3(R6),X'20'  MARK EL AS FORMATTED                                
*                          CHECK ILLEGAL                                        
         ZIC   R1,1(R6)                                                         
MOVREC1D LA    R2,3(R1,R6)                                                      
         CLC   0(2,R2),=C'##'                                                   
         BE    MOVREC1E                                                         
         BCT   R1,MOVREC1D                                                      
         B     MOVREC1X                                                         
MOVREC1E NI    3(R6),X'DF'                                                      
         MVC   SGNMSG(21),=C'CONSECUTIVE #''S FOUND'                            
*                                                                               
MOVREC1X BAS   RE,NEXTEL                                                        
         BE    MOVREC1B                                                         
*                                                                               
MOVREC2  MVC   KEY,KEYT                 WRITE NEW RECORD                        
         L     R2,AIO                                                           
         MVC   0(36,R2),KEY                                                     
         OI    DMINBTS,X'08'                                                    
         GOTO1 HIGH                                                             
         NI    DMINBTS,X'F7'                                                    
         CLC   KEY(36),KEYSAVE                                                  
         BNE   MOVREC3                                                          
         TM    KEY+36,X'80'   EXISTS..DELETED?                                  
         BO    *+6                                                              
         DC    H'0'                                                             
         MVC   AIO,AIO2                                                         
         OI    DMINBTS,X'08'                                                    
         GOTO1 GETREC                                                           
         NI    DMINBTS,X'F7'                                                    
         L     R1,AIO1                                                          
         L     R2,AIO2                                                          
         CLC   KEY(36),0(R2)                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         ST    R1,AIO                                                           
         LH    R0,36(R1)                                                        
         MVC   0(44,R1),0(R2)                                                   
         STH   R0,36(R1)                                                        
*        MVC   0(36,R1),KEY                                                     
         NI    38(R1),X'7F'                                                     
         GOTO1 PUTREC                                                           
         NI    KEY+36,X'7F'                                                     
         GOTO1 WRITE                                                            
         B     MOVREC4                                                          
*                                                                               
MOVREC3  MVC   KEY,KEYSAVE                                                      
         GOTO1 ADDREC                                                           
*                                                                               
MOVREC4  MVC   KEY,KEYT                                                         
         ZIC   R1,MSGKPAGE                                                      
         LA    R1,1(R1)                                                         
         STC   R1,MSGKPAGE                                                      
         MVC   KEYT,KEY                                                         
*                                                                               
         CH    R1,=H'255'                                                       
         BE    XIT                                                              
         MVC   KEY,KEYF                                                         
         STC   R1,MSGKPAGE                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(36),KEYSAVE                                                  
         BNE   MOVREC5                                                          
*                                                                               
         GOTO1 GETREC                                                           
         L     R2,AIO                                                           
         CLC   KEY(36),0(R2)                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         B     MOVREC13                                                         
*                                                                               
MOVREC5  MVC   KEY,KEYT           TRAP FOR BUG                                  
         ZIC   R3,MSGKPAGE                                                      
         L     R6,AIO                                                           
MOVREC5A BCTR  R3,0                                                             
         STC   R3,MSGKPAGE                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(36),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETREC                                                           
         CLC   KEY(36),0(R6)                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   MSGKPAGE,0                                                       
         BNE   MOVREC5A                                                         
         B     XIT                                                              
         EJECT                                                                  
*              MISC                                                             
         SPACE 3                                                                
BUMP     ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         BR    RE                                                               
         SPACE 1                                                                
XIT      OI    PAGCMMDH+6,X'81'                                                 
         OI    SGNMSGH+6,X'80'                                                  
         XIT1                                                                   
         SPACE 1                                                                
         DROP  RB                                                               
*                   GETEL MUST LIE WITHIN ADDRESSABILITY                        
*                   RANGE OF R7 AS MUST RELO                                    
         GETEL R6,DATADISP,ELCODE                                               
         SPACE 1                                                                
RELO     DS    A                                                                
         LTORG                                                                  
         EJECT                                                                  
*                              RUNOUT                                           
*                                                                               
*        THIS MODULE ACCEPTS AS INPUT A WORKAREA OF LINE ELEMENTS,              
*        A STARTING RECORD/ELEMENT PAIR, AND AN ENDING REC/EL PAIR.             
*        IT DELETES FROM THE MSG RECORDS ALL ELS FROM THE START                 
*        PAIR TO THE END PAIR, INCLUDING THE START, BUT NOT THE END.            
*        IT INSERTS INTO THE RECORDS THE ELS IN THE WORKAREA, AND               
*        MOVES ELS FROM RECORD TO RECORD TO KEEP THE RECORDS FILLED.            
*                                                                               
         SPACE 3                                                                
RUNOUT   SUBNTR                                                                 
*                                                                               
         ZIC   R1,RSREC                                                         
         LA    R1,1(R1)                                                         
         STC   R1,RNTREC                                                        
         STC   R1,RNFREC                                                        
*                                                                               
         MVC   KEY,DKEY        GET START RECORD                                 
         MVC   MSGKPAGE,RSREC                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(36),KEYSAVE                                                  
         MVC   KEY(36),KEYSAVE                                                  
         BNE   RUNLP3N  NEW PG                                                  
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         TKEY                                                                   
         MVC   ELPTR,RSEL      SET TO START EL                                  
*                                                                               
RUNLP1   L     R6,AIO         GET NEXT EL                                       
         MVI   ELCODE,X'31'    LINE EL                                          
         BAS   RE,GETEL                                                         
         BNE   RUNLP1A  BLANK REC                                               
RUNLP1C  CLC   ELPTR,2(R6)                                                      
         BE    RUNLP1B                                                          
         BAS   RE,NEXTEL                                                        
         BNE   RUNLP1A  DONE                                                    
         B     RUNLP1C                                                          
*                                                                               
RUNLP1B  MVC   WORK(1),MSGKPAGE  COPY TO WORKA IF AFTER END                     
         MVC   WORK+1(1),ELPTR                                                  
         CLC   WORK(2),REREC                                                    
         BL    RUNLP1D                                                          
         ZIC   R1,1(R6)                                                         
         EXMVC R1,0(R5),0(R6)                                                   
         LA    R5,0(R1,R5)                                                      
         GOTOR REMBLNK                                                          
*                                                                               
RUNLP1D  MVI   0(R6),99      DELETE THIS ELEMENT                                
         MVI   ELCODE,99                                                        
         GOTO1 REMELEM                                                          
         MVI   ELCODE,X'31'                                                     
*                                                                               
         ZIC   R1,ELPTR     DO NEXT EL                                          
         LA    R1,1(R1)                                                         
         STC   R1,ELPTR                                                         
         B     RUNLP1                                                           
*                                                                               
RUNLP1A  TKEY                                                                   
         GOTO1 PUTREC    SAVE REC                                               
         GOTOR RUFILL FILL UP WORKA                                             
         MVC   MSGKPAGE,RSREC  GET REC BACK                                     
         GOTO1 HIGH                                                             
         GOTO1 GETREC                                                           
         TKEY                                                                   
*                                                                               
         MVC   ELPTR,RSEL   SET TO FIRST EL                                     
*                                                                               
RUNLP2   C     R5,AMSGWORK CHECK IF WORKA EMPTY                                 
         BE    RUNLPE                                                           
*                                                                               
         L     R6,AIO CHECK IF NEXT EL WILL FIT                                 
         L     R2,AMSGWORK                                                      
         ZIC   R1,1(R2)                                                         
         AH    R1,36(R6)                                                        
         CH    R1,=H'1000'                                                      
         BH    RUNLP2E                                                          
*                                                                               
         ZIC   R3,1(R2)     MOVE ELEMENT TO RECORD                              
         EXMVC R3,ELEMENT,0(R2)                                                 
         MVI   ELEMENT,X'31'                                                    
         MVC   ELEMENT+2(1),ELPTR                                               
         GOTO1 ADDELEM                                                          
         TBADLEN                                                                
*                      DELETE EL FROM WORKA                                     
         AR    R2,R3   FROM                                                     
         LR    R1,R5                                                            
         SR    R1,R2   LEN                                                      
         SR    R5,R3   ADJUST                                                   
         LR    R3,R1   LEN                                                      
         L     R0,AMSGWORK TO                                                   
         MVCL  R0,R2                                                            
*                                                                               
         ZIC   R1,ELPTR DO NEXT EL IF MORE                                      
         LA    R1,1(R1)                                                         
         STC   R1,ELPTR                                                         
         B     RUNLP2                                                           
*                                                                               
RUNLP2E  TKEY                                                                   
         GOTO1 PUTREC    SAVE REC                                               
*                                                                               
RUNLP3   GOTOR RUFILL REFILL WORK AREA                                          
*                                                                               
         ZIC   R1,RNTREC                                                        
         STC   R1,MSGKPAGE                                                      
         LA    R1,1(R1)                                                         
         STC   R1,RNTREC                                                        
         GOTO1 HIGH                                                             
         CLC   KEY(36),KEYSAVE                                                  
         MVC   KEY(36),KEYSAVE                                                  
         BE    RUNLP3B                                                          
*                                                                               
RUNLP3N  OI    DMINBTS,X'08'  ADDING TO END?                                    
         GOTO1 HIGH                                                             
         CLC   KEY(36),KEYSAVE                                                  
         MVC   KEY(36),KEYSAVE                                                  
         BE    RUNLP3A                                                          
*                                                                               
         L     R6,AIO                                                           
         XC    0(240,R6),0(R6)                                                  
         MVC   0(36,R6),KEY                                                     
         MVC   ELEMENT,=X'300801007B7B7B7B'                                     
         GOTO1 ADDELEM                                                          
         GOTO1 ADDREC                                                           
         MVC   KEY(36),0(R6)                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(36),KEYSAVE                                                  
         BE    RUNLP3B                                                          
         DC    H'0'                                                             
*                                                                               
RUNLP3A  GOTO1 GETREC                                                           
         TKEY                                                                   
         L     R6,AIO                                                           
         NI    38(R6),X'7F'                                                     
         TKEY                                                                   
         GOTO1 PUTREC                                                           
         GOTO1 READ                                                             
         NI    KEY+36,X'7F'                                                     
         GOTO1 WRITE                                                            
*                                                                               
RUNLP3B  GOTO1 GETREC                                                           
         TKEY                                                                   
         MVI   ELCODE,X'30'                                                     
         GOTO1 REMELEM                                                          
         MVI   ELCODE,X'31'                                                     
         GOTO1 REMELEM                                                          
         MVI   ELCODE,X'32'                                                     
         GOTO1 REMELEM                                                          
         MVI   ELCODE,X'33'                                                     
         GOTO1 REMELEM                                                          
         MVI   ELCODE,X'34'                                                     
         GOTO1 REMELEM                                                          
         NI    DMINBTS,X'F7'                                                    
*                                                                               
         MVI   ELPTR,1      SET TO FIRST EL                                     
*                                                                               
RUNLP3C  L     R6,AIO CHECK IF NEXT EL WILL FIT                                 
         L     R2,AMSGWORK                                                      
         ZIC   R1,1(R2)                                                         
         AH    R1,36(R6)                                                        
         CH    R1,=H'1000'                                                      
         BH    RUNLPE                                                           
*                                                                               
         ZIC   R3,1(R2)     MOVE ELEMENT TO RECORD                              
         EXMVC R3,ELEMENT,0(R2)                                                 
         MVI   ELEMENT,X'31'                                                    
         MVC   ELEMENT+2(1),ELPTR                                               
         GOTO1 ADDELEM                                                          
         TBADLEN                                                                
*                      DELETE EL FROM WORKA                                     
         AR    R2,R3   FROM                                                     
         LR    R1,R5                                                            
         SR    R1,R2   LEN                                                      
         SR    R5,R3   ADJUST                                                   
         LR    R3,R1   LEN                                                      
         L     R0,AMSGWORK TO                                                   
         MVCL  R0,R2                                                            
*                                                                               
         ZIC   R1,ELPTR DO NEXT EL IF MORE                                      
         LA    R1,1(R1)                                                         
         STC   R1,ELPTR                                                         
         C     R5,AMSGWORK                                                      
         BNE   RUNLP3C                                                          
*                                                                               
RUNLPE   C     R5,AMSGWORK CHECK IF WORKA EMPTY                                 
         BE    RUNLP4                                                           
         TKEY                                                                   
         GOTO1 PUTREC    SAVE REC                                               
         B     RUNLP3                                                           
*                                                                               
RUNLP4   ZIC   R3,ELPTR  REMOVE TRAILING BLANK LINES                            
RUNLP4A  L     R6,AIO                                                           
         MVI   ELCODE,X'31'    LINE EL                                          
         BAS   RE,GETEL                                                         
         BNE   RUNLP4B                                                          
RUNLP4A1 ZIC   R1,2(R6)                                                         
         CR    R1,R3                                                            
         BE    RUNLP4A2                                                         
         BAS   RE,NEXTEL                                                        
         BNE   RUNLP4A3                                                         
         B     RUNLP4A1                                                         
RUNLP4A2 CLI   1(R6),4                                                          
         BNE   RUNLP4A4                                                         
         CLI   3(R6),0                                                          
         BNE   RUNLP4A4                                                         
         MVI   0(R6),99                                                         
         MVI   ELCODE,99                                                        
         GOTO1 REMELEM                                                          
RUNLP4A3 BCT   R3,RUNLP4A                                                       
*                                                                               
RUNLP4A4 L     R6,AIO   DELETE REC IF NO LINES                                  
         MVI   ELCODE,X'31'    LINE EL                                          
         BAS   RE,GETEL                                                         
         L     R6,AIO                                                           
         BE    RUNLP4C                                                          
RUNLP4B  CLI   MSGKPAGE,0  DON'T DELETE FIRST                                   
         BE    RUNLP4C                                                          
         OI    38(R6),X'80'                                                     
RUNLP4C  TKEY                                                                   
         GOTO1 PUTREC                                                           
         TM    38(R6),X'80'                                                     
         BZ    RUNLP5                                                           
         GOTO1 READ                                                             
         OI    KEY+36,X'80'                                                     
         GOTO1 WRITE                                                            
*                                                                               
RUNLP5   ZIC   R1,MSGKPAGE  DELETE REST OF RECS                                 
         LA    R1,1(R1)                                                         
         STC   R1,MSGKPAGE                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(36),KEYSAVE                                                  
         MVC   KEY(36),KEYSAVE                                                  
         BNE   RUNXIT                                                           
         GOTO1 GETREC                                                           
         TKEY                                                                   
         L     R6,AIO                                                           
         OI    38(R6),X'80'                                                     
         TKEY                                                                   
         GOTO1 PUTREC                                                           
         GOTO1 READ                                                             
         OI    KEY+36,X'80'                                                     
         GOTO1 WRITE                                                            
         B     RUNLP5                                                           
RUNXIT   SUBXIT                                                                 
         EJECT                                                                  
*                              RUNOUT FILL WORKA                                
         SPACE 3                                                                
RUFILL   SUBNTR                                                                 
         L     R1,AMSGWORK CHECK FULL                                           
         LA    R1,2000(R1) FULL IF > 2 PGS                                      
         CR    R1,R5                                                            
         BL    RUXIT                                                            
*                                                                               
         MVC   MSGKPAGE,RNFREC  GET NEXT REC                                    
         GOTO1 HIGH                                                             
         CLC   KEY(36),KEYSAVE                                                  
         MVC   KEY(36),KEYSAVE                                                  
         BNE   RUXIT    NO MORE                                                 
*                                                                               
         ZIC   R1,RNFREC   BUMP TO NEXT FOR NEXT CALL                           
         LA    R1,1(R1)                                                         
         STC   R1,RNFREC                                                        
*                                                                               
         GOTO1 GETREC  MOVE ANY ELS TO WORKA                                    
         TKEY                                                                   
         L     R6,AIO                                                           
         MVI   ELCODE,X'31'    LINE EL                                          
         BAS   RE,GETEL                                                         
         BNE   RUXIT                                                            
RUFLPA   MVC   WORK(1),MSGKPAGE  COPY TO WORKA IF AFTER END                     
         MVC   WORK+1(1),2(R6)                                                  
         CLC   WORK(2),REREC                                                    
         BL    RUFLPB                                                           
         ZIC   R1,1(R6)                                                         
         EXMVC R1,0(R5),0(R6)                                                   
         LA    R5,0(R1,R5)                                                      
         GOTOR REMBLNK                                                          
RUFLPB   BAS   RE,NEXTEL                                                        
         BE    RUFLPA                                                           
*                                                                               
RUXIT    XIT1  REGS=(R5)                                                        
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                              REMOVE TRAILING BLANKS                           
         SPACE 3                                                                
         USING REMBLNK,RF                                                       
REMBLNK  BCTR  R5,0                                                             
         BCTR  R1,0                                                             
         CH    R1,=H'4'                                                         
         BL    REMBLNKE                                                         
         CLI   0(R5),0                                                          
         BE    REMBLNK                                                          
         CLI   0(R5),C' '                                                       
         BE    REMBLNK                                                          
REMBLNKE SR    R5,R1                                                            
         LA    R1,1(R1)                                                         
         STC   R1,1(R5)                                                         
         LA    R5,0(R1,R5)                                                      
         BR    RE                                                               
         DROP  RF                                                               
         EJECT                                                                  
*                              SEND MESSAGE DIRECT TO PQ                        
         SPACE 3                                                                
SPQ      SUBNTR                                                                 
*              RESTORE KEY                                                      
SPQS1    MVC   KEY,KEYF                                                         
         MVI   MSGKPAGE,0                                                       
         GOTO1 HIGH                                                             
*              SET UP WHEN                                                      
         MVC   REMUSER,=C'MSG'                                                  
         MVI   TWAWHEN,0                                                        
         XC    TWAOUT,TWAOUT                                                    
         MVC   TWADEST,TWAORIG                                                  
         MVI   WHEN,X'40'                                                       
*                                                                               
         L     R2,AIO              USE EXTENDED OPEN                            
         XC    0(128,R2),0(R2)                                                  
         ST    R2,SPOOLQLK                                                      
         OI    SPOOLIND,X'40'                                                   
         USING PQPLD,R2                                                         
         MVI   QLEXTRA,X'FF'                                                    
         MVC   QLCLASS,SGNONMC                                                  
         DROP  R2                                                               
*                                                                               
         GOTO1 STRTPRNT            OPEN PRINT INTERVAL                          
*                          BUILD MSG ON PQ                                      
         SPACE 3                                                                
         LA    R1,HEDSPECS                                                      
         ST    R1,SPECS                                                         
         LA    R1,HOOK                                                          
         ST    R1,HEADHOOK                                                      
*                                                                               
         LA    RB,SGRAPH                                                        
         USING SGRAPH,RB                                                        
         B     SGRAP1                                                           
         EJECT                                                                  
*                              SEND MESSAGE VIA GRAPHNET                        
         SPACE 3                                                                
SGRAPH   SUBNTR                                                                 
         LA    R2,P05SWKA                                                       
         MVC   0(60,R2),SPACES                                                  
         MVC   0(3,R2),=C'TWX'                                                  
         ZIC   R1,1(R6)                                                         
         SH    R1,=H'5'                                                         
         LA    R2,4(R2)                                                         
         EX    R1,SGRAEX                                                        
         LA    R2,2(R1,R2)                                                      
         L     R6,AIO                                                           
         BAS   RE,GETEL                                                         
SGRAS0   BNE   SGRAS1                                                           
         CLI   2(R6),2                                                          
         BE    *+12                                                             
         BAS   RE,NEXTEL                                                        
         B     SGRAS0                                                           
         ZIC   R1,1(R6)                                                         
         SH    R1,=H'5'                                                         
         BL    SGRAS1                                                           
         EX    R1,SGRAEX                                                        
         B     SGRAS1                                                           
SGRAEX   MVC   0(0,R2),4(R6)                                                    
*              RESTORE KEY                                                      
SGRAS1   MVC   KEY,KEYF                                                         
         MVI   MSGKPAGE,0                                                       
         GOTO1 HIGH                                                             
*              SET UP WHEN                                                      
         MVC   REMUSER,=C'MSG'                                                  
         MVI   TWAWHEN,0                                                        
         XC    TWAOUT,TWAOUT                                                    
         MVC   TWADEST,TWAORIG                                                  
         MVI   WHEN,X'40'                                                       
*                                                                               
         L     R2,AIO              USE EXTENDED OPEN                            
         XC    0(128,R2),0(R2)                                                  
         ST    R2,SPOOLQLK                                                      
         OI    SPOOLIND,X'40'                                                   
         USING PQPLD,R2                                                         
         MVI   QLEXTRA,X'FF'                                                    
         MVI   QLCLASS,C'T'    ***CHANGE TO G AFTER TESTED                      
         DROP  R2                                                               
         XC    HEADHOOK,HEADHOOK                                                
*                                                                               
         GOTO1 STRTPRNT            OPEN PRINT INTERVAL                          
*                          BUILD TWX ON PQ                                      
*                                 BUILD TWX HEADER FOR GRAPHNET                 
         MVC   P,SPACES                                                         
         GOTO1 GETREC                                                           
         MVC   P(13),=C'BLNY     GFDM'                                          
         MVI   WORK,1                                                           
         MVC   WORK+1(2),TWAORIG                                                
         GOTO1 GETSGN                                                           
         MVC   P(4),WORK+1                                                      
         CLI   P+3,C' '                                                         
         BNE   *+10                                                             
         MVC   P(4),WORK                                                        
*                                                                               
         LA    R6,P05SWKA                                                       
         MVC   P+14(40),4(R6)                                                   
*                                CHECK HERE FOR GRAPH=                          
*        CLC   P+14(6),=C'GRAPH='                                               
*        BNE   SGRAP0                                                           
*        MVC   P+9(4),P+20                                                      
*        MVC   P+14(25),SPACES                                                  
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVI   LINE,99                                                          
*                                                                               
         SPACE 3                                                                
SGRAP0   LA    R1,HEDSPECS                                                      
         ST    R1,SPECS                                                         
         LA    R1,HOOK                                                          
         ST    R1,HEADHOOK                                                      
         B     SGRAP2                                                           
*                                                                               
SGRAP1   GOTO1 GETREC                                                           
SGRAP2   GOTO1 SPOOL,DMCB,(R8)                                                  
         SR    R2,R2                                                            
         LA    R2,1  EL PTR                                                     
SGRAP2A  L     R6,AIO                                                           
         MVI   ELCODE,X'31'                                                     
         BAS   RE,GETEL                                                         
SGRAP2D  BNE   SGRAP2B                                                          
         ZIC   R1,2(R6)                                                         
         CR    R1,R2                                                            
         BE    SGRAP2C                                                          
         BAS   RE,NEXTEL                                                        
         B     SGRAP2D                                                          
SGRAP2C  CLI   1(R6),4                                                          
         BE    SGRAP2C1                                                         
         ZIC   R1,1(R6)                                                         
         SH    R1,=H'5'                                                         
         EXMVC R1,P,4(R6)                                                       
         OC    P,SPACES                                                         
SGRAP2C1 GOTO1 SPOOL,DMCB,(R8)                                                  
         LA    R2,1(R2)                                                         
         B     SGRAP2A                                                          
*                                                                               
SGRAP2B  GOTO1 SEQ                                                              
         CLC   KEY(35),KEYSAVE                                                  
         BE    SGRAP1   SAME MSG                                                
SGRA8A   GOTO1 STOPPRNT                                                         
*                                                                               
         MVC   SGNMSG,SPACES                                                    
*        L     R6,AIO2                                                          
*        MVC   SGNMSG+15(45),0(R6)                                              
         SUBXIT                                                                 
         EJECT                                                                  
*                     PRINT HEADING                                             
         SPACE 3                                                                
HOOK     SUBNTR                                                                 
*                                                                               
         MVC   H1+2(10),=C'**********'                                          
         MVC   H1+14(13),=C'MESSAGE FOR -'                                      
         MVC   H1+28(08),WORK                                                   
*                                                                               
         MVC   H2+20(4),=C'FROM'                                                
         MVC   H2+20+5(8),MSGKFROM                                              
*                                  AT                                           
         MVC   WORK+1(2),MSGKFRAT                                               
         MVI   WORK,1                                                           
         GOTO1 GETSGN                                                           
         MVC   H2+20+14(8),WORK                                                 
*                                  ADJUST DATE AND TIME                         
         MVC   WORK(1),SGNONOO                                                  
         CLC   OPNFILE,=C'INBOX   '                                             
         BNE   *+10                                                             
         MVC   WORK(1),SGNONOI                                                  
         CLI   WORK,C'F'                                                        
         BE    *+10                                                             
         XC    MSGKDATE(4),=X'FFFFFFFF'                                         
*                                  DATE                                         
         GOTO1 DATCON,DMCB,(2,MSGKDATE),(5,WORK)                                
         MVC   H2+49(8),WORK                                                    
*                                  TIME                                         
         SR    R0,R0                                                            
         ST    R0,WORK                                                          
         MVC   WORK+2(2),MSGKTIME                                               
         L     R1,WORK                                                          
         D     R0,=F'30'                                                        
         SR    R0,R0                                                            
         D     R0,=F'60'                                                        
         LA    R1,8(R1)                                                         
         CVD   R1,WORK       HOURS                                              
         UNPK  WORK(4),WORK+6(3)                                                
         MVC   H2+64(2),WORK                                                    
         MVI   H2+66,C':'                                                       
         CVD   R0,WORK       MINUTES                                            
         UNPK  WORK(4),WORK+6(3)                                                
         MVC   H2+67(2),WORK                                                    
*                                  SUBJECT                                      
         MVC   H3,SPACES                                                        
         MVC   H3+10(8),=C'SUBJECT='                                            
         MVI   ELCODE,X'30'                                                     
         L     R6,AIO                                                           
         BAS   RE,GETEL                                                         
         MVC   H3+18(40),SPACES                                                 
         ZIC   R1,1(R6)                                                         
         SH    R1,=H'5'                                                         
         EXMVC R1,H3+18,4(R6)                                                   
         OC    H3,SPACES                                                        
         SUBXIT                                                                 
         SPACE 1                                                                
HEDSPECS DC    X'00'                                                            
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DMPRTQL                                                        
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE PEMSGFILE                                                      
       ++INCLUDE PEMSGFFD                                                       
         PRINT ON                                                               
         ORG   SGNIH                                                            
       ++INCLUDE PEMSGFAD                                                       
         PRINT OFF                                                              
CONHEADH EQU   SGNMSGH                                                          
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE PEMSGWORKD                                                     
         PRINT ON                                                               
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'149PEMSG05   05/01/02'                                      
         END                                                                    
