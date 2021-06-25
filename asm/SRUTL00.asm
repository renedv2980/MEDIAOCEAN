*          DATA SET SRUTL00    AT LEVEL 007 AS OF 08/10/07                      
*          DATA SET SRUTL00    AT LEVEL 003 AS OF 06/19/02                      
*PHASE T12300A                                                                  
*INCLUDE FAVIRTRM                                                               
         TITLE '$UTL - DISPLAY UTL DATA'                                        
         PRINT NOGEN                                                            
UTL      CSECT                                                                  
         NMOD1 WRKX-WRKD,**$UTL**,RR=RE                                         
         USING WRKD,RC                                                          
         ST    RE,RELO                                                          
         LA    RE,*+10                                                          
         O     RE,=XL4'80000000'                                                
         BSM   0,RE                THIS IS NOW IN XA MODE                       
         USING SRPARMD,R1                                                       
         MVC   SRPARS,SRPARM1      MOVE CALLING PARAMS                          
         DROP  R1                                                               
*                                                                               
         L     R3,SRPAR6           A(TWA)                                       
         USING SRUTLFFD,R3                                                      
         L     RA,SRPAR1           A(SYSFAC)                                    
         USING SYSFACD,RA                                                       
         L     R4,SRPAR4                                                        
         USING COMFACSD,R4                                                      
         MVC   VHEXOUT,CHEXOUT                                                  
         MVC   VTERMVAL,CTERMVAL                                                
         L     RF,=V(VIRTRM)                                                    
         A     RF,RELO                                                          
         ST    RF,AVIRTRM                                                       
         MVI   VIOLATE,C'N'                                                     
         MVI   ME,C' '                                                          
         MVI   DELETED,C' '                                                     
         MVI   WEB,C' '                                                         
         MVI   VIRTUAL,C' '                                                     
         MVI   VIRTACT,C' '                                                     
         MVI   LU62,C'N'                                                        
         MVI   TRMFILT,0                                                        
         XC    INPTNUM,INPTNUM                                                  
         L     RE,SRPAR3           EXTRACT MY UTL NUMBER FOR LOCAL COPY         
         MVC   MYTNUM,TNUM-UTLD(RE)                                             
         DROP  R4                                                               
         EJECT                                                                  
P1VAL    L     R5,VUTL             P1 - TERMINAL LUID                           
         USING UTLD,R5                                                          
         BAS   RE,SETBXLE          R5=A(FIRST UTL ENTRY)                        
         LA    R2,SRVP1H                                                        
         SR    R1,R1                                                            
         ICM   R1,1,5(R2)                                                       
         BZ    P1VALX              NO TERMINAL LUID INPUT                       
*                                                                               
         CLC   8(7,R2),=C'VIOLATE' SHOW VIOLATIONS                              
         BNE   *+12                                                             
         MVI   VIOLATE,C'Y'                                                     
         B     P1VALX                                                           
*                                                                               
         CLC   8(4,R2),=C'WEBREAL' SHOW WEB BASED REAL LUS                      
         BNE   *+16                                                             
         MVI   WEB,C'Y'                                                         
         MVI   VIRTUAL,C'R'                                                     
         B     P1VALX                                                           
         CLC   8(4,R2),=C'WEBVIRT' SHOW WEB BASED VIRTUAL LUS                   
         BNE   *+16                                                             
         MVI   WEB,C'Y'                                                         
         MVI   VIRTUAL,C'V'                                                     
         B     P1VALX                                                           
         CLC   8(3,R2),=C'WEB'     SHOW ALL WEB BASED LUS                       
         BNE   *+12                                                             
         MVI   WEB,C'Y'                                                         
         B     P1VALX                                                           
*                                                                               
         CLC   8(3,R2),=C'VIRTUAL' SHOW VIRTUAL LUS                             
         BNE   *+12                                                             
         MVI   VIRTUAL,C'V'                                                     
         B     P1VALX                                                           
         CLC   8(3,R2),=C'REAL'    SHOW REAL LUS                                
         BNE   *+12                                                             
         MVI   VIRTUAL,C'R'                                                     
         B     P1VALX                                                           
*                                                                               
         CLC   8(3,R2),=C'LU6'     SHOW LU6.2 DEVICES ONLY                      
         BNE   *+12                                                             
         MVI   LU62,C'Y'                                                        
         B     P1VALX                                                           
*                                                                               
         CLC   8(2,R2),=C'ME'      THIS MEANS MY TERMINAL                       
         BNE   P1V1                                                             
         L     RE,SRPAR3           GET A(LOCAL UTL ENTRY)                       
         MVC   INPTNUM,TNUM-UTLD(RE)                                            
         SR    RE,RE                                                            
         ICM   RE,3,INPTNUM                                                     
         AHI   RE,-1               TNUM-1                                       
         L     RF,VUTL                                                          
         MH    RE,0(RF)                                                         
         LA    R5,6(RE,RF)         R5=A(ACTUAL UTL ENTRY)                       
         ST    R5,INPAUTL                                                       
         B     P1VALX                                                           
*                                                                               
P1V1     CLI   8(R2),C'>'          TEST IF VIRTUAL TOKEN INPUT                  
         BNE   P1V2                                                             
         GOTO1 AVIRTRM,DMCB,(1,0),8(R2),(RA)                                    
         CLI   0(R1),0                                                          
         BNE   ERROR1                                                           
         L     R5,4(R1)            R5=A(VIRTUAL TERMINAL UTL ENTRY)             
         ST    R5,INPAUTL          SAVE INPUT TERMINAL A(UTL)                   
         MVC   INPTNUM,TNUM        SAVE INPUT TERMINAL NUM                      
         B     P1VALX                                                           
*                                                                               
P1V2     TM    4(R2),X'08'         TEST IF TERMINAL NUMBER INPUT                
         BO    P1V3                YES                                          
         LA    RF,8(R2)                                                         
P1V2A    CLI   0(RF),C'*'          TEST IF ANY WILD CARD CHRS IN UTL            
         BNE   *+12                                                             
         OI    TRMFILT,X'80'       SET WILD CARD FLAG                           
         B     P1VALX                                                           
         LA    RF,1(RF)                                                         
         BCT   R1,P1V2A                                                         
         CLI   5(R2),6                                                          
         BH    *+12                                                             
         OI    TRMFILT,X'40'       SET PART TERM FLAG                           
         B     P1VALX                                                           
*                                                                               
P1V3     GOTO1 VTERMVAL,DMCB,(R2)                                               
         L     R5,DMCB+4                                                        
         LTR   R5,R5                                                            
         BZ    ERROR1                                                           
         ST    R5,INPAUTL          SAVE INPUT TERMINAL A(UTL)                   
         MVC   INPTNUM,TNUM        SAVE INPUT TERMINAL NUM                      
P1VALX   EQU   *                                                                
         SPACE 2                                                                
P2VAL    LA    R2,SRVP2H           P2 - VIRTUAL TERMINAL ACTION                 
         SR    R1,R1                                                            
         ICM   R1,1,5(R2)                                                       
         BZ    P2VALX              NO DATA INPUT IN P2                          
         OC    INPTNUM,INPTNUM                                                  
         BZ    ERROR2              NO LUID INPUT IN P1                          
*                                                                               
P2V2     CLC   8(3,R2),=C'ADD'     LUID/ADD                                     
         BNE   P2V3                                                             
         MVI   VIRTACT,C'A'                                                     
         B     P2VALX                                                           
*                                                                               
P2V3     CLC   8(3,R2),=C'DEL'     LUID/DEL                                     
         BNE   P2V4                                                             
         TM    TVIFLAG,TVIVIRT     TERMINAL IN P1 MUST BE VIRTUAL               
         BZ    ERROR2                                                           
         MVI   VIRTACT,C'D'                                                     
         B     P2VALX                                                           
*                                                                               
P2V4     CLC   8(5,R2),=C'CLEAR'   LUID/CLEAR                                   
         BNE   P2V5                                                             
         TM    TVIFLAG,TVIVIRT     TERMINAL IN P1 MUST BE REAL                  
         BO    ERROR2                                                           
         MVI   VIRTACT,C'E'                                                     
         B     P2VALX                                                           
*                                                                               
P2V5     CLC   8(3,R2),=C'TOK'     LUID/TOKEN DISPLAY TOKEN                     
         BNE   P2V6                                                             
         TM    TVIFLAG,TVIVIRT     TERMINAL IN P1 MUST BE VIRTUAL               
         BZ    ERROR2                                                           
         MVI   VIRTACT,C'T'                                                     
         B     P2VALX                                                           
*                                                                               
P2V6     CLC   8(3,R2),=C'DIS'     LUID/DISP DISPLAY REAL/VIRTUAL SET           
         BE    *+14                                                             
         CLC   8(3,R2),=C'OWN'                                                  
         BNE   P2V7                                                             
         TM    TVIFLAG,TVIVIRT     TEST IF TRM IN P1 IS VIRTUAL                 
         BZ    P2V6A               NO                                           
         MVC   INPTNUM,TVIOWNER    YES SWITCH TO OWNER TERMINAL                 
         SR    RE,RE                                                            
         ICM   RE,3,INPTNUM                                                     
         BZ    ERROR2                                                           
         AHI   RE,-1               TNUM-1                                       
         L     RF,VUTL                                                          
         MH    RE,0(RF)                                                         
         LA    R5,6(RE,RF)         R5=A(OWNER UTL ENTRY)                        
         ST    R5,INPAUTL                                                       
P2V6A    MVI   ME,C'Y'             SET THE ME FLAG                              
         L     R5,VUTL                                                          
         AHI   R5,6                START SEARCH FROM FIRST UTL ENTRY            
         B     P2VALX                                                           
*                                                                               
P2V7     EQU   *                                                                
*                                                                               
P2VALX   EQU   *                                                                
         SPACE 2                                                                
P3VAL    LA    R2,SRVP3H           P3 - OTHER OPTIONS                           
         SR    R1,R1                                                            
         ICM   R1,1,5(R2)                                                       
         BZ    P3VALX              NO DATA INPUT IN P2                          
*                                                                               
         CLC   8(3,R2),=C'DELETED' SHOW DELETIONS                               
         BNE   *+12                                                             
         MVI   DELETED,C'Y'                                                     
         B     P3VALX                                                           
*                                                                               
P3VALX   EQU   *                                                                
         EJECT                                                                  
UT01     CLI   VIRTACT,C'A'        ADD A NEW VIRTUAL TERMINAL                   
         BNE   UT02                                                             
         L     R0,INPAUTL                                                       
         CLC   INPTNUM,MYTNUM                                                   
         BNE   *+6                                                              
         SR    R0,R0                                                            
         GOTO1 AVIRTRM,DMCB,(6,0),(R0),(RA)                                     
         SR    RE,RE                                                            
         ICM   RE,7,1(R1)                                                       
         MVC   VIRTTOK,0(RE)                                                    
         XC    SRVMSG,SRVMSG                                                    
         MVC   SRVMSG(10),VIRTTOK                                               
         OI    SRVMSGH+6,X'80'                                                  
         B     EXIT                                                             
*                                                                               
UT02     CLI   VIRTACT,C'D'        DELETE A VIRTUAL TERMINAL                    
         BNE   UT03                                                             
         L     R0,INPAUTL                                                       
         CLC   INPTNUM,MYTNUM                                                   
         BNE   *+6                                                              
         SR    R0,R0                                                            
         GOTO1 AVIRTRM,DMCB,(7,0),(R0),(RA)                                     
         SR    RE,RE                                                            
         ICM   RE,7,1(R1)                                                       
         MVC   VIRTTOK,0(RE)                                                    
         XC    SRVMSG,SRVMSG                                                    
         MVC   SRVMSG(10),VIRTTOK                                               
         OI    SRVMSGH+6,X'80'                                                  
         B     EXIT                                                             
*                                                                               
UT03     CLI   VIRTACT,C'E'        DELETE ALL VIRTUAL TRERMINALS                
         BNE   UT04                                                             
         L     R0,INPAUTL                                                       
         CLC   INPTNUM,MYTNUM                                                   
         BNE   *+6                                                              
         SR    R0,R0                                                            
         GOTO1 AVIRTRM,DMCB,(8,0),(R0),(RA)                                     
         SR    RE,RE                                                            
         ICM   RE,7,1(R1)                                                       
         MVC   VIRTTOK,0(RE)                                                    
         CLI   VIRTTOK,C' '                                                     
         BNE   *+10                                                             
         MVC   VIRTTOK,=CL10'*ALL GONE'                                         
         XC    SRVMSG,SRVMSG                                                    
         MVC   SRVMSG(10),VIRTTOK                                               
         OI    SRVMSGH+6,X'80'                                                  
         B     EXIT                                                             
*                                                                               
UT04     CLI   VIRTACT,C'T'        DISPLAY VIRTUAL TERMINAL TOKEN               
         BNE   UT05                                                             
         GOTO1 AVIRTRM,DMCB,(3,0),INPAUTL,(RA)                                  
         SR    RE,RE                                                            
         ICM   RE,7,1(R1)                                                       
         MVC   VIRTTOK,0(RE)                                                    
         XC    SRVMSG,SRVMSG                                                    
         MVC   SRVMSG(10),VIRTTOK                                               
         OI    SRVMSGH+6,X'80'                                                  
         B     EXIT                                                             
*                                                                               
UT05     EQU   *                                                                
*                                                                               
UT10     LA    R2,SRVLN1           R5 POINTS TO FIRST UTL ENTRY                 
         USING UTLINE,R2                                                        
*                                                                               
UT11     XC    LUID,LUID           SET LUID OF NEXT TERMINAL                    
         CLI   TRMFILT,0           ANY TERMINAL FILTERING                       
         BE    UT11X               NO                                           
         TM    TRMFILT,X'01'       OK IF HAVE FOUND FIRST QUALIFIER             
         BO    UT11X                                                            
         MVC   LUID,TLUID                                                       
         LA    RF,SRVP1            RF=A(INPUT LUID)                             
         SR    R1,R1               R1=L'INPUT LUID                              
         IC    R1,SRVP1H+5                                                      
         TM    TRMFILT,X'80'       WILD CARD                                    
         BO    UT11A                                                            
         TM    TRMFILT,X'40'       SHORT FORM                                   
         BO    UT11B                                                            
         B     UT11X                                                            
UT11A    LA    RE,LUID             WILD CARD MATCHING                           
         LTR   R1,R1                                                            
         BZ    UT11X                                                            
UT11A1   CLI   0(RF),C'*'                                                       
         BE    *+14                                                             
         CLC   0(1,RF),0(RE)                                                    
         BNE   UTNEXT                                                           
         LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         BCT   R1,UT11A1                                                        
         B     UT11X                                                            
UT11B    BCTR  R1,0                SHORT LUID MATCHING                          
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   LUID(0),0(RF)                                                    
         BNE   UTNEXT                                                           
         OI    TRMFILT,X'01'       SET WE HAVE FOUND FIRST MATCH                
         B     UT11X                                                            
UT11X    EQU   *                                                                
*                                                                               
UT12     CLI   VIOLATE,C'Y'        TEST VIOLATIONS ONLY                         
         BNE   *+12                                                             
         TM    TSTAT1,TSTATSSV     TEST SECURITY VIOLATION                      
         BZ    UTNEXT                                                           
*                                                                               
UT13     CLI   ME,C'Y'             TEST ME AND MY VIRTUALS ONLY                 
         BNE   UT14                                                             
         CLC   INPTNUM,TNUM        MATCH ON REAL TERMINAL NUMBER                
         BE    UTDISP                                                           
         TM    TVIFLAG,TVIVIRT                                                  
         BZ    UTNEXT                                                           
         CLC   INPTNUM,TVIOWNER    MATCH ON VIRTUAL TERM OWNER                  
         BE    UTDISP                                                           
         B     UTNEXT                                                           
*                                                                               
UT14     CLI   LU62,C'Y'           TEST LU62 TERMINALS                          
         BNE   *+12                                                             
         TM    TSTAT4,TST4LU62                                                  
         BZ    UTNEXT                                                           
*                                                                               
UT15     CLI   WEB,C'Y'            TEST WEB TERMINALS                           
         BNE   *+12                                                             
         TM    TSTAT1,TSTATWEB                                                  
         BZ    UTNEXT                                                           
*                                                                               
UT16     CLI   VIRTUAL,C'R'        TEST REAL TERMINALS ONLY                     
         BNE   *+12                                                             
         TM    TVIFLAG,TVIVIRT                                                  
         BO    UTNEXT                                                           
*                                                                               
UT17     CLI   VIRTUAL,C'V'        TEST VIRTUAL TERMINALS ONLY                  
         BNE   *+12                                                             
         TM    TVIFLAG,TVIVIRT                                                  
         BZ    UTNEXT                                                           
*                                                                               
UTDISP   CLI   TLUID,C'*'          TEST IF DELETED                              
         BNE   *+12                                                             
         CLI   DELETED,C'Y'                                                     
         BNE   UTNEXT                                                           
*                                                                               
UTDISP2  SR    R0,R0                                                            
         ICM   R0,3,TNUM                                                        
         BAS   RE,CVD                                                           
         MVC   UTNUM,WORK+5                                                     
*                                                                               
UTDISP3  LR    R0,R5                                                            
         BAS   RE,CVHEX                                                         
         MVC   UTAUTL,WORK+2                                                    
         CLC   TNUM,MYTNUM                                                      
         BNE   *+8                                                              
         MVI   UTAUTL-1,C'='       FLAG MY OWN TERMINAL                         
*                                                                               
UTDISP4  CLI   LUID,0                                                           
         BNE   *+10                                                             
         MVC   LUID,TLUID                                                       
         MVC   UTSYM,LUID                                                       
*                                                                               
         TM    TSTATU,TSTATAVA                                                  
         BZ    *+8                                                              
         MVI   UTSYM+L'UTSYM,C'.'  FLAG AVAILABLE                               
*                                                                               
UTDISP5  MVC   UTDEVC,=C'TRM '                                                  
         OC    TPRNT,TPRNT                                                      
         BZ    UTDISP5A                                                         
         MVC   UTDEVC,=C'PRN '                                                  
         TM    TTYPE,X'10'                                                      
         BZ    *+10                                                             
         MVC   UTDEVC,=C'SHU '                                                  
         B     UTDISP6                                                          
UTDISP5A CLI   TVICOUNT,0          TEST IF OWNS VIRTUAL TERMINALS               
         BE    UTDISP6                                                          
         MVI   UTDEVC+3,C'+'       FLAG ENTRY                                   
         CLI   TVICOUNT,9                                                       
         BH    UTDISP6                                                          
         MVC   UTDEVC+3(1),TVICOUNT                                             
         OI    UTDEVC+3,X'F0'      SHOW COUNT IF 1 THRU 9                       
*                                                                               
UTDISP6  MVC   UTTYPE,=C'3270'                                                  
         TM    TTYPE,TTYPETWX                                                   
         BZ    *+10                                                             
         MVC   UTTYPE,=C'TWX '                                                  
         TM    TSTAT4,TST4LU62                                                  
         BZ    *+10                                                             
         MVC   UTTYPE,=C'LU62'                                                  
*                                                                               
UTDISP7  TM    TVIFLAG,TVIVIRT     DIFFERENT DISPLAY IF VIRTUAL                 
         BZ    UTDISP8                                                          
         SR    R0,R0                                                            
         ICM   R0,3,TVIOWNER       OWNER TERMINAL NUMBER                        
         BAS   RE,CVD                                                           
         MVC   UTDEVC-1(5),WORK+5                                               
         CLI   UTDEVC-1,C'0'                                                    
         BNE   *+8                                                              
         MVI   UTDEVC-1,C' '                                                    
         GOTO1 AVIRTRM,DMCB,(3,0),(R5),(RA)                                     
         SR    RE,RE                                                            
         ICM   RE,7,1(R1)                                                       
         MVC   VIRTTOK,0(RE)       VIRTUAL TOKEN                                
         MVC   UTTYPE-1(10),VIRTTOK                                             
*                                                                               
UTDISP8  ZIC   RE,SRVLN1H          GET LENGTH OF LINE + HEADER,                 
         LA    RF,SRVLAST+7                                                     
         BXLE  R2,RE,UTNEXT        GOTO NEXT SCREEN LINE.                       
         LA    RF,SRVLAST+8        A HALF SCREEN IS FINISHED.                   
         CR    R2,RF               WHICH HALF.....                              
         BH    EXIT                BOTH HALVES ARE FULL                         
         LA    R2,SRVLN1+40        START SECOND HALF OF SCREEN.                 
*                                                                               
UTNEXT   BXLE  R5,R6,UT11                                                       
*                                                                               
EXIT     XIT1  ,                                                                
         ORG   *-2                                                              
         BSM   0,RE                RESTORE CALLERS ADDRESSING MODE              
         EJECT                                                                  
ERROR1   MVC   SRVMSG(L'ERR1),ERR1                                              
         B     ERRXT                                                            
ERROR2   MVC   SRVMSG(L'ERR2),ERR2                                              
         B     ERRXT                                                            
ERR1     DC    CL60'** ERROR ** TERMINAL LUID NOT VALID'                        
ERR2     DC    CL60'** ERROR ** INVALID VIRTUAL ACTION'                         
*                                                                               
ERRXT    NI    SRVIDH+6,X'BF'      UNSET CURSOR                                 
         OI    6(R2),X'40'         INSERT CURSOR AT ERROR                       
         B     EXIT                                                             
         SPACE 2                                                                
SETBXLE  LH    R6,0(R5)                                                         
         L     R7,2(R5)                                                         
         LA    R5,6(R5)                                                         
         BR    RE                                                               
*                                                                               
CVD      SLL   R0,8                                                             
         SRL   R0,8                                                             
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK(10),DUB                                                     
         BR    RE                                                               
         SPACE 2                                                                
CVHEX    NTR1                                                                   
         XC    WORK,WORK                                                        
         ST    R0,DUB                                                           
         GOTO1 VHEXOUT,DMCB,DUB,WORK+2,4,=C'TOG'                                
         XIT1                                                                   
*                                                                               
DOTS     DC    10C'.'                                                           
SPACES   DC    10C' '                                                           
         LTORG                                                                  
         EJECT                                                                  
WRKD     DSECT                                                                  
*                                                                               
SRPARS   DS    0CL24                                                            
SRPAR1   DS    A                                                                
SRPAR2   DS    A                                                                
SRPAR3   DS    A                                                                
SRPAR4   DS    A                                                                
SRPAR5   DS    A                                                                
SRPAR6   DS    A                                                                
*                                                                               
RELO     DS    A                                                                
AVIRTRM  DS    A                                                                
VHEXOUT  DS    A                                                                
VTERMVAL DS    A                                                                
*                                                                               
DMCB     DS    CL24                                                             
DUB      DS    D                                                                
LUID     DS    CL8                                                              
INPAUTL  DS    A                                                                
INPTNUM  DS    XL2                                                              
MYTNUM   DS    XL2                                                              
WORK     DS    CL10                                                             
TRMFILT  DS    X                                                                
VIOLATE  DS    C                                                                
LU62     DS    C                                                                
DELETED  DS    C                                                                
ME       DS    C                                                                
WEB      DS    C                                                                
VIRTUAL  DS    C                                                                
VIRTACT  DS    C                                                                
VIRTRSLT DS    C                                                                
VIRTLUID DS    CL8                                                              
VIRTTOK  DS    CL10                                                             
WRKX     EQU   *                                                                
         SPACE 2                                                                
UTLINE   DSECT                                                                  
*                                                                               
UTNUM    DS    CL5                 TRM NUMBER NNN                               
         DS    CL1                                                              
UTAUTL   DS    CL8                 A(UTL ENTRY)                                 
         DS    CL1                                                              
UTSYM    DS    CL8                 TRM SYMBOLIC ID                              
         DS    CL1                                                              
UTDEVC   DS    CL4                 TRM DEVICE                                   
         DS    CL1                                                              
UTTYPE   DS    CL4                 TRM TYPE                                     
         DS    CL1                                                              
         DS    CL5                 SPARE                                        
*                                                                               
*        MAX LEN = 39                                                           
         EJECT                                                                  
* DDCOMFACS                                                                     
       ++INCLUDE DDCOMFACS                                                      
         EJECT                                                                  
* FADSECTS                                                                      
       ++INCLUDE FADSECTS                                                       
         EJECT                                                                  
SRUTLFFD DSECT                                                                  
         DS    CL64                                                             
* SRUTLFFD                                                                      
       ++INCLUDE SRUTLFFD                                                       
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007SRUTL00   08/10/07'                                      
         END                                                                    
