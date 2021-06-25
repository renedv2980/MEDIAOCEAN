*          DATA SET CTLNK00    AT LEVEL 001 AS OF 04/26/01                      
*PHASE TA2200A                                                                  
CTLNK00  TITLE '- BUILD FALINK INPUT STRINGS'                                   
CTLNK00  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORKL,**LNK0**,R7                                                
         USING WORKD,RC                                                         
                                                                                
         MVC   VCOMFACS,12(R1)     SET COMFACS ADDRESS                          
                                                                                
         L     R9,20(R1)                                                        
         LR    RA,R9                                                            
         AHI   RA,4096                                                          
         USING TWAD,R9,RA          R9/RA=A(TWA)                                 
                                                                                
         L     RF,28(R1)                                                        
         USING TIOBD,RF                                                         
         SR    R0,R0               EXTRACT AND MODIFY PFKEY                     
         IC    R0,TIOBAID                                                       
         CHI   R0,12                                                            
         BNH   *+8                                                              
         SHI   R0,12                                                            
         STC   R0,PFKEY                                                         
         DROP  RF                                                               
                                                                                
         OI    LNKSRVH+1,X'01'                                                  
         OI    LNKSRVH+6,X'80'                                                  
         OI    LNKMSGH+6,X'80'                                                  
         OI    LNKPHSH+6,X'40'                                                  
         MVC   LNKMSG,REGMSG                                                    
                                                                                
         L     RF,VCOMFACS         GET ADDRESSES OF SUBS                        
         USING COMFACSD,RF         FROM COMFACS                                 
         MVC   VCALLOV,CCALLOV                                                  
         MVC   VDATAMGR,CDATAMGR                                                
         MVC   VHEXIN,CHEXIN                                                    
         MVC   VHEXOUT,CHEXOUT                                                  
         MVC   VXSORT,CXSORT                                                    
         DROP  RF                                                               
                                                                                
         CLI   SCREEN,0            TEST FIRST TIME                              
         BNE   *+8                                                              
         MVI   SCREEN,X'FF'        SET FF SCREEN IS LOADED                      
         CLI   SCREEN,X'FF'        TEST FF SCREEN IS LOADED                     
         BNE   *+14                                                             
         MVC   SAVEPHS,LNKPHS      YES - SAVE PHASE FIELD                       
         MVI   FLAG,0                                                           
                                                                                
         CLI   PFKEY,7             TEST START AGAIN                             
         BE    LNK04                                                            
                                                                                
         CLI   SCREEN,X'FD'        TEST FD SCREEN LOADED                        
         BNE   LNK02                                                            
         MVC   LNKMSG,ENDMSG1      YES - SEND YOU'RE DONE MESSAGE               
         OI    STRFSTH+6,X'C0'     POSITION CURSOR ON PHASE FIELD               
         J     EXIT                                                             
                                                                                
LNK02    CLC   SAVEPHS,LNKPHS      TEST CHANGE OF PHASE                         
         BE    LNK06                                                            
         OI    FLAG,X'04'                                                       
                                                                                
LNK04    GOTOR VCALLOV,DMCB,(X'FF',LNKMSGH),0,0,0                               
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVI   LNKLAST+1,1                                                      
         MVI   LNKLAST+2,0                                                      
                                                                                
         MVI   SCREEN,X'FF'                                                     
         TM    FLAG,X'04'                                                       
         MVI   FLAG,0                                                           
         JZ    EXIT                                                             
         MVC   LNKPHS,SAVEPHS                                                   
         OI    LNKPHSH+6,X'80'                                                  
         MVI   FLAG,0                                                           
         MVC   LNKMSG,REGMSG       OUTPUT REGULAR MESSAGE                       
                                                                                
LNK06    CLI   LNKPHS,C'T'                                                      
         BE    *+14                                                             
         MVC   LNKMSG,WARNMSG      ERROR - FIRST LETTER IS NOT 'T'              
         J     EXIT                                                             
                                                                                
         MVC   WORK(L'LNKPHS),LNKPHS                                            
         MVI   WORK,C'0'                                                        
         GOTOR VHEXIN,DMCB,WORK,PHASENUM,L'LNKPHS                               
         OC    12(4,R1),12(R1)                                                  
         BNZ   *+14                                                             
         MVC   LNKMSG,NOTHEX                                                    
         J     EXIT                                                             
                                                                                
         USING CTPHRECD,IO                                                      
         XC    CTPHPKEY,CTPHPKEY                                                
         MVI   CTPHID,CTPHIDQ                                                   
         MVI   CTPHSUBI,CTPHSUBQ                                                
         MVC   CTPHHEXN(L'PHASENUM),PHASENUM                                    
         GOTOR VDATAMGR,DMCB,(0,DMREAD),CTFILE,CTPHPKEY,CTPHPKEY                
         BE    *+14                                                             
         MVC   LNKMSG,NOPHSMS      SEND PHASE NOT FOUND                         
         J     EXIT                                                             
                                                                                
         SR    R0,R0                                                            
         ICM   R0,B'1000',=C'R'                                                 
         ICM   R0,B'0110',PHASENUM                                              
         LA    RF,LOADADDR                                                      
         ST    RF,APHASE                                                        
         GOTOR VCALLOV,DMCB,(RF),(R0),0,0                                       
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         SR    RE,RE                                                            
         ICM   RE,7,9(R1)                                                       
         A     RE,APHASE                                                        
         ST    RE,APHASEX                                                       
                                                                                
         NI    FLAG,X'FF'-X'08'    TURN OFF DESCRIPTIONS FLAG                   
         CLI   LNKSHRTH+5,0        CHECK IF THE USER WANTS ALL                  
         BNE   *+12                DESCRIPTIONS TO APPEAR                       
         MVI   LNKSHRT,C'N'                                                     
         OI    LNKSHRTH+6,X'80'                                                 
         CLI   LNKSHRT,C'N'                                                     
         BE    LNK08                                                            
         CLI   LNKSHRT,C'Y'                                                     
         BNE   *+12                                                             
         OI    FLAG,X'08'                                                       
         B     LNK08                                                            
         MVC   LNKMSG,ERRORMSG                                                  
         OI    LNKSHRTH+6,X'C0'                                                 
         J     EXIT                                                             
                                                                                
LNK08    CLI   PFKEY,6             IF PF6 WAS HIT                               
         BE    NXTELE                                                           
         CLI   PFKEY,8             IF PF8 WAS HIT                               
         BNE   VALHDR                                                           
         TM    FLAG,X'20'          CHECK IF HEADER DESCRIPTIONS FIT             
         BZ    NXTELE              ON ONE SCREEN, IF NOT, DON'T ALLOW           
         MVC   LNKMSG,MSG6         TO HIT PF8, UNTIL ALL DESCR ARE OUT          
         J     EXIT                                                             
         EJECT                                                                  
VALHDR   CLI   SCREEN,X'FF'        AT THIS POINT CAN ONLY BE FF SCREEN          
         JNE   EXIT                                                             
                                                                                
         LA    R2,LOADADDR                                                      
         LA    RE,1                                                             
         L     RF,APHASEX                                                       
         CLC   MAPLIT,0(R2)        LOOK FOR MAP IN LOADED PHASE                 
         BE    VALHDR02                                                         
         BXLE  R2,RE,*-10                                                       
         MVC   LNKMSG,ERMSG        CAN'T FIND MAP IN PHASE                      
         J     EXIT                                                             
                                                                                
VALHDR02 SR    R0,R0               HEADER COUNTER                               
         LA    R2,L'MAPLIT(R2)     POINT TO FIRST HEADER                        
         USING MHELD,R2                                                         
         LA    R3,HDRTAB                                                        
         USING HDRTABD,R3                                                       
         SR    R0,R0                                                            
                                                                                
VALHDR04 MVC   HDRTCOD,MHCODE      SET HEADER CODE                              
         LA    RF,MHELD                                                         
         S     RF,APHASE                                                        
         STCM  RF,3,HDRTDSP        SET DISPLACEMENT TO HEADER                   
         AHI   R3,HDRTABL          BUMP TO NEXT TABLE ENTRY                     
         ICM   RF,3,MHDISP                                                      
         AR    R2,RF               ADD DISPLACEMENT TO NEXT HEADER              
         AHI   R0,1                BUMP NUMBER OF TABLE ENTRIES                 
         CLI   0(R2),0             TEST END OF MAP                              
         BE    VALHDR06                                                         
         CHI   R0,HDRMAX           TEST TABLE MAXIMUM EXCEEDED                  
         BNH   VALHDR04                                                         
         MVC   LNKMSG,HDOVMSG      YES - TOO MANY MAPS ERROR                    
         J     EXIT                                                             
                                                                                
VALHDR06 STC   R0,HDRNUM                                                        
         MVI   HDRTABD,HDRTEOTQ    SET END OF HEADER TABLE                      
                                                                                
         LA    R3,HDRTAB                                                        
         USING HDRTABD,R3                                                       
         LA    R2,LNKHDRH                                                       
         USING LNKHDRH,R2                                                       
VALHDR08 GOTOR VHEXOUT,DMCB,HDRTCOD,LNKHDR,L'HDRTCOD                            
         OI    LNKHDRH+6,X'80'                                                  
         AHI   R2,HDRLEN                                                        
         AHI   R3,HDRTABL                                                       
         CLI   HDRTABD,HDRTEOTQ                                                 
         BNE   VALHDR08                                                         
         DROP  R2                                                               
                                                                                
         LA    R2,LNKHDRH          POINT TO FIRST INPUT LINE                    
         USING LNKHDRH,R2          POINT BACK TO FIRST INPUT LINE               
         LA    R3,HDRTAB                                                        
VALHDR10 CLI   LNKNUMH+5,0                                                      
         BNE   VALHDR12            IF SMTH ENTERED, GO CHECK FURTHER            
         CLI   LNKSEQH+5,0                                                      
         BNE   HDRERROR                                                         
         MVI   HDRTSEQ,X'FE'       TO SORT UNSELECTED FLDS LAST                 
         B     VALHDR14            GET READY TO CHECK NXT NUMB FIELD            
                                                                                
VALHDR12 TM    LNKNUMH+4,X'08'     TEST NUMBER IS VALID                         
         BZ    HDRERROR                                                         
         MVC   HDRTNUM,LNKNUM                                                   
         NI    HDRTNUM,X'0F'                                                    
                                                                                
         CLI   LNKSEQH+5,0         TEST SEQUENCE IS ENTERED                     
         BNE   *+12                                                             
         MVI   HDRTSEQ,X'FE'       NO - SORT TO BOTTOM                          
         B     VALHDR14                                                         
                                                                                
         TM    LNKSEQH+4,X'08'                                                  
         BZ    HDRERROR                                                         
         SR    R1,R1                                                            
         IC    R1,LNKSEQH+5                                                     
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,LNKSEQ(0)                                                    
         CVB   R1,DUB                                                           
         STC   R1,HDRTSEQ                                                       
                                                                                
VALHDR14 AHI   R2,HDRLEN           BUMP TO NEXT INPUT LINE                      
         AHI   R3,HDRTABL          BUMP TO NEXT TABLE ENTRY                     
         CLI   HDRTABD,HDRTEOTQ    TEST ALL DONE                                
         BNE   VALHDR10                                                         
         DROP  R2                                                               
                                                                                
         CLI   PFKEY,5             IF PF5 HIT, DIFFR SCREEN                     
         JNE   EXIT                                                             
         EJECT                                                                  
DISELE   CLI   SCREEN,X'FF'        PF5 IS HIT TO CNG FF SCR TO FE               
         JNE   EXIT                                                             
                                                                                
         SR    R0,R0                                                            
         IC    R0,HDRNUM           SORT HEADER TABLE INTO SEQUENCE              
         GOTOR VXSORT,DMCB,HDRTAB,(R0),HDRTABL,L'HDRTSEQ,0                      
                                                                                
         GOTOR VCALLOV,DMCB,(X'FE',LNKLOADH),0,0,0                              
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVI   SCREEN,X'FE'        SCREEN CHANGED                               
         NI    FLAG,X'FF'-X'80'                                                 
         MVI   ELMLAST+1,1                                                      
         MVI   ELMLAST+2,1                                                      
                                                                                
         LA    R3,HDRTAB           POINT TO MY TABLE                            
DISELE02 CLI   HDRTNUM,0           IF HDR NUMB NOT 0, PRT DESCRIPTION           
         BNE   DISELE04                                                         
         AHI   R3,HDRTABL          IF 0, FIND 1ST WHICH IS NOT 0                
         CLI   HDRTABD,HDRTEOTQ                                                 
         BNE   DISELE02                                                         
         MVC   LNKMSG,ERMSG3       NO HDRS SELECTED                             
         MVI   SCREEN,0                                                         
         MVC   ELMHDR,=C'0000'                                                  
         OI    ELMHDRH+6,X'80'                                                  
         J     EXIT                                                             
                                                                                
DISELE04 LA    R0,HDRTAB           SAVE DISPLACEMENT TO TABLE                   
         SR    R3,R0                                                            
         STCM  R3,3,HDRTABP                                                     
                                                                                
         LA    R0,STRING                                                        
         LHI   R1,STRINGL                                                       
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         LA    R5,STRING           SAVE DISPL TO STRING                         
         MVC   0(4,R5),=C'UEO '                                                 
         LA    R5,4(R5)                                                         
         LA    R0,STRING                                                        
         SR    R5,R0                                                            
         STCM  R5,3,STRINGP                                                     
                                                                                
         GOTOR NXTDSC              OUTPUT DESCRIPTION OF FIRST HEADER           
         TM    FLAG,X'20'                                                       
         JZ    EXIT                                                             
         MVC   LNKMSG,MSG3         NO HDRS SELECTED                             
         J     EXIT                                                             
         EJECT                                                                  
NXTELE   CLI   SCREEN,X'FF'        THERE'S NO PF6 ON FF SCREEN                  
         JE    EXIT                                                             
                                                                                
         OI    ELMINPH+6,X'C0'     POSITION CURSOR TO 1ST INPUT FIELD           
         NI    FLAG,X'FF'-X'20'                                                 
                                                                                
         SR    R3,R3                                                            
         ICM   R3,3,HDRTABP                                                     
         LA    R3,HDRTAB(R3)                                                    
                                                                                
         GOTOR VHEXIN,DMCB,ELMHDR,WORK,L'ELMHDR                                 
         CLC   HDRTCOD,WORK        COMPARE HEADER CODE FROM SCREEN              
         BE    *+14                WITH CURRENT HEADER CODE IN MY TABLE         
         MVC   LNKMSG,ERMSG1       HEADER MISMATCH                              
         J     EXIT                IF NOT =, ERROR                              
                                                                                
         LA    R2,ELMDESCH         POINT R2 TO 1ST DESCRIPTION FLD              
         LA    R6,ELMLDSCH         --"-- R6 TO LAST --- " ---                   
         SR    R5,R5                                                            
         ICM   R5,3,STRINGP                                                     
         LA    R5,STRING(R5)                                                    
         TM    FLAG,X'80'          IF FLAG IS EMPTY, NEW HDR                    
         BZ    NXTELE02                                                         
         SR    R4,R4                                                            
         ICM   R4,3,DISPHDEL       IF NOT, LOAD PTR TO NXT DESCRIPT,            
         A     R4,APHASE           WHICH DID NOT FIT ON ONE SCREEN              
         B     NXTELE06                                                         
                                                                                
NXTELE02 TM    FLAG,X'40'                                                       
         BNZ   NXTELE04                                                         
         MVI   0(R5),C'D'                                                       
         LA    RF,HDRTCOD                                                       
         LA    R0,L'HDRTCOD                                                     
         CLI   HDRTCOD,0                                                        
         BNE   *+16                                                             
         MVI   0(R5),C'B'                                                       
         AHI   RF,1                                                             
         SHI   R0,1                                                             
         MVI   1(R5),C'='                                                       
         AHI   R5,2                                                             
         GOTOR VHEXOUT,DMCB,(RF),(R5),(R0)                                      
         SLL   R0,1                                                             
         AR    R5,R0                                                            
                                                                                
NXTELE04 SR    R4,R4                                                            
         ICM   R4,3,HDRTDSP                                                     
         A     R4,APHASE                                                        
         SR    R0,R0                                                            
         IC    R0,MHLEN-MHELD(R4)                                               
         AR    R4,R0                                                            
         USING MDELD,R4                                                         
                                                                                
NXTELE06 CLI   MDLEN,0             END OF HEADER?                               
         BNE   *+12                                                             
         NI    FLAG,X'FF'-X'80'    IF YES, RESET FLAG TO INDIC NEW HDR          
         B     NXTELE22                                                         
         CR    R2,R6               PAST THE LAST SCREEN FIELD?                  
         BH    NXTELE20            IF YES, SET FLAG TO INDIC MORE DESCR         
         TM    FLAG,X'08'                                                       
         BNZ   *+12                                                             
         CLI   MDLEN,14                                                         
         BNE   NXTELE18            IF ELEM LENGTH IS NOT 14 - SKIP              
         CLC   8(2,R2),MDTEXT      MATCH SCREEN TEXT TO PHASE                   
         BE    *+14                                                             
         MVC   LNKMSG,ERMSG2       NO - ERROR                                   
         J     EXIT                                                             
                                                                                
         SR    R0,R0                                                            
         IC    R0,0(R2)            GET TO USER INPUT FIELD                      
         AR    R2,R0               TO VALIDATE DATA                             
         CLI   5(R2),0                                                          
         BE    NXTELE16                                                         
                                                                                
         CLI   MDTYPE,MDTMDQ                                                    
         BNE   NXTELE08                                                         
         MVI   0(R5),C'%'                                                       
         LA    R5,1(R5)                                                         
         B     NXTELE12                                                         
                                                                                
NXTELE08 SR    R1,R1                                                            
         IC    R1,5(R2)                                                         
         GOTOR TRTLEN              TRANSLATE LENGTH                             
         SR    R0,R0                                                            
         ICM   R0,1,PLUSCTR        ARE THERE ANY +'S IN LENGTH?                 
         BZ    NXTELE10                                                         
         MVI   0(R5),C'+'          PUT +'S IF ANY                               
         AHI   R5,1                                                             
         BCT   R0,*-8                                                           
                                                                                
NXTELE10 MVC   0(1,R5),LENGTH      PUT LENGTH                                   
         LA    R5,1(R5)                                                         
                                                                                
NXTELE12 SR    R1,R1                                                            
         ICM   R1,3,MDCODE                                                      
         GOTOR TRTLEN              TRANSLATE INTO LENGTH                        
         SR    R0,R0                                                            
         ICM   R0,1,PLUSCTR                                                     
         BZ    NXTELE14                                                         
         MVI   0(R5),C'+'                                                       
         AHI   R5,1                                                             
         BCT   R0,*-8                                                           
                                                                                
NXTELE14 MVC   0(1,R5),LENGTH                                                   
         AHI   R5,1                                                             
                                                                                
         CLI   MDTYPE,9                                                         
         BE    NXTELE16                                                         
                                                                                
         SR    R1,R1                                                            
         ICM   R1,1,5(R2)           GET INP LENGTH FOR EX MOVE                  
         BZ    NXTELE16                                                         
         BCTR  R1,0                                                             
         EX    R1,*+4               MOVE INPUT INTO STRING                      
         MVC   0(0,R5),8(R2)                                                    
         AHI   R1,1                 RESTORE INP LENGTH                          
         AR    R5,R1                ADJUST STRING POINTER BY INP LENGTH         
                                                                                
NXTELE16 SR    R0,R0                GET TO NXT DESCR FIELD ON SCREEN            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
                                                                                
NXTELE18 SR    R0,R0                GET TO NXT DESCR FIELD IN PHASE             
         IC    R0,MDLEN                                                         
         AR    R4,R0                                                            
         B     NXTELE06                                                         
         DROP  R4                                                               
                                                                                
NXTELE20 OI    FLAG,X'80'                                                       
         MVC   LNKMSG,OVFLMSG       DISPL MSG THAT THERE'S MORE DESCR           
         S     R4,APHASE            IF DESCR DIDN'T FIT REMEMBER WHERE          
         STCM  R4,3,DISPHDEL        IT STOPPED                                  
                                                                                
NXTELE22 LA    R0,STRING                                                        
         SR    R5,R0                SAVE DISPL WHERE LEFT OF IN STRING          
         STCM  R5,3,STRINGP                                                     
                                                                                
         LA    R1,ELMDESCH                                                      
         LA    RE,L'ELMDESCH+L'ELMDESC+L'ELMINPH+L'ELMINP                       
         LA    RF,ELMPFKH-1                                                     
         USING ELMDESCH,R1                                                      
NXTELE24 XC    ELMDESC,ELMDESC                                                  
         OI    ELMDESCH+6,X'80'                                                 
         XC    ELMINP,ELMINP                                                    
         OI    ELMINPH+1,X'20'                                                  
         OI    ELMINPH+6,X'80'                                                  
         BXLE  R1,RE,NXTELE24                                                   
         DROP  R1                                                               
                                                                                
         CLI   PFKEY,8                                                          
         BNE   *+12                                                             
         OI    FLAG,X'40'                                                       
         B     NXTELE30                                                         
         NI    FLAG,X'FF'-X'40'                                                 
         TM    FLAG,X'80'                                                       
         BO    NXTELE30                                                         
                                                                                
         SR    R0,R0                                                            
         IC    R0,HDRTNUM          DECREMENT HDR NUMBER                         
         BCTR  R0,0                IN CASE HEADER REQUESTED                     
         STC   R0,HDRTNUM          MORE THAN ONCE                               
                                                                                
NXTELE26 CLI   HDRTABD,HDRTEOTQ    CHECK FOR END OF HDR TABLE                   
         BE    PUTSTR                                                           
         CLI   HDRTNUM,0           IF NUMB=0, GOTO NXT TAB ENTRY                
         BNE   NXTELE28                                                         
         AHI   R3,HDRTABL                                                       
         B     NXTELE26                                                         
                                                                                
NXTELE28 LA    R0,HDRTAB                                                        
         SR    R3,R0                                                            
         STCM  R3,3,HDRTABP                                                     
                                                                                
NXTELE30 GOTOR NXTDSC              DISPL DESCR OF NXT HEADER OR                 
         TM    FLAG,X'20'                                                       
         JZ    EXIT                                                             
         MVC   LNKMSG,MSG3         NO HEADERS SELECTED                          
         J     EXIT                WHAT DIDN'T FIT ON FULL SCREEN               
         EJECT                                                                  
PUTSTR   GOTOR VCALLOV,DMCB,(X'FD',LNKFDSCH),0,0,0                              
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVI   SCREEN,X'FD'                                                     
         MVI   STRLAST+1,1                                                      
         MVI   STRLAST+2,1                                                      
                                                                                
         LA    R2,STRING                                                        
         LA    R1,STRFSTH                                                       
         LA    RE,L'STRFSTH+L'STRFST                                            
         LA    RF,STRLST                                                        
         MVC   L'STRFSTH(L'STRFST,R1),0(R2)                                     
         AHI   R2,L'STRFST                                                      
         BXLE  R1,RE,*-10                                                       
                                                                                
         MVC   LNKMSG,ENDMSG                                                    
         J     EXIT                                                             
         EJECT                                                                  
NXTDSC   NTR1  ,                                                                
         SR    R3,R3                                                            
         ICM   R3,3,HDRTABP                                                     
         LA    R3,HDRTAB(R3)                                                    
         USING HDRTABD,R3                                                       
         GOTOR VHEXOUT,DMCB,HDRTCOD,ELMHDR,L'HDRTCOD                            
         OI    ELMHDRH+6,X'80'                                                  
         LA    R2,ELMDESCH                                                      
         LA    R6,ELMLDSCH                                                      
                                                                                
         TM    FLAG,X'80'        IF NO FLAG, NEW HEADER                         
         BZ    NXTDSC02                                                         
         TM    FLAG,X'01'        IS IT NEXT PAGE                                
         BZ    *+14                                                             
         MVC   DISPHDEL,DSPLST   GIVE IT NEXT PAGE DISPLACEMENT                 
         NI    FLAG,X'FF'-X'01'                                                 
         SR    R4,R4                                                            
         ICM   R4,3,DISPHDEL     IF YES, START DESCR WHERE LEFT OFF             
         A     R4,APHASE                                                        
         B     NXTDSC04                                                         
                                                                                
NXTDSC02 SR    R4,R4                                                            
         ICM   R4,3,HDRTDSP                                                     
         A     R4,APHASE                                                        
         SR    R0,R0                                                            
         IC    R0,MHLEN-MHELD(R4)                                               
         AR    R4,R0                                                            
                                                                                
         USING MDELD,R4                                                         
NXTDSC04 CLI   MDLEN,0                                                          
         BE    NXTDSC10                                                         
         CR    R2,R6                                                            
         BH    NXTDSC08                                                         
         TM    FLAG,X'08'                                                       
         BNZ   *+12                                                             
         CLI   MDLEN,14            IF ELEM LENGTH IS NOT 14 SKIP                
         BNE   NXTDSC06                                                         
         MVC   ELMDESC-ELMDESCH(L'MDTEXT,R2),MDTEXT                             
         OI    6(R2),X'80'                                                      
         SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         NI    1(R2),X'FF'-X'20'                                                
         OI    6(R2),X'80'                                                      
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
NXTDSC06 SR    R0,R0                                                            
         IC    R0,MDLEN                                                         
         AR    R4,R0                                                            
         B     NXTDSC04                                                         
         DROP  R3,R4                                                            
                                                                                
NXTDSC08 OI    FLAG,X'20'                                                       
         S     R4,APHASE                                                        
         TM    FLAG,X'80'          IS IT EXCEEDING 2ND PAGE?                    
         BZ    *+12                                                             
         STCM  R4,3,DSPLST         YES, KEEP DISPL TO 3RD PAGE OF DESC          
         OI    FLAG,X'01'          AND DON'T TOUCH DESCR FOR 2ND PAGE           
         B     *+8                                                              
         STCM  R4,3,DISPHDEL       (PRETTY STUPID, I HOPE TO CHNGE IT)          
         B     NXTDSCX                                                          
                                                                                
NXTDSC10 CLI   ELMINPH+5,0                                                      
         BNE   NXTDSCX                                                          
         OI    ELMHDRH+6,X'C0'                                                  
                                                                                
NXTDSCX  J     EXIT                                                             
         EJECT                                                                  
TRTLEN   SR    R0,R0               '+' COUNTER                                  
TRTLEN02 CHI   R1,X'3F'                                                         
         BNH   TRTLEN04                                                         
         AHI   R0,1                IF HIGHER THQN 3F ADD '+'                    
         SHI   R1,X'3F'            AND SUBTRACT 3F FROM NUMBER                  
         B     TRTLEN02                                                         
TRTLEN04 STC   R1,LENGTH                                                        
         TR    LENGTH,LENGTHS                                                   
         STC   R0,PLUSCTR                                                       
         BR    RE                                                               
                                                                                
HDRERROR MVC   LNKMSG,ERRORMSG                                                  
         OI    6(R2),X'C0'                                                      
                                                                                
EXIT     XIT1  ,                                                                
         EJECT                                                                  
ERRORMSG DC    CL60'Invalid input'                                              
ERMSG    DC    CL60'FAMAP not found'                                            
HDOVMSG  DC    CL60'More than max headers found - can''t proceed'               
ERMSG1   DC    CL60'Header code mismatch'                                       
ERMSG2   DC    CL60'Description mismatch'                                       
ERMSG3   DC    CL60'No headers were selected - hit Pf7 to go back'              
NOPHSMS  DC    CL60'Check your phase entry'                                     
REGMSG   DC    CL60'Please make your selections'                                
OVFLMSG  DC    CL60'Here''s more descriptions for the same header'              
WARNMSG  DC    CL60'First character of phase name must be "T"'                  
NOTHEX   DC    CL60'System/program not valid hexadecimal'                       
MSG6     DC    CL60'There''s more for the same header, hit Pf6'                 
MSG3     DC    CL60'Header is too long don''t hit Pf8'                          
ENDMSG   DC    CL60'String displayed'                                           
ENDMSG1  DC    CL60'You''re done, hit PF7 to start again'                       
                                                                                
LENGTHS  DC    C'%ABCDEFGHIJKLMNO'                     00-0F                    
         DC    C'PQRSTUVWXYZ01234'                     10-1F                    
         DC    C'56789abcdefghijk'                     20-2F                    
         DC    C'lmnopqrstuvwxyz+'                     30-3F                    
                                                                                
DMREAD   DC    C'DMREAD  '                                                      
CTFILE   DC    C'CTFILE  '                                                      
MAPLIT   DC    C'**FAMAP*'                                                      
                                                                                
         LTORG                                                                  
                                                                                
LOADADDR DS    0D                  PHASE LOADED HERE                            
         EJECT                                                                  
WORKD    DSECT                                                                  
VDATAMGR DS    A                                                                
VCALLOV  DS    A                                                                
VHEXIN   DS    A                                                                
VHEXOUT  DS    A                                                                
VXSORT   DS    A                                                                
VCOMFACS DS    A                                                                
APHASE   DS    A                   A(LOADED PHASE)                              
APHASEX  DS    A                   A(END OF LOADED PHASE)                       
                                                                                
DUB      DS    D                                                                
LENGTH   DS    X                                                                
PLUSCTR  DS    X                                                                
DMCB     DS    6F                                                               
WORK     DS    XL64                                                             
IO       DS    1000X                                                            
WORKL    EQU   *-WORKD                                                          
         EJECT                                                                  
       ++INCLUDE FATWA                                                          
       ++INCLUDE CTLNKFFD                                                       
HDRLEN   EQU   L'LNKHDRH+L'LNKHDR+L'LNKNUMH+L'LNKNUM+L'LNKSEQH+L'LNKSEQ         
HDRMAX   EQU   (LNKPFKH-LNKHDRH)/HDRLEN                                         
         ORG   LNKLOADH                                                         
       ++INCLUDE CTLNKFED                                                       
         ORG   LNKFDSCH                                                         
       ++INCLUDE CTLNKFDD                                                       
         ORG   LNKWORK             SAVED STORAGE                                
                                                                                
PFKEY    DS    XL1                                                              
SCREEN   DS    XL1                                                              
PHASENUM DS    XL2                                                              
FLAG     DS    XL1                                                              
SAVEPHS  DS    CL(L'LNKPHS)                                                     
                                                                                
DISPHDEL DS    AL2                                                              
DSPLST   DS    AL2                                                              
                                                                                
HDRNUM   DS    X                                                                
HDRTABP  DS    AL2                                                              
HDRTAB   DS    (HDRMAX)XL(HDRTABL)                                              
                                                                                
STRINGP  DS    AL2                                                              
STRINGL  EQU   1600                                                             
STRING   DS    (STRINGL)X                                                       
                                                                                
HDRTABD  DSECT                     ** MAP HEADER TABLE **                       
HDRTEOTQ EQU   X'FF'               END OF TABLE INDICATOR                       
HDRTSEQ  DS    XL1                 SEQUENCE                                     
HDRTCOD  DS    XL(L'MHCODE)        CODE                                         
HDRTDSP  DS    XL2                 DISPLACEMENT TO HEADER                       
HDRTNUM  DS    XL1                 NUMBER OF ENTRIES                            
HDRTABL  EQU   *-HDRTABD                                                        
                                                                                
         PRINT OFF                                                              
       ++INCLUDE FATIOB                                                         
       ++INCLUDE DDCOMFACSD                                                     
       ++INCLUDE FALINKBLK                                                      
       ++INCLUDE CTGENPHASE                                                     
         PRINT ON                                                               
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001CTLNK00   04/26/01'                                      
         END                                                                    
