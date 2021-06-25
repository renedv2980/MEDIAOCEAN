*          DATA SET TAGEND0    AT LEVEL 013 AS OF 07/20/12                      
*PHASE T702D0C,*                                                                
         TITLE 'T702D0 - FINAL CAST COMPLETION REPORT'                          
T702D0   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T702D0,R7,RR=R2                                                
         L     RC,0(R1)            RC=GENCON STORAGE AREA                       
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(TWA)                                    
         USING T702FFD,RA                                                       
         L     R9,ASYSD            R9=SYSTEM STORAGE AREA                       
         USING SYSD,R9                                                          
         L     R8,ASPOOLD          R8=SPOOL STORAGE AREA                        
         USING SPOOLD,R8                                                        
         EJECT                                                                  
*                                                                               
         SPACE 1                                                                
         GOTO1 INITIAL,DMCB,0                                                   
*                                                                               
         CLI   MODE,VALKEY                                                      
         BNE   FCL10                                                            
         BAS   RE,VKEY                                                          
         B     XIT                                                              
*                                                                               
FCL10    CLI   MODE,PRINTREP                                                    
         BNE   XIT                                                              
*                                                                               
         CLI   PFAID,23            RERUN REPORT/NO COMMENT                      
         BE    FCL20                                                            
         CLI   PFAID,24            ADD NEW COMMENT TO REPORT                    
         BNE   ENTCOM                                                           
*                                                                               
         CLI   FCLLIN1H+5,0        CHECK FOR INPUT                              
         BNE   FCL15                                                            
         CLI   FCLLIN2H+5,0                                                     
         BNE   FCL15                                                            
         CLI   FCLLIN3H+5,0                                                     
         BNE   FCL15                                                            
         CLI   FCLLIN4H+5,0                                                     
         BE    NOCOMER                                                          
FCL15    BAS   RE,BLDREC           ADD F TYPE COMMENT RECORD                    
*                                                                               
FCL20    BAS   RE,GETDET           GET COMM'L DETAILS FOR HEADHOOK              
         ST    R2,RELO             RELOCATION FACTOR                            
         LA    R1,MYSPECS          SET A(SPECS)                                 
         ST    R1,SPECS                                                         
         LA    R1,HDHOOK           SET A(HEADINGS ROUTINE)                      
         ST    R1,HEADHOOK                                                      
         MVI   FORCEHED,C'Y'       SET TO START ON NEW PAGE                     
*                                                                               
         BAS   RE,PREP             PRINT REPORT                                 
*                                                                               
         MVC   CONACT,=C'DISPLAY'  AFTER RETURNING FROM REPORT CHANGE           
         OI    CONACTH+6,X'80'     ACTION TO DISPLAY AND CLEAR PRINT            
         XC    CONWHEN,CONWHEN     FIELD                                        
         OI    CONWHENH+6,X'80'                                                 
         MVI   GOAGAIN,C'Y'        REDISPLAY COMM'L RECORD                      
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO VALIDATE KEY                                          
         SPACE 1                                                                
VKEY     NTR1                                                                   
         LA    R2,FCLAGYH            FILL IN AGENCY AND CID ON SCREEN           
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'20',FCLAGYH)  SET GLOBAL AGENCY           
         MVC   8(L'TGAGY,R2),TGAGY                                              
         OI    4(R2),X'20'           SET VALIDATED                              
         OI    6(R2),X'80'           TRANSMIT                                   
         GOTO1 RAVPPLSA,DMCB,0     SEE IF REC / ACT INVALID FOR P+              
         JNE   ERPPLSI                                                          
*                                                                               
         LA    R2,FCLCIDH                                                       
         GOTO1 RECVAL,DMCB,TLCOICDQ,(X'20',FCLCIDH)  SET GLOBAL CID             
         MVC   8(L'TGCID,R2),TGCID                                              
         OI    4(R2),X'20'           SET VALIDATED                              
         OI    6(R2),X'80'           TRANSMIT                                   
         L     R3,AIO                                                           
         USING TLCOD,R3                                                         
         MVC   TGCOM,TLCOCOM                                                    
         DROP  R3                                                               
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO ADD TYPE F COMMENT RECORD                             
         SPACE 1                                                                
BLDREC   NTR1                                                                   
*                                                                               
         MVI   TGSEQ,0                                                          
         MVI   TGTYPE,TLCMTFCL   COMMENT TYPE F                                 
         MVI   TGVER,0           DON'T USE VERSION FOR NOW                      
         GOTO1 RECVAL,DMCB,TLCMCDQ,(X'84',TGTYPE)  READ FOR LATEST CMNT         
         MVI   TGSEQ,X'FF'       START SEQUENCE AT COMPLEMENT OF ZERO           
         CLC   KEY(TLCMVER-TLCMD),KEYSAVE                                       
         BNE   BLDR20                                                           
*                                F TYPE COMMENT RECORD ALREADY EXISTS           
         USING TLCMD,R3          SUBTRACT 1 FROM SEQUENCE TO GET                
         LA    R3,KEY            NEXT SEQUENCE                                  
         ZIC   RE,TLCMSEQ                                                       
         AHI   RE,-1                                                            
         CR    RE,0              MAKE SURE THE SEQUENCE IS > THAN 0             
         BNL   *+6                                                              
         DC    H'00'                                                            
         STC   RE,TGSEQ                                                         
*                                                                               
BLDR20   GOTO1 RECVAL,DMCB,TLCMCDQ,(X'C0',TGTYPE)  BUILD KEY                    
*                                                                               
         L     R3,AIO                                                           
         LA    RE,10                                                            
BLDR25   XC    0(200,R3),0(R3)      CLEAR AIO                                   
         LA    R3,200(R3)                                                       
         BCT   RE,BLDR25                                                        
*                                                                               
         L     R3,AIO                                                           
         USING TLCMD,R3                                                         
         MVC   TLCMKEY,KEY          STORE KEY IN AIO                            
         DROP  R3                                                               
*                                                                               
         LA    R2,FCLLIN1H          COMMENTS                                    
         LA    R4,4                 R4 = # OF COMMENT FIELDS                    
         LA    R5,1                 R5 = SEQUENCE NUMBER                        
BLDR30   CLI   5(R2),0                                                          
         BE    BLDR40                                                           
         XC    ELEMENT,ELEMENT      BUILD ELEMENT FOR EACH                      
         LA    R3,ELEMENT           COMMENT FIELD WITH DATA                     
         USING TAXCD,R3                                                         
         MVI   TAXCEL,TAXCELQ       ELEMENT CODE                                
         ZIC   RE,5(R2)                                                         
         AHI   RE,4                                                             
         STC   RE,TAXCLEN           ELEMENT LENGTH                              
         STC   R5,TAXCSEQ           SEQUENCE NUMBER                             
         ZIC   RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   TAXCCMNT(0),8(R2)    COMMENT                                     
         GOTO1 ADDELEM                                                          
BLDR40   ZIC   RE,0(R2)             BUMP TO NEXT COMMENT FIELD                  
         AR    R2,RE                                                            
         LA    R5,1(R5)             INCREMENT SEQUENCE NUMBER                   
         BCT   R4,BLDR30                                                        
         DROP  R3                                                               
*                                                                               
         GOTO1 ACTVIN,DMCB,0        ADD LAST CHANGED ELEMENT                    
         GOTO1 ADDREC                                                           
         B     XIT                                                              
         SPACE 3                                                                
         EJECT                                                                  
*              ROUTINE TO PRINT REPORT                                          
         SPACE 1                                                                
PREP     NTR1                                                                   
         LA    R3,P                R3=A(PRINT LINE)                             
         USING PRNTD,R3                                                         
*                                                                               
         MVC   AIO,AIO1                                                         
*                                                                               
         BAS   RE,PRNTCOMH       PRINT COMM'L INFO HEADINGS                     
*                                                                               
         BAS   RE,SVCDETL        SAVE SOME COMM'L DETAILS                       
*                                                                               
         MVI   YESLIFT,0         INITIALIZE LIFT FLAG                           
         MVI   YESVERS,0         INITIALIZE VERSION FLAG                        
         MVI   SVCSEQ,TLCOV026   INITIALIZE SEQUENCE NUMBER                     
*                                                                               
         L     R4,AIO            CHECK FOR LIFT                                 
         MVI   ELCODE,TALFELQ    GET LIFT ELEMENT                               
         BAS   RE,GETEL                                                         
         BNE   PREP20                                                           
         MVI   YESLIFT,1         SET LIFT FLAG                                  
         MVC   AIO,AIO1          USE MAIN COMERCIAL TO GET INFO                 
         B     PREP40                                                           
*                                                                               
PREP20   L     R4,AIO                                                           
         MVC   TGCOM,SVCOM       SET GLOBAL COMM ID                             
         MVI   ELCODE,TAVRELQ    GET VERSION ELEMENT                            
         BAS   RE,GETEL                                                         
         BNE   PREP40                                                           
         MVI   YESVERS,1         SET VERSIONS FLAG                              
         USING TAVRD,R4                                                         
PREP30   MVC   SVVER,TAVRVERS    SAVE VERSION LETTER                            
         MVC   PRNTLET+1(1),TAVRVERS                                            
         MVC   PRNTISCI,TAVRCID  VERSION COMM ID                                
         EDIT  (B1,TAVRSEC),(3,PRNTLEN),ALIGN=LEFT    LENGTH IN SECONDS         
*                                                                               
         TM    TGSYSTAT,TASYS3VR                                                
         BZ    PREP35                                                           
         EDIT  TAVRVERS,PRNTLET,ALIGN=LEFT                                      
         DROP  R4                                                               
PREP35   MVC   AIO,AIO2                                                         
         GOTO1 RECVAL,DMCB,TLVRCDQ,(X'A4',SVVER)                                
         BE    PREP40                                                           
         MVC   AIO,AIO1          USE MAIN COMM'L IF NO VERSION RECORD           
*                                                                               
PREP40   BAS   RE,PRNTCOMM       PRINT COMM'L INFO                              
         CLI   YESVERS,1         IF NO VERSIONS, DON'T GET NEXT ELEMENT         
         BNE   PREP50                                                           
         MVI   ELCODE,TAVRELQ                                                   
         BAS   RE,NEXTEL                                                        
         BE    PREP30                                                           
*                                                                               
PREP45   CLI   SVCSEQ,TLCOV250   IF NO MORE VERSIONS ON THIS COMM'L             
         BE    PREP60            RECORD, CHECK REST OF COMMERCIAL RECS          
*                                                                               
         ZIC   RE,SVCSEQ         ADD TO LATEST SEQUENCE NUMBER                  
         AHI   RE,1                                                             
         STC   RE,SVCSEQ                                                        
*                                                                               
         L     R4,AIO1                                                          
         XC    KEY,KEY                                                          
         MVC   KEY(L'TLCOKEY),0(R4)                                             
         MVC   KEY+TLCOVER-TLCOD(1),SVCSEQ                                      
         GOTO1 HIGH                                                             
         CLC   KEY(L'TLCOKEY),KEYSAVE                                           
         BNE   PREP45                                                           
         MVC   AIO,AIO3          PREPARE TO READ INTO AIO1                      
         GOTO1 GETREC                                                           
*                                                                               
         L     R4,AIO                                                           
         MVI   ELCODE,TAVRELQ    ENSURE RECORD HAS VERSION                      
         BAS   RE,GETEL                                                         
         BNE   PREP45                                                           
         B     PREP30                                                           
*                                                                               
PREP50   CLI   YESLIFT,1         IF COMM HAS A LIFT PRINT LIFT DETAILS          
         BNE   PREP60                                                           
         USING TALFD,R4          R4=A(LIFT ELEMENT)                             
         MVI   PRNTLET+1,C'L'    L FOR LIFT                                     
         MVC   PRNTISCI,TALFLID  LIFT ID                                        
         EDIT  (B1,TALFSEC),(3,PRNTLEN),ALIGN=LEFT    LENGTH IN SECONDS         
         DROP  R4                                                               
         MVI   YESLIFT,2         CHANGE FLAG SO WE DON'T REPEAT THIS,           
         B     PREP40            BUT WE STILL KNOW WE HAVE A LIFT               
*                                                                               
PREP60   MVC   AIO,AIO1                                                         
         BAS   RE,PRNTCAST       PRINT CAST INFO                                
*                                                                               
         BAS   RE,PRNTCMNH       PRINT COMMENT HEADINGS                         
*                                                                               
         MVC   TGCOM,SVCOM       SET GLOBAL COMM ID                             
         MVI   TGVER,0           NO VERSION                                     
         MVI   TGTYPE,TLCMTFCL   COMMENT TYPE F                                 
         MVI   TGSEQ,X'FF'       START WITH 1ST COMPLEMENTED SEQUENCE           
         MVC   AIO,AIO2                                                         
         GOTO1 RECVAL,DMCB,TLCMCDQ,(X'A0',TGTYPE)  FIRST TIME                   
         BE    PREP75                                                           
*                                                                               
         MVI   TGSEQ,0           SEE IF WE ARE MISSING 1ST CMNT RECS            
         GOTO1 RECVAL,DMCB,TLCMCDQ,(X'84',TGTYPE)  READ FOR LATEST CMNT         
         CLC   KEY(TLCMVER-TLCMD),KEYSAVE                                       
         BNE   PREPX             IF NO CMNT RECORDS, EXIT                       
         MVI   TGSEQ,X'FF'       START WITH 1ST COMPLEMENTED SEQUENCE           
PREP65   BAS   RE,PRNTBCMT       PRINT BLANK COMMENT                            
         ZIC   R0,TGSEQ          DECREMENT SEQUENCE BY 1                        
         AHI   R0,-1                                                            
         STC   R0,TGSEQ                                                         
         GOTO1 RECVAL,DMCB,TLCMCDQ,(X'A0',TGTYPE)  GET NEXT CMNT RECORD         
         BNE   PREP65                                                           
         B     PREP75            PRINT CMNT AND SEE IF WE HAVE MORE             
*                                                                               
PREP70   GOTO1 RECVAL,DMCB,TLCMCDQ,(X'A0',TGTYPE)  GET COMMENT RECORD           
         BNE   PREPX                                                            
PREP75   BAS   RE,PRNTCMNT       PRINT COMMENT INFO                             
         ZIC   R0,TGSEQ          DECREMENT SEQUENCE BY 1                        
         AHI   R0,-1                                                            
         STC   R0,TGSEQ                                                         
         B     PREP70            SEE IF THERE ARE MORE COMMENT RECORDS          
*                                                                               
PREPX    B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
*              ROUTINE TO PRINT COMMERCIAL INFORMATION                          
         SPACE 1                                                                
PRNTCOMH NTR1                                                                   
         MVC   P+1(L'LTCOM),LTCOM  PRINT COMM DETAIL HEADINGS                   
         MVC   P2+1(L'LTCOMU),LTCOMU                                            
         BAS   RE,PRNTIT                                                        
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO PRINT COMMERCIAL INFORMATION                          
         SPACE 1                                                                
         USING PRNTD,R3            R3=A(PRINT LINE)                             
PRNTCOMM NTR1                                                                   
         CLC   PRNTISCI,SPACES     IF ISCI IS FROM VERSION ELEMENT,             
         BNE   PRCOM10             WE ALREADY HAVE ISCI AND LENGTH              
         L     R4,AIO              R4--> COMM RECORD                            
         MVI   ELCODE,TACOELQ                                                   
         BAS   RE,GETEL            GET COMM'L DETAILS ELEMENT                   
         BE    *+6                                                              
         DC    H'00'                                                            
         USING TACOD,R4                                                         
         MVC   PRNTISCI,TACOCID    COMM ISCI                                    
         EDIT  (B1,TACOSEC),(3,PRNTLEN),ALIGN=LEFT    LENGTH IN SECONDS         
*                                                                               
PRCOM10  GOTO1 CHAROUT,DMCB,TANAELQ,0   COMM NAME                               
         MVC   PRNTTITL,TGNAME                                                  
*                                                                               
         BAS   RE,GETCON         GET MUSIC CONTRACTS AND TRACKS                 
         BE    *+8                                                              
         BAS   RE,PRNTIT         IF NO CONTRACTS, PRINT COMM'L DETAILS          
*                                                                               
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
*              ROUTINE TO PRINT CAST INFORMATION                                
         SPACE 1                                                                
PRNTCAST NTR1                                                                   
         MVC   P+1(L'LTCAST),LTCAST   PRINT CAST DETAIL HEADINGS                
         MVC   P2+1(L'LTCAST2),LTCAST2                                          
         MVC   P3+1(L'LTCASTU),LTCASTU                                          
         BAS   RE,PRNTIT                                                        
*                                                                               
         LA    R2,TSARAREA       R2=A(SORT RECORD)                              
         USING SORTD,R2                                                         
*                                                                               
         MVC   AIO,AIO2                                                         
         USING TLCAD,R4                                                         
         LA    R4,KEY                                                           
         XC    KEY,KEY           READ CAST RECORDS                              
         MVI   TLCACD,TLCACDQ                                                   
         MVC   TLCACOM,TGCOM                                                    
         GOTO1 HIGH                                                             
         B     PRCST20                                                          
PRCST10  GOTO1 SEQ                                                              
PRCST20  MVC   SVKEY,KEY                                                        
         CLC   KEY(TLCASORT-TLCAD),KEYSAVE  SAME COMMERCIAL?                    
         BNE   PRCST30                                                          
         GOTO1 GETREC                                                           
         L     R4,AIO                                                           
*                                                                               
         LA    RE,TSARAREA       INITIALIZE TSARAREA TO SPACES                  
         MVC   0(L'SPACES,RE),SPACES                                            
         AHI   RE,L'SPACES                                                      
         MVC   0(L'TSARAREA-L'SPACES,RE),SPACES                                 
*                                                                               
         MVC   SORTSRT,TLCASORT  CAST SORT KEY                                  
         MVC   SORTSSN,TLCASSN   SSN                                            
         BAS   RE,GETCAT         GET EXPANDED CATEGORY NAME                     
         BAS   RE,GETCADT        GET CAST DETAILS ELEMENT                       
         BAS   RE,GETW4NM        GET W4 NAME/CORP NAME                          
         BAS   RE,GETCASTV       GET VERSIONS A CAST IS ON                      
         BAS   RE,GETOVSC        GET OVERSCALE PERCENTAGE                       
*                                                                               
         MVI   MYTSACTN,TSAADD   ADD TSAR RECORD                                
         BAS   RE,CALLTSAR                                                      
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         MVC   KEY(L'SVKEY),SVKEY  RESTORE KEY                                  
         GOTO1 HIGH                                                             
         B     PRCST10                                                          
*                                                                               
PRCST30  BAS   RE,SRTPRT         PRINT SORTED TSAR RECORDS                      
*                                                                               
         GOTO1 TSARCNTL,DMCB,0   INDICATE NO LONGER USING TSAR                  
         MVC   AIO,AIO1                                                         
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO PRINT COMMENT HEADINGS                                
         SPACE 1                                                                
PRNTCMNH NTR1                                                                   
         MVC   P+1(L'LTVER),LTVER  PRINT VERSION DETAIL HEADINGS                
         MVC   P2+1(L'LTVERU),LTVERU                                            
         BAS   RE,PRNTIT                                                        
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO PRINT COMMENT INFORMATION                             
         SPACE 1                                                                
         USING PRNTD,R3            R3=A(PRINT LINE)                             
PRNTCMNT NTR1                                                                   
         MVC   SVSEQ,TGSEQ                                                      
         XI    SVSEQ,X'FF'       UNCOMPLEMENT SEQUENCE                          
         ZIC   R0,SVSEQ          INCREMENT SEQUENCE SO WE START WITH 1          
         AHI   R0,1                                                             
         EDIT  (R0),PRNTVER,ALIGN=RIGHT,ZERO=NOBLANK                            
*                                                                               
         L     R4,AIO        PRINT USER ID AND DATE COMMENT WAS ADDED           
         GOTO1 ACTVOUT,DMCB,(X'20',FULL)                                        
         OC    0(4,R1),0(R1)                                                    
         BZ    PCMNT05                                                          
         L     R4,0(R1)                                                         
         USING TAACD,R4                                                         
         GOTO1 DATCON,DMCB,(1,TAACCDTE),(8,PRNTDATE)                            
         MVC   PRNTUSER,TAACSTAF                                                
         DROP  R4                                                               
*                                                                               
PCMNT05  L     R4,AIO              R4--> COMMENT RECORD                         
         MVI   ELCODE,TAXCELQ                                                   
         BAS   RE,GETEL            IF NO COMMENTS ELEMENTS, JUST PRINT          
         BE    PCMNT20             ACTIVITY ELEMENT INFO                        
         BAS   RE,PRNTIT                                                        
         B     PCMNTX                                                           
PCMNT10  BAS   RE,NEXTEL                                                        
         BNE   PCMNTX                                                           
         USING TAXCD,R4                                                         
PCMNT20  ZIC   RE,TAXCLEN                                                       
         SHI   RE,5                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   PRNTCMT(0),TAXCCMNT   PRINT COMMENT                              
         BAS   RE,PRNTIT                                                        
         MVI   ELCODE,TAXCELQ                                                   
         B     PCMNT10                                                          
         DROP  R4                                                               
*                                                                               
PCMNTX   B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
*              ROUTINE TO PRINT BLANK (MISSING) COMMENT                         
*              DUE TO FILE CONVERSION - 1ST FEW COMMENTS ARE MISSING            
         SPACE 1                                                                
         USING PRNTD,R3            R3=A(PRINT LINE)                             
PRNTBCMT NTR1                                                                   
         MVC   SVSEQ,TGSEQ                                                      
         XI    SVSEQ,X'FF'       UNCOMPLEMENT SEQUENCE                          
         ZIC   R0,SVSEQ          INCREMENT SEQUENCE SO WE START WITH 1          
         AHI   R0,1                                                             
         EDIT  (R0),PRNTVER,ALIGN=RIGHT,ZERO=NOBLANK                            
         BAS   RE,PRNTIT                                                        
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
*              ROUTINE TO SAVE INTERNAL COMM ID AND FILM DATE,                  
*              RECORDING DATE, AND MUSIC DATE                                   
         SPACE 1                                                                
SVCDETL  NTR1                                                                   
         L     R4,AIO                                                           
         USING TLCOD,R4                                                         
         MVC   SVCOM,TLCOCOM     SAVE INTERNAL COMM ID                          
         DROP  R4                                                               
*                                                                               
         MVI   ELCODE,TACSELQ                                                   
         MVI   BYTE,TACSTYPF                                                    
         GOTO1 GETL,DMCB,(1,BYTE)  GET FILM STUDIO ELEMENT                      
         BNE   SVCD10                                                           
         L     R4,TGELEM                                                        
         USING TACSD,R4                                                         
         MVC   SVFDAT,TACSDATE     SAVE FILM DATE                               
SVCD10   MVI   BYTE,TACSTYPR                                                    
         GOTO1 GETL,DMCB,(1,BYTE)  GET RECORDING STUDIO ELEMENT                 
         BNE   SVCD15                                                           
         L     R4,TGELEM                                                        
         MVC   SVRDAT,TACSDATE     SAVE RECORDING DATE                          
SVCD15   MVI   BYTE,TACSTYPM                                                    
         GOTO1 GETL,DMCB,(1,BYTE)  GET MUSIC STUDIO ELEMENT                     
         BNE   XIT                                                              
         L     R4,TGELEM                                                        
         MVC   SVMDAT,TACSDATE     SAVE MUSIC DATE                              
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*              ROUTINE TO GET MUSIC CONTRACT DETAILS                            
         SPACE 1                                                                
         USING PRNTD,R3            R3=A(PRINT LINE)                             
GETCON   NTR1                                                                   
         BAS   RE,TRN2TAMC                                                      
*                                                                               
         L     R4,AIO                                                           
         MVI   ELCODE,TAMCELQ      GET MUSIC CONTRACT DETAILS ELEMENT           
         BAS   RE,GETEL                                                         
         BNE   GCON20              IF NONE, TRY OLD CONTRACT ELEMENT            
         B     *+12                                                             
GCON10   BAS   RE,NEXTEL                                                        
         BNE   YES                                                              
         USING TAMCD,R4                                                         
         MVC   PRNTCON,TAMCCON     CONTRACT NUMBER                              
         MVC   PRNTBASC,TAMCBAS    AFM BASIC                                    
         MVC   PRNTTRK,TAMCTRK     TRACK                                        
         BAS   RE,PRNTIT                                                        
         B     GCON10                                                           
         DROP  R4                                                               
*                                                                               
GCON20   L     R4,AIO                                                           
         MVI   ELCODE,TACCELQ      CONTRACT ELEMENT                             
         BAS   RE,GETEL                                                         
         BNE   NO                                                               
         USING TACCD,R4                                                         
         ZIC   R0,TACCNCON         NUMBER OF CONTRACTS                          
         LA    R5,TACCCON          R5=A(1ST CONTRACT IN ELEMENT)                
GCON30   MVC   PRNTCON,0(R5)       CONTRACT NAME                                
         BAS   RE,PRNTIT                                                        
         LA    R5,L'TACCCON(R5)    BUMP TO NEXT CONTRACT                        
         BCT   R0,GCON30                                                        
         B     YES                                                              
         DROP  R3,R4                                                            
         EJECT                                                                  
*              ROUTINE TO PRINT SORTED TSAR RECORDS                             
         SPACE 1                                                                
         USING PRNTD,R3            R3=A(PRINT LINE)                             
SRTPRT   NTR1                                                                   
         MVC   MYTSRNUM,=H'1'                                                   
         MVI   MYTSACTN,TSAGET                                                  
         B     *+8                                                              
SRPRT05  MVI   MYTSACTN,TSANXT                                                  
         BAS   RE,CALLTSAR       GET TSAR RECORD                                
         BNE   XIT               NO MORE RECORDS, EXIT                          
         MVI   SSNFLG,0          RESET NEW SSN FLAG                             
         USING SORTD,R2                                                         
         L     R2,TCATSAR        R2=A(TSAR REC)                                 
*                                                                               
SRPRT10  CLC   LASTSSN,SORTSSN   IS NEW SSN THE SAME AS LAST SSN?               
         BNE   *+12              PRINT NAME AND SSN ONCE                        
         MVI   SSNFLG,1          SET FLAG FOR SAME SSN                          
         B     SRPRT20                                                          
*                                                                               
         MVC   PRNTSSN,SORTSSN   SSN                                            
         MVC   LASTSSN,SORTSSN   SAVE AS LAST SSN FOR NEXT TIME                 
*                                                                               
         TM    TGSYSTAT,TASYSPID ARE WE USING PID?                              
         BZ    SRPRT15                                                          
         MVC   PRNTSSN,SPACES                                                   
         GOTO1 SSNPACK,DMCB,SORTSSN,PRNTSSN                                     
*                                                                               
SRPRT15  XC    NAMEAREA,NAMEAREA                                                
         MVC   NAMEAREA(L'SORTFNAM),SORTFNAM                                    
         MVC   NAMEAREA+17(L'SORTLNAM),SORTLNAM                                 
         GOTO1 SQUASHER,DMCB,NAMEAREA,33                                        
         MVC   PRNTNAME,NAMEAREA   FIRST AND LAST NAME                          
         MVC   PRNTCORP,SORTCORP   CORPORATION NAME, IF ANY                     
*                                                                               
SRPRT20  MVC   PRNTCAT,SORTCAT     CATEGORY                                     
         MVC   PRNTCAM,SORTCAM     ON/OFF CAMERA                                
         MVC   PRNTUNI,SORTUNI     UNION                                        
         GOTO1 DATCON,DMCB,(1,SORT1SER),(8,PRNT1SER)   1ST SERVICES             
         MVC   PRNTOV1,SORTOV1     OVERSCALE %                                  
*                                                                               
         XC    SVCOM2,SVCOM2                                                    
         XC    SVCOM3,SVCOM3                                                    
         LA    RE,SORTCOM                                                       
         MVC   PRNTCOM,SORTCOM     VERSIONS THE CAST IS ON                      
         AHI   RE,L'PRNTCOM                                                     
         MVC   SVCOM2,0(RE)                                                     
         AHI   RE,L'PRNTCOM                                                     
         MVC   SVCOM3,0(RE)                                                     
*                                                                               
         MVC   PRNTLFI,SORTLFI   LIFTED FROM ISCI                               
         BAS   RE,PRNTIT                                                        
         CLI   SSNFLG,1          IF SAME SSN, DON'T PRINT CORPID AGAIN          
         BNE   SRPRT30                                                          
         MVC   SORTFID,SPACES                                                   
         CLC   SVCOM2,SPACES                                                    
         BNH   SRPRT05                                                          
         MVC   PRNTCOM,SVCOM2    2ND LINE OF VERSIONS CAST IS ON                
         B     SRPRT40                                                          
*                                                                               
SRPRT30  CLC   SORTFID,SPACES    IF THERE IS A CORPID (FEDERAL ID)              
         BNH   SRPRT35           PRINT ON THE NEXT LINE                         
         MVC   PRNTSSN,SORTFID                                                  
*                                                                               
         TM    TGSYSTAT,TASYSPID ARE WE USING PID?                              
         BZ    SRPRT33                                                          
         MVC   PRNTSSN,SPACES                                                   
         GOTO1 SSNPACK,DMCB,SORTFID,PRNTSSN                                     
*                                                                               
SRPRT33  CLC   SVCOM2,SPACES     ALSO PRINT 2ND LINE OF VERSIONS                
         BNH   SRPRT40                                                          
         MVC   PRNTCOM,SVCOM2                                                   
         B     SRPRT40                                                          
*                                                                               
SRPRT35  CLC   SVCOM2,SPACES     NO CORPID, IS THERE A 2ND LINE                 
         BNH   SRPRT05           OF VERSIONS?                                   
         MVC   PRNTCOM,SVCOM2                                                   
*                                                                               
SRPRT40  BAS   RE,PRNTIT                                                        
         CLC   SVCOM3,SPACES     IS THERE A 3RD LINE OF VERSIONS?               
         BNH   SRPRT05                                                          
         MVC   PRNTCOM(L'SVCOM3),SVCOM3                                         
         BAS   RE,PRNTIT                                                        
         B     SRPRT05           GET NEXT RECORD                                
*                                                                               
         DROP  R2,R3                                                            
         EJECT                                                                  
*              ROUTINE TO GET CATEGORY NAME                                     
         SPACE 1                                                                
         USING SORTD,R2        R2 --> TSAR RECORD                               
         USING TLCAD,R4        R4 --> CAST RECORD                               
GETCAT   NTR1                                                                   
         XC    NAMEAREA,NAMEAREA                                                
         L     RF,TGACATS                                                       
         USING CATTABD,RF                                                       
GCAT10   CLI   0(RF),X'FF'     TEST END OF TABLE                                
         BE    XIT                                                              
         CLC   CATCDE,TLCACAT                                                   
         BE    GCAT20                                                           
         ZIC   R1,CATLEN                                                        
         AR    RF,R1                                                            
         B     GCAT10                                                           
GCAT20   ZIC   RE,CATLEN      SET CATEGORY NAME                                 
         SH    RE,=Y(CATNAME-CATTABD+1)                                         
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   SORTCAT(0),CATNAME                                               
         B     XIT                                                              
         DROP  R2,R4                                                            
         EJECT                                                                  
*              ROUTINE TO GET CAST DETAILS ELEMENT                              
         SPACE 1                                                                
         USING SORTD,R2        R2 --> TSAR RECORD                               
         USING TLCAD,R4        R4 --> CAST RECORD                               
GETCADT  NTR1                                                                   
         MVC   AIO,AIO2                                                         
         MVI   ELCODE,TACAELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         USING TACAD,R4                                                         
         MVC   SORTCAM,TACAONOF  ON/OFF CAMERA                                  
         MVC   SORTUNI,TACAUN    UNION                                          
         MVC   SORTCRPN,TACACORP CORPORATION NUMBER                             
         MVC   SORT1SER,TACAFRST 1ST SERVICES                                   
         OC    SORT1SER,SORT1SER IF NO 1ST SERVICES ON CAST RECORD              
         BNZ   GCADT20                                                          
         CLC   SORTUNI,=C'AFM'   IF AFM CAST, USE MUSIC DATE                    
         BE    GCADT15                                                          
         MVC   SORT1SER,SVFDAT   NON AFM, USE FILM DATE IF ON CAMERA OR         
         CLC   TACAONOF,=CL3'ON'                                                
         BE    GCADT10                                                          
         MVC   SORT1SER,SVRDAT   RECORDING DATE IF OFF CAMERA                   
GCADT10  OC    SORT1SER,SORT1SER IF NO FILM OR RECORDING DATE,                  
         BNZ   GCADT20                                                          
GCADT15  MVC   SORT1SER,SVMDAT   USE MUSIC DATE IF AVAILABLE                    
*                                                                               
GCADT20  CLI   YESLIFT,2         DOES THIS COMM'L HAVE A LIFT?                  
         BNE   GCADT30                                                          
         TM    TACASTAT,TACASTLF+TACASTLO   ONLY ON LIFT?                       
         BNO   *+12                                                             
         MVI   SORTCOM,C'O'                                                     
         B     GCADT30                                                          
         TM    TACASTAT,TACASTLF PERFORMER ON LIFT AND MAIN COMM'L?             
         BNO   GCADT30                                                          
         MVI   SORTCOM,C'Y'                                                     
*                                                                               
GCADT30  OC    TACALFTF,TACALFTF   LIFTED FROM ISCI FIELD?                      
         BZ    XIT                                                              
         MVC   AIO,AIO3                                                         
         GOTO1 RECVAL,DMCB,TLCOCCDQ,(X'A0',TACALFTF)                            
         BE    *+6                                                              
         DC    H'00'                                                            
         MVC   AIO,AIO2                                                         
         DROP  R4                                                               
         L     R4,AIO3                                                          
         MVI   ELCODE,TACOELQ      GET COMM DETAILS ELEMENT                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         USING TACOD,R4                                                         
         MVC   SORTLFI,TACOCID     LIFTED FROM ISCI                             
         B     XIT                                                              
         DROP  R2,R4                                                            
         EJECT                                                                  
*              ROUTINE TO GET W4 NAME AND/OR CORP NAME                          
         SPACE 1                                                                
         USING SORTD,R2        R2 --> TSAR RECORD                               
         USING TLCAD,R4        R4 --> CAST RECORD                               
GETW4NM  NTR1                                                                   
         MVC   AIO,AIO3                                                         
         GOTO1 RECVAL,DMCB,TLW4CDQ,(X'A0',TLCASSN)   GET W4 RECORD              
         BE    GW4NM10                                                          
         MVC   SORTFNAM(15),=CL15'** NOT FOUND **'  IF NOT FOUND                
         B     GW4NMX                                                           
         DROP  R4                                                               
*                                                                               
GW4NM10  BAS   RE,GETNAME                                                       
*                                                                               
         L     R4,AIO                                                           
         MVI   ELCODE,TATIELQ      GET TAX ID ELEMENT                           
         MVI   HALF,TATITYCO                                                    
         MVC   HALF+1(1),SORTCRPN  CORPORATION NUMBER                           
         GOTO1 GETL,DMCB,(2,HALF)                                               
         BNE   GW4NMX                                                           
         USING TATID,R4                                                         
         L     R4,TGELEM                                                        
         MVC   SORTFID,TATIID      SAVE CORPID TO PRINT ON NEXT LINE            
         GOTO1 RECVAL,DMCB,TLW4CDQ,(X'A0',TATIID)  GET W4 REC OF CORPID         
         BE    *+6                                                              
         DC    H'00'                                                            
         BAS   RE,GETNAME                                                       
GW4NMX   MVC   AIO,AIO2                                                         
         B     XIT                                                              
         DROP  R2,R4                                                            
         EJECT                                                                  
*                                                                               
*              ROUTINE TO GET W4 NAME AND/OR CORP NAME                          
         SPACE 1                                                                
         USING SORTD,R2        R2 --> TSAR RECORD                               
GETNAME  NTR1                                                                   
         L     R4,AIO          R4 --> W4 RECORD                                 
         MVI   ELCODE,TAW4ELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         USING TAW4D,R4                                                         
         CLI   TAW4TYPE,TAW4TYCO     IS W4 TYPE CORPORATION?                    
         BE    GNAME10                                                          
         MVC   SORTFNAM(L'TAW4NAM1),TAW4NAM1                                    
         MVC   SORTLNAM(L'TAW4NAM2),TAW4NAM2                                    
         B     *+10                                                             
GNAME10  MVC   SORTCORP(L'TAW4CRPN),TAW4CRPN                                    
         B     XIT                                                              
         DROP  R2,R4                                                            
         EJECT                                                                  
*              ROUTINE TO GET VERSIONS A CAST MEMBER IS ON                      
*              R4 --> CAST RECORD                                               
         SPACE 1                                                                
         USING SORTD,R2        R2 --> TSAR RECORD                               
GETCASTV NTR1                                                                   
         CLI   YESLIFT,2       IF COMM'L HAS A LIFT, SKIP THIS                  
         BE    XIT                                                              
*                                                                               
         TM    TGSYSTAT,TASYS3VR  ALSO SKIP IF WE'RE USING NUMERIC              
         BO    XIT                VERSIONS, UNTIL WE HAVE TIME                  
*                                                                               
         XC    BLOCK(34),BLOCK    DUMMY FIELD FOR CHAROUT                       
         MVI   BLOCK,34  L'FLD=8(L'HDR)+26(MAX NUM OF VERSIONS)                 
         GOTO1 CHAROUT,DMCB,TAFNELQ,BLOCK,TAFNTVER                              
         BNE   XIT                                                              
         MVC   SORTCOM,BLOCK+8                                                  
         OC    SORTCOM,SPACES                                                   
         B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
*              ROUTINE TO GET OVERSCALE AMOUNT                                  
*              R4 --> CAST RECORD                                               
         SPACE 1                                                                
         USING SORTD,R2        R2 --> TSAR RECORD                               
GETOVSC  NTR1                                                                   
         MVI   ELCODE,TAOPELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         USING TAOPD,R4                                                         
         ICM   R5,15,TAOPPCT                                                    
         GOTO1 DR2DEC,DMCB,(R5),SORTOV1,3                                       
         DROP  R2,R4                                                            
         B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE DISPLAYS THE 2 DEC PLACE NUMBER FOUND IN PARM1 TO THE            
* DESTINATION POINTED TO BY PARM2.  IF THE DISPLAYED AMOUNT IS BIGGER           
* THEN THE LENGTH SPECIFIED IN PARM3 THEN IT WILL DISPLAY '*'S INSTEAD.         
* THE ROUTINE WILL RETURN THE LENGTH IN PARM1.                                  
*                                                                               
DR2DEC   NTR1                                                                   
         LM    R2,R4,0(R1)         R2 = NUMBER                                  
*                                  R3 = A(DESTINATION)                          
         BCTR  R4,0                R4 = LENGTH OF DESTINATION - 1               
*                                                                               
         EX    R4,*+8              PRE-CLEAR DESTINATION                        
         B     *+10                                                             
         XC    0(0,R3),0(R3)                                                    
*                                                                               
         LTR   R2,R2               IF NUMBER = 0                                
         BNZ   *+18                                                             
         MVI   0(R3),C'0'          THEN DISPLAY '0' AND RETURN                  
         MVC   0(4,R1),=F'1'                                                    
         B     DR2DECX                                                          
*                                  ELSE EDIT PERCENTAGE INTO BLOCK              
         EDIT  (R2),(10,BLOCK),2,ALIGN=LEFT                                     
*                                                                               
         LA    RF,BLOCK            RF = A(LAST CHAR IN BLOCK)                   
         AR    RF,R0                                                            
         BCTR  RF,0                                                             
*                                                                               
DR2DEC10 CLI   0(RF),C'.'          IF CHAR IS '.' THEN DONE                     
         BE    DR2DEC50                                                         
         CLI   0(RF),C'0'          ELSE IF CHAR IS <> '0' THEN DONE             
         BNE   DR2DEC60                                                         
         BCT   RF,DR2DEC10         ELSE BACK UP POINTER AND LOOP BACK           
*                                                                               
DR2DEC50 BCTR  RF,0                BACK UP ONE MORE FOR '.'                     
*                                                                               
DR2DEC60 LA    RE,BLOCK            RF = LENGTH OF NUMBER - 1                    
         SR    RF,RE                                                            
*                                                                               
         CR    RF,R4               IF L(NUMBER) - 1 > L(DEST) - 1               
         BH    DR2DEC90            THEN DISPLAY '*'S                            
*                                                                               
         EX    RF,*+8              ELSE MOVE NUMBER TO DESTINATION              
         B     *+10                                                             
         MVC   0(0,R3),BLOCK                                                    
*                                                                               
         LA    RF,1(RF)            RETURN LENGTH IN PARM1                       
         ST    RF,0(R1)                                                         
         B     DR2DECX                                                          
*                                                                               
DR2DEC90 MVI   0(R3),C'*'          DISPLAY '*'S                                 
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   1(0,R3),0(R3)                                                    
*                                                                               
         LA    RF,2(R4)            RETURN LENGTH IN PARM1                       
         ST    RF,0(R1)                                                         
*                                                                               
DR2DECX  B     XIT                                                              
         EJECT                                                                  
*              ROUTINE CALLS TSAR USING TSARAREA FOR TSAR RECORD                
*              MYTSRNUM AND MYTSACTN ARE SET                                    
*              RETURNS CONDITION CODE                                           
CALLTSAR NTR1                                                                   
         LA    R1,TSARAREA                                                      
         ST    R1,TCATSAR          SET A(TSAR RECORD)                           
         LA    R1,SORTLNQ                                                       
         STCM  R1,3,MYTSRECL       REC LENGTH                                   
         MVI   MYTSKEYL,SORTLKEY   KEY LENGTH                                   
         MVI   MYTSPAGN,6          SET 6 14K PAGES                              
         MVI   MYTSINDS,TSIALLOC+TSIRTNAF  SET FOR TEMPEST                      
         GOTO1 TSARCNTL,DMCB,TCATSAR                                            
         BE    YES                                                              
         B     NO                                                               
         EJECT                                                                  
ENTCOM   MVI   MYMSGNO1,113        ENTER COMMENT AND HIT PF24                   
         LA    R2,FCLLIN1H                                                      
         J     MSGEND                                                           
                                                                                
NOCOMER  MVI   MYMSGNO1,114                                                     
         LA    R2,FCLLIN1H                                                      
         J     MSGEND                                                           
                                                                                
ERPPLSI  MVC   MYMSGNO,=Y(ERRIAPPA)   RECORD / ACTION INVALID FOR P+            
         J     ERREND                                                           
                                                                                
ERREND   MVI   MYMTYP,GTMERR       ERROR MESSAGE EXIT                           
MSGEND   OI    GENSTAT2,USGETTXT                                                
         B     MESXIT                                                           
*                                                                               
MESXIT   XC    DMCB,DMCB                                                        
         GOTO1 EXIT,DMCB                                                        
         SPACE 3                                                                
*                                                                               
YES      XR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         SPACE 3                                                                
         GETEL (R4),DATADISP,ELCODE                                             
         EJECT                                                                  
         SPACE 1                                                                
*              ROUTINE TO PRINT A LINE                                          
         SPACE 1                                                                
PRNTIT   NTR1                                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
         EJECT                                                                  
*              SAVE COMM'L DETAILS FOR HEADHOOK ROUTINE                         
         SPACE 1                                                                
GETDET   NTR1                                                                   
         MVC   AIO,AIO1                                                         
*                                                                               
         GOTO1 RECVAL,DMCB,TLCOICDQ,(X'20',FCLCIDH) GET COMM'L RECORD           
         BE    *+6                                                              
         DC    H'00'                                                            
         L     R4,AIO              R4=A(COMM'L RECORD)                          
         USING TLCOD,R4                                                         
*                                                                               
         MVC   SVAGY,TLCOAGY    AGENCY                                          
*                                                                               
         MVC   AIO,AIO3                                                         
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'A0',TLCOAGY)   GET AGENCY RECORD          
         BE    *+6                                                              
         DC    H'00'                                                            
         GOTO1 CHAROUT,DMCB,TANAELQ,0                                           
         MVC   SVAGYN,TGNAME    AGENCY NAME                                     
*                                                                               
         GOTO1 RECVAL,DMCB,TLCLCDQ,(X'A0',TLCOCLI)   GET CLIENT RECORD          
         BE    *+6                                                              
         DC    H'00'                                                            
         GOTO1 CHAROUT,DMCB,TANAELQ,0                                           
         MVC   SVCLIN,TGNAME                         CLIENT NAME                
*                                                                               
         OC    TLCOPRD,TLCOPRD   PRODUCT NOT REQUIRED                           
         BZ    GDET20                                                           
         GOTO1 RECVAL,DMCB,TLPRCDQ,(X'A0',TLCOPRD)   GET PRODUCT RECORD         
         BE    *+6                                                              
         DC    H'00'                                                            
         DROP  R4                                                               
*                                                                               
         GOTO1 CHAROUT,DMCB,TANAELQ,0                                           
         MVC   SVPRDN,TGNAME                         PRODUCT NAME               
*                                                                               
         L     R4,AIO                                                           
         MVI   ELCODE,TAPIELQ      GET PRODUCT INFORMATION                      
         BAS   RE,GETEL                                                         
         BNE   GDET20                                                           
         USING TAPID,R4                                                         
         CLI   TAPILEN,TAPILNQ2    ELEMENT MUST BE NEW LENGTH                   
         BNE   GDET20                                                           
         OC    TAPIPTYP,TAPIPTYP   PTYPE IS NOT REQUIRED                        
         BZ    GDET20                                                           
         GOTO1 RECVAL,DMCB,TLPTCDQ,(X'A0',TAPIPTYP) GET PTYPE REC               
         BE    *+6                                                              
         DC    H'00'                                                            
         GOTO1 CHAROUT,DMCB,TANAELQ,0                                           
         MVC   SVPTYPN,TGNAME      PRODUCT TYPE NAME                            
         DROP  R4                                                               
*                                                                               
GDET20   MVC   AIO,AIO1                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TACOELQ      GET COMM'L DETAILS                           
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         USING TACOD,R4                                                         
*                                                                               
         L     RE,TGAMEDS                                                       
         USING MEDIAD,RE                                                        
GDET30   CLI   MEDEQU,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'00'                                                            
         CLC   TACOMED,MEDNAME      COMPARE 1ST LETTER OF MEDNAME               
         BE    GDET40                                                           
         LA    RE,MEDNEXT                                                       
         B     GDET30                                                           
GDET40   MVC   SVMED,MEDNAME        MEDIA NAME                                  
         DROP  RE                                                               
*                                                                               
         MVC   SVFCYC,TACOFCYC      FIRST FIXED CYCLE                           
         MVC   SVAIR,TACOAIR        FIRST AIR DATE                              
         MVC   SVEXP,TACOEXP        EXPIRY DATE                                 
         DROP  R4                                                               
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
*              HEADLINE ROUTINES                                                
         SPACE 1                                                                
HDHOOK   NTR1                                                                   
*                                                                               
         MVC   H4+11(L'SVAGY),SVAGY      AGENCY                                 
         MVC   H4+23(L'SVAGYN),SVAGYN    AGENCY NAME                            
         MVC   H5+11(L'SVCLIN),SVCLIN    CLIENT NAME                            
*                                                                               
         OC    SVPRDN,SVPRDN    PRODUCT NOT REQUIRED                            
         BZ    HDHK10                                                           
         MVC   H6+11(L'SVPRDN),SVPRDN    PRODUCT NAME                           
*                                                                               
         OC    SVPTYPN,SVPTYPN   PTYPE IS NOT REQUIRED                          
         BZ    HDHK10                                                           
         MVC   H5+67(L'SVPTYPN),SVPTYPN   PRODUCT TYPE NAME                     
*                                                                               
HDHK10   MVC   H5+111(L'SVMED),SVMED   MEDIA NAME                               
*                                                                               
         GOTO1 DATCON,DMCB,(1,SVFCYC),(8,H6+67)  1ST FIXED CYCLE                
         GOTO1 DATCON,DMCB,(1,SVAIR),(8,H6+121)  1ST AIR DATE                   
         GOTO1 DATCON,DMCB,(1,SVEXP),(8,H7+15)   EXPIRY DATE                    
*                                                                               
                                                                                
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TRANSLATES TATR ELEMENT INTO TAMC FORMAT             *         
*        ON ENTRY ... AIO1=A(COMMERCIAL RECORD)                       *         
***********************************************************************         
                                                                                
TRN2TAMC NTR1                                                                   
         USING TATRTABD,R2                                                      
         LA    R2,BLOCK                                                         
                                                                                
         USING TATRD,R4                                                         
         L     R4,AIO1                                                          
         MVI   ELCODE,TATRELQ      READ ALL NEW STYLE TRACK ASSOICATION         
         BAS   RE,GETEL            ELEMENTS                                     
         B     *+8                                                              
T2TAMC10 BAS   RE,NEXTEL                                                        
         BNE   T2TAMC20                                                         
         MVC   TTCOM,TATRCOM       SAVE THEM IN TABLE                           
         MVC   TTTRK,TATRTRK                                                    
         LA    R2,TTLNQ(R2)                                                     
         B     T2TAMC10                                                         
         DROP  R4                                                               
                                                                                
T2TAMC20 MVI   0(R2),X'FF'                                                      
         DROP  R2                                                               
                                                                                
***********************************************************************         
                                                                                
         CLI   BLOCK,X'FF'         IF ANY NEW STYLE TRACK ASSOCIATIONS          
         BE    XIT                 EXIST                                        
                                                                                
         USING TATRTABD,R2                                                      
         LA    R2,BLOCK                                                         
                                                                                
         USING TLCOPD,R3                                                        
         LA    R3,KEY                                                           
                                                                                
T2TAMC30 XC    KEY,KEY                                                          
         MVI   TLCOPCD,TLCOCCDQ                                                 
         MVC   TLCOCCOM,TTCOM                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(L'TLCOPKEY),KEYSAVE                                          
         BNE   T2TAMC40                                                         
                                                                                
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1                                                         
                                                                                
         USING TACOD,R4                                                         
         L     R4,AIO2                                                          
         MVI   ELCODE,TACOELQ                                                   
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
                                                                                
         USING TAMCD,R1                                                         
         LA    R1,ELEMENT                                                       
         MVI   TAMCEL,TAMCELQ                                                   
         MVI   TAMCLEN,TAMCLNQ                                                  
         MVC   TAMCCON,TACOCID                                                  
         MVC   TAMCTRK,TTTRK                                                    
         GOTO1 ADDELEM                                                          
         DROP  R1,R3,R4                                                         
                                                                                
T2TAMC40 CLI   TTLNQ(R2),X'FF'                                                  
         BE    XIT                                                              
         LA    R2,TTLNQ(R2)                                                     
         B     T2TAMC30                                                         
         DROP  R2                                                               
         EJECT                                                                  
*                                                                               
ERRINV   MVI   ERROR,INVALID                                                    
         B     ERRXIT                                                           
*                                                                               
ERRMIS   MVI   ERROR,MISSING                                                    
         B     ERRXIT                                                           
*                                                                               
ERRXIT   XC    DMCB,DMCB                                                        
         GOTO1 EXIT,DMCB                                                        
         SPACE 3                                                                
         EJECT                                                                  
*                                                                               
*              LITERALS FOR PRINTED REPORT                                      
         SPACE 1                                                                
LTCOM    DC    C'        ISCI                    Title                 X        
               Length  AFM Basic    Contract #  Track'                          
LTCOMU   DC    C'--- ------------ ------------------------------------ X        
               ------ ------------ ------------ -----'                          
LTCAST   DC    C'                                                      X        
                                                          First        X        
                Appl Lifted From'                                               
LTCAST2  DC    C' PID/FID             Name                Corporation NX        
               ame       Category     Cam Uni Commercials Service  Ov1%X        
                Sess     ISCI'                                                  
LTCASTU  DC    C'--------- --------------------------- ----------------X        
               ------ --------------- --- --- ----------- -------- ----X        
                ---- ------------'                                              
LTVER    DC    C'Version   Date     Created By                         X        
                  Comments'                                                     
LTVERU   DC    C'------- --------- ------------ -----------------------X        
               -------------------------------------'                           
         EJECT                                                                  
         SPACE 1                                                                
         DS    0A                                                               
RELO     DC    AL4(0)                                                           
NAMEAREA DS    CL36                                                             
SVKEY    DS    CL32                                                             
TCATSAR  DS    A                                                                
TSARAREA DS    CL(SORTLNQ)        TSAR RECORD AREA                              
SSNFLG   DS    X                  NEW SSN FLAG                                  
YESVERS  DS    X                  VERSIONS FLAG                                 
YESLIFT  DS    X                  LIFT FLAG                                     
LASTSSN  DS    CL9                SAVED LAST SSN                                
SVVER    DS    CL1                SAVED VERSION LETTER                          
SVSEQ    DS    XL1                SAVED SEQUENCE NUMBER                         
SVCOM    DS    XL4                SAVED INTERNAL COMM ID                        
SVFDAT   DS    XL3                SAVED FILM DATE                               
SVRDAT   DS    XL3                SAVED RECORDING DATE                          
SVMDAT   DS    XL3                SAVED MUSIC DATE                              
SVCOM2   DS    CL(PRNTCLNQ)       SAVED VERSIONS LINE 2                         
SVCOM3   DS    CL(26-(PRNTCLNQ*2))  SAVED VERSIONS LINE 3                       
SVAGY    DS    CL6                SAVED AGENCY                                  
SVAGYN   DS    CL(L'TGNAME)       SAVED AGENCY NAME                             
SVCLIN   DS    CL(L'TGNAME)       SAVED CLIENT NAME                             
SVPRDN   DS    CL(L'TGNAME)       SAVED PRODUCT NAME                            
SVPTYPN  DS    CL(L'TGNAME)       SAVED PRODUCT TYPE NAME                       
SVMED    DS    CL5                SAVED MEDIA                                   
SVFCYC   DS    XL3                SAVED FIRST FIXED CYCLE                       
SVAIR    DS    XL3                SAVED FIRST AIR DATE                          
SVEXP    DS    XL3                SAVED EXPIRY DATE                             
SVCSEQ   DS    X                  SAVED COMMERCIAL SEQUENCE NUMBER              
         EJECT                                                                  
*              SPECS TO COVER PRINTED ADVICE FORM                               
         SPACE 1                                                                
MYSPECS  DS    0H                                                               
         SPACE 1                                                                
         SSPEC H1,62,RUN                                                        
         SSPEC H1,105,REPORT                                                    
         SSPEC H2,62,REQUESTOR                                                  
         SSPEC H3,62,PAGE                                                       
         SPACE 1                                                                
         SSPEC H1,28,C'Final Cast List'                                         
         SSPEC H2,28,C'---------------'                                         
         SPACE 1                                                                
         SSPEC H4,2,C'Agency:'                                                  
         SSPEC H5,2,C'Client:'                                                  
         SSPEC H5,49,C'Product Type:'                                           
         SSPEC H5,105,C'Media:'                                                 
         SSPEC H6,2,C'Product:'                                                 
         SSPEC H6,49,C'First Fixed Cycle:'                                      
         SSPEC H6,105,C'First Air Date:'                                        
         SSPEC H7,2,C'Expiry Date:'                                             
         SPACE 1                                                                
         DC    X'00'                                                            
         SPACE 2                                                                
         LTORG                                                                  
         DROP  R7                  DROP SECONDARY BASE REGISTERS                
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
         PRINT ON                                                               
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCRFBD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR18D                                                       
         ORG                                                                    
         EJECT                                                                  
*              DSECT TO COVER SORT RECORD                                       
         SPACE 1                                                                
SORTD    DSECT                                                                  
SORTSRT  DS    CL6                 CAST SORT KEY                                
SORTSSN  DS    CL9                 SOCIAL SECURITY NUMBER                       
SORTCAT  DS    CL36                CATEGORY                                     
SORTLKEY EQU   *-SORTD             L'KEY                                        
*                                                                               
SORTUNI  DS    CL3                 UNION                                        
SORTLNAM DS    CL20                LAST NAME                                    
SORTFNAM DS    CL16                FIRST NAME                                   
SORTCORP DS    CL36                CORPORATION NAME                             
SORTCAM  DS    CL3                 ON/OFF CAMERA                                
SORTCOM  DS    CL26                COMMERCIAL (BY VERSION LETTER)               
SORT1SER DS    PL3                 FIRST SERVICE                                
SORTOV1  DS    CL3                 OVERSCALE RATE 1                             
SORTLFI  DS    CL12                LIFTED FROM ISCI                             
SORTFID  DS    CL9                 FEDERAL ID                                   
SORTCRPN DS    CL1                 CORPORATION NUMBER                           
SORTLNQ  EQU   *-SORTD                                                          
         SPACE 1                                                                
*              DSECT TO COVER PRINT LINE                                        
         SPACE 1                                                                
PRNTD    DSECT                     DSECT TO COVER COMMERCIAL DETAILS            
         DS    CL1                                                              
PRNTLET  DS    CL3                 COMMERCIAL VERSION LETTER                    
         DS    CL1                                                              
PRNTISCI DS    CL12                ISCI                                         
         DS    CL1                                                              
PRNTTITL DS    CL36                TITLE                                        
         DS    CL3                                                              
PRNTLEN  DS    CL2                 LENGTH                                       
         DS    CL3                                                              
PRNTBASC DS    CL12                AFM BASIC                                    
         DS    CL1                                                              
PRNTCON  DS    CL12                CONTRACT #                                   
         DS    CL3                                                              
PRNTTRK  DS    CL1                 TRACK NUMBER                                 
         SPACE 1                                                                
         ORG   PRNTD               DSECT TO COVER PERFORMER DETAILS             
         DS    CL1                                                              
PRNTSSN  DS    CL9                 SOCIAL SECURITY NUMBER/FID                   
         DS    CL1                                                              
PRNTNAME DS    CL27                PERFORMER NAME                               
         DS    CL1                                                              
PRNTCORP DS    CL22                CORPORATION NAME                             
         DS    CL1                                                              
PRNTCAT  DS    CL15                CATEGORY                                     
         DS    CL1                                                              
PRNTCAM  DS    CL3                 ON/OFF CAMERA                                
         DS    CL1                                                              
PRNTUNI  DS    CL3                 UNION                                        
         DS    CL1                                                              
PRNTCOM  DS    CL11                COMMERCIALS (BY VERSION LETTER)              
PRNTCLNQ EQU   *-PRNTCOM                                                        
         DS    CL1                                                              
PRNT1SER DS    CL8                 FIRST SERVICE                                
         DS    CL1                                                              
PRNTOV1  DS    CL3                 OVERSCALE RATE 1                             
         DS    CL3                                                              
PRNTAPPS DS    CL1                 APPLIED SESSION                              
         DS    CL3                                                              
PRNTLFI  DS    CL12                LIFTED FROM ISCI                             
         SPACE 1                                                                
         ORG   PRNTD               DSECT TO COVER COMMENT DETAILS               
         DS    CL2                                                              
PRNTVER  DS    CL3                 VERSION                                      
         DS    CL5                                                              
PRNTDATE DS    CL8                 DATE                                         
         DS    CL3                                                              
PRNTUSER DS    CL8                 LAST CHANGED                                 
         DS    CL3                                                              
PRNTCMT  DS    CL60                COMMENT                                      
         EJECT                                                                  
* TAGENWORKD                                                                    
* TASYSDSECT                                                                    
* TASYSEQUS                                                                     
* TAGENEQUS                                                                     
* DDSPLWORKD                                                                    
* DDSPOOLD                                                                      
* TAGENFILE                                                                     
* DDTSARD                                                                       
* DDPERVALD                                                                     
         PRINT OFF                                                              
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE TAGENEQUS                                                      
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE DDTSARD                                                        
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE FAGETTXTD                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'013TAGEND0   07/20/12'                                      
         END                                                                    
