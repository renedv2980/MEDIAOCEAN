*          DATA SET TAREP55    AT LEVEL 003 AS OF 01/08/04                      
*PHASE T70355A,*                                                                
*INCLUDE DLFLD                                                                  
         TITLE 'T70355 - AGENCY INFORMATION REPORT (DISK/DOWNLOAD)'             
T70355   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T70355,R6                                                      
         L     RC,0(R1)                                                         
         USING GEND,RC             RC=A(CONTROLLER W/S)                         
         L     RA,ATWA                                                          
         USING T703FFD,RA          RA=A(SCREEN)                                 
         L     R9,ASUBSYSD                                                      
         USING SUBSYSD,R9          R9=A(SYSTEM W/S)                             
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8           R8=A(SPOOL DSECT)                            
         LA    R7,BUFF                                                          
         LA    R7,8(R7)                                                         
         USING WSD,R7              R7=A(LOCAL W/S)                              
         EJECT                                                                  
*              MODE CONTROLLED ROUTINES                                         
*                                                                               
         GOTO1 INITIAL,DMCB,0      INITIALIZE                                   
         SPACE 1                                                                
         CLI   MODE,VALREC         VALIDATE SCREEN                              
         BE    *+12                                                             
         CLI   MODE,DISPREC                                                     
         BNE   MODE10                                                           
         BAS   RE,VKEY                                                          
         B     XIT                                                              
         SPACE 1                                                                
MODE10   CLI   MODE,PRINTREP       PROCESS REPORT                               
         BNE   XIT                                                              
         SPACE 1                                                                
         USING TWADCOND,R2                                                      
         MVC   AMASTD,TWAMASTC     SAVE ADDRESS OF MASTER                       
         L     R2,TWADCONS                                                      
         MVC   ALOGOC,TLOGOC       SAVE A(LOGOC)                                
         MVC   APRNTBL,TPRNTBL     SAVE A(PRNTBL)                               
         MVC   APRNT,TWAVPRNT      SAVE A(PRINT)                                
         DROP  R2                                                               
         SPACE 1                                                                
         USING MASTD,R2                                                         
         L     R2,AMASTD                                                        
         NI    MCPRTIND,X'FF'-MCPRTINL                                          
         DROP  R2                                                               
         SPACE 1                                                                
         BAS   RE,PREPD                                                         
         SPACE 1                                                                
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         SPACE 2                                                                
         EJECT                                                                  
*              VALIDATE KEY ROUTINE                                             
         SPACE                                                                  
VKEY     NTR1                                                                   
         LR    RE,R7               A(LOCAL WORKING STORAGE)                     
         LA    RF,WSLNQ                                                         
         XCEFL ,                                                                
         XC    TIFILTS,TIFILTS     CLEAR SYSIO FILTERS                          
         XC    SITAGYN,SITAGYN                                                  
         OI    SITCLINH+6,X'80'    CLEAR AGENCY NAME                            
         XC    SITCLIN,SITCLIN                                                  
         OI    SITCLINH+6,X'80'    CLEAR CLIENT NAME                            
         SPACE 1                                                                
         CLC   SITAGY(3),=C'ALL'                                                
         BE    VK10                                                             
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'08',SITAGYH),SITAGYNH                     
         MVC   TIFAGY,TGAGY        SET SYSIO AGENCY FILTER                      
         SPACE 1                                                                
VK10     XC    SITPER,SITPER       CLEAR PERIOD                                 
         OI    SITPERH+6,X'80'                                                  
         SPACE 1                                                                
         CLI   SITCLINH+5,0        IF CLIENT INPUTTED                           
         BE    VK20                                                             
         GOTO1 RECVAL,DMCB,TLCLCDQ,(X'08',(R2)),SITCLINH                        
         MVC   TIFCLI,TGCLI        SET SYSIO FILTER                             
         SPACE 1                                                                
VK20     MVI   OPTIONS,0                                                        
         SPACE 1                                                                
         LA    R2,SITOPTH          R2=A(OPTIONS FIELD)                          
         CLI   5(R2),0                                                          
         BE    VKX                                                              
         SPACE 1                                                                
         LA    R3,BLOCK                                                         
         USING SCAND,R3                                                         
         GOTO1 SCANNER,DMCB,(R2),(R3),0                                         
         CLI   4(R1),0                                                          
         BE    FLDINV                                                           
         ZIC   R0,4(R1)                                                         
         SPACE 1                                                                
VK30     CLC   =C'NOEMAIL',SCDATA1   NOEMAIL                                    
         BNE   VK40                                                             
         OI    OPTIONS,NOEMAIL                                                  
         B     VK50                                                             
         SPACE 1                                                                
VK40     CLC   =C'LOCKED',SCDATA1    LOCKED                                     
         BNE   FLDINV                                                           
         CLI   SCDATA2,C'Y'                                                     
         BE    VK50                                                             
         CLI   SCDATA2,C'N'                                                     
         BNE   FLDINV                                                           
         OI    OPTIONS,NOLOCK                                                   
         SPACE 1                                                                
VK50     LA    R3,SCANNEXT                                                      
         BCT   R0,VK30                                                          
VKX      B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO GENERATE REPORTS - CALLS SYSIO                        
         SPACE 1                                                                
PREPD    NTR1                                                                   
         USING MASTD,R2                                                         
         L     R2,AMASTD           SET TO NOT PRINT LOGOS                       
         DROP  R2                                                               
         SPACE 1                                                                
         USING DLCBD,R5                                                         
         LA    R5,DLBLOCK          R5=A(DOWNLOAD BLOCK)                         
         SPACE 1                                                                
         BAS   RE,INITDWN          INITIALIZE DLBLOCK FOR DOWNLOAD              
         SPACE 1                                                                
         BAS   RE,PRTHEAD          PRINT SPREADSHEET HEADER LINE                
         SPACE 1                                                                
         XC    AYCOUNT,AYCOUNT     CLEAR AGENCY COUNTER                         
         SPACE 1                                                                
         LA    R1,IOHOOK           SET A(I/O HOOK) FOR PROCESSING               
         ST    R1,TIHOOK                                                        
         MVC   TIACOMFC,ACOMFACS   SET UP SYSIO BLOCK                           
         MVC   TIUSERID,TWAORIG    REQUESTING ID                                
         MVC   TIQSTAFF,TGCTSTAF                                                
         SPACE 1                                                                
         MVI   TIREAD,TLAYCDQ      READ AGENCY RECORDS                          
         GOTO1 TASYSIO,DMCB,TASYSIOD  OFF TO SYSIO TO DO I/O                    
         SPACE 1                                                                
PREPDX   B     XIT                                                              
         EJECT                                                                  
*              INITIALIZE DOWNLOAD BLOCK                                        
         SPACE 1                                                                
INITDWN  NTR1                                                                   
         XC    DLCBD(DLCBXLX),DLCBD                                             
         SPACE 1                                                                
         MVI   DLCBACT,DLCBINIT    INITIALIZE FOR DOWNLOAD                      
         LA    R1,SPLATDWN         A(HOOK ROUTINE FOR PRINTING)                 
         ST    R1,DLCBAPR                                                       
         LA    R1,P                A(PRINT LINE)                                
         ST    R1,DLCBAPL                                                       
         MVC   DLCBAED,EDITOR                                                   
         MVC   DLCXMAXL,=Y(L'P)    MAX LENGTH OF PRINT LINE                     
         MVI   DLCXDELC,C' '       DELIMITER                                    
         MVI   DLCXEOTC,C'"'       TEXT DELIMITER                               
         MVI   DLCXEOTA,C''''      TEXT DELIMITER ALTERNATE                     
         MVI   DLCXEOLC,X'5E'      SEMI-COLON FOR END OF LINE                   
         MVI   DLCXEORC,C':'       END OF REPORT                                
         GOTO1 =V(DLFLD),DLCBD                                                  
         B     PREPDX                                                           
         EJECT                                                                  
*              PRINT HEADER LINE FOR SPREADSHEET                                
         SPACE 1                                                                
PRTHEAD  NTR1                                                                   
         USING HEADD,R2                                                         
         LA    R2,HEADTAB          R2=A(TABLE OF HEADERS)                       
         SPACE 1                                                                
PH10     CLI   HEADLEN,X'FF'       IF NO MORE HEADERS, SEND LINE                
         BE    PHX                 TO DLFLD AND EXIT                            
         SPACE 1                                                                
         MVI   DLCBACT,DLCBPUT     PUT ITEM TO PRINT LINE                       
         MVI   DLCBTYP,DLCBTXT     DATA TYPE IS TEXT                            
         MVC   DLCBLEN,HEADLEN                                                  
         ZIC   RE,HEADLEN                                                       
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   DLCBFLD(0),HEADER                                                
         GOTO1 =V(DLFLD),DLCBD     SEND TO DLFLD                                
         SPACE 1                                                                
         ZIC   RE,HEADLEN                                                       
         AHI   RE,1                                                             
         AR    R2,RE                                                            
         B     PH10                                                             
         DROP  R2                                                               
         SPACE 1                                                                
PHX      MVI   DLCBACT,DLCBEOL     END OF LINE                                  
         GOTO1 =V(DLFLD),DLCBD                                                  
         B     PREPDX                                                           
         EJECT                                                                  
*              HOOK FROM SYSIO                                                  
         SPACE                                                                  
IOHOOK   NTR1                                                                   
         MVC   AIO,TIAREC          SET IOAREA AS AGENCY RECORD                  
         SPACE 1                                                                
         USING TACMD,R4                                                         
         TM    OPTIONS,NOEMAIL     NO EMAIL FILTER PROCESSING                   
         BZ    IO10                                                             
         MVI   ELCODE,TACMELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TACMTYPI))                                     
         BNE   IO10                                                             
         L     R4,TGELEM                                                        
         MVC   FULL,TACMCOMM                                                    
         OC    FULL,SPACES                                                      
         CLC   FULL,=C'NONE'                                                    
         BNE   IOHOOKX                                                          
         DROP  R4                                                               
         SPACE 1                                                                
         USING TAAYD,R4                                                         
IO10     TM    OPTIONS,NOLOCK      NOLOCK FILTER PROCESSING                     
         BZ    IO20                                                             
         L     R4,AIO                                                           
         MVI   ELCODE,TAAYELQ                                                   
         BAS   RE,GETEL            R4=A(AGENCY ELEMENT)                         
         BE    *+6                                                              
         DC    H'00'                                                            
         TM    TAAYSTA3,TAAYSLCK                                                
         BO    IOHOOKX                                                          
         DROP  R4                                                               
         SPACE 1                                                                
IO20     MVI   DLCBACT,DLCBPUT     PREPARE DOWNLOAD BLOCK                       
         MVI   DLCBTYP,DLCBTXT     FOR AGENCY NAME                              
         MVC   DLCBFLD,SPACES                                                   
         SPACE 1                                                                
         USING TANAD,R4                                                         
         L     R4,AIO              R4=A(AGENCY RECORD)                          
         MVI   ELCODE,TANAELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         ZIC   RE,TANALEN          R4=A(LONG AGENCY NAME ELEMENT)               
         AHI   RE,-2                                                            
         STC   RE,DLCBLEN                                                       
         AHI   RE,-1                                                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   DLCBFLD(0),TANANAME MOVE AGENCY NAME TO DOWNLOAD BLK             
         GOTO1 =V(DLFLD),DLCBD     AND PUT IT                                   
         DROP  R4                                                               
         SPACE 1                                                                
         USING TAADD,R4                                                         
         L     R4,AIO              R4=A(AGENCY RECORD)                          
         MVI   ELCODE,TAADELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         ZIC   R3,TAADLNES         R4=A(ADDRESS ELEMENT)                        
         LA    R2,TAADADD                                                       
IO30     MVI   DLCBACT,DLCBPUT     PREPARE DOWNLOAD BLOCK                       
         MVI   DLCBTYP,DLCBTXT     FOR ADDRESS LINES                            
         MVI   DLCBLEN,L'TAADADD                                                
         MVC   DLCBFLD(L'TAADADD),0(R2)                                         
         LA    R2,L'TAADADD(R2)    MOVE ADDRESS LINE TO DOWNLOAD BLK            
         GOTO1 =V(DLFLD),DLCBD     AND PUT IT                                   
         BCT   R3,IO30                                                          
         LHI   R2,4                                                             
         ZIC   R3,TAADLNES                                                      
         SR    R2,R3                                                            
         LTR   R2,R2               IF LESS THAN 4 ADDRESS LINES                 
         BZ    IO50                FILL UP REMAINING LINES WITH                 
IO40     MVI   DLCBACT,DLCBPUT     SPACES                                       
         MVI   DLCBTYP,DLCBTXT                                                  
         MVI   DLCBLEN,L'TAADADD                                                
         MVC   DLCBFLD(L'TAADADD),SPACES                                        
         GOTO1 =V(DLFLD),DLCBD                                                  
         BCT   R2,IO40                                                          
         DROP  R4                                                               
         SPACE 1                                                                
IO50     MVI   DLCBACT,DLCBPUT     PREPARE DOWNLOAD BLOCK                       
         MVI   DLCBTYP,DLCBTXT     FOR AGENCY CODE                              
         MVI   DLCBLEN,L'TIAGY     MOVE AGENCY CODE TO DOWNLOAD BLK             
         MVC   DLCBFLD(L'TIAGY),TIAGY          AND PUT IT                       
         GOTO1 =V(DLFLD),DLCBD                                                  
         SPACE 1                                                                
         USING TANUD,R4                                                         
         MVC   FAX,SPACES                                                       
         MVI   ELCODE,TANUELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TANUTFAX))                                     
         BNE   IO60                                                             
         L     R4,TGELEM                                                        
         ZIC   RE,TANULEN          R4=A(FAX FREE FORM ELEMENT)                  
         AHI   RE,-4                                                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   FAX(0),TANUMBER     SAVE FAX NUMBER FOR LATER USE                
         DROP  R4                                                               
         SPACE 1                                                                
         USING TACMD,R4                                                         
IO60     MVC   EMAIL,SPACES                                                     
         MVI   ELCODE,TACMELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TACMTYPI))                                     
         BNE   IO70                                                             
         L     R4,TGELEM           R4=A(EMAIL COMMENT ELEMENT)                  
         ZIC   RE,TACMLEN                                                       
         AHI   RE,-4                                                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   EMAIL(0),TACMCOMM   SAVE E-MAIL ADDRESS FOR LATER USE            
         DROP  R4                                                               
         SPACE 1                                                                
         USING TANLD,R4                                                         
IO70     MVI   NWK,C'N'                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TANLELQ      R4=A(NETWORK USERID ELEMENT)                 
         BAS   RE,GETEL                                                         
         BNE   IO80                                                             
         MVI   NWK,C'Y'            SAVE NETTAL STATUS FOR LATER USE             
         DROP  R4                                                               
         SPACE 1                                                                
IO80     MVI   DLCBACT,DLCBPUT     PREPARE DOWNLOAD BLOCK FOR                   
         MVI   DLCBTYP,DLCBTXT     TALENT/PRINT STATUS                          
         MVI   DLCBLEN,6                                                        
         SPACE 1                                                                
         USING TAAYD,R4                                                         
         L     R4,AIO              R4=A(AGENCY RECORD)                          
         MVI   ELCODE,TAAYELQ                                                   
         BAS   RE,GETEL            R4=A(AGENCY ELEMENT)                         
         BE    *+6                                                              
         DC    H'00'                                                            
         MVC   DLCBFLD(6),=CL6'PRINT'  IF OFFICE CODE IS NUMERIC                
         CLI   TAAYTPOF,C'0'           THEN PRINT                               
         BL    *+10                    IF OFFICE CODE IF ALPHA                  
         MVC   DLCBFLD(6),=CL6'TALENT' THEN TALENT                              
         GOTO1 =V(DLFLD),DLCBD         PUT DOWNLOAD BLOCK                       
         SPACE 1                                                                
*        BAS   RE,DLUSER           PUT USER ID TO DOWNLOAD                      
         BAS   RE,DLUSER2          PUT USER ID TO DOWNLOAD                      
         SPACE 1                                                                
         MVI   DLCBACT,DLCBPUT     PREPARE DOWNLOAD BLOCK FOR                   
         MVI   DLCBTYP,DLCBTXT     TELEPHONE NUMBER                             
         MVI   DLCBLEN,L'TAAYTEL   MOVE TO DOWNLOAD BLOCK AND PUT IT            
         MVC   DLCBFLD(L'TAAYTEL),TAAYTEL                                       
         GOTO1 =V(DLFLD),DLCBD                                                  
         SPACE 1                                                                
         MVI   DLCBACT,DLCBPUT     PREPARE DOWNLOAD BLOCK FOR                   
         MVI   DLCBTYP,DLCBTXT     FAX NUMBER                                   
         MVI   DLCBLEN,L'FAX       MOVE TO DOWNLOAD BLOCK AND PUT IT            
         MVC   DLCBFLD(L'FAX),FAX                                               
         GOTO1 =V(DLFLD),DLCBD                                                  
         SPACE 1                                                                
         MVI   DLCBACT,DLCBPUT     PREPARE DOWNLOAD BLOCK FOR OFFICE            
         MVI   DLCBTYP,DLCBTXT     MOVE TO DOWNLOAD BLOCK AND PUT IT            
         MVI   DLCBLEN,L'TAAYTPOF                                               
         MVC   DLCBFLD(L'TAAYTPOF),TAAYTPOF                                     
         GOTO1 =V(DLFLD),DLCBD                                                  
         SPACE 1                                                                
         MVI   DLCBACT,DLCBPUT     PREPARE DOWNLOAD BLOCK FOR EMAIL             
         MVI   DLCBTYP,DLCBTXT     MOVE TO DOWNLOAD BLOCK AND PUT IT            
         MVI   DLCBLEN,L'EMAIL                                                  
         MVC   DLCBFLD(L'EMAIL),EMAIL                                           
         GOTO1 =V(DLFLD),DLCBD                                                  
         SPACE 1                                                                
         MVI   DLCBACT,DLCBPUT     PREPARE DOWNLOAD BLOCK FOR NETTAL            
         MVI   DLCBTYP,DLCBTXT     STATUS                                       
         MVI   DLCBLEN,L'NWK       MOVE TO DOWNLOAD BLOCK AND PUT IT            
         MVC   DLCBFLD(L'NWK),NWK                                               
         GOTO1 =V(DLFLD),DLCBD                                                  
         SPACE 1                                                                
         MVI   DLCBACT,DLCBPUT     PREPARE DOWNLOAD BLOCK FOR TP                
         MVI   DLCBTYP,DLCBTXT     ASSIGN PERSON                                
         MVI   DLCBLEN,L'TAAYTPC   MOVE TO DOWNLOAD BLOCK AND PUT IT            
         MVC   DLCBFLD(L'TAAYTPC),TAAYTPC                                       
         GOTO1 =V(DLFLD),DLCBD                                                  
         SPACE 1                                                                
         MVI   DLCBACT,DLCBPUT     PREPARE DOWNLOAD BLOCK FOR                   
         MVI   DLCBTYP,DLCBTXT     BILLING DAYS DUE                             
         MVI   DLCBLEN,2           MOVE TO DOWNLOAD BLOCK AND PUT IT            
         MVC   DLCBFLD,=C'00'                                                   
         CLI   TAAYDAYS,X'FF'                                                   
         BE    IO90                                                             
         EDIT  (1,TAAYDAYS),(2,DLCBFLD),ALIGN=LEFT                              
IO90     GOTO1 =V(DLFLD),DLCBD                                                  
         DROP  R4                                                               
         SPACE 1                                                                
         USING TABRD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TABRELQ      R4=A(AGENCY BILLING RULES ELEMENT)           
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         SPACE 1                                                                
         MVI   DLCBACT,DLCBPUT     PREPARE DOWNLOAD BLOCK FOR                   
         MVI   DLCBTYP,DLCBTXT     AGENCY HANDLING RATE                         
         MVI   DLCBLEN,2           MOVE AGENCY HANDLING RATE TO                 
         EDIT  (1,TABRTYPE),(2,DLCBFLD),ALIGN=LEFT,ZERO=NOBLANK                 
         GOTO1 =V(DLFLD),DLCBD                                                  
         SPACE 1                                                                
         MVI   DLCBACT,DLCBPUT     PREPARE DOWNLOAD BLOCK FOR                   
         MVI   DLCBTYP,DLCBTXT     AGENCY HANDLING RATE                         
         MVI   DLCBLEN,5           MOVE AGENCY HANDLING RATE TO                 
         MVC   AYHAND,TABRHAND     DOWNLOAD BLOCK AND PUT IT                    
         EDIT  TABRHAND,(5,DLCBFLD),2,ALIGN=LEFT,ZERO=NOBLANK                   
         GOTO1 =V(DLFLD),DLCBD                                                  
         DROP  R4                                                               
         SPACE 1                                                                
         BAS   RE,DLHANDLE                                                      
         SPACE 1                                                                
         L     RE,AYCOUNT          INCREMENT AGENCY COUNTER                     
         AHI   RE,1                                                             
         ST    RE,AYCOUNT                                                       
         SPACE 1                                                                
         MVI   DLCBACT,DLCBEOL     MARK END OF LINE                             
         GOTO1 =V(DLFLD),DLCBD                                                  
IOHOOKX  B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO GET USER ID CODE FROM ATTN. STAFF REC                 
         SPACE                                                                  
DLUSER2  NTR1                                                                   
         MVC   USERTAB,SPACES                                                   
         SPACE 1                                                                
         USING TAFND,R4                                                         
         L     R4,AIO              R4=A(AGENCY RECORD)                          
         MVI   ELCODE,TAFNELQ                                                   
         BAS   RE,GETEL                                                         
         B     *+8                                                              
DLU210   BAS   RE,NEXTEL           R4=A(FREE FORM NAME ELEMENT)                 
         BNE   DLU315                                                           
         CLI   TAFNTYPE,TAFNTATT   ONLY WANT ATTENTION NAME ELEMENT             
         BNE   DLU210                                                           
         SPACE 1                                                                
         LA    R2,TAFNNAME         FIND END OF ATTENTION PERSON'S               
         ZIC   R3,TAFNLEN          FIRST NAME                                   
         AHI   R3,-3                                                            
         SR    RF,RF                                                            
DLU230   CLI   0(R2),C' '                                                       
         BE    DLU240                                                           
         LA    R2,1(R2)            USE RF TO COUNT LENGTH OF                    
         AHI   RF,1                FIRST NAME                                   
         BCT   R3,DLU230                                                        
         B     DLU315                                                           
         SPACE 1                                                                
DLU240   CHI   RF,0                                                             
         BE    DLU315                                                           
         SPACE 1                                                                
         LA    R2,1(R2)            R2=A(FIRST BYTE OF LAST NAME)                
         LR    R1,R2                                                            
         SR    RE,RE                                                            
DLU250   CLI   0(R1),C' '          USE RE TO COUNT LENGTH OF                    
         BE    DLU260              LAST NAME                                    
         LA    R1,1(R1)                                                         
         AHI   RE,1                                                             
         BCT   R3,DLU250                                                        
         AHI   RE,-1                                                            
         SPACE 1                                                                
DLU260   CHI   RE,0                                                             
         BE    DLU315                                                           
         SPACE 1                                                                
         USING TLSTPD,R3                                                        
         LA    R3,KEY              BUILD PASSIVE STAFF RECORD KEY               
         XC    KEY,KEY                                                          
         MVI   TLSTPCD,TLSTNCDQ                                                 
         MVC   TLSTNLST,SPACES                                                  
         AHI   RE,-1                                                            
         CHI   RE,11                                                            
         BNH   *+8                                                              
         LHI   RE,11                                                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   TLSTNLST(0),0(R2)    WITH LAST NAME                              
         MVC   TLSTNFST,SPACES                                                  
         AHI   RF,-1                                                            
         CHI   RF,11                                                            
         BNH   *+8                                                              
         LHI   RF,11                                                            
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   TLSTNFST(0),TAFNNAME AND FIRST NAME                              
         DROP  R3,R4                                                            
         SPACE 1                                                                
         GOTO1 HIGH                READ STAFF RECORD PASSIVE KEY                
         CLC   KEY(TLSTNSEQ-TLSTPD),KEYSAVE                                     
         BNE   DLU310                                                           
         MVC   AIO,AIO1                                                         
         GOTO1 GETREC              AND GET THE RECORD                           
         SPACE 1                                                                
         USING TLSTD,R4                                                         
         L     R4,AIO              SAVE USER CODE INTO USER TABLE               
         MVC   USERTAB(4),TLSTSTAF+4                                            
         DROP  R4                                                               
         SPACE 1                                                                
DLU310   MVC   AIO,TIAREC         RESTORE AIO TO AGENCY                         
         MVC   KEY,TIKEY          RESTORE AGENCY KEY                            
         GOTO1 HIGH               RESTORE READ SEQUENCE                         
         SPACE 1                                                                
DLU315   MVI   DLCBACT,DLCBPUT     PUT ITEM TO PRINT LINE                       
         MVI   DLCBTYP,DLCBTXT     DATA TYPE IS TEXT                            
         MVI   DLCBLEN,4                                                        
         MVC   DLCBFLD(4),USERTAB                                               
         GOTO1 =V(DLFLD),DLCBD                                                  
DLU2X    B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO GET USER ID CODE FROM INTERFACE RECORD                
         SPACE                                                                  
DLUSER   NTR1                                                                   
         LA    R3,USERTAB                                                       
         MVC   USERTAB(USERMAX*L'USERTAB),SPACES                                
         XC    USERCNT,USERCNT                                                  
         SPACE 1                                                                
         USING TLIFD,R4                                                         
         LA    R4,KEY            BUILD KEY FOR AGENCY INTERFACE                 
         XC    TLIFKEY,TLIFKEY   RECORD                                         
         MVI   TLIFCD,TLIFCDQ                                                   
         MVC   TLIFAGY,TIAGY                                                    
         GOTO1 HIGH                                                             
         B     DLU20                                                            
DLU10    GOTO1 SEQ                                                              
DLU20    CLC   KEY(TLIFCLI-TLIFD),KEYSAVE                                       
         BNE   DLU40                                                            
         DROP  R4                                                               
         SPACE 1                                                                
         MVC   AIO,AIO1           GET AGENCY INTERFACE RECORD                   
         GOTO1 GETREC                                                           
         SPACE 1                                                                
         USING TAIFD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TAIFELQ     GET INTERFACE ELEMENT                         
         BAS   RE,GETEL                                                         
         BNE   DLU40                                                            
         SPACE 1                                                                
         LA    RE,USERTAB         IF ID ALREADY IN USER LIST                    
         LHI   RF,USERMAX         SKIP IT                                       
DLU30    CLC   TAIFAGY,0(RE)                                                    
         BE    DLU10                                                            
         LA    RE,L'TAIFAGY(RE)                                                 
         BCT   RF,DLU30                                                         
         SPACE 1                                                                
         MVC   0(L'TAIFAGY,R3),TAIFAGY  SAVE USER CODE INTO USER                
         LA    R3,L'TAIFAGY(R3)         TABLE                                   
         LH    RE,USERCNT                                                       
         AHI   RE,1                                                             
         CHI   RE,USERMAX                                                       
         BNH   *+6                                                              
         DC    H'00'                                                            
         STH   RE,USERCNT                                                       
         B     DLU10                                                            
         DROP  R4                                                               
         SPACE 1                                                                
DLU40    MVC   AIO,TIAREC         RESTORE AIO TO AGENCY                         
         MVC   KEY,TIKEY          RESTORE AGENCY KEY                            
         GOTO1 HIGH               RESTORE READ SEQUENCE                         
         SPACE 1                                                                
         LA    R2,USERTAB                                                       
         LHI   R4,USERMAX                                                       
DLU50    MVI   DLCBACT,DLCBPUT     PUT ITEM TO PRINT LINE                       
         MVI   DLCBTYP,DLCBTXT     DATA TYPE IS TEXT                            
         MVI   DLCBLEN,L'USERTAB                                                
         MVC   DLCBFLD(L'USERTAB),0(R2)                                         
         GOTO1 =V(DLFLD),DLCBD                                                  
DLU60    AHI   R2,L'TAIFAGY                                                     
         BCT   R4,DLU50                                                         
DLUX     B     XIT                                                              
*              ROUTINE TO GET CLIENTS WITH SPECIAL HANDLING RATES               
         SPACE                                                                  
DLHANDLE NTR1                                                                   
         USING TLCLD,R4                                                         
         LA    R4,KEY            BUILD KEY FOR CLIENT RECORDS                   
         XC    TLCLKEY,TLCLKEY                                                  
         MVI   TLCLCD,TLCLCDQ                                                   
         MVC   TLCLAGY,TIAGY                                                    
         GOTO1 HIGH                                                             
         B     DLH20                                                            
DLH10    GOTO1 SEQ                                                              
DLH20    CLC   KEY(TLCLCLI-TLCLD),KEYSAVE                                       
         BNE   DLH30                                                            
         LA    R4,KEY                                                           
         MVC   TICLI,TLCLCLI                                                    
         DROP  R4                                                               
         SPACE 1                                                                
         MVC   AIO,AIO1           GET CLIENT RECORD                             
         GOTO1 GETREC                                                           
         SPACE 1                                                                
         USING TABRD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TABRELQ     GET BILLING RULES ELEMENT                     
         BAS   RE,GETEL                                                         
         BNE   DLH10                                                            
         SPACE 1                                                                
         CLC   TABRHAND,AYHAND    IF HANDLING RATE IS DIFFERENT                 
         BE    DLH10              ON CLIENT                                     
         SPACE 1                                                                
         MVI   DLCBACT,DLCBPUT     PUT ITEM TO PRINT LINE                       
         MVI   DLCBTYP,DLCBTXT     DATA TYPE IS TEXT                            
         MVI   DLCBLEN,13                                                       
         MVC   DLCBFLD(L'TICLI),TICLI                                           
         EDIT  TABRHAND,(5,DLCBFLD+7),2,ALIGN=LEFT,ZERO=NOBLANK                 
         GOTO1 =V(DLFLD),DLCBD                                                  
         B     DLH10                                                            
         DROP  R4                                                               
         SPACE 1                                                                
DLH30    MVC   AIO,TIAREC         RESTORE AIO TO AGENCY                         
         MVC   KEY,TIKEY          RESTORE AGENCY KEY                            
         GOTO1 HIGH               RESTORE READ SEQUENCE                         
         SPACE 1                                                                
DLHX     B     XIT                                                              
         EJECT                                                                  
*              USER SUPPLIED PRINT ROUTINE - DOWNLOAD                           
         SPACE 1                                                                
SPLATDWN NTR1                                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVI   LINE,1              PREVENT PAGE BREAK                           
         B     PREPDX                                                           
         EJECT                                                                  
*              ERRORS, EXITS, CONSTANTS, ETC.                                   
*                                                                               
FLDINV   MVI   ERROR,INVALID       INVALID INPUT FIELD                          
         GOTO1 ERREX                                                            
         SPACE 2                                                                
         GETEL R4,DATADISP,ELCODE                                               
         SPACE 2                                                                
         EJECT                                                                  
*              SPREADSHEET HEADER LINE TABLE                                    
         SPACE 1                                                                
HEADTAB  DC    X'0B',C'AGENCY NAME'                                             
         DC    X'0A',C'ADDRESS #1'                                              
         DC    X'0A',C'ADDRESS #2'                                              
         DC    X'0A',C'ADDRESS #3'                                              
         DC    X'0A',C'ADDRESS #4'                                              
         DC    X'0B',C'AGENCY CODE'                                             
         DC    X'0F',C'TALENT OR PRINT'                                         
         DC    X'0A',C'USER ID #1'                                              
         DC    X'10',C'TELEPHONE NUMBER'                                        
         DC    X'0A',C'FAX NUMBER'                                              
         DC    X'09',C'TP OFFICE'                                               
         DC    X'0E',C'E-MAIL ADDRESS'                                          
         DC    X'07',C'NETTAL?'                                                 
         DC    X'09',C'TP ASSIGN'                                               
         DC    X'10',C'BILLING DAYS DUE'                                        
         DC    X'0C',C'BILLING TYPE'                                            
         DC    X'14',C'AGENCY HANDLING RATE'                                    
         DC    X'1D',C'SPECIAL CLIENT HANDLING RATES'                           
         DC    X'FF'                                                            
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              DSECT TO COVER LOCAL WORKING STORAGE                             
*                                                                               
WSD      DSECT                                                                  
         DS    0A                                                               
AMASTD   DS    A                   A(MASTER)                                    
ALOGOC   DS    A                   A(LOGOC)                                     
ALOGO    DS    A                   A(LOGO)                                      
AREMOT   DS    A                   A(REMOTE)                                    
APRNTBL  DS    A                   A(PRNTBL)                                    
APRNT    DS    A                   A(PRNT)                                      
*                                                                               
AYCOUNT  DS    F                                                                
AYHAND   DS    XL2                                                              
*                                                                               
FAX      DS    CL12                                                             
EMAIL    DS    CL35                                                             
NWK      DS    CL1                                                              
*                                                                               
OPTIONS  DS    X                                                                
NOEMAIL  EQU   X'80'                                                            
NOLOCK   EQU   X'40'                                                            
*                                                                               
DLBLOCK  DS    CL(DLCBXLX)                                                      
DWNP     DS    CL(L'P)                                                          
*                                                                               
USERMAX  EQU   5                                                                
USERCNT  DS    H                                                                
USERTAB  DS    (USERMAX)XL4                                                     
WSLNQ    EQU   *-WSD                                                            
         EJECT                                                                  
               DSECT TO COVER HEADER TABLE                                      
HEADD    DSECT                                                                  
HEADLEN  DS    X                                                                
HEADER   DS    0C                                                               
         EJECT                                                                  
       ++INCLUDE TAREPFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TAREPC0D                                                       
         EJECT                                                                  
* DDGENTWA   (MUST FOLLOW LAST SCREEN)                                          
* DDSPOOLD                                                                      
* DDLOGOD                                                                       
* DDSPLWORKD                                                                    
* DDBIGBOX                                                                      
* DDMASTD                                                                       
* DDREMOTED                                                                     
* TAGENFILE                                                                     
* TASYSDSECT                                                                    
* TASYSEQUS                                                                     
* DDPERVALD                                                                     
* DDTWADCONS                                                                    
* TAREPWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDWIDED                                                        
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DDLOGOD                                                        
       ++INCLUDE DDMASTD                                                        
       ++INCLUDE DDREMOTED                                                      
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE DDDLCB                                                         
       ++INCLUDE DDPERVALD                                                      
TWADCOND DSECT                                                                  
       ++INCLUDE DDTWADCONS                                                     
       ++INCLUDE TAREPWORKD                                                     
         PRINT ON                                                               
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003TAREP55   01/08/04'                                      
         END                                                                    
