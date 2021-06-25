*          DATA SET TAGENFD    AT LEVEL 007 AS OF 05/29/15                      
*PHASE T702FDE,*                                                                
         TITLE 'T702FD - CAST VERSION MAINTENANCE'                              
T702FD   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 TMPLNQ,T702FD                                                    
         LR    R6,RC                                                            
         USING TMPD,R6                                                          
         LR    RE,RC                                                            
         L     RC,0(R1)            RC=GENCON STORAGE AREA                       
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(TWA)                                    
         USING T702FFD,RA                                                       
         L     R9,ASYSD            R9=ROOT STORAGE AREA                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD          R8=SPOOL STORAGE AREA                        
         USING SPOOLD,R8                                                        
                                                                                
         BAS   RE,INIT             INITIALIZE                                   
                                                                                
         BAS   RE,VKEY             VALIDATE KEY                                 
                                                                                
         BAS   RE,DREC             DISPLAY RECORD                               
                                                                                
         BAS   RE,VREC             VALIDATE RECORD                              
                                                                                
         BAS   RE,UNVERIFY         UNVERIFY COMMERCIAL                          
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO INITIALIZE PROGRAM                                    
                                                                                
INIT     NTR1                                                                   
         GOTO1 INITIAL,DMCB,PFTAB  HANDLE PF KEY INPUT                          
                                                                                
         GOTO1 FLDVAL,DMCB,(X'80',VCAAVERH),(X'80',VCADVERH)                    
         BE    INIT10                                                           
         NI    LISTSTAT,X'FF'-DONEVCAS                                          
                                                                                
INIT10   TM    LISTSTAT,FROMLIST+DONEVCAS    IF ALL VERSIONS HAVE               
         BNO   INIT20                        BEEN LISTED AND WE                 
         MVI   PFAID,21                      CAME FROM CAST/LIST                
         OI    TRNSTAT,OKINTPFK                                                 
         MVI   CALLSP,1                                                         
         GOTO1 INITIAL,DMCB,BACKTAB          RETURN TO CAST LIST                
                                                                                
INIT20   LA    R2,STNOCHG           R2=A(TABLE OF STAFF CATEGORIES              
INIT30   CLI   0(R2),X'FF'               THAT ARE NOT ALLOWED TO                
         BE    XIT                       CHANGE VERSIONS)                       
         CLC   TGCTSTTY,0(R2)                                                   
         BE    INIT40               PROTECT THAT ADD AND DELETE                 
         LA    R2,1(R2)             FIELDS FOR THESE CATEGORIES                 
         B     INIT30                                                           
INIT40   GOTO1 FLDVAL,DMCB,(X'0A',VCAAVERH),VCADVERH                            
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO VALIDATE KEY FIELDS                                   
                                                                                
VKEY     NTR1                                                                   
         GOTO1 FLDVAL,DMCB,(X'40',VCAAGYH),VCASORTH                             
         BE    XIT                                                              
                                                                                
         MVI   LISTSTAT,0                                                       
         CLI   THISLSEL,C'V'                                                    
         BNE   *+8                                                              
         OI    LISTSTAT,FROMLIST                                                
                                                                                
         MVI   LSTDVER,0                                                        
                                                                                
         GOTO1 RECVAL,DMCB,TLCOICDQ,(X'0C',0),VCACIDNH                          
         BNE   INVRACT                                                          
                                                                                
         L     RE,AIO                                                           
         XC    SVCOKEY,SVCOKEY                                                  
         MVC   SVCOKEY(L'TLCOKEY),0(RE)                                         
                                                                                
         GOTO1 SSNUNPK,DMCB,VCASSN,TGSSN                                        
         GOTO1 RECVAL,DMCB,TLW4CDQ,(X'0C',0),VCASSNNH                           
         BNE   INVRACT                                                          
                                                                                
         GOTO1 HEXIN,DMCB,VCASORT+1,TGCSORT,L'VCASORT-2                         
                                                                                
         MVC   TGCAT,VCACAT                                                     
         OC    TGCAT,SPACES                                                     
                                                                                
         GOTO1 RECVAL,DMCB,TLCACDQ,(X'24',0)                                    
         BNE   INVRACT                                                          
                                                                                
         USING TAFND,R4                                                         
         XC    SVWID,SVWID                                                      
         MVI   ELCODE,TAFNELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TAFNTWEB))                                     
         BNE   VK10                                                             
         L     R4,TGELEM                                                        
         MVC   SVWID,TAFNNAME                                                   
         DROP  R4                                                               
                                                                                
         USING TACAD,R4                                                         
VK10     L     R4,AIO                                                           
         MVI   ELCODE,TACAELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         MVC   VCAONOF,TACAONOF                                                 
         MVC   VCAUNIT,TACAUNIT                                                 
         MVC   VCAUNLO(3),TACAUN                                                
         MVC   VCAUNLO+4(3),TACALOCL                                            
         MVC   VCAYEAR,TACAYEAR                                                 
                                                                                
         MVI   MUSICIAN,TACOUVNM                                                
         CLC   TACAUN,=C'AFM'                                                   
         BNE   *+8                                                              
         MVI   MUSICIAN,TACOUVMU                                                
         DROP  R4                                                               
                                                                                
         GOTO1 FLDVAL,DMCB,(X'2A',VCAAGYH),VCAYEARH                             
         CLI   TGCTSTTY,TASTTYPP   FOR NON-PROGRAMMERS                          
         BE    VK20                                                             
         OI    VCASORTH+1,X'0C'    NEVER DISPLAY SORT KEY                       
                                                                                
VK20     NI    VCAAVERH+1,X'DF'    IF PERFORMER HAS NO TRACK                    
         OI    VCAAVERH+6,X'80'    ASSOCIATIONS, UNPROTECT                      
         NI    VCADVERH+1,X'DF'    ADD AND DELETE FIELDS                        
         OI    VCADVERH+6,X'80'                                                 
         L     R4,AIO                                                           
         MVI   ELCODE,TATRELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         OI    VCAAVERH+1,X'20'    IF PERFORMER HAS TRACK                       
         MVI   VCAAVERH+5,0        ASSOCIATIONS, PROTECT AND                    
         XC    VCAAVER,VCAAVER     CLEAR ADD AND DELETE FIELDS                  
         OI    VCADVERH+1,X'20'                                                 
         MVI   VCADVERH+5,0                                                     
         XC    VCADVER,VCADVER                                                  
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO DISPLAY VERSIONS                                      
                                                                                
DREC     NTR1                                                                   
         CLI   MODE,DISPREC                                                     
         BE    DR10                                                             
         CLI   MODE,XRECPUT                                                     
         BNE   XIT                                                              
                                                                                
DR10     BAS   RE,BLDCOTBL                                                      
                                                                                
         GOTO1 FLDVAL,DMCB,(3,VCAVR1H),VCAVRLH                                  
                                                                                
         BRAS  RE,TRNTATR                                                       
                                                                                
         MVI   ELCODE,TAFNELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TAFNTVER))                                     
         BNE   DR80                                                             
                                                                                
         USING TAFND,R4                                                         
         L     R4,TGELEM                                                        
                                                                                
         CLI   TAFNNAME,251      CAST MEMBER ON ALL VERSIONS                    
         BNE   DR20                                                             
         MVI   VCAVR1,C'*'                                                      
         MVC   VCAVR1N,=C'ALL VERSIONS'                                         
         B     DR80                                                             
                                                                                
DR20     ZIC   R5,TAFNLEN                                                       
         SHI   R5,3                                                             
                                                                                
         LA    R4,TAFNNAME                                                      
                                                                                
         LA    R2,VCAVR1H                                                       
         LA    R3,VCAVRLH                                                       
                                                                                
DR30     CLC   LSTDVER,0(R4)                                                    
         BH    DR70                                                             
                                                                                
         EDIT  (1,0(R4)),(3,8(R2)),ALIGN=LEFT                                   
                                                                                
         ZIC   RE,0(R2)                                                         
         AR    R2,RE                                                            
         MVC   8(9,R2),=C'NOT FOUND'                                            
                                                                                
         USING COVRTBLD,RE                                                      
         LA    RE,COVRTBL                                                       
DR40     CLI   0(RE),X'FF'                                                      
         BE    DR60                                                             
         CLC   COVRCD,0(R4)                                                     
         BE    DR50                                                             
         LA    RE,COVRLNQ(RE)                                                   
         B     DR40                                                             
DR50     MVC   8(L'VCAVR1N,R2),COVRID                                           
         DROP  RE                                                               
                                                                                
DR60     ZIC   RE,0(R2)                                                         
         AR    R2,RE                                                            
         CR    R2,R3                                                            
         BNH   DR70                                                             
         MVC   LSTDVER,0(R4)                                                    
         MVC   MYMSGNO,=H'261'                                                  
         B     DR90              END OF SCREEN REACHED                          
                                                                                
DR70     LA    R4,1(R4)                                                         
         BCT   R5,DR30           END OF VERSIONS REACHED                        
DR80     MVI   LSTDVER,0                                                        
         MVC   MYMSGNO,=H'262'                                                  
         OI    LISTSTAT,DONEVCAS                                                
                                                                                
DR90     LA    R2,VCAAVERH                                                      
                                                                                
         CLI   MODE,XRECPUT                                                     
         BNE   DR100                                                            
         GOTO1 FLDVAL,DMCB,(X'80',VCAAVERH),(X'80',VCADVERH)                    
         BE    DR100                                                            
         GOTO1 FLDVAL,DMCB,(3,VCAAVERH),(X'80',VCADVERH)                        
         B     XIT                                                              
                                                                                
DR100    MVI   MYMTYP,GTMINF                                                    
         MVI   BLOCK,0                                                          
         OI    GENSTAT2,USGETTXT                                                
         B     MSGEXIT                                                          
         EJECT                                                                  
*              ROUTINE TO VALIDATE VERSIONS                                     
                                                                                
VREC     NTR1                                                                   
         CLI   MODE,VALREC                                                      
         BNE   XIT                                                              
                                                                                
         GOTO1 FLDVAL,DMCB,(X'80',VCAAVERH),(X'80',VCADVERH)                    
         BE    XIT                                                              
                                                                                
         BAS   RE,BLDCOTBL       BUILD TABLE OF VERSIONS ON COMMERCIAL          
         BAS   RE,BLDCATBL       BUILD TABLE OF CAST MEMBER'S VERSIONS          
                                                                                
         CLI   VCAAVERH+5,0      CANNOT ADD AND DELETE AT SAME TIME             
         BE    VR90                                                             
         CLI   VCADVERH+5,0                                                     
         BNE   DELINV                                                           
                                                                                
VR10     LA    R2,VCAAVERH       IF ADDING A VERSION ...                        
                                                                                
         CLI   COVRTBL,X'FF'     COMMERCIAL MUST HAVE VERSIONS                  
         BE    FLDINV                                                           
         CLI   CAVRTBL,251       AND CAST CANNOT ALREADY BE ON ALL              
         BE    FLDINV            VERSIONS                                       
                                                                                
         BAS   RE,BLDINTBL       BUILD TABLE OF INPUTTED VERSIONS               
         BAS   RE,VALWEBID       VALIDATE INPUT FOR WEB CAST                    
                                                                                
         LA    RE,INPUTBL                                                       
VR20     CLI   0(RE),X'FF'                                                      
         BE    VR50                                                             
         LA    RF,CAVRTBL                                                       
VR30     CLI   0(RF),X'FF'       GIVE INVALID MESSAGE IF ANY VERSIONS           
         BE    VR40              TO ADD ARE ALREADY ON CAST RECORD              
         CLC   0(1,RE),0(RF)                                                    
         BE    FLDINV                                                           
         LA    RF,1(RF)                                                         
         B     VR30                                                             
VR40     LA    RE,1(RE)                                                         
         B     VR20                                                             
                                                                                
VR50     GOTO1 DELL,DMCB,(1,=AL1(TAFNTVER))                                     
                                                                                
         USING TAFND,R4                                                         
         LA    R4,ELEMENT        INITIALIZE NEW VERSION ELEMENT                 
         MVI   TAFNEL,TAFNELQ                                                   
         MVI   TAFNTYPE,TAFNTVER                                                
                                                                                
         LHI   R1,TAFNLNQ        R1=ELEMENT LENGTH ACCUMULATOR                  
         LA    R3,TAFNNAME       R3=A(WHERE TO BLD VERS LST IN ELEMENT)         
         LA    RE,CAVRTBL        RE=A(CAST MEMBER'S ORIGINAL VERSIONS)          
         LA    RF,INPUTBL        RF=A(CAST MEMBER'S VERSIONS TO ADD)            
                                                                                
         CLI   INPUTBL,251       IF CAST IS TO BE ON ALL VERSIONS               
         BNE   VR60                                                             
         MVI   0(R3),251         PUT ALL VERSIONS INTO ELEMENT                  
         AHI   R1,1                                                             
         B     VR180                                                            
                                                                                
VR60     CLI   0(RE),X'FF'       BUILD VERSION LIST IN ELEMENT                  
         BE    VR70                                                             
         CLC   0(1,RF),0(RE)                                                    
         BL    VR70                                                             
         MVC   0(1,R3),0(RE)                                                    
         LA    RE,1(RE)                                                         
         B     VR80                                                             
                                                                                
VR70     CLI   0(RF),X'FF'                                                      
         BE    VR180                                                            
         MVC   0(1,R3),0(RF)                                                    
         LA    RF,1(RF)                                                         
VR80     LA    R3,1(R3)                                                         
         AHI   R1,1                                                             
         B     VR60                                                             
         DROP  R4                                                               
                                                                                
VR90     LA    R2,VCADVERH       IF DELETING A VERSION ...                      
                                                                                
         CLI   CAVRTBL,X'FF'     CAST MUST HAVE VERSIONS                        
         BE    FLDINV                                                           
         BAS   RE,BLDINTBL       BUILD TABLE OF INPUTTED VERSIONS               
         BAS   RE,VALWEBID       VALIDATE INPUT FOR WEB CAST                    
                                                                                
         CLI   INPUTBL,251                                                      
         BE    VR130                                                            
                                                                                
         LA    RE,INPUTBL                                                       
VR100    CLI   0(RE),X'FF'                                                      
         BE    VR130                                                            
         LA    RF,CAVRTBL                                                       
VR110    CLI   0(RF),X'FF'       GIVE INVALID MESSAGE IF VERSION CODE           
         BE    FLDINV            TO DELETE IS NOT ALREADY ON CAST               
         CLC   0(1,RE),0(RF)     RECORD                                         
         BE    VR120                                                            
         LA    RF,1(RF)                                                         
         B     VR110                                                            
VR120    LA    RE,1(RE)                                                         
         B     VR100                                                            
                                                                                
VR130    GOTO1 DELL,DMCB,(1,=AL1(TAFNTVER))                                     
                                                                                
         CLI   INPUTBL,251       DONE IF DELETING ALL VERSIONS                  
         BE    XIT                                                              
                                                                                
         USING TAFND,R4                                                         
         LA    R4,ELEMENT        INITIALIZE NEW VERSION ELEMENT                 
         MVI   TAFNEL,TAFNELQ                                                   
         MVI   TAFNTYPE,TAFNTVER                                                
                                                                                
         LHI   R1,TAFNLNQ        R1=ELEMENT LENGTH ACCUMULATOR                  
         LA    R3,TAFNNAME       R3=A(WHERE TO BLD VERS LST IN ELEMENT)         
         LA    RE,CAVRTBL        RE=A(CAST MEMBER'S ORIGINAL VERSIONS)          
                                                                                
VR140    CLI   0(RE),X'FF'       BUILD VERSION LIST IN ELEMENT                  
         BE    VR180                                                            
         LA    RF,INPUTBL                                                       
VR150    CLI   0(RF),X'FF'                                                      
         BE    VR160                                                            
         CLC   0(1,RF),0(RE)                                                    
         BE    VR170                                                            
         LA    RF,1(RF)                                                         
         B     VR150                                                            
VR160    MVC   0(1,R3),0(RE)                                                    
         LA    R3,1(R3)                                                         
         AHI   R1,1                                                             
VR170    LA    RE,1(RE)                                                         
         B     VR140                                                            
                                                                                
VR180    CHI   R1,3                                                             
         BE    XIT                                                              
         STC   R1,TAFNLEN                                                       
         GOTO1 ADDELEM           ADD VERSION ELEMENT                            
         GOTO1 PUTREC                                                           
                                                                                
         MVI   IOOPT,C'Y'                                                       
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*              ROUTINE TO UNVERIFY COMMERCIAL                                   
                                                                                
UNVERIFY NTR1                                                                   
         CLI   MODE,XRECPUT                                                     
         BNE   XIT                                                              
                                                                                
         MVC   KEY,SVCOKEY       READ COMMERCIAL RECORD                         
         GOTO1 HIGH                                                             
         BE    *+6                                                              
         DC    H'00'                                                            
         MVC   AIO,AIO2                                                         
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
                                                                                
         USING TACOD,R4                                                         
         L     R4,AIO            GET COMMERCIAL DETAILS ELEMENT                 
         MVI   ELCODE,TACOELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         XC    TACOVDTE,TACOVDTE CLEAR DATE OF VERIFICATION                     
         XC    TACOVTIM,TACOVTIM TIME OF VERIFICATION                           
         XC    TACOVSTU,TACOVSTU USER ID                                        
         XC    TACOVST,TACOVST   AND STAFF ID                                   
         OC    TACOUVST,MUSICIAN SET UNVERIFIED STATUS                          
         DROP  R4                                                               
                                                                                
         GOTO1 PUTREC                                                           
                                                                                
         MVC   AIO,AIO1                                                         
         GOTO1 RECVAL,DMCB,TLCACDQ,(X'34',0)                                    
         BE    *+6                                                              
         DC    H'00'                                                            
                                                                                
         LA    R2,VCAAVERH                                                      
         MVC   MYMSGNO,=H'265'                                                  
         MVI   MYMTYP,GTMINF                                                    
         MVI   BLOCK,0                                                          
         OI    GENSTAT2,USGETTXT                                                
         B     MSGEXIT                                                          
         EJECT                                                                  
*              BUILD VERSION TABLE FOR COMMERCIAL                               
                                                                                
BLDCOTBL NTR1                                                                   
         LA    RE,COVRTBL        CLEAR COMMERCIAL VERSION TABLE                 
         LHI   RF,13                                                            
BCT10    XC    0(250,RE),0(RE)                                                  
         BCT   RF,BCT10                                                         
                                                                                
         MVC   AIO,AIO2                                                         
                                                                                
         USING COVRTBLD,R2                                                      
         LA    R2,COVRTBL        R2=A(COMMERCIAL VERSION TABLE)                 
                                                                                
         MVC   KEY,SVCOKEY       READ ALL COMMERCIAL RECORDS                    
BCT20    GOTO1 HIGH                                                             
         CLC   KEY(L'TLCOKEY),KEYSAVE                                           
         BNE   BCT40                                                            
         GOTO1 GETREC                                                           
                                                                                
         USING TAVRD,R4                                                         
         L     R4,AIO            READ ALL VERSION ELEMENTS                      
         MVI   ELCODE,TAVRELQ                                                   
         BAS   RE,GETEL                                                         
         B     *+8                                                              
BCT30    BAS   RE,NEXTEL                                                        
         BNE   BCT40                                                            
         MVC   COVRCD,TAVRVERS   SAVE VERSION CODES                             
         MVC   COVRID,TAVRCID    AND ID'S                                       
         LA    R2,COVRLNQ(R2)    IN COMMERCIAL VERSION TABLE                    
         B     BCT30                                                            
         DROP  R4                                                               
                                                                                
BCT40    ZIC   RE,KEYSAVE+TLCOVER-TLCOD                                         
         AHI   RE,1                                                             
         CHI   RE,TLCOV250                                                      
         BH    BCT50                                                            
         MVC   KEY,KEYSAVE                                                      
         STC   RE,KEY+TLCOVER-TLCOD                                             
         B     BCT20                                                            
                                                                                
BCT50    MVI   0(R2),X'FF'       MARK END OF VERSION TABLE                      
                                                                                
         MVC   AIO,AIO1                                                         
         GOTO1 RECVAL,DMCB,TLCACDQ,(X'34',0)                                    
         BE    *+6                                                              
         DC    H'00'                                                            
         B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
*              ROUTINE TO BUILD VERSION TABLE FOR CAST                          
                                                                                
BLDCATBL NTR1                                                                   
         XC    CAVRTBL,CAVRTBL   CLEAR VERSION TABLE                            
         MVI   CAVRTBL,X'FF'                                                    
                                                                                
         MVI   ELCODE,TAFNELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TAFNTVER))                                     
         BNE   XIT                                                              
                                                                                
         USING TAFND,R4                                                         
         L     R4,TGELEM                                                        
         ZIC   RF,1(R4)                                                         
         SHI   RF,4                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   CAVRTBL(0),TAFNNAME                                              
                                                                                
         LA    RE,CAVRTBL                                                       
         AR    RE,RF                                                            
         MVI   1(RE),X'FF'                                                      
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*              VALIDATE AND SORT ALL VERSION INPUT INTO INPUT TABLE             
*              ON ENTRY ... R2=A(VERSION INPUT FIELD)                           
                                                                                
BLDINTBL NTR1                                                                   
         XC    INPUTBL,INPUTBL   INITIALIZE INPUT                               
         XC    SORTTBL,SORTTBL   AND SORT TABLES                                
         MVI   SORTTBL,X'FF'                                                    
                                                                                
         BAS   RE,ALLVER         IF USER IS INPUTTING ALL VERSIONS              
         BE    XIT               BUILD TABLE AND EXIT                           
                                                                                
         USING SCAND,R5                                                         
         LA    R5,BLOCK                                                         
         GOTO1 SCANNER,DMCB,(R2),(R5),0                                         
         CLI   4(R1),0                                                          
         BE    FLDINV                                                           
         ZIC   R0,4(R1)                                                         
                                                                                
BIT10    CLC   SCDATA2,SPACES    ENTRY SHOULD NEVER INCLUDE AN                  
         BNE   FLDINV            EQUALS SIGN                                    
                                                                                
         XC    RANGEH(L'RANGEH+L'RANGE),RANGEH   CLEAR RANGE INFO               
                                                                                
         SR    R1,R1             USE R1 TO COUNT NUMBER OF DASHES               
         LA    RE,SCDATA1        IN ENTRY                                       
         LA    RF,L'SCDATA1                                                     
BIT20    CLI   0(RE),C'-'                                                       
         BNE   BIT30                                                            
         MVI   0(RE),C'='        REPLACE DASHES WITH EQUAL SIGNS                
         AHI   R1,1                                                             
BIT30    LA    RE,1(RE)                                                         
         BCT   RF,BIT20                                                         
                                                                                
         CHI   R1,0              NO DASHES MEANS ENTRY IS NOT A RANGE           
         BE    BIT80                                                            
         CHI   R1,1              MAXIMUM OF ONE DASH PERMITTED                  
         BNE   FLDINV                                                           
                                                                                
         MVI   RANGEH,L'RANGEH+L'RANGE     IF ENTRY IS A RANGE                  
         MVC   RANGEH+5(1),SCLEN1          BUILD MOCK FIELD TO SCAN             
         MVC   RANGE,SCDATA1                                                    
         ST    R5,SVENTRY        AND SAVE A(CURR ENTRY IN SCAN BLOCK)           
         ST    R0,SVENCNT                                                       
                                                                                
         LA    R5,WORK                                                          
         GOTO1 SCANNER,DMCB,RANGEH,(R5),0                                       
         CLI   4(R1),0                                                          
         BE    FLDINV                                                           
         ZIC   R0,4(R1)                                                         
                                                                                
         TM    SCVAL1,X'80'      FIRST ENTRY IN RANGE MUST BE NUMERIC           
         BZ    FLDINV                                                           
         CLC   SCBIN1,=F'1'      BETWEEN 1                                      
         BL    FLDINV                                                           
         CLC   SCBIN1,=F'249'    AND 249                                        
         BH    FLDINV                                                           
                                                                                
         TM    SCVAL2,X'80'      SECOND ENTRY IN RANGE MUST BE NUMERIC          
         BZ    FLDINV                                                           
         CLC   SCBIN2,=F'2'      BETWEEN 2                                      
         BL    FLDINV                                                           
         CLC   SCBIN2,=F'250'    AND 250                                        
         BH    FLDINV                                                           
                                                                                
BIT40    CLC   SCBIN1,SCBIN2     PROCESS ALL NUMBERS IN RANGE UNTIL             
         BNH   BIT50             FIRST ENTRY EXCEEDS SECOND                     
         L     R5,SVENTRY                                                       
         L     R0,SVENCNT                                                       
         XC    RANGE,RANGE                                                      
         B     BIT170                                                           
                                                                                
BIT50    LA    R3,CAVRTBL        R3=A(CAST MEMBER'S ORIGINAL VERSIONS)          
                                                                                
         CLI   VCAAVERH+5,0      IF ADDING A RANGE                              
         BE    BIT70                                                            
BIT60    CLI   0(R3),X'FF'                                                      
         BE    BIT80                                                            
         CLC   0(1,R3),SCBIN1+3  DO NOT ATTEMPT TO ADD ANY VERSIONS             
         BE    BIT160            THAT CAST IS ALREADY ON                        
         LA    R3,1(R3)                                                         
         B     BIT60                                                            
                                                                                
BIT70    CLI   0(R3),X'FF'       IF DELETING A RANGE                            
         BE    BIT160                                                           
         CLC   0(1,R3),SCBIN1+3  DO NOT ATTEMPT TO DELETE ANY VERSIONS          
         BE    BIT80             THAT CAST IS NOT ALREADY ON                    
         LA    R3,1(R3)                                                         
         B     BIT70                                                            
                                                                                
         USING COVRTBLD,R3                                                      
BIT80    LA    R3,COVRTBL        R3=A(COMMERCIAL VERSION TABLE)                 
                                                                                
         CLI   SCLEN1,3          IF FIELD CONTAINS A VERSION CODE               
         BH    BIT110            IT MUST BE 3 CHARACTERS OR LESS                
         TM    SCVAL1,X'80'      AND VALID NUMERIC                              
         BZ    BIT110                                                           
         MVC   ACTUAL,SCBIN1+3   SAVE CODE IN ACTUAL                            
BIT90    CLI   0(R3),X'FF'                                                      
         BE    BIT100                                                           
         CLC   COVRCD,ACTUAL                                                    
         BE    BIT130                                                           
         LA    R3,COVRLNQ(R3)                                                   
         B     BIT90                                                            
                                                                                
BIT100   OC    RANGE,RANGE       CODE MUST EXIST ON COMMERCIAL                  
         BZ    FLDINV            UNLESS IT IS PART OF A RANGE                   
         B     BIT160                                                           
                                                                                
BIT110   CLI   0(R3),X'FF'                                                      
         BE    FLDINV            ELSE, ASSUME A VERSION ID                      
         CLC   COVRID,SCDATA1    WAS ENTERED                                    
         BE    BIT120            IF THAT VERSION ID IS NOT ON THIS              
         LA    R3,COVRLNQ(R3)    COMMERCIAL, RETURN ERROR                       
         B     BIT110            IF VERSION IS FOUND,                           
BIT120   MVC   ACTUAL,COVRCD     SAVE CODE IN ACTUAL                            
         DROP  R3                                                               
                                                                                
BIT130   LA    RE,SORTTBL                                                       
BIT140   CLI   0(RE),X'FF'       SAVE ACTUAL INTO SORT TABLE'S FIRST            
         BE    BIT150            EMPTY SLOT                                     
         CLC   ACTUAL,0(RE)      DO NOT ALLOW VERSION TO BE DUPLICATED          
         BE    FLDINV                                                           
         LA    RE,1(RE)                                                         
         B     BIT140                                                           
BIT150   MVC   0(1,RE),ACTUAL                                                   
         MVI   1(RE),X'FF'                                                      
                                                                                
         OC    RANGE,RANGE       IF VERSION IS PART OF RANGE                    
         BZ    BIT170                                                           
BIT160   ZIC   RE,SCBIN1+3       BUMP TO NEXT NUMBER IN RANGE                   
         AHI   RE,1              AND GO PROCESS                                 
         STC   RE,SCBIN1+3                                                      
         B     BIT40                                                            
                                                                                
BIT170   LA    R5,SCANNEXT       ELSE, BUMP TO NEXT ENTRY IN SCANNER            
         BCT   R0,BIT10          BLOCK                                          
         DROP  R5                                                               
                                                                                
         LA    R1,INPUTBL                                                       
                                                                                
BIT180   LA    RE,SORTTBL                                                       
BIT190   CLI   0(RE),X'FF'       FIND FIRST MEANINGFUL VALUE IN                 
         BE    BIT230            SORT TABLE                                     
         CLI   0(RE),0                                                          
         BNE   BIT200                                                           
         LA    RE,1(RE)                                                         
         B     BIT190                                                           
                                                                                
BIT200   LR    RF,RE                                                            
BIT210   LA    RF,1(RF)          COMPARE IT TO NEXT MEANINGFUL VALUE            
         CLI   0(RF),X'FF'       IN SORT TABLE                                  
         BE    BIT220                                                           
         CLI   0(RF),0                                                          
         BE    BIT210                                                           
         CLC   0(1,RE),0(RF)                                                    
         BL    BIT210                                                           
         LR    RE,RF                                                            
         B     BIT210                                                           
                                                                                
BIT220   AHI   R0,1              ADD VERSIONS TO INPUT TABLE FROM               
         MVC   0(1,R1),0(RE)     SMALLEST TO LARGEST                            
         LA    R1,1(R1)                                                         
         MVI   0(RE),0                                                          
         B     BIT180                                                           
                                                                                
BIT230   MVI   0(R1),X'FF'                                                      
         B     XIT                                                              
         EJECT                                                                  
*              VALIDATE VERSION INPUT FOR WEB APPLICATION STAMPED CAST          
*              ON ENTRY ... R2=A(VERSION INPUT FIELD)                           
                                                                                
VALWEBID NTR1                                                                   
         CLC   =C'VC',SVWID      IF CAST IS STAMPED WITH VITA                   
         BE    WEBERR            COMPLETIONS ID                                 
         CLC   =C'TC',SVWID                                                     
         BE    WEBERR                                                           
         CLC   =C'RC',SVWID      OR VITA RADIO COMPLETIONS ID                   
         BE    WEBERR            RETURN ERROR                                   
                                                                                
         CLC   =C'VS',SVWID      IF CAST IS STAMPED WITH VITA                   
         BE    VWID00            SESSION ID                                     
         CLC   =C'TS',SVWID                                                     
         BE    VWID00                                                           
         CLC   =C'RS',SVWID      OR VITA RADIO SESSION ID                       
         BNE   XIT                                                              
VWID00   LA    R1,INPUTBL                                                       
VWID10   CLI   0(R1),X'FF'       ENSURE THAT VERSION 1 OR 2 IS NOT              
         BE    XIT               INPUT                                          
         CLI   0(R1),2                                                          
         BNH   WEBERR                                                           
                                                                                
         CLI   0(R1),251         IF INPUT IS *                                  
         BNE   VWID20                                                           
         CLI   VCAAVERH+5,0      AND ADDING VERSIONS                            
         BE    WEBERR                                                           
         LA    RE,CAVRTBL        ENSURE THAT CAST IS ALREADY ON                 
         CLI   1(RE),2           VERSION 1 AND 2                                
         BNE   WEBERR                                                           
                                                                                
VWID20   LA    R1,1(R1)                                                         
         B     VWID10                                                           
         EJECT                                                                  
*              ROUTINE TO SEE IF ALL VERSIONS ARE INPUTTED                      
*              ON ENTRY ... R2=A(VERSION INPUT FIELD)                           
                                                                                
ALLVER   NTR1                                                                   
         CLI   5(R2),1           IF INPUT IS ONE CHARACTER LONG                 
         BNE   AVER10                                                           
         CLI   8(R2),C'*'        AND IS A C'*'                                  
         BE    AVER20                                                           
         B     NO                USER IS INPUTTING ALL VERSIONS                 
                                                                                
AVER10   CLI   5(R2),3           IF INPUT IS THREE CHARACTERS LONG              
         BNE   NO                                                               
         CLC   =C'ALL',8(R2)     AND C'ALL'                                     
         BNE   NO                                                               
AVER20   MVI   INPUTBL,251       USER IS INPUTTING ALL VERSIONS                 
         MVI   INPUTBL+1,X'FF'                                                  
         B     YES                                                              
         EJECT                                                                  
*              ERROR MESSAGES, EXITS AND CONSTANTS                              
                                                                                
INVRACT  LA    R2,CONACTH          INVALID RECORD/ACTION                        
         MVI   ERROR,INVRCACT                                                   
         XC    VCACIDN,VCACIDN                                                  
         B     MSGEXIT                                                          
                                                                                
ADDINV   LA    R2,VCAAVERH         INVALID ADD FIELD                            
         B     FLDINV                                                           
                                                                                
DELINV   LA    R2,VCADVERH         INVALID DELETE FIELD                         
         B     FLDINV                                                           
                                                                                
FLDINV   MVI   ERROR,INVALID       INVALID INPUT FIELD                          
         B     MSGEXIT                                                          
                                                                                
WEBERR   MVC   MYMSGNO,=Y(ERUSEWEB) RECORD MUST BE UPDATED FROM                 
         B     EXTEXIT              WEB APPLICATION                             
*                                                                               
EXTEXIT  MVI   BLOCK,0                                                          
         MVI   MYMTYP,GTMERR                                                    
         OI    GENSTAT2,USGETTXT                                                
         B     MSGEXIT                                                          
                                                                                
RECEXIT  LA    R2,CONRECH                                                       
MSGEXIT  GOTO1 EXIT,DMCB,0                                                      
                                                                                
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
                                                                                
         GETEL R4,DATADISP,ELCODE                                               
                                                                                
HEXFFS   DC    16X'FF'                                                          
                                                                                
*              TABLE OF REGULAR PF KEYS                                         
                                                                                
PFTAB    DS    0H                                                               
         DC    AL1(PF13X-*,13,0,(PF13X-PF13)/KEYLNQ,0)                          
         DC    CL3'CH ',CL8'CHECK   ',CL8'LIST'                                 
PF13     DC    AL1(KEYTYGLB,L'TGSSN-1),AL2(TGSSN-TGD)                           
         DC    AL1(KEYTYGLB,L'TGAGY-1),AL2(TGAGY-TGD)                           
         DC    AL1(KEYTYGLB,L'TGCID-1),AL2(TGCID-TGD)                           
PF13X    EQU   *                                                                
         DC    AL1(PF14X-*,14,0,(PF14X-PF14)/KEYLNQ,0)                          
         DC    CL3'FT ',CL8'FTRACK  ',CL8'REPORT'                               
PF14     DC    AL1(KEYTYGLB,L'TGSSN-1),AL2(TGSSN-TGD)                           
         DC    AL1(KEYTYGLB,L'TGAGY-1),AL2(TGAGY-TGD)                           
         DC    AL1(KEYTYGLB,L'TGCID-1),AL2(TGCID-TGD)                           
         DC    AL1(KEYTYTWA,3-1),AL2(VCASORT+10-T702FFD)                        
PF14X    EQU   *                                                                
         DC    AL1(PF15X-*,15,0,(PF15X-PF15)/KEYLNQ,0)                          
         DC    CL3'FT ',CL8'FTRACK  ',CL8'LIST'                                 
PF15     DC    AL1(KEYTYGLB,L'TGAGY-1),AL2(TGAGY-TGD)                           
         DC    AL1(KEYTYGLB,L'TGCID-1),AL2(TGCID-TGD)                           
PF15X    EQU   *                                                                
         DC    AL1(PF16X-*,16,0,(PF16X-PF16)/KEYLNQ,0)                          
         DC    CL3'   ',CL8'CERROR  ',CL8'DISPLAY'                              
PF16     DC    AL1(KEYTYGLB,L'TGSSN-1),AL2(TGSSN-TGD)                           
         DC    AL1(KEYTYGLB,L'TGCID-1),AL2(TGCID-TGD)                           
         DC    AL1(KEYTYTWA,3-1),AL2(VCASORT+10-T702FFD)                        
PF16X    EQU   *                                                                
         DC    AL1(PF17X-*,17,0,(PF17X-PF17)/KEYLNQ,0)                          
         DC    CL3'   ',CL8'USAGE   ',CL8'DISPLAY'                              
PF17     DC    AL1(KEYTYGLB,L'TGAGY-1),AL2(TGAGY-TGD)                           
         DC    AL1(KEYTYGLB,L'TGCID-1),AL2(TGCID-TGD)                           
         DC    AL1(KEYTYCOM,0),AL2(0)                                           
         DC    AL1(KEYTYGLB,L'TGSSN-1),AL2(TGSSN-TGD)                           
         DC    AL1(KEYTYTWA,3-1),AL2(VCASORT+10-T702FFD)                        
PF17X    EQU   *                                                                
         DC    X'FF'                                                            
                                                                                
*              PF KEY TO GO BACK TO CAST LIST                                   
                                                                                
BACKTAB  DS    0H                                                               
         DC    AL1(BT21X-*,21,PFTRPROG+PFTINT,0,0)                              
         DC    CL3' ',CL8' ',CL8' '                                             
BT21X    EQU   *                                                                
         DC    X'FF'                                                            
                                                                                
*              TABLE OF STAFF CATEGORIES THAT MAY NOT UPDATE CAST               
                                                                                
STNOCHG  DC    A(TASTTYP8)                                                      
         DC    A(TASTTYPA)                                                      
         DC    A(TASTTYPB)                                                      
         DC    A(TASTTYPC)                                                      
         DC    A(TASTTYPD)                                                      
         DC    A(TASTTYPF)                                                      
         DC    X'FF'                                                            
                                                                                
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE TATRNTR                                                        
*              DSECT TO COVER COMMERCIAL VERSION TABLE ENTRY                    
                                                                                
COVRTBLD DSECT                                                                  
COVRCD   DS    X                                                                
COVRID   DS    CL12                                                             
COVRLNQ  EQU  *-COVRTBLD                                                        
         EJECT                                                                  
*              DSECT TO COVER TEMPORARY STORAGE                                 
                                                                                
TMPD     DSECT                                                                  
CAVRTBL  DS    XL251                                                            
COVRTBL  DS    XL((250*COVRLNQ)+1)                                              
                                                                                
INPUTBL  DS    XL251                                                            
SORTTBL  DS    XL251                                                            
TMPLNQ   EQU   *-TMPD                                                           
         EJECT                                                                  
* TAGENWORKD                                                                    
* TAGENFILE                                                                     
* TASYSDSECT                                                                    
* TAGENEQUS                                                                     
* TASYSEQUS                                                                     
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* DDPERVALD                                                                     
* FAGETTXTD                                                                     
         PRINT OFF                                                              
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TAGENEQUS                                                      
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE FAGETTXTD                                                      
         PRINT ON                                                               
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCRFDD                                                       
         EJECT                                                                  
*              LOCAL VARIABLES                                                  
         DS    100X                                                             
                                                                                
LISTSTAT DS    X                                                                
FROMLIST EQU   X'80'             ACCESSED FROM CAST/LIST                        
DONEVCAS EQU   X'40'             DONE WITH THIS VCAST                           
                                                                                
MUSICIAN DS    C                 IS CAST MUSICIAN?                              
                                                                                
SVCOKEY  DS    XL(L'KEY)         SAVED COMMERCIAL KEY                           
SVWID    DS    CL18              SAVED WEB APPLICATION ID                       
                                                                                
LSTDVER  DS    X                 LAST DISPLAYED VERSION                         
                                                                                
SVENTRY  DS    F                 SAVED ADDRESS OF SCANNER BLOCK ENTRY           
SVENCNT  DS    F                 SAVED NUMBER OF ENTRIES IN SCAN BLOCK          
RANGEH   DS    XL8               FIELD FOR FAKING OUT SCANNER                   
RANGE    DS    XL7               INTO UNDERSTANDING RANGES                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007TAGENFD   05/29/15'                                      
         END                                                                    
