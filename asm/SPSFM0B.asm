*          DATA SET SPSFM0B    AT LEVEL 030 AS OF 05/01/02                      
*PHASE T2170BA                                                                  
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE        T2170B - SIR SCHEME RECORD MAINTENANCE                *         
*                                                                     *         
*  CALLED FROM  GENCON VIA T21700 (SFM CONTROLLER)                    *         
*                                                                     *         
*  COMMENTS     SUPPORTS ADD, DISPLAY, DELETE, RESTORE, CHANGE, LIST, *         
*               AND REPORT                                            *         
*                                                                     *         
*  INPUTS       SCREEN T217BB (MAINTENANCE)                           *         
*               SCREEN T217AB (LIST)                                  *         
*               SCREEN T217CB (REPORT)                                *         
*                                                                     *         
*  OUTPUTS      UPDATED SCHEME RECORDS                                *         
*                                                                     *         
*  REGISTERS    R0 -- WORK                                            *         
*               R1 -- WORK                                            *         
*               R2 -- WORK                                            *         
*               R3 -- WORK                                            *         
*               R4 -- WORK                                            *         
*               R5 -- WORK                                            *         
*               R6 -- WORK                                            *         
*               R7 -- SECOND BASE                                     *         
*               R8 -- PRINT REPORT                                    *         
*               R9 -- SYSD                                            *         
*               RA -- TWA                                             *         
*               RB -- FIRST BASE                                      *         
*               RC -- GEND                                            *         
*               RD -- SYSTEM                                          *         
*               RE -- SYSTEM                                          *         
*               RF -- SYSTEM                                          *         
*                                                                     *         
*  I/O AREAS    IO1 - SCHEME RECORD                                   *         
*               IO2 - MISC.                                           *         
*                                                                     *         
***********************************************************************         
         TITLE 'T2170B - SIR SCHEME DEFINITION RECORDS'                         
T2170B   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T2170B                                                         
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
*                                                                               
         USING T2170B,RB,R7                                                     
         LA    R7,2048(RB)                                                      
         LA    R7,2048(R7)                                                      
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LR                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,PRINTREP       PRINT REPORT                                 
         BE    PR                                                               
         CLI   MODE,XRECPUT                                                     
         BE    DR                                                               
         CLI   MODE,XRECADD                                                     
         BE    DR                                                               
*                                                                               
         CLI   MODE,RECDEL         DELETE NOT ALLOWED                           
         BNE   *+16                                                             
         LA    R2,CONACTH                                                       
         MVI   ERROR,INVACT                                                     
         B     TRAPERR                                                          
*                                                                               
         CLI   MODE,RECREST        RESTORE NOT ALLOWED EITHER                   
         BNE   *+16                                                             
         LA    R2,CONACTH                                                       
         MVI   ERROR,INVACT                                                     
         B     TRAPERR                                                          
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
* VALIDATE KEY ROUTINE                                                          
*                                                                               
VK       LA    R6,SVKEY                                                         
         USING SIRKEY,R6                                                        
         XC    SVKEY,SVKEY                                                      
         MVI   SIRKTYPE,SIRKTYPQ   SIR RECORD TYPE                              
*                                                                               
         LA    R2,=X'0900000000010000E3'                                        
         GOTO1 VALIMED             FAKE THE MEDIA FIELD TO 'T'                  
         MVC   SIRKAM,BAGYMD                                                    
*                                                                               
         CLI   ACTNUM,ACTREP       SCHEME ABSENT ON REPORT SCREEN               
         BE    VKX                                                              
*                                                                               
         LA    R2,SIRSCHH          SCHEME FIELD                                 
         CLI   5(R2),0             TEST SCHEME GIVEN                            
         BNE   VK10                YES                                          
*                                                                               
         CLI   ACTNUM,ACTLIST      TEST ACTION LIST                             
         BE    VKX                 YES - OPTIONAL                               
         MVI   ERROR,MISSING       REQUIRED FIELD                               
         B     TRAPERR                                                          
*                                                                               
VK10     CLC   =C'ALL',8(R2)       TEST SCHEME FOR ENTIRE AGENCY                
         BE    VKX                                                              
         OC    8(3,R2),=C'   '                                                  
         GOTO1 CLPACK,DMCB,8(R2),SIRKCODE                                       
         CLI   DMCB,0              TEST VALID SCHEME CODE                       
         BE    VKX                 YES                                          
         MVI   ERROR,INVSCH                                                     
         B     TRAPERR                                                          
*                                                                               
VKX      MVC   SCHEME,SIRKCODE     HANG ON TO SCHEME CODE                       
         MVC   KEY,SVKEY                                                        
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
* VALIDATE RECORD                                                               
*                                                                               
VR       LA    R2,SIRNAMEH         NAME IS REQUIRED                             
         CLI   5(R2),0                                                          
         BNE   *+12                                                             
         MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
*                                                                               
         MVI   ELCODE,ESNCODEQ     NAME ELEMENT                                 
         GOTO1 REMELEM                                                          
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING ESNELEM,R6                                                       
         MVI   ESNCODE,ESNCODEQ                                                 
         MVI   ESNLEN,ESNLENEQ                                                  
         MVC   ESNNAME,8(R2)                                                    
         GOTO1 ADDELEM                                                          
         DROP  R6                                                               
*                                                                               
         MVI   ELCODE,ERSCODEQ     DEFAULT RATINGS SOURCE ELEMENT               
         GOTO1 REMELEM                                                          
*                                                                               
         LA    R2,SIRSRCH          DEFAULT RATINGS SOURCE                       
         CLI   5(R2),0                                                          
         BE    VR3                 NOT REQUIRED                                 
*                                                                               
         GOTO1 VALISRC             TEST SOURCE IS VALID                         
         XC    ELEM,ELEM           YES                                          
         LA    R6,ELEM                                                          
         USING ERSELEM,R6                                                       
         MVI   ERSCODE,ERSCODEQ                                                 
         MVI   ERSLEN,ERSLENEQ                                                  
         MVC   ERSSRC,BKVALSRC                                                  
         GOTO1 ADDELEM                                                          
         DROP  R6                                                               
*                                                                               
VR3      MVI   ELCODE,ECLCODEQ     CLIENT ELEMENT                               
         GOTO1 REMELEM                                                          
*                                                                               
         LA    R2,SIROPTSH         OPTIONS FIELD                                
         CLI   5(R2),0                                                          
         BE    VR5                 NOT REQUIRED                                 
*                                                                               
         L     R3,AIO2             AIO2=AREA FOR SCANNER BLOCKS                 
         XC    0(256,R3),0(R3)     R3-SCANNER BLOCK AREA                        
         GOTO1 SCANNER,DMCB,(R2),(R3)                                           
         ZIC   R0,DMCB+4           NUMBER OF ENTRIES                            
         LTR   R0,R0                                                            
         BNZ   *+12                                                             
         MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
*                                                                               
         SR    R6,R6               OPTION NUMBER                                
VR4      LA    R6,1(R6)            INCREMENT OPTION NUMBER                      
         ZIC   R1,0(R3)            LENGTH OF KEYWORD                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R3),=C'CLIENT' CLIENT=                                      
         BNE   SCANERR             NO OTHER VALID OPTIONS                       
*                                                                               
         MVI   ERROR,INVCLI                                                     
         CLI   1(R3),3                                                          
         BH    TRAPERR                                                          
         CLI   1(R3),2                                                          
         BL    TRAPERR                                                          
         GOTO1 CLPACK,DMCB,22(R3),HALF                                          
         CLI   0(R1),0                                                          
         BNE   TRAPERR             INVALID CLIENT CODE                          
         MVC   SAVEKEY,KEY         SAVE KEY                                     
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),HALF                                                    
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     WAS CLIENT KEY FOUND?                        
         BNE   TRAPERR                                                          
         MVC   KEY,SAVEKEY         YES -- RESTORE KEY                           
*                                                                               
         XC    ELEM,ELEM           BUILD CLIENT ELEMENT                         
         LA    R5,ELEM                                                          
         USING ECLELEM,R5                                                       
         MVI   ECLCODE,ECLCODEQ                                                 
         MVI   ECLLEN,ECLLENEQ                                                  
         MVC   ECLCLT,HALF         CLIENT CODE (PACKED)                         
         GOTO1 ADDELEM                                                          
         DROP  R5                                                               
*                                                                               
         LA    R3,32(R3)           BUMP TO NEXT ENTRY                           
         CLI   0(R3),0             ANY MORE OPTIONS?                            
         BNE   VR4                 YES                                          
*                                                                               
VR5      LA    R2,SIRDAYH                                                       
         MVI   ELCODE,EDCCODEQ     DAYPART CODES/NAMES ELEMENT                  
*                                                                               
VR10     XC    MYWORK,MYWORK       SAVE OLD CODES HERE                          
         MVI   MYWORK+63,X'FF'     STOP BYTE                                    
*                                                                               
         L     R6,AIO                                                           
         BAS   RE,GETEL                                                         
         BNE   VR30                                                             
*                                                                               
         SR    R0,R0                                                            
         ZIC   R1,1(R6)                                                         
         SH    R1,=H'2'                                                         
         D     R0,=F'8'            R1 = NO. OF CODES                            
         LA    R6,2(R6)                                                         
*                                                                               
         LA    R3,MYWORK                                                        
VR20     MVC   0(1,R3),0(R6)                                                    
         LA    R3,1(R3)                                                         
         LA    R6,8(R6)                                                         
         BCT   R1,VR20                                                          
         GOTO1 REMELEM                                                          
*                                                                               
VR30     LA    R5,3                3 TWA INPUT LINES                            
         L     RE,AIO2             AIO2=AREA FOR SCANNER BLOCKS                 
         LA    RF,2000                                                          
         XCEF                                                                   
*                                  R2-SCREEN FIELDS                             
         SR    R4,R4               R3-SCANNER BLOCK AREA                        
         L     R3,AIO2             R4-TOTAL NUM OF BLOCKS                       
         B     VR50                R5-BCT LIMIT 3 TWA FIELDS                    
*                                                                               
VR40     ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
VR50     CLI   5(R2),0             IS TWA FIELD EMPTY                           
         BE    VR100                                                            
         GOTO1 SCANNER,DMCB,(R2),(R3)                                           
*                                                                               
* CHECK SCANNER OUTPUT                                                          
* DISPLAY ROUTINE SENSITIVE TO BLANKS                                           
*                                                                               
         LA    R6,1                KEEP COUNT OF POSITION IN OUTBLOCK           
VR60     CLI   0(R3),1             L'1ST FIELD=1                                
         BNE   SCANERR                                                          
         CLI   1(R3),7             L'2ND FIELD=MAX7                             
         BH    SCANERR                                                          
         CLI   1(R3),1                                                          
         BL    SCANERR                                                          
         CLI   12(R3),C'A'         TEST 1ST FIELD=ALPHANUMERIC                  
         BL    SCANERR             NO                                           
         CLI   ELCODE,EDCCODEQ     TEST WE'RE DOING DAYPARTS                    
         BE    *+12                YES -- IT'S OK                               
         CLI   12(R3),C'Z'         PROGRAM TYPES ARE ALPHABETIC ONLY            
         BH    SCANERR                                                          
         CLI   22(R3),C' '         1ST BYTE 2ND FIELD=C' '                      
         BE    SCANERR                                                          
*                                                                               
         LA    RF,6                CHECK FIELD 2 FOR IMBEDDED BLANKS            
         LA    RE,28(R3)           RE - LAST BYTE OF FIELD                      
VR70     CLI   0(RE),C' '                                                       
         BNE   *+14                                                             
         BCTR  RE,0                                                             
         BCT   RF,VR70                                                          
         B     VR90                                                             
*                                                                               
         BCTR  RE,0                                                             
         BCTR  RF,0                                                             
VR80     CLI   0(RE),C' '                                                       
         BE    SCANERR                                                          
         BCTR  RE,0                                                             
         BCT   RF,VR80                                                          
*                                                                               
VR90     LA    R6,1(R6)            CHECK NEXT BLOCK                             
         LA    R4,1(R4)                                                         
         LA    R3,32(R3)                                                        
         OC    0(2,R3),0(R3)                                                    
         BNZ   VR60                                                             
*                                                                               
VR100    BCT   R5,VR40                                                          
         LTR   R4,R4                                                            
         BNZ   VR110                                                            
*                                                                               
         LA    R2,SIRDAYH          POINT TO FIRST FIELD                         
         MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
*                                                                               
VR110    CH    R4,=H'1'                                                         
         BE    VR130                                                            
*                                                                               
         L     R3,AIO2                                                          
VR120    BAS   RE,DUPCHK                                                        
         LA    R3,32(R3)                                                        
         OC    0(2,R3),0(R3)                                                    
         BNZ   VR120                                                            
*                                                                               
VR130    L     R3,AIO2             RESET R3 TO BEG OF BLOCK AREA                
         GOTO1 XSORT,DMCB,(0,(R3)),(R4),32,1,12                                 
         LA    R6,ELEM                                                          
         XC    ELEM,ELEM                                                        
         MVC   ELEM(1),ELCODE                                                   
         LR    R1,R4               GET LENGTH OF ELEM                           
         MH    R1,=H'8'                                                         
         LA    R1,2(R1)            ADD CODE/L' BYTES                            
         STC   R1,ELEM+1                                                        
         LA    R5,ELEM+2                                                        
*                                                                               
VR140    MVC   0(1,R5),12(R3)      CODE                                         
         LA    R1,MYWORK                                                        
*                                                                               
VR150    CLI   0(R1),X'FF'         TEST END OF SAVED LIST                       
         BE    VR160               YES                                          
         CLC   0(1,R1),0(R5)       TEST THIS IS THE CODE                        
         BE    *+12                                                             
         LA    R1,1(R1)            NO, TRY NEXT IN LIST                         
         B     VR150                                                            
         MVI   0(R1),0             MARK AS FOUND                                
*                                                                               
VR160    MVC   1(7,R5),22(R3)      NAME                                         
         LA    R5,8(R5)                                                         
         LA    R3,32(R3)                                                        
         BCT   R4,VR140                                                         
*                                                                               
         LA    R1,MYWORK                                                        
VR170    CLI   0(R1),X'FF'         TEST END OF SAVED LIST                       
         BE    VR180               YES - THERE ARE NO DELETED CODES             
         CLI   0(R1),0                                                          
         BNE   FEWCODES            THERE IS A DELETED CODE                      
         LA    R1,1(R1)                                                         
         B     VR170                                                            
*                                                                               
VR180    GOTO1 ADDELEM                                                          
*                                                                               
         CLI   ELCODE,EPCCODEQ     PROGTYP CODES/NAMES                          
         BE    VR190                                                            
         MVI   ELCODE,EPCCODEQ                                                  
         LA    R2,SIRPRGH                                                       
         CLI   5(R2),0             PROGTYP ELEM OPTIONAL                        
         BNE   VR10                                                             
         GOTO1 REMELEM                                                          
*                                                                               
VR190    LA    R2,SIRPERH          PERIOD NAMES                                 
         CLI   5(R2),0                                                          
         BNE   *+12                                                             
         MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,EPNCODEQ                                                  
         BAS   RE,GETEL                                                         
         BNE   VR200                                                            
         MVC   SVPERLEN,1(R6)      SAVE LENGTH OF ELEMENT                       
         GOTO1 REMELEM                                                          
*                                                                               
VR200    XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         MVI   0(R6),EPNCODEQ                                                   
         LA    R1,1                PERIOD NUMBER                                
         LA    R6,2(R6)                                                         
         LA    R3,SIRPERLH         A(LAST PERIOD)                               
*                                                                               
VR210    OC    8(4,R2),=C'    '    PAD WITH BLANKS                              
         CLI   5(R2),1             MUST BE GREATER THAN ONE CHARACTER           
         BE    INVPER                                                           
*                                                                               
         ZIC   R0,5(R2)            TEST NAME ALPHANUMERIC                       
         LA    RF,8(R2)                                                         
VR215    CLI   0(RF),C'A'                                                       
         BL    INVPER                                                           
         LA    RF,1(RF)                                                         
         BCT   R0,VR215                                                         
*                                                                               
         STC   R1,0(R6)                                                         
         MVC   1(4,R6),8(R2)       PERIOD NAME                                  
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         ZIC   R0,0(R2)            BUMP TO NEXT PERIOD NAME                     
         AR    R2,R0                                                            
         CR    R2,R3               TEST ALL PERIOD FIELDS USED                  
         BH    VR220                                                            
*                                                                               
         CLI   5(R2),0             TEST ANY MORE PERIODS                        
         BE    VR220                                                            
         LA    R1,1(R1)            INCREMENT PERIOD NUMBER                      
         LA    R6,5(R6)                                                         
         B     VR210                                                            
*                                                                               
VR220    MH    R1,=H'5'            COMPUTE LENGTH OF ELEMENT                    
         LA    R1,2(R1)                                                         
         LA    R6,ELEM                                                          
         STC   R1,1(R6)                                                         
         ZIC   R3,SVPERLEN         FORMER ELEMENT LENGTH                        
         CR    R1,R3                                                            
         BL    FEWPERDS                                                         
*                                                                               
         LA    R6,2(R6)            A(FIRST NAME)                                
VR230    LA    R5,5(R6)            A(NEXT NAME)                                 
         CLI   0(R5),0             TEST END OF NAMES                            
         BE    VR250                                                            
*                                                                               
VR240    CLC   1(4,R6),1(R5)                                                    
         BE    DUPPERDS                                                         
         LA    R5,5(R5)                                                         
         CLI   0(R5),0             TEST END OF NAMES                            
         BNE   VR240                                                            
         LA    R6,5(R6)                                                         
         B     VR230                                                            
*                                                                               
VR250    GOTO1 ADDELEM                                                          
*                                                                               
         MVI   ELCODE,EDMCODEQ     DEMO MENU NAME                               
         GOTO1 REMELEM                                                          
*                                                                               
         LA    R2,SIRDEMH          DEMO MENU                                    
         CLI   5(R2),0                                                          
         BE    VR260               NOT GIVEN                                    
*                                                                               
         MVC   SAVEKEY,KEY         SAVE KEY                                     
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING DMNRECD,R6                                                       
         MVC   DMNKTYP,=X'0D26'    DEMO MENU RECORD TYPE                        
         MVC   DMNKAGMD,BAGYMD     A/M                                          
         MVC   DMNKCODE,8(R2)      DEMO MENU NAME                               
         OC    DMNKCODE,=C'    '   PAD WITH BLANKS                              
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     TEST NAME IS VALID                           
         BNE   INVMENU                                                          
         DROP  R6                                                               
*                                                                               
         XC    ELEM,ELEM           CREATE THE ELEMENT                           
         LA    R6,ELEM                                                          
         USING EDMELEM,R6                                                       
         MVI   EDMCODE,EDMCODEQ                                                 
         MVI   EDMLEN,EDMLENEQ                                                  
         MVC   EDMMENU,8(R2)                                                    
         OC    EDMMENU,=C'    '                                                 
         GOTO1 ADDELEM                                                          
         MVC   KEY,SAVEKEY         RESTORE KEY                                  
         DROP  R6                                                               
*                                                                               
VR260    LA    R2,SIRYRH           YEAR IS REQUIRED                             
         XC    WORK,WORK                                                        
         GOTO1 PERVAL,DMCB,(SIRYRH+5,SIRYR),('PVINSGLO',WORK)                   
*                                                                               
         CLI   DMCB+4,PVRCOK       YEAR WAS OK?                                 
         BE    *+12                                                             
         MVI   ERROR,INVALID                                                    
         B     TRAPERR             NO                                           
*                                                                               
         MVI   ELCODE,EDYCODEQ     YEAR ELEMENT                                 
         GOTO1 REMELEM                                                          
         XC    ELEM,ELEM           CREATE THE ELEMENT                           
         LA    R6,ELEM                                                          
         USING EDYELEM,R6                                                       
         MVI   EDYCODE,EDYCODEQ                                                 
         MVI   EDYLEN,EDYLENEQ                                                  
         LA    RF,WORK             PERVAL OUTPUT AREA                           
         MVC   EDYYEAR,PVALBSTA-PERVALD(RF)  BINARY YEAR                        
         GOTO1 ADDELEM                                                          
         DROP  R6                                                               
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
* DISPLAY RECORD                                                                
*                                                                               
DR       LA    R2,SIRNAMEH         CLEAR SCREEN                                 
*                                                                               
DR10     TM    1(R2),X'20'         IS IT A PROTECTED FIELD                      
         BO    DR20                                                             
         ZIC   RF,0(R2)            RF HAS LENGTH OF FIELD                       
         SH    RF,=H'9'            SUBTRACT HEADER LENGTH +1                    
         TM    1(R2),X'02'         TEST HEADER EXTENDED                         
         BZ    *+8                                                              
         SH    RF,=H'8'                                                         
*                                                                               
         EX    RF,*+8              PAD WITH BLANKS                              
         B     *+10                                                             
         OC    8(0,R2),=80X'40'                                                 
         EX    RF,*+8              TEST FIELD EMPTY                             
         B     *+10                                                             
         CLC   8(0,R2),=80X'40'                                                 
         BE    DR20                YES                                          
         EX    RF,*+8              NO - CLEAR FIELD                             
         B     *+10                                                             
         XC    8(0,R2),8(R2)                                                    
         OI    6(R2),X'80'         XMIT                                         
*                                                                               
DR20     ZIC   R0,0(R2)                                                         
         AR    R2,R0               BUMP                                         
         CLI   0(R2),0             TEST END OF SCREEN                           
         BNE   DR10                                                             
*                                                                               
         CLI   ACTNUM,ACTSEL       TEST SELECT FROM LIST                        
         BNE   DR35                                                             
         LA    R3,LISTDIR          LIST SCREEN INFO                             
         CLI   SELLISTN,0          TEST FIRST ON LIST SCREEN                    
         BE    DR30                                                             
         ZIC   R0,SELLISTN         RELATIVE LINE NUMBER                         
         LA    R3,6(R3)            NEXT LIST ENTRY                              
         BCT   R0,*-4                                                           
DR30     CLI   0(R3),C'D'          TEST DELETE ACTION                           
         BE    DRX                 DON'T DISPLAY RECORD ON DELETE               
*                                                                               
DR35     L     R6,AIO                                                           
         MVI   ELCODE,ESNCODEQ     SCHEME NAME                                  
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING ESNELEM,R6                                                       
         MVC   SIRNAME,ESNNAME                                                  
         OI    SIRNAMEH+6,X'80'                                                 
         DROP  R6                                                               
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,ERSCODEQ     DEFAULT RATINGS SOURCE                       
         BAS   RE,GETEL                                                         
         BNE   DR44                                                             
*                                                                               
         USING ERSELEM,R6                                                       
         MVC   SIRSRC,=C'ARB'                                                   
         CLI   ERSSRC,C'A'                                                      
         BE    DR42                                                             
         MVC   SIRSRC,=C'BBM'                                                   
         CLI   ERSSRC,C'B'                                                      
         BE    DR42                                                             
         MVC   SIRSRC,=C'CSI'                                                   
         CLI   ERSSRC,C'C'                                                      
         BE    DR42                                                             
         MVC   SIRSRC,=C'NSI'                                                   
         CLI   ERSSRC,C'N'                                                      
         BE    DR42                                                             
         DC    H'0'                                                             
*                                                                               
******   CLI   SVAPROF+7,C'C'      TEST CANADIAN AGENCY                         
******   BNE   DR40                NO                                           
******   CLI   ERSSRC,C'A'         TEST ARBITRON                                
******   BNE   *+14                NO                                           
******   MVC   SIRSRC,=C'BBM'      IT'S CANADIAN ARBITRON                       
******   B     DR42                                                             
******   MVC   SIRSRC,=C'CSI'      IT'S CANADIAN NIELSON                        
******   B     DR42                                                             
*                                                                               
*DR40    CLI   ERSSRC,C'A'         TEST U.S. ARBITRON                           
*****    BNE   *+14                                                             
******   MVC   SIRSRC,=C'ARB'      IT'S U.S. ARBITRON                           
******   B     DR42                                                             
******   MVC   SIRSRC,=C'NSI'      IT'S U.S. NIELSON                            
*                                                                               
DR42     OI    SIRSRCH+6,X'80'     XMIT                                         
         DROP  R6                                                               
*                                                                               
DR44     L     R6,AIO                                                           
         MVI   ELCODE,ECLCODEQ     CLIENT CODE                                  
         BAS   RE,GETEL                                                         
         BNE   DR45                                                             
         USING ECLELEM,R6                                                       
         MVC   SIROPTS(7),=C'CLIENT='                                           
         GOTO1 CLUNPK,DMCB,ECLCLT,SIROPTS+7                                     
         OI    SIROPTSH+6,X'80'                                                 
         DROP  R6                                                               
*                                                                               
DR45     L     R6,AIO                                                           
         MVI   ELCODE,EDCCODEQ     DAYPART CODES/NAMES                          
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R2,SIRDAYH          R2 - SCREEN FIELD HEADER                     
DR50     LA    R5,BLOCK            R5 - BLOCK                                   
         XC    BLOCK(256),BLOCK                                                 
         SR    R0,R0                                                            
         ZIC   R1,1(R6)                                                         
         SH    R1,=H'2'                                                         
         D     R0,=F'8'            R1 - NUM OF CD/NMS IN ELEM                   
         LA    R6,2(R6)            R6 - CD/NM                                   
*                                                                               
DR60     MVC   0(1,R5),0(R6)       CODE                                         
         MVC   2(7,R5),1(R6)       NAME                                         
         MVI   1(R5),C'='                                                       
DR70     CLI   0(R5),C' '          GET END OF STRING FOR COMMA                  
         BNH   *+12                                                             
         LA    R5,1(R5)                                                         
         B     DR70                                                             
*                                                                               
         MVI   0(R5),C','          GOT IT / MOVE IN COMMA                       
         LA    R5,1(R5)                                                         
         LA    R6,8(R6)                                                         
         BCT   R1,DR60                                                          
         LA    R5,BLOCK            SET R5 - START OF BLOCK                      
         BAS   RE,SPLTLIN          DIVIDE OUTPUT AND DISPLAY                    
*                                                                               
         CLI   ELCODE,EPCCODEQ     PROGTYP CODES & NAMES                        
         BE    DR80                                                             
         MVI   ELCODE,EPCCODEQ                                                  
         L     R6,AIO                                                           
         BAS   RE,GETEL                                                         
         BNE   DR80                FIELD IS OPTIONAL                            
         LA    R2,SIRPRGH                                                       
         B     DR50                                                             
*                                                                               
DR80     L     R6,AIO              PERIOD NAMES                                 
         MVI   ELCODE,EPNCODEQ                                                  
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         SR    R0,R0                                                            
         ZIC   R1,1(R6)                                                         
         SH    R1,=H'2'                                                         
         D     R0,=F'5'            R1 = NO. OF CODES                            
*                                                                               
         LA    R6,2(R6)                                                         
         LA    R2,SIRPERH          A(FIRST FIELD)                               
         LA    R3,SIRPERLH         A(LAST FIELD)                                
*                                                                               
DR90     MVC   8(4,R2),1(R6)                                                    
         OI    6(R2),X'80'         XMIT PERIOD NAME                             
         LA    R6,5(R6)            NEXT PERIOD NAME                             
         ZIC   R0,0(R2)                                                         
         AR    R2,R0               BUMP TO PERIOD NUMBER FIELD                  
         ZIC   R0,0(R2)                                                         
         AR    R2,R0               BUMP TO PERIOD NAME FIELD                    
         BCT   R1,DR90             TEST END OF PERIODS                          
*                                                                               
         LA    R2,SIRDMNMH         A(FIRST DEMO NAME FIELD)                     
         LA    R3,SIRDLSTH         A(LAST DEMO NAME FIELD)                      
         L     R6,AIO                                                           
         MVI   ELCODE,EDMCODEQ     DEMO MENU                                    
         BAS   RE,GETEL                                                         
         BNE   DR110                                                            
*                                                                               
         USING EDMELEM,R6                                                       
         MVC   SIRDEM,EDMMENU                                                   
         OI    SIRDEMH+6,X'80'                                                  
         DROP  R6                                                               
*                                                                               
         LA    R2,SIRDEMH          MENU NAME FIELD                              
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING DMNRECD,R6                                                       
         MVC   DMNKTYP,=X'0D26'    DEMO MENU RECORD TYPE                        
         MVC   DMNKAGMD,BAGYMD     A/M                                          
         MVC   DMNKCODE,SIRDEM     DEMO MENU NAME                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     TEST NAME IS VALID                           
         BNE   INVMENU                                                          
         DROP  R6                                                               
*                                                                               
         LA    R2,SIRDMNMH         DISPLAY THE NAMES                            
         L     R6,AIO2                                                          
         ST    R6,AIO                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         MVI   ELCODE,X'05'        DEMO NAME ELEMENT                            
         BAS   RE,GETEL                                                         
         BNE   DR110               NO DEMOS IN LIST                             
*                                                                               
         USING DMNEL05,R6                                                       
DR100    MVC   8(L'SIRDMNM,R2),DMNRTG                                           
         OI    6(R2),X'80'         TRANSMIT                                     
         ZIC   R0,0(R2)            BUMP TO NEXT FIELD                           
         AR    R2,R0                                                            
         CR    R2,R3               TEST END OF NAME FIELDS                      
         BH    DR120               YES                                          
         BAS   RE,NEXTEL                                                        
         BE    DR100                                                            
*                                                                               
DR110    XC    8(L'SIRDMNM,R2),8(R2)                                            
         OI    6(R2),X'80'         CLEAR REMAINING NAME FIELDS                  
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         CR    R2,R3               TEST END OF NAME FIELDS                      
         BNH   DR110               NO                                           
         DROP  R6                                                               
*                                                                               
DR120    MVC   AIO,AIO1            RESTORE AIO                                  
         L     R6,AIO                                                           
         MVI   ELCODE,EDYCODEQ     DEFAULT YEAR                                 
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING EDYELEM,R6                                                       
         MVC   FULL(1),EDYYEAR                                                  
         MVC   FULL+1(2),=X'0101'  PRETEND IT'S JAN01                           
         GOTO1 DATCON,DMCB,(3,FULL),(20,DUB)                                    
         MVC   SIRYR,DUB+2         PRINTABLE YY                                 
         OI    SIRYRH+6,X'80'      XMIT                                         
         DROP  R6                                                               
*                                                                               
DRX      B     XIT                                                              
         EJECT                                                                  
* DIVIDE BLOCK OUTPUT AREA (MAX 78 BYTES) *                                     
*                                                                               
SPLTLIN  LA    R3,78(R5)           R5 - START OF BLOCK AREA                     
         CLI   0(R3),C','          R3 - END OF BLOCK AREA                       
         BNE   SPL10                                                            
         MVI   0(R3),C' '                                                       
         B     SPL20                                                            
SPL10    BCTR  R3,0                                                             
         CR    R5,R3               R3 MUST NOT BE LESS THEN R5                  
         BL    *+6                                                              
         DC    H'0'                                                             
         CLI   0(R3),C','                                                       
         BNE   SPL10                                                            
         MVI   0(R3),C' '                                                       
*                                                                               
SPL20    LR    RF,R3               R3 - END OF BLOCK AREA                       
         SR    RF,R5               R5 - BEG OF BLOCK AREA                       
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),0(R5)                                                    
         OI    6(R2),X'80'         XMIT                                         
*                                                                               
         LA    R5,1(R3)            SET R5 TO BEG OF NEXT BLOCK AREA             
         OC    0(7,R5),=7X'40'                                                  
         CLC   0(7,R5),=7X'40'                                                  
         BE    SPLX                NO MORE DATA                                 
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         B     SPLTLIN                                                          
*                                                                               
SPLX     BR    RE                                                               
         SPACE 5                                                                
DUPCHK   NTR1                      CHECK FOR DUPLICATE CODES/NAMES              
         LA    R4,32(R3)                                                        
DPK10    CLC   12(1,R3),12(R4)                                                  
         BE    DPK20                                                            
         CLC   22(10,R3),22(R4)                                                 
         BE    DPK20                                                            
         LA    R4,32(R4)                                                        
         OC    0(2,R4),0(R4)                                                    
         BNZ   DPK10                                                            
         B     XIT                                                              
*                                                                               
DPK20    XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(21),=C'DUPLICATE INPUT ERROR'                            
         GOTO1 ERREX2                                                           
         EJECT                                                                  
* DISPLAY KEY                                                                   
*                                                                               
DK       L     R6,AIO              RECORD SELECTED                              
         USING SIRREC,R6                                                        
*                                                                               
         OI    SIRSCHH+6,X'80'     XMIT                                         
         OC    SIRKCODE,SIRKCODE   TEST SCHEME 'ALL'                            
         BNZ   *+14                                                             
         MVC   SIRSCH,=C'ALL'                                                   
         B     DKX                                                              
         GOTO1 CLUNPK,DMCB,SIRKCODE,SIRSCH                                      
*                                                                               
DKX      B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
* LIST RECORDS                                                                  
*                                                                               
LR       LA    R4,KEY                                                           
         USING SIRKEY,R4                                                        
*                                                                               
         OC    KEY(13),KEY         TEST FIRST TIME THROUGH                      
         BNZ   LR10                                                             
         MVI   SIRKTYPE,SIRKTYPQ   RECORD TYPE                                  
         MVC   SIRKAM,BAGYMD       AGY/MED                                      
         MVC   SIRKCODE,SCHEME     SCHEME CODE                                  
         MVC   SAVEKEY,KEY                                                      
*                                                                               
LR10     MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                FIRST RECORD                                 
         B     LR30                                                             
*                                                                               
LR20     LA    R4,KEY                                                           
         SR    R1,R1                                                            
         ICM   R1,3,SIRKCODE                                                    
         LA    R1,1(R1)            LOOK FOR NEXT SCHEME KEY                     
         STCM  R1,3,SIRKCODE                                                    
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
*                                                                               
LR30     CLC   KEY(2),SAVEKEY      TEST SAME TYPE/AGY/MED                       
         BNE   LRX                                                              
         OC    KEY+4(9),KEY+4      TEST SCHEME RECORD                           
         BNZ   LR20                                                             
*                                                                               
         XC    LISTAR,LISTAR       FILL IN LIST LINE                            
         OC    SIRKCODE,SIRKCODE   TEST SCHEME 'ALL'                            
         BNZ   *+14                                                             
         MVC   LSTSCH,=C'ALL'                                                   
         B     LR40                                                             
         GOTO1 CLUNPK,DMCB,SIRKCODE,LSTSCH   SCHEME                             
*                                                                               
LR40     MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         MVI   ELCODE,ESNCODEQ     NAME ELEMENT                                 
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
         USING ESNELEM,R6                                                       
         MVC   LSTNAME,ESNNAME     NAME                                         
         DROP  R6                                                               
*                                                                               
         GOTO1 LISTMON             DISPLAY LINE                                 
         B     LR20                                                             
*                                                                               
LRX      B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
* PRINT SCHEME REPORT                                                           
*                                                                               
PR       L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
*                                                                               
         LA    R1,HEDSPECS         SET UP HEADHOOK AND SPECS                    
         ST    R1,SPECS                                                         
         LA    R1,HOOK                                                          
         ST    R1,HEADHOOK                                                      
         MVI   HDHOOKOK,C'Y'                                                    
*                                                                               
         XC    SAVEKEY,SAVEKEY                                                  
         LA    R4,KEY              SET UP REGISTER FOR KEY IN DIRECTORY         
         USING SIRKEY,R4                                                        
*                                                                               
         MVI   SIRKTYPE,SIRKTYPQ   LOAD TYPE OF RECORD LOOKING FOR              
         MVC   SIRKAM,BAGYMD       '0C' A/M INFORMATION                         
         MVC   SAVEKEY,KEY                                                      
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         B     PR30                                                             
*                                                                               
PR20     LA    R4,KEY                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 SEQ                                                              
*                                                                               
PR30     CLC   KEY(2),SAVEKEY      CHECK CORRECT TYPE                           
         BNE   PR110                                                            
         OC    KEY+4(9),KEY+4      CHECK IF SCHEME TYPE                         
         BNZ   PR20                                                             
*                                                                               
         MVI   RECFOUND,C'Y'       YES, FOUND SCHEME REC                        
         MVC   PRSCH,=C'SCHEME'    PRINT SCHEME LINE                            
         OC    SIRKCODE,SIRKCODE                                                
         BNZ   *+14                                                             
         MVC   PRSCHM,=C'ALL'                                                   
         B     PR40                                                             
         GOTO1 CLUNPK,DMCB,SIRKCODE,PRSCHM                                      
*                                                                               
PR40     MVI   PRSKIP6,0                                                        
         L     R6,AIO                                                           
         USING ESNELEM,R6                                                       
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         MVI   ELCODE,ESNCODEQ                                                  
         BAS   RE,GETEL            GET THE NAME ELEMENT                         
         BE    *+6                                                              
         DC    H'0'                IF NO NAME ELEMENT, DIE                      
*                                                                               
         MVC   PRNM,=C'NAME'                                                    
         MVC   PRNAM,ESNNAME       ENTER NAME IN PRINT                          
*                                                                               
         MVC   PRSRC,=C'SOURCE'                                                 
         L     R6,AIO                                                           
         MVI   ELCODE,ERSCODEQ     DEFAULT RATINGS SOURCE                       
         BAS   RE,GETEL                                                         
         BE    *+14                                                             
         MVC   PRSRCN(4),=C'NONE'                                               
         B     PR55                                                             
*                                                                               
         USING ERSELEM,R6                                                       
         MVC   PRSRCN,=C'ARB'                                                   
         CLI   ERSSRC,C'A'                                                      
         BE    PR55                                                             
         MVC   PRSRCN,=C'BBM'                                                   
         CLI   ERSSRC,C'B'                                                      
         BE    PR55                                                             
         MVC   PRSRCN,=C'CSI'                                                   
         CLI   ERSSRC,C'C'                                                      
         BE    PR55                                                             
         MVC   PRSRCN,=C'NSI'                                                   
         CLI   ERSSRC,C'N'                                                      
         BE    PR55                                                             
         DC    H'0'                                                             
*                                                                               
******   CLI   SVAPROF+7,C'C'      TEST CANADIAN AGENCY                         
******   BNE   PR50                NO                                           
******   CLI   ERSSRC,C'A'         TEST ARBITRON                                
******   BNE   *+14                NO                                           
******   MVC   PRSRCN,=C'BBM'      IT'S CANADIAN ARBITRON                       
******   B     PR55                                                             
******   MVC   PRSRCN,=C'CSI'      IT'S CANADIAN NIELSON                        
******   B     PR55                                                             
*                                                                               
*PR50    CLI   ERSSRC,C'A'         TEST U.S. ARBITRON                           
******   BNE   *+14                                                             
******   MVC   PRSRCN,=C'ARB'      IT'S U.S. ARBITRON                           
******   B     PR55                                                             
******   MVC   PRSRCN,=C'NSI'      IT'S U.S. NIELSON                            
*                                                                               
PR55     GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         MVI   P1,0                                                             
         MVC   PROPT,=C'OPTIONS'   OPTIONS FIELD                                
         L     R6,AIO                                                           
         USING ECLELEM,R6                                                       
         MVI   ELCODE,ECLCODEQ                                                  
         BAS   RE,GETEL            GET THE CLIENT ELEMENT                       
         BE    *+14                                                             
         MVC   PROPTS(4),=C'NONE'                                               
         B     PR57                                                             
         MVC   PROPTS(7),=C'CLIENT='                                            
         GOTO1 CLUNPK,DMCB,ECLCLT,PROPTS+7                                      
         DROP  R6                                                               
*                                                                               
PR57     MVI   P3,0                                                             
         MVI   P4,0                                                             
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         L     R6,AIO                                                           
         USING EDCELEM,R6                                                       
         MVI   P1,0                                                             
         MVI   P2,0                                                             
         MVI   P3,0                                                             
         MVC   PRDAY,=C'DAYPART CODES AND NAMES'                                
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVI   ELCODE,EDCCODEQ                                                  
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                MUST BE DAYPART ELEMENTS                     
*                                                                               
         MVI   P1,0                                                             
         MVI   P2,0                                                             
         MVI   P3,0                                                             
*                                                                               
         LA    R3,PRSTART                                                       
         USING PRSTART,R3                                                       
         DROP  R4                                                               
*                                                                               
         XR    R5,R5                                                            
         ZIC   R4,1(R6)                                                         
         SH    R4,=H'2'            FIND THE LENGTH OF THE DATA                  
         SRDL  R4,32                                                            
         D     R4,=F'8'            NUMBER OF DATAE                              
*                                                                               
         LTR   R4,R4               IF REMAINDER, THEN INCORRECT DATAE           
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LR    RE,R5               SAVE NUMBER OF DAYPARTS                      
         LR    R4,R5               QUOTIENT INTO EVEN REG TO DIVIDE             
         C     R4,=F'5'            TEST DAYPARTS WILL FIT ON ONE ROW            
         BH    *+12                NO                                           
         LA    R5,1                YES - SET LOOP COUNTER TO MINIMUM            
         B     LOOP1                                                            
*                                                                               
         SRDL  R4,32                                                            
         D     R4,=F'5'            SET OUTER LOOP COUNTER                       
         LTR   R4,R4               TEST ANY REMAINDER                           
         BZ    LOOP1               NO                                           
         LA    R5,1(R5)            R5 = NUMBER OF ROWS                          
*                                                                               
LOOP1    LA    R3,PRDYC                                                         
         USING PRDYC,R3                                                         
         C     RE,=F'5'            TEST WE'RE ON LAST ROW                       
         BNL   *+10                NO                                           
         LR    R0,RE                                                            
         B     LOOP2                                                            
*                                                                               
         LA    R0,5                                                             
LOOP2    MVC   PRDYC,EDCDCODE                                                   
         MVI   PREQUAL,PREQUALQ                                                 
         MVC   PRDYN,EDCDNAME                                                   
         LA    R6,L'EDCDATA(R6)    BUMP THROUGH RECORD                          
         LA    R3,PRLEN(R3)        BUMP THROUGH PRINT LINE                      
         BCT   R0,LOOP2                                                         
*                                                                               
         LA    R3,L'PRSTART(R3)    GET TO END OF PRINT LINE                     
         S     RE,=F'5'                                                         
         BCT   R5,LOOP1                                                         
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
         DROP  R3,R6                                                            
*                                                                               
         MVI   P1,0                                                             
         MVI   P2,0                                                             
         MVI   P3,0                                                             
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         L     R6,AIO                                                           
         USING EPCELEM,R6                                                       
         MVI   P1,0                                                             
         MVI   P2,0                                                             
         MVI   P3,0                                                             
         MVC   PRPROG,=C'PROGRAM TYPES CODES AND NAMES'                         
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVI   P1,0                                                             
         MVI   P2,0                                                             
         MVI   P3,0                                                             
         MVI   P4,0                                                             
         MVI   ELCODE,EPCCODEQ                                                  
         BAS   RE,GETEL                                                         
         BNE   TH4                                                              
*                                                                               
         LA    R3,PRBEG                                                         
         USING PRBEG,R3                                                         
*                                                                               
         ZIC   R4,1(R6)                                                         
         SH    R4,=H'2'                                                         
         SRDL  R4,32                                                            
         D     R4,=F'8'            R5 = NUMBER OF PROGRAM TYPES                 
*                                                                               
         LTR   R4,R4                                                            
         BZ    *+6                                                              
         DC    H'0'                DIE IF DIVISION ISN'T EQUAL                  
*                                                                               
         LR    RE,R5               SAVE NUMBER OF PROGRAM TYPES                 
         LR    R4,R5                                                            
         C     R4,=F'5'                                                         
         BH    *+12                                                             
         LA    R5,1                SET LOOP COUNTERS                            
         B     LOOP3                                                            
*                                                                               
         SRDL  R4,32                                                            
         D     R4,=F'5'                                                         
         LTR   R4,R4                                                            
         BZ    LOOP3                                                            
         LA    R5,1(R5)                                                         
*                                                                               
LOOP3    LA    R3,PRTYC                                                         
         USING PRTYC,R3            BUMP THROUGH DATA                            
         C     RE,=F'5'                                                         
         BNL   *+10                                                             
         LR    R0,RE                                                            
         B     LOOP4                                                            
*                                                                               
         LA    R0,5                                                             
LOOP4    MVC   PRTYC,EPCDCODE                                                   
         MVI   PREQUL,PREQUALQ                                                  
         MVC   PRTYN,EPCDNAME                                                   
         LA    R6,L'EPCDATA(R6)                                                 
         LA    R3,PRLENT(R3)                                                    
         BCT   R0,LOOP4                                                         
*                                                                               
         LA    R3,L'PRBEG(R3)      PUSH TO NEXT PRINT LINE                      
         S     RE,=F'5'                                                         
         BCT   R5,LOOP3                                                         
         B     PR60                                                             
*                                                                               
TH4      LA    R3,PRBEG                                                         
         USING PRBEG,R3                                                         
         MVC   PRTYC(4),=C'NONE'                                                
*                                                                               
PR60     GOTO1 SPOOL,DMCB,(R8)                                                  
         DROP  R3                                                               
*                                                                               
         MVI   P1,0                                                             
         MVI   P2,0                                                             
         MVI   P3,0                                                             
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         MVI   P1,0                                                             
         L     R6,AIO                                                           
         LA    R3,PRORI                                                         
         USING PRORI,R3            EXTRACT PERIOD DATA                          
         MVC   PRPER1,=C'PERIOD NAMES'                                          
         USING EPNELEM,R6                                                       
         MVI   ELCODE,EPNCODEQ                                                  
         BAS   RE,GETEL                                                         
         BNE   PR70                                                             
*                                                                               
         XR    RE,RE                                                            
         XR    R5,R5                                                            
         LA    R4,2                                                             
         DROP  R3                                                               
*                                                                               
         LA    R3,PRORI                                                         
         USING PRORI,R3                                                         
LOOP5    LA    R3,PRNUM                                                         
         USING PRNUM,R3                                                         
         LA    R5,6                                                             
*                                                                               
LOOP6    LA    RE,1(RE)                                                         
         EDIT  (RE),(2,PRNUM),ALIGN=LEFT                                        
         MVC   PRPER,=C'    '                                                   
         LA    R3,PRPLEN(R3)                                                    
         BCT   R5,LOOP6                                                         
         LA    R3,L'PRORI(R3)                                                   
         BCT   R4,LOOP5                                                         
*                                                                               
         XR    R5,R5                                                            
         ZIC   R4,1(R6)            LENGTH OF ELEMENTS                           
         SH    R4,=H'2'                                                         
         SRDL  R4,32                                                            
         D     R4,=F'5'                                                         
         DROP  R3                                                               
*                                                                               
         LA    R3,PRORI                                                         
         USING PRORI,R3                                                         
LOOP7    LA    R3,PRNUM                                                         
         USING PRNUM,R3                                                         
         C     R5,=F'6'                                                         
         BH    *+14                                                             
         LR    R0,R5                                                            
         LA    R4,1                                                             
         B     LOOP8                                                            
*                                                                               
         LA    R0,6                                                             
         LA    R4,2                                                             
LOOP8    MVC   PRPER,EPNNAME                                                    
         LA    R6,L'EPNDATA(R6)                                                 
         LA    R3,PRPLEN(R3)                                                    
         BCT   R0,LOOP8                                                         
         LA    R3,L'PRORI(R3)                                                   
         S     R5,=F'6'                                                         
         BCT   R4,LOOP7                                                         
*                                                                               
PR70     GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         MVI   P1,0                                                             
         MVI   P2,0                                                             
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         L     R6,AIO                                                           
         USING EDMELEM,R6                                                       
         MVI   ELCODE,EDMCODEQ                                                  
         MVC   PRDEM,=C'DEMO MENU' INSERT DEMO MENU                             
         BAS   RE,GETEL                                                         
         BE    *+14                                                             
         MVC   PRFIL(4),=C'NONE'                                                
         B     PR80                                                             
*                                                                               
         LA    R3,PRORN                                                         
         USING PRORN,R3                                                         
         MVC   PRFIL,EDMMENU       DEMO MENU NAME                               
         DROP  R6                                                               
*                                                                               
         MVC   SAVEKEY,KEY         SAVE SCHEME KEY                              
         XC    KEY,KEY             BUILD THE KEY FOR DEMO REC                   
         LA    R6,KEY                                                           
         USING DMNRECD,R6                                                       
         MVC   DMNKTYP,=X'0D26'                                                 
         MVC   DMNKAGMD,BAGYMD                                                  
         MVC   DMNKCODE,PRFIL      DEMO MENU NAME                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                FIND RECORD                                  
         CLC   KEY(13),KEYSAVE     TEST CORRECT RECORD                          
         BE    TH3                                                              
TH7      MVC   PRFIL(32),=C'* WARNING *  DEMO MENU NOT FOUND'                   
         B     PR80                                                             
         DROP  R6                                                               
*                                                                               
TH3      L     R6,AIO2                                                          
         ST    R6,AIO              CHANGE OUTPUT AREA                           
         USING DMNEL05,R6                                                       
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         MVI   ELCODE,X'05'                                                     
         BAS   RE,GETEL            EXTRACT THE ELEMENTS                         
         BNE   TH7                                                              
         DROP  R3                                                               
*                                                                               
         LA    R5,3                3 ROWS OF DEMO NAMES                         
LOOP9    LA    R3,PRSTUFF          A(FIRST PRINT POSITION)                      
         USING PRSTUFF,R3                                                       
*                                                                               
         LA    R4,5                5 NAMES PER ROW                              
LOOP10   MVC   PRSTUFF,DMNRTG                                                   
         LA    R3,PRSTLEN(R3)                                                   
         BAS   RE,NEXTEL           FIND '05' ELEMENT                            
         BNE   PRNT                                                             
         BCT   R4,LOOP10                                                        
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
         LA    R3,40(R3)           END OF PRINT LINE                            
         BCT   R5,LOOP9                                                         
         DROP  R3                                                               
*                                                                               
PRNT     XC    KEY,KEY                                                          
         MVC   KEY,SAVEKEY                                                      
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
*                                                                               
PR80     GOTO1 SPOOL,DMCB,(R8)                                                  
         MVC   AIO,AIO1            RESTORE IO AREA                              
         L     R6,AIO                                                           
         USING EDYELEM,R6                                                       
         MVC   PRYR,=C'CURRENT YEAR'                                            
         MVI   ELCODE,EDYCODEQ                                                  
         MVI   P1,0                                                             
         BAS   RE,GETEL                                                         
         BNE   PR100                                                            
*                                                                               
         MVC   FULL(1),EDYYEAR                                                  
         MVC   FULL+1(2),=X'0101'  PRETEND IT'S JAN01                           
         GOTO1 DATCON,DMCB,(3,FULL),(20,DUB)                                    
         MVC   PRCYR,DUB+2         PRINTABLE YY                                 
*                                                                               
PR100    GOTO1 SPOOL,DMCB,(R8)                                                  
         MVI   FORCEHED,C'Y'                                                    
         B     PR20                NEXT RECORD ENTRY                            
*                                                                               
PR110    CLI   RECFOUND,C'Y'       REPORT HAS DATA IN IT                        
         BE    PRX                                                              
         MVI   HDHOOKOK,C'N'                                                    
         MVC   PRNAM(16),=C'NO RECORDS FOUND'                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
PRX      B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
HOOK     NTR1                      HEADLINE ROUTINES                            
*                                                                               
         CLI   HDHOOKOK,C'N'       TEST OK TO DO HEADHOOK                       
         BE    HOOKX                                                            
*                                                                               
         MVI   H3,0                SKIP LINES                                   
         MVI   H7,0                                                             
         MVI   H9,0                                                             
         MVI   H10,0                                                            
*                                                                               
         OC    ABOX,ABOX           TEST WE HAVE BOXES                           
         BZ    HOOKX                                                            
*                                                                               
         L     R4,ABOX             ADDRESS OF BOX DSECT                         
         USING BOXD,R4                                                          
         MVC   BOXROWS,SPACES                                                   
         MVI   BOXROWS+18,C'T'                                                  
         MVI   BOXROWS+27,C'M'                                                  
         MVI   BOXROWS+37,C'M'                                                  
         MVI   BOXROWS+55,C'B'                                                  
         MVC   BOXCOLS,SPACES                                                   
         MVI   BOXCOLS+25,C'L'                                                  
         MVI   BOXCOLS+107,C'R'                                                 
         MVI   BOXWT,1                                                          
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXINIT,0                                                        
         MVI   BOXOFF,0                                                         
         DROP  R4                                                               
*                                                                               
HOOKX    B     XIT                                                              
         SPACE 5                                                                
HEDSPECS SSPEC H1,2,C'MEDIA     SPOT T.V.'                                      
         SSPEC H2,2,REQUESTOR                                                   
         SSPEC H1,52,C'SCHEME REPORT'                                           
         SSPEC H2,52,C'-------------'                                           
         SSPEC H1,95,AGYNAME                                                    
         SSPEC H2,95,AGYADD                                                     
         SSPEC H4,95,RUN                                                        
         SSPEC H5,95,REPORT                                                     
         SSPEC H5,112,PAGE                                                      
         DC    X'00'                                                            
         EJECT                                                                  
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
TRAPERR  GOTO1 ERREX                                                            
*                                                                               
SCANERR  OI    6(R2),X'40'         SET CURSOR                                   
         LA    R2,CONHEADH                                                      
         XC    CONHEAD,CONHEAD                                                  
         MVC   8(23,R2),=C'** ERROR IN FIELD    **'                             
         EDIT  (R6),(2,26(R2)),ALIGN=LEFT                                       
         GOTO1 ERREX2                                                           
*                                                                               
FEWCODES XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'FEWCODEM),FEWCODEM                                     
         GOTO1 ERREX2                                                           
FEWCODEM DC    C'* ERROR * DPT/PGM CODES MAY NOT BE DELETED *'                  
*                                                                               
FEWPERDS XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'FEWPERDM),FEWPERDM                                     
         GOTO1 ERREX2                                                           
FEWPERDM DC    C'* ERROR * NUMBER OF PERIODS MAY NOT BE REDUCED *'              
*                                                                               
DUPPERDS XC    CONHEAD,CONHEAD                                                  
         LA    R2,SIRPERH                                                       
         MVC   CONHEAD(L'DUPPERDM),DUPPERDM                                     
         GOTO1 ERREX2                                                           
DUPPERDM DC    C'* ERROR * DUPLICATE PERIOD NAMES EXIST *'                      
*                                                                               
INVMENU  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'INVMENUM),INVMENUM                                     
         GOTO1 ERREX2                                                           
INVMENUM DC    C'* ERROR * INVALID DEMO MENU NAME *'                            
*                                                                               
INVPER   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'INVPERM),INVPERM                                       
         GOTO1 ERREX2                                                           
INVPERM  DC    C'* ERROR * INVALID BUYING PERIOD NAME *'                        
         EJECT                                                                  
         LTORG                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE SPSFMFFD                                                       
         PRINT ON                                                               
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPSFMBBD                                                       
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE SPSFMWORKD                                                     
         PRINT ON                                                               
         SPACE 5                                                                
* WORK AREA                                                                     
*                                                                               
         ORG   SYSSPARE                                                         
SVPERLEN DS    X                                                                
SAVEKEY  DS    XL48                                                             
MYWORK   DS    XL64                                                             
HDHOOKOK DS    C                                                                
RECFOUND DS    C                                                                
SCHEME   DS    XL2                                                              
         PRINT OFF                                                              
       ++INCLUDE SPGENDMN                                                       
         PRINT ON                                                               
         EJECT                                                                  
SIRRECD  DSECT                                                                  
       ++INCLUDE SPGENSIR                                                       
         EJECT                                                                  
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
LSTSCH   DS    CL3                                                              
         DS    CL5                                                              
LSTNAME  DS    CL24                                                             
         SPACE 3                                                                
SPOOLD   DSECT                                                                  
*              DSECT FOR THE PRINT LINES                                        
         ORG   P                                                                
PRSKIP1  DS    C                                                                
         ORG   P2                                                               
PRSKIP2  DS    C                                                                
         ORG   P3                                                               
PRSKIP3  DS    C                                                                
         ORG   P4                                                               
PRSKIP4  DS    C                                                                
*              PRINT SCHEME NAME                                                
         ORG   P                                                                
         DS    CL26                                                             
PRSCH    DS    CL6                                                              
         DS    CL4                                                              
PRSCHM   DS    CL3                                                              
         ORG   P2                                                               
         DS    CL26                                                             
PRSKIP6  DS    C                                                                
*              PRINT NAME AND RATINGS SOURCE                                    
         ORG   P3                                                               
         DS    CL26                                                             
PRNM     DS    CL4                                                              
         DS    CL6                                                              
PRNAM    DS    CL24                                                             
         DS    CL5                                                              
PRSRC    DS    CL6                                                              
         DS    CL4                                                              
PRSRCN   DS    CL3                                                              
         ORG   P4                                                               
PRSKIP8  DS    C                                                                
*              PRINT OPTIONS                                                    
         ORG   P2                                                               
         DS    CL26                                                             
PROPT    DS    CL7                                                              
         DS    CL3                                                              
PROPTS   DS    CL70                                                             
*              PRINT DAYPART CODES AND NAMES                                    
         ORG   P2                                                               
         DS    CL54                                                             
PRDAY    DS    CL23                                                             
*              PRINT DAYPART DATA                                               
         ORG   P                                                                
PRSTART  DS    CL26                                                             
PRDYC    DS    C                                                                
PREQUAL  DS    C                                                                
PREQUALQ EQU   C'='                                                             
PRDYN    DS    CL7                                                              
         DS    CL7                                                              
PRLEN    EQU   *-PRDYC                                                          
*              PRINT PROGRAM TYPES                                              
         ORG   P2                                                               
         DS    CL51                                                             
PRPROG   DS    CL29                                                             
*              PROGRAM TYPES DATA                                               
         ORG   P1                                                               
PRBEG    DS    CL26                                                             
PRTYC    DS    C                                                                
PREQUL   DS    C                                                                
PRTYN    DS    CL7                                                              
         DS    CL7                                                              
PRLENT   EQU   *-PRTYC                                                          
*              PRINT PERIOD ELEMENTS                                            
         ORG   P2                                                               
PRORI    DS    CL26                                                             
PRPER1   DS    CL12                                                             
         DS    CL8                                                              
PRNUM    DS    CL2                                                              
         DS    C                                                                
PRPER    DS    CL4                                                              
         DS    CL3                                                              
PRPLEN   EQU   *-PRNUM                                                          
*              PRINT DEMO MENU AND ELEMENTS                                     
         ORG   P1                                                               
PRORN    DS    CL26                                                             
PRDEM    DS    CL9                                                              
         DS    CL4                                                              
PRFIL    DS    CL4                                                              
         DS    CL3                                                              
*              DEMO RECORD ELEMENTS                                             
PRSTUFF  DS    CL7                                                              
         DS    CL3                                                              
PRSTLEN  EQU   *-PRSTUFF                                                        
*              PRINT CURRENT YEAR                                               
         ORG   P2                                                               
         DS    CL26                                                             
PRYR     DS    CL12                                                             
         DS    C                                                                
PRCYR    DS    CL2                                                              
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'030SPSFM0B   05/01/02'                                      
         END                                                                    
