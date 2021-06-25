*          DATA SET RESFM1B    AT LEVEL 192 AS OF 08/10/00                      
*PHASE T8181BA                                                                  
         TITLE 'T8181B - SECDEF LIST/MAINTENANCE'                               
***********************************************************************         
*                                                                     *         
*  RESFM1B (T8181B) --- SECURITY DEFINITION RECORDS (SECDEF)          *         
*                       LIST/MAINTENANCE MODULE                       *         
*                                                                     *         
* ------------------------------------------------------------------- *         
* UPDATE HISTORY:                                                     *         
*                                                                     *         
* 16MAR00 (RHV) DATE OF BIRTH                                         *         
*                                                                     *         
***********************************************************************         
T8181B   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*T8181B*,R7,RR=R3                                              
         L     RC,0(R1)            STANDARD CODING                              
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN + OUR SCREEN                     
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R3,RELO                                                          
                                                                                
         MVI   ACTELOPT,C'N'       DON'T WANT ACTIVITIES ELEMENT                
         MVI   IOOPT,C'Y'          DO MY OWN I/O'S                              
         OI    GENSTAT2,RETEQSEL                                                
         OI    GENSTAT4,NODELLST                                                
*                                                                               
MAIN00   DS    0H                                                               
         CLI   MODE,VALKEY         VALIDATE KEY?                                
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD?                             
         BE    VR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY?                                 
         BE    DK                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD?                              
         BE    DR                                                               
         CLI   MODE,LISTRECS       LIST RECORDS?                                
         BE    LR                                                               
         CLI   MODE,RECDEL         DELETE?                                      
         BE    VR                                                               
         CLI   MODE,RECREST        RESTORE?                                     
         BE    VR                                                               
         CLI   MODE,PROCPFK        PF KEY HANDLER                               
         BE    PFK                                                              
         B     EXITL                                                            
                                                                                
*                                                                               
EXITH    CLI   *,0                 SET CC HIGH                                  
         B     EXIT                                                             
EXITL    CLI   *,X'FF'             SET CC LOW                                   
         B     EXIT                                                             
EXITOK   CR    RB,RB               SET CC EQUAL                                 
EXIT     XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* PF KEY HANDLER                                                                
***********************************************************************         
PFK      DS    0H                                                               
         CLI   PFAID,12                                                         
         BE    PF12                                                             
         CLI   PFAID,24                                                         
         BE    PF12                                                             
         B     EXITL                                                            
*                                                                               
PF12     DS    0H                                                               
         CLI   ACTNUM,ACTSEL       SELECT ACTION NOW?                           
         BNE   EXITL               PF12 NOT APPROPRIATE                         
         NI    GENSTAT2,X'FF'-RETEQSEL                                          
         OI    GENSTAT2,NEXTSEL                                                 
         B     EXITOK                                                           
***********************************************************************         
* VALIDATE KEY                                                                  
***********************************************************************         
VK       DS    0H                                                               
         CLI   ACTNUM,ACTLIST           ACTION LIST                             
         BE    VK005                                                            
         TM    SDFPRGH+4,X'80'          VALIDATE PROGRAM FIELD                  
         BO    VK005                                                            
         TM    SDFRECH+4,X'80'          VALIDATE RECORD FIELD                   
         BO    VK005                                                            
         B     EXIT                                                             
*                                                                               
VK005    DS    0H                                                               
*                                                                               
         XC    KEY,KEY             READ REP RECORD                              
         LA    R6,KEY                                                           
         USING RREPREC,R6                                                       
         MVI   RREPKTYP,X'01'                                                   
         MVC   RREPKREP,AGENCY                                                  
         MVC   AIO,AIO1                                                         
         GOTOX (RFGETREC,REPFACS),DMCB,KEY,AIO,0,RFBLOCK                        
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R6,AIO                                                           
*                                                                               
         CLC   =X'0000',RREPMAST   MASTER/SUBSIDIARY?                           
         BE    VK008               NO  - ACCEPT                                 
         CLC   =X'4040',RREPMAST   MASTER/SUBSIDIARY?                           
         BE    VK008               NO  - ACCEPT                                 
         CLC   =X'FFFF',RREPMAST   YES - MASTER OR SUBSIDIARY?                  
         BE    VK008               MASTER - ACCEPT                              
         MVC   RERROR,=AL2(806)    ERR - MUST BE MASTER                         
         LA    R2,CONACTH                                                       
         B     ERREND                                                           
*                                                                               
VK008    DS    0H                                                               
         MVC   FLTRPRG,SPACES                                                   
         MVC   FLTRREC,SPACES                                                   
*                                                                               
         XC    LEVFIRST,LEVFIRST                                                
         XC    LEVLAST,LEVLAST                                                  
*                                                                               
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RSDFKEY,R6                                                       
         MVC   RSDFKTYP(2),=X'1502'                                             
         MVC   RSDFKREP,AGENCY                                                  
*                                                                               
         LA    R2,SDFPRGH          VALIDATE PROGRAM FIELD                       
         CLI   5(R2),0                                                          
         BNE   *+16                                                             
         CLI   ACTNUM,ACTLIST      NOT ENTERED - OK FOR ACTION LIST             
         BE    VK010                                                            
         B     MISSFLD                                                          
*                                                                               
         XC    WORK,WORK                                                        
         ZIC   R5,5(R2)                                                         
         BCTR  R5,0                                                             
         EX    R5,*+4                                                           
         MVC   WORK(0),8(R2)                                                    
         BAS   RE,VPROG            VALIDATE PROGRAM                             
         BL    INVLFLD             INVALID                                      
         MVC   RSDFKPRG,WORK                                                    
VK010    DS    0H                                                               
         OC    RSDFKPRG,SPACES                                                  
         MVC   FLTRPRG,RSDFKPRG                                                 
*                                                                               
         LA    R2,SDFRECH          VALIDATE RECORD FIELD                        
         MVC   RSDFKREC,8(R2)                                                   
         OC    RSDFKREC,SPACES                                                  
         MVC   FLTRREC,RSDFKREC                                                 
*                                                                               
         CLI   ACTNUM,ACTADD                                                    
         BNE   VKX                                                              
*                                                                               
         CLC   RSDFKPRG,=C'CON'    SPECIAL CHECK FOR CONTRACT                   
         BNE   VK020                                                            
         CLC   RSDFKREC,=CL8'CONACT'                                            
         BE    VK020               OK                                           
         CLC   RSDFKREC,=CL8'BUYACT'                                            
         BE    VK020               OK                                           
         B     INVLFLD                                                          
*                                                                               
VK020    DS    0H                                                               
         LA    R2,SDFLV1H          FIRST LEVEL EDIT SCREEN LINE                 
         TM    1(R2),X'20'         PROTECTED?                                   
         BZ    VKX                 NO                                           
         BAS   RE,CLRLA            YES - CLEAR OUT EVERYTHING                   
*                                                                               
VKX      DS    0H                                                               
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* DISPLAY KEY                                                                   
***********************************************************************         
DK       DS    0H                                                               
         L     R6,AIO                                                           
         USING RSDFREC,R6                                                       
         MVC   SDFPRG,RSDFKPRG                                                  
         OI    SDFPRGH+6,X'80'    XMIT                                          
         MVC   SDFREC,RSDFKREC                                                  
         OI    SDFRECH+6,X'80'    XMIT                                          
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE RECORD                                                               
***********************************************************************         
VR       DS    0H                                                               
         MVI   LOCSTAT,0                                                        
*                                                                               
         CLI   ACTNUM,ACTADD                                                    
         BNE   VR010                                                            
         BAS   RE,NEWREC           FOR ACTION ADD, INITIALIZE RECORD            
*                                                                               
VR010    DS    0H                                                               
         CLI   MODE,RECDEL         DELETE?                                      
         BNE   VR040                                                            
         OI    KEY+27,X'80'                                                     
         GOTO1 WRITE                                                            
         L     R6,AIO                                                           
         OI    29(R6),X'80'        DELETE                                       
         B     VR120                                                            
*                                                                               
VR040    DS    0H                                                               
         CLI   MODE,RECREST        RESTORE?                                     
         BNE   VR100                                                            
         TM    KEY+27,X'80'        DELETED?                                     
         BZ    INVREST             NO - CANT'T RESTORE                          
         NI    KEY+27,X'FF'-X'80'                                               
         GOTO1 WRITE                                                            
         L     R6,AIO                                                           
         NI    29(R6),X'FF'-X'80'                                               
         B     VR120                                                            
*                                                                               
*                                                                               
VR100    DS    0H                                                               
         BAS   RE,VLALINE          VALIDATE LEVEL-ACTION LINES                  
*                                                                               
         BAS   RE,VDFLT            VALIDATE DEFAULT FIELD                       
*                                                                               
         CLI   ACTNUM,ACTADD                                                    
         BNE   VR120                                                            
*                                                                               
         GOTO1 ADDREC                                                           
         CLI   DMCB+8,0                                                         
         BE    DR                                                               
         DC    H'0'                                                             
                                                                                
VR120    DS    0H                                                               
         GOTO1 PUTREC                                                           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
VRX      DS    0H                                                               
         LA    R2,SDFLUFH          POINT AT LAST UNPT FLD                       
         OI    1(R2),X'01'         MODIFIED                                     
         OI    4(R2),X'80'                                                      
         OI    6(R2),X'80'                                                      
         B     DR                                                               
         EJECT                                                                  
*                                                                               
* NEWREC - INITIALIZE NEW SECDEF RECORD                                         
*                                                                               
NEWREC   NTR1                                                                   
         L     R6,AIO                                                           
         USING RSDFREC,R6                                                       
         XC    RSDFREC(RSDFIEL-RSDFREC+RSDFILNQ+1),RSDFREC                      
         MVC   RSDFKEY,KEY                                                      
         MVI   RSDFICD,1                                                        
         MVI   RSDFILN,RSDFILNQ                                                 
         MVI   RSDFLEN+1,RSDFIEL-RSDFREC+RSDFILNQ+1                             
         B     EXITOK                                                           
         DROP  R6                                                               
*                                                                               
* VLALINE - VALIDATE LEVEL/ACTION EDIT LINES                                    
*                                                                               
VLALINE  NTR1                                                                   
         XC    CURRLS,CURRLS                                                    
         XC    NEXTLS,NEXTLS                                                    
*                                                                               
         LA    R2,SDFLV1H          FIRST LEVEL EDIT SCREEN LINE                 
         CLI   ACTNUM,ACTADD                                                    
         BE    VLA020                                                           
         TM    1(R2),X'20'         PROTECTED?                                   
         BZ    VLA020              NO                                           
         GOTO1 HELLO,DMCB,(C'G',SYSFIL),(X'20',AIO),(9,LEVFIRST)                
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                MUST FIND THIS                               
         L     R6,12(R1)                                                        
*                                                                               
         MVC   CURRLS,2(R6)        CURRENT LEVEL-SEQENCE                        
         ZIC   R1,1(R6)                                                         
         AR    R1,R6                                                            
         CLI   0(R1),X'20'                                                      
         BNE   *+10                                                             
         MVC   NEXTLS,2(R1)        NEXT LEVEL-SEQUENCE                          
         B     VLA020                                                           
*                                                                               
VLA010   DS    0H                                                               
         LA    RF,SDFLVXH                                                       
         CR    R2,RF               LAST LINE ON SCREEN                          
         BNL   EXITOK              DONE                                         
*                                                                               
         ZIC   RF,0(R2)                                                         
         AR    R2,RF               ACTIONS FIELD                                
         ZIC   RF,0(R2)                                                         
         AR    R2,RF               NEXT LEVEL FIELD                             
*                                                                               
         CLI   ACTNUM,ACTADD                                                    
         BE    VLA020                                                           
*                                                                               
         SR    R6,R6                                                            
         MVC   CURRLS,NEXTLS                                                    
         XC    NEXTLS,NEXTLS                                                    
         OC    CURRLS,CURRLS                                                    
         BZ    VLA020                                                           
         GOTO1 HELLO,DMCB,(C'G',SYSFIL),(X'20',AIO),(9,CURRLS)                  
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                MUST FIND THIS                               
         L     R6,12(R1)                                                        
         ZIC   R1,1(R6)                                                         
         AR    R1,R6                                                            
         CLI   0(R1),X'20'                                                      
         BNE   *+10                                                             
         MVC   NEXTLS,2(R1)                                                     
*                                                                               
VLA020   DS    0H                                                               
         ZIC   RF,0(R2)                                                         
         LA    R3,0(RF,R2)         ACTIONS FIELD                                
*                                                                               
         TM    1(R2),X'20'         PROTECTED?                                   
         BO    VLA050              YES                                          
*                                                                               
         XC    CURRLS,CURRLS       NO CORRESPONDING ELEMENT YET                 
         XC    NEXTLS,NEXTLS                                                    
*                                                                               
         CLI   5(R2),0                                                          
         BNE   VLA030                                                           
         CLI   5(R3),0                                                          
         BE    VLA010                                                           
         B     MISSFLD             ACTIONS W/O LEVEL                            
VLA030   CLI   5(R3),0                                                          
         BNE   VLA100                                                           
         LR    R3,R2                                                            
         B     MISSFLD             LEVEL W/O ACTIONS                            
*                                                                               
VLA050   DS    0H                                                               
         CLI   ACTNUM,ACTADD                                                    
         BE    VLA010                                                           
         TM    4(R3),X'80'         INPUT?                                       
         BZ    VLA010              NO - DON'T DO ANYTHING                       
         LTR   R6,R6               R6 SHOULD = CURRENT ELEM                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
         OC    8(8,R2),SPACES                                                   
         CLC   8(8,R2),2(R6)       SCREEN LEVEL VS. ELEM                        
         BE    *+6                                                              
         DC    H'0'                SHOULD MATCH                                 
         MVC   LEVNEXT,0(RF)       SAVE IT                                      
         GOTO1 HELLO,DMCB,(C'D',SYSFIL),(X'20',AIO),(9,CURRLS)                  
         CLI   12(R1),0            DELETE EXISTING ELEM                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
VLA100   DS    0H                                                               
         OI    LOCSTAT,X'80'       CHANGE MADE                                  
*                                                                               
         CLC   =C'ALL',8(R2)                                                    
         BE    INVLFLD                                                          
         CLC   =C'DEFAULT',8(R2)                                                
         BE    INVLFLD                                                          
         CLC   =C'NOACCESS',8(R2)                                               
         BE    INVLFLD                                                          
*                                                                               
         CLI   5(R3),0             ANY ACTIONS?                                 
         BNE   VLA103              YES                                          
         CLC   CURRLS,LEVFIRST     DID WE JUST DELETE 1ST LINE?                 
         BNE   VLA010              NO - JUST GOT TO NEXT LINE                   
         MVC   LEVFIRST,NEXTLS     RESET IT TO NEXT LINE                        
         B     VLA010                                                           
*                                                                               
VLA103   DS    0H                                                               
         MVI   BYTE,0              FIND NEXT ELEM SEQ NUMBER                    
         MVC   WORK(8),8(R2)                                                    
         OC    WORK(8),SPACES                                                   
VLA105   DS    0H                                                               
         MVC   WORK+8(1),BYTE                                                   
         GOTO1 HELLO,DMCB,(C'G',SYSFIL),(X'20',AIO),(9,WORK)                    
         CLI   12(R1),6                                                         
         BE    VLA110              NO OTHER ELEMS                               
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R6,12(R1)                                                        
         USING RSDFDEL,R6                                                       
         IC    RF,RSDFDSQ                                                       
         DROP  R6                                                               
         LA    RF,1(RF)                                                         
         STC   RF,BYTE                                                          
         B     VLA105                                                           
*                                                                               
VLA110   DS    0H                  BUILD EMPTY ELEM                             
         XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING RSDFDEL,R6                                                       
         MVI   RSDFDCD,X'20'                                                    
         MVI   RSDFDLN,RSDFDSEL-RSDFDEL                                         
         MVC   RSDFDLV,8(R2)                                                    
         OC    RSDFDLV,SPACES                                                   
         MVC   RSDFDSQ,BYTE                                                     
*                                                                               
         GOTO1 SCANNER,DMCB,(R3),AIO3,C',=,~'   '~' IS GARBAGE                  
         ZIC   R5,4(R1)                                                         
         LTR   R5,R5                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         L     R3,AIO3                                                          
*                                                                               
VLA120   DS    0H                                                               
         ZIC   RF,RSDFDLN                                                       
         LA    R4,0(RF,R6)         END OF ELEM                                  
         ZIC   RF,0(R3)            LEN OF SCANNER INPUT                         
         LA    R1,11(RF,R3)        END OF INPUT DATA                            
         CLI   0(R1),C' '          STRIP LEADING/TRAILING BLANKS                
         BNE   VLA125                                                           
         BCTR  R1,0                                                             
         BCT   RF,*-10                                                          
VLA125   LA    R1,12(R3)           START OF INPUT DATA                          
         CLI   0(R1),C' '                                                       
         BNE   VLA130                                                           
         LA    R1,1(R1)                                                         
         BCT   RF,*-12                                                          
VLA130   DS    0H                                                               
         STC   RF,0(R4)                                                         
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   1(0,R4),0(R1)       WRITE INPUT TO END OF ELEM                   
         ZIC   R1,RSDFDLN                                                       
         LA    RF,2(R1,RF)                                                      
         CHI   RF,255                                                           
         BNH   *+6                                                              
         DC    H'0'                                                             
         STC   RF,RSDFDLN                                                       
         DROP  R6                                                               
*                                                                               
         LA    R3,32(R3)           NEXT SCANNER LINE                            
         BCT   R5,VLA120                                                        
*                                                                               
         GOTO1 HELLO,DMCB,(C'P',SYSFIL),AIO,ELEM,0                              
         CLI   12(R1),0                                                         
         BE    VLA010              OK - NEXT SCREEN LINE                        
         MVC   RERROR,=AL2(861)                                                 
         CLI   12(R1),5            REC TOO LONG                                 
         BE    ERREND                                                           
         DC    H'0'                                                             
*                                                                               
* VDFLT - VALIDATE DEFAULT FIELD                                                
*                                                                               
VDFLT    NTR1                                                                   
         L     R6,AIO                                                           
         USING RSDFREC,R6                                                       
         LA    R2,SDFDFTH                                                       
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
         MVC   RSDFIDF,8(R2)                                                    
         OC    RSDFIDF,SPACES                                                   
         CLC   RSDFIDF,=CL8'ALL'                                                
         BE    EXITOK                                                           
         CLC   RSDFIDF,=CL8'NOACCESS'                                           
         BE    EXITOK                                                           
         GOTO1 HELLO,DMCB,(C'G',SYSFIL),(X'20',AIO),(8,RSDFIDF)                 
         CLI   12(R1),0                                                         
         BNE   INVLFLD                                                          
         B     EXITOK                                                           
*                                                                               
* VPROG  - VALIDATE PROGRAM NAME                                                
*    INPUT:   WORK - 3 CHAR PROGRAM CODE                                        
*    OUTPUT:  CODE FOUND: CC EQUAL                                              
*                         WORK - 3 CHAR CODE, 8 CHAR EXPANSION                  
*             NOT FOUND:  CC NOT EQUAL                                          
*                                                                               
VPROG    NTR1                                                                   
         LA    R4,RESDFPRG           PROGRAM TABLE                              
         B     *+8                                                              
VPROG10  DS    0H                                                               
         LA    R4,L'RESDFPRG(R4)                                                
         CLI   0(R4),X'FF'                                                      
         BE    EXITL                                                            
         CLC   0(3,R4),WORK                                                     
         BNE   VPROG10                                                          
         MVC   WORK(L'RESDFPRG),0(R4)                                           
         B     EXITOK                                                           
*                                                                               
***********************************************************************         
* DISPLAY RECORD                                                                
***********************************************************************         
DR       DS    0H                                                               
         CLI   ACTNUM,ACTADD                                                    
         BE    DR100                                                            
         GOTO1 GETREC                                                           
*                                                                               
DR100    DS    0H                                                               
         BAS   RE,CLRLA            CLEAR LEVEL/ACTION LINES                     
*                                                                               
         L     R6,AIO                                                           
         USING RSDFREC,R6                                                       
*                                                                               
         TM    29(R6),X'80'        DELETED?                                     
         BO    DR500               DISP BLANK RECORD                            
*                                                                               
         LA    R2,SDFDFTH                                                       
         MVC   8(L'SDFDFT,R2),RSDFIDF    DEFAULT LEVEL                          
         OI    6(R2),X'80'                                                      
         DROP  R6                                                               
*                                                                               
         CLI   MODE,DISPREC                                                     
         BE    DR120                                                            
         CLI   MODE,VALREC                                                      
         BNE   DR180                                                            
         TM    LOCSTAT,X'80'             CHANGE MADE?                           
         BO    DR180                     YES - JUST REDISP SCREEN               
*                                                                               
DR120    DS    0H                                                               
         OC    LEVLAST,LEVLAST           HAVE A LAST REC TO CONTINUE?           
         BZ    *+14                      NO                                     
         MVC   LEVFIRST,LEVLAST          YES - START WITH IT                    
         B     *+10                                                             
         XC    LEVFIRST,LEVFIRST         ELSE START OVER AT TOP                 
*                                                                               
DR180    DS    0H                                                               
         LA    RF,LEVFIRST                                                      
         ST    RF,DMCB+8                                                        
         OC    LEVFIRST,LEVFIRST                                                
         BZ    *+8                                                              
         MVI   DMCB+8,9                                                         
         GOTO1 HELLO,DMCB,(C'G',SYSFIL),(X'20',AIO),                            
         CLI   12(R1),6                                                         
         BE    DR500               NO ELEMS                                     
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R6,12(R1)           1ST 20 ELEM                                  
         USING RSDFDEL,R6                                                       
         LA    R2,SDFLV1H                                                       
         MVC   LEVFIRST,2(R6)                                                   
*                                                                               
DR210    DS    0H                                                               
         MVC   LEVLAST,2(R6)                                                    
*                                                                               
         MVC   8(L'SDFLV1,R2),RSDFDLV                                           
         OI    1(R2),X'20'         PROTECT LEVEL NAME                           
         DROP  R6                                                               
*                                                                               
         ZIC   RF,0(R2)                                                         
         AR    R2,RF               ACTIONS FIELD                                
         ZIC   RF,1(R6)                                                         
         AR    RF,R6               END OF ELEMENT                               
         LA    R4,RSDFDSEL-RSDFDEL(R6)  1ST MINI ELEM                           
         LA    R3,8(R2)                                                         
         B     *+12                                                             
DR250    DS    0H                                                               
         MVI   0(R3),C','                                                       
         LA    R3,1(R3)                                                         
         ZIC   R1,0(R4)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   0(0,R3),1(R4)                                                    
         LA    R3,1(R1,R3)                                                      
         LA    R4,2(R1,R4)         NEXT MINI ELEM                               
         CR    R4,RF                                                            
         BL    DR250                                                            
*                                                                               
         ZIC   RF,0(R2)                                                         
         AR    R2,RF               NEXT LINE                                    
         LA    RF,SDFLVXH                                                       
         CR    R2,RF               END OF SCREEN?                               
         BH    DR500                                                            
         ZIC   RF,1(R6)                                                         
         AR    R6,RF               NEXT ELEMENT                                 
         CLI   0(R6),X'20'                                                      
         BE    DR210               HAVE ONE                                     
         XC    LEVLAST,LEVLAST                                                  
*                                                                               
DR500    DS    0H                  DISPLAY PFKEYS                               
         LA    R2,SDFLUFH          POINT AT SVC REQ FLD HDR                     
         OI    1(R2),X'01'         MODIFIED                                     
         OI    4(R2),X'80'                                                      
         OI    6(R2),X'80'                                                      
*                                                                               
         LA    R2,SDFPFKH                                                       
         OI    6(R2),X'80'                                                      
         XC    8(L'SDFPFK,R2),8(R2)                                             
         LA    R2,08(R2)                                                        
*                                                                               
         CLI   ACTNUM,ACTSEL       SELECT ACTION NOW?                           
         BNE   DR510                                                            
         MVC   0(11,R2),=C'PF12=Return'                                         
         LA    R2,13(R2)                                                        
*                                                                               
DR510    DS    0H                                                               
         B     EXITOK                                                           
*                                                                               
* CLRLA - CLEAR LEVEL/ACTION LINES                                              
*                                                                               
CLRLA    NTR1                                                                   
         LA    R2,SDFDFTH          DEFAULT FIELD                                
         NI    1(R2),X'FF'-X'20'                                                
         NI    4(R2),X'FF'-X'80'                                                
         OI    6(R2),X'80'                                                      
         XC    8(L'SDFDFT,R2),8(R2)                                             
*                                                                               
         LA    R2,SDFLV1H                                                       
CLA010   DS    0H                                                               
         NI    1(R2),X'FF'-X'20'   LEVEL FIELD                                  
         NI    4(R2),X'FF'-X'80'                                                
         OI    6(R2),X'80'                                                      
         XC    8(L'SDFLV1,R2),8(R2)                                             
         ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         NI    1(R2),X'FF'-X'20'   ACTIONS FIELD                                
         NI    4(R2),X'FF'-X'80'                                                
         OI    6(R2),X'80'                                                      
         XC    8(L'SDFAC1,R2),8(R2)                                             
         ZIC   RF,0(R2)                                                         
         AR    R2,RF               NEXT LINE                                    
         LA    RF,SDFLVXH                                                       
         CR    R2,RF                                                            
         BH    EXITOK                                                           
         B     CLA010                                                           
***********************************************************************         
* LIST RECORD                                                                   
***********************************************************************         
LR       DS    0H                                                               
         XC    LEVFIRST,LEVFIRST                                                
         XC    LEVLAST,LEVLAST                                                  
*                                                                               
         CLC   KEY(2),=X'1502'                                                  
         BE    LR10                                                             
                                                                                
         LA    R6,KEY                                                           
         USING RSDFKEY,R6                                                       
                                                                                
         XC    KEY,KEY                                                          
         MVC   RSDFKTYP(2),=X'1502'                                             
         MVC   RSDFKREP,AGENCY                                                  
         MVC   RSDFKPRG,FLTRPRG                                                 
         MVC   RSDFKREC,FLTRREC                                                 
         DROP  R6                                                               
                                                                                
LR10     DS    0H                                                               
         GOTO1 HIGH                                                             
                                                                                
LR20     DS    0H                                                               
         CLC   KEY(15),KEYSAVE                                                  
         BNE   LRX                                                              
         CLC   FLTRPRG,SPACES      HAVE PROGRAM FILTER?                         
         BE    LR25                NO                                           
         CLC   KEY(18),KEYSAVE                                                  
         BNE   LRX                                                              
*                                                                               
LR25     DS    0H                                                               
         MVC   LISTAR,SPACES                                                    
                                                                                
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         USING RSDFREC,R6                                                       
         MVC   LSDFPRG(3),RSDFKPRG                                              
         MVC   LSDFREC,RSDFKREC                                                 
         MVC   LSDFDFT,RSDFIDF                                                  
         DROP  R6                                                               
                                                                                
         GOTO1 LISTMON             SEND LINE TO SCREEN                          
                                                                                
LRSEQ    GOTO1 SEQ                 GET NEXT RECORD                              
         B     LR20                                                             
                                                                                
LRX      B     EXIT                                                             
         EJECT                                                                  
         EJECT                                                                  
*                                                                               
* ERROR MESSAGES                                                                
*                                                                               
MISSFLD  MVC   RERROR,=AL2(MISSING)                                             
         B     ERREND                                                           
*                                                                               
INVLFLD  MVC   RERROR,=AL2(INVALID)                                             
         B     ERREND                                                           
*                                                                               
SOFSINV  MVC   RERROR,=AL2(700)                                                 
         B     ERREND                                                           
*                                                                               
DUPLIC8  MVC   RERROR,=AL2(401)                                                 
         B     ERREND                                                           
*                                                                               
INVLSUB  MVC   RERROR,=AL2(402)                                                 
         B     ERREND                                                           
*                                                                               
INVLMAS  MVC   RERROR,=AL2(403)                                                 
         B     ERREND                                                           
*                                                                               
CANNOTD  MVC   RERROR,=AL2(405)                                                 
         B     ERREND                                                           
*                                                                               
INVREST  MVC   RERROR,=AL2(870)                                                 
         B     ERREND                                                           
*                                                                               
ERREND   GOTO1 MYERROR                                                          
*                                                                               
         GETEL R6,DATADISP,ELCODE  USED FOR THE GETEL OPERATIONS                
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE RESDFPRG                                                       
         EJECT                                                                  
       ++INCLUDE DDSPOOLD          (GENERAL PRINT AREAS)                        
       ++INCLUDE DDSPLWORKD        (GENERAL CONTROLLER AREAS)                   
       ++INCLUDE FATIOB                                                         
       ++INCLUDE RESFMFFD                                                       
       ++INCLUDE DDGENTWA                                                       
         PRINT ON                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE RESFM90D          (OUR MAINTENANCE SCREEN OVERLAY)             
         ORG   CONTAGH                                                          
       ++INCLUDE RESFM91D          (OUR LIST SCREEN OVERLAY)                    
       ++INCLUDE RESFMWORKD                                                     
       ++INCLUDE REGENREPA                                                      
       ++INCLUDE REGENSDF                                                       
*                                                                               
* APPLICATION WORK AREA                                                         
*                                                                               
         ORG   SYSSPARE                                                         
RELO     DS    A                                                                
CURRLS   DS    CL9                 CURRENT LEVEL-SEQUENCE                       
NEXTLS   DS    CL9                 NEXT LEVEL-SEQUENCE                          
LEVFIRST DS    CL9                 1ST LEVEL ON SCREEN + ELEM SEQUENCE          
LEVNEXT  DS    CL9                                                              
LEVLAST  DS    CL9                 LAST LEVEL ON SCREEN + ELEM SEQUENCE         
LOCSTAT  DS    X                   LOCAL STATUS FLAGS                           
*              80 - CHANGE MADE                                                 
*                                                                               
*                                                                               
FLTRPRG  DS    CL3                 LIST PROGRAM FILTER                          
FLTRREC  DS    CL8                 LIST RECORD FILTER                           
         EJECT                                                                  
*                                                                               
* ONLINE LIST LINE                                                              
*                                                                               
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
LSDFPRG  DS    CL8                                                              
         DS    CL2                                                              
LSDFREC  DS    CL8                                                              
         DS    CL2                                                              
LSDFDFT  DS    CL8                                                              
*                                                                               
* OFFLINE LIST LINE                                                             
*                                                                               
SPOOLD   DSECT                                                                  
         ORG   P                                                                
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'192RESFM1B   08/10/00'                                      
         END                                                                    
