*          DATA SET SPADD02    AT LEVEL 002 AS OF 07/23/91                      
*PHASE T21202A,*                                                                
***********************************************************************         
*                                                                               
*  TITLE: T21202 - MAINTENANCE OF BUYER MARKET ASSIGNMENTS                      
*                                                                               
*  COMMENTS: MAINTAINS BUYER MARKET ASSIGNMENTS                                 
*                                                                               
*  CALLED FROM: ADDS CONTROLLER (T21200), WHICH CALLS                           
*               DDGENCON (T00A30) WHICH CALLS THIS.                             
*                                                                               
*  CALLS TO:    DATAMGR                                                         
*                                                                               
*  INPUTS: SCREENS SPADDF2 (T212F2) -- ASSIGN                                   
*                                                                               
*  OUTPUTS: UPDATED OR NEW BUYER MARKET ASSIGNMENTS                             
*                                                                               
*  LOCALS: REGISTER USAGE                                                       
*          R0 - WORK                                                            
*          R1 - WORK                                                            
*          R2 - POINTER TO SCREEN FLDH, ERREX WILL POSITION CURSOR              
*          R3 - WORK                                                            
*          R4 - WORK                                                            
*          R5 - POINTS TO THE OVERLAY STORAGE AREA DSECT                        
*          R6 - USED FOR GETEL ELEMENT DSECT POINTER                            
*          R7 - SECOND BASE                                                     
*          R8 - SPOOLD                                                          
*          R9 - SYSD                                                            
*          RA - TWA                                                             
*          RB - FIRST BASE                                                      
*          RC - GEND                                                            
*          RD - SYSTEM                                                          
*          RE - SYSTEM                                                          
*          RF - SYSTEM                                                          
*                                                                               
***********************************************************************         
         TITLE 'T21202 - ADDS BUYER MARKET ASSIGNMENT'                          
T21202   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T21202,R7,RR=R4                                                
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         LA    R5,SYSSPARE                                                      
         USING MYAREAD,R5                                                       
         ST    R4,RELO                                                          
*                                                                               
         GOTO1 INITIAL,DMCB,0      INITIALIZE PFKEYS                            
         OI    CONSERVH+6,X'81'    SET MODIFIED AND XMIT                        
*                                                                               
         CLI   MODE,VALKEY         DO EVERYTHING IN VK                          
         BE    VK                                                               
         CLI   MODE,VALREC                                                      
         BE    EXIT                                                             
         DC    H'0'                                                             
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
* VALIDATE KEY ROUTINE                                                          
*                                                                               
VK       MVI   KEYCHANG,C'N'                                                    
*                                                                               
         LA    R2,MRKMEDH          VALIDATE MEDIA FIELD                         
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
         TM    4(R2),X'20'                                                      
         BO    VKMKT                                                            
         NI    MRKMKTH+4,X'FF'-X'20'                                            
         GOTO1 VALIMED                                                          
         OI    4(R2),X'20'                                                      
*                                                                               
VKMKT    LA    R2,MRKMKTH          VALIDATE START AT MARKET FIELD               
         TM    4(R2),X'20'                                                      
         BO    VKX                                                              
*                                                                               
         XC    STARTMKT,STARTMKT   DEFAULT TO ZERO                              
         CLI   5(R2),0                                                          
         BE    VKMKT10                                                          
*                                                                               
         TM    4(R2),X'08'         TEST NUMERIC                                 
         BZ    INVLFLD                                                          
*                                                                               
         ZIC   RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2) *EXECUTED*                                           
*                                                                               
         CVB   R0,DUB                                                           
         ST    R0,FULL                                                          
*                                                                               
         MVC   STARTMKT,FULL+2                                                  
         CLC   STARTMKT,=H'9999'                                                
         BH    INVLFLD             ERROR IF HIGHER THAN 9999                    
*                                                                               
VKMKT10  MVI   KEYCHANG,C'Y'                                                    
         XC    MRKMKT,MRKMKT       ERASE FIELD AND TRANSMIT                     
         OI    4(R2),X'20'                                                      
         OI    6(R2),X'80'                                                      
*                                                                               
*                                                                               
VKX      CLI   KEYCHANG,C'Y'                                                    
         BE    DR                                                               
         B     VR                                                               
         EJECT                                                                  
* VALIDATE RECORD                                                               
*                                                                               
VR       MVI   RECCHANG,C'N'                                                    
*                                                                               
         LA    R2,MRKL1H           POINT TO FIRST LINE                          
*                                                                               
VR10     CLC   8(4,R2),=C'    '    TEST NO MORE MARKETS                         
         BNH   VR100                                                            
*                                                                               
         PACK  DUB,8(4,R2)         CONVERT MARKET TO BINARY                     
         CVB   R1,DUB                                                           
         STCM  R1,3,MYBMKT                                                      
*                                                                               
         ZIC   R0,0(R2)            BUMP TO ASSIGNMENT FIELD                     
         AR    R2,R0                                                            
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
*                                                                               
         CLI   5(R2),0             IF FIELD IS NOT EMPTY                        
         BE    VR90                                                             
         TM    4(R2),X'20'         AND HAS NOT BEEN VALIDATED                   
         BO    VR90                                                             
         MVI   RECCHANG,C'Y'       THEN SET RECORD CHANGED FLAG                 
*                                                                               
         XC    PREVDATE,PREVDATE   CLEAR PREVIOUS DATE                          
         XC    PREVBYR,PREVBYR     CLEAR PREVIOUS BUYER                         
*                                                                               
         LA    R4,KEY                                                           
         USING BYRKEY,R4                                                        
         XC    KEY,KEY                                                          
         MVI   BYRKTYP,BYRKTYPQ                                                 
         MVI   BYRKSUB,BYRKSUB2                                                 
         MVC   BYRKAM,BAGYMD                                                    
         MVC   BYRKMKT2,MYBMKT                                                  
         GOTO1 HIGH                                                             
*                                                                               
         MVI   RECFOUND,C'Y'                                                    
         CLC   KEY(5),KEYSAVE      IF PASSIVE KEY NOT FOUND                     
         BE    VR20                                                             
         MVI   RECFOUND,C'N'                                                    
         B     VR40                                                             
*                                                                               
VR20     MVC   PREVBYR,BYRKBUY2    SAVE CURRENT BUYER                           
         MVC   PREVDATE,BYRKDAT2   SAVE CURRENT DATE                            
*                                                                               
VR40     DS    0H                                                               
         LA    R3,BLOCK            VALIDATE BUYER ASSIGNMENT                    
         GOTO1 SCANNER,DMCB,(R2),BLOCK,0                                        
         CLI   4(R1),1                                                          
         BNE   INVLFLD                                                          
         CLI   0(R3),0                                                          
         BE    INVLFLD                                                          
         CLI   0(R3),3             BUYER ONLY, EFFECTIVE IMMEDIATELY            
         BH    VR50                                                             
         MVC   NEWBYR,12(R3)                                                    
         OC    NEWBYR,SPACES                                                    
         GOTO1 DATCON,DMCB,(5,0),(15,FULL)   GET TODAY'S DATE                   
         B     VR60                                                             
VR50     CLI   1(R3),0                                                          
         BE    INVLFLD                                                          
*                                  VALIDATE DATE                                
         GOTO1 DATVAL,DMCB,(0,12(R3)),THISDATE                                  
         OC    0(4,R1),0(R1)                                                    
         BZ    INVLFLD                                                          
         MVC   NEWBYR,22(R3)                                                    
*                                                                               
         GOTO1 DATCON,DMCB,(0,THISDATE),(15,FULL)                               
VR60     SP    FULL(4),=P'999999'  COMPLEMENT IT                                
         OI    FULL+3,X'0F'                                                     
         L     R1,FULL                                                          
         SRL   R1,4                                                             
         STCM  R1,7,NEWDATE                                                     
*                                                                               
         OC    PREVDATE,PREVDATE   NO PREVIOUS DATE ENTRY                       
         BNZ   VR65                                                             
         GOTO1 DATCON,DMCB,(5,0),(15,FULL)   GET TODAY'S DATE                   
         SP    FULL(4),=P'999999'  COMPLEMENT IT                                
         OI    FULL+3,X'0F'                                                     
         L     R1,FULL                                                          
         SRL   R1,4                                                             
         STCM  R1,7,PREVDATE                                                    
*                                                                               
VR65     CLC   NEWDATE,PREVDATE    MUST BE AFTER PREVIOUS DATE                  
         BH    INVLFLD                                                          
         CLC   NEWBYR,PREVBYR      BUYER ALREADY CURRENT                        
         BE    INVLBYR                                                          
*                                                                               
         XC    KEY,KEY                                                          
         MVI   BYRKTYP,BYRKTYPQ    GET PRIMARY BUYER KEY                        
         MVI   BYRKSUB,BYRKSUBQ                                                 
         MVC   BYRKAM,BAGYMD                                                    
         MVC   BYRKBYR,NEWBYR                                                   
         OC    BYRKBYR,SPACES                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(L'BYRKEY),KEYSAVE                                            
         BNE   INVLFLD             ERROR, BUYER NOT THERE                       
*                                                                               
         MVC   DISKADD,KEY+14                                                   
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
*                                  DELETE MARKET ELEMENT FROM BUYER             
         GOTO1 HELLO,DMCB,(C'D',=C'SPTFILE'),(X'30',AIO),(2,MYBMKT),0           
         CLI   DMCB+12,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R6,ELEM                                                          
         USING BYRMKAD,R6                                                       
         XC    ELEM,ELEM                                                        
         MVI   BYRMKAEL,BYRMKAEQ   MARKET ASSIGNMENT ELEMENT                    
         MVI   BYRMKALN,BYRMKALQ   MARKET ASSIGNMENT LENGTH                     
         MVC   BYRMKANO,MYBMKT     MARKET CODE                                  
         MVC   BYRMKADT,NEWDATE                                                 
         DROP  R6                                                               
*                                                                               
         L     R6,AIO                                                           
         GOTO1 ADDELEM             ADD MARKET TO BUYER                          
         CLI   DMCB+12,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 PUTREC                                                           
         CLI   DMCB+8,0            UPDATE BUYER RECORD                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
VR70     DS    0H                                                               
         XC    KEY,KEY             FIND PREVIOUS PASSIVE MARKET KEYS            
         MVI   BYRKTYP,BYRKTYPQ                                                 
         MVI   BYRKSUB,BYRKSUB2                                                 
         MVC   BYRKAM,BAGYMD                                                    
         MVC   BYRKMKT2,MYBMKT                                                  
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(5),KEYSAVE                                                   
         BNE   VR80                                                             
         MVC   MYSVKEY,KEY                                                      
*                                                                               
VR75     GOTO1 SEQ                                                              
*                                                                               
         CLC   KEY(5),MYSVKEY      SAME MARKET??                                
         BNE   VR80                                                             
*                                                                               
         OI    KEY+13,X'80'        DELETE ALL PREVIOUS PASSIVE POINTERS         
         GOTO1 WRITE                                                            
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
*                                  AND DELETE ELEMENTS FOR THAT MARKET          
         GOTO1 HELLO,DMCB,(C'D',SYSFIL),(X'30',AIO),(2,MYBMKT),0                
         CLI   DMCB+12,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 PUTREC                                                           
*                                                                               
         B     VR75                                                             
*                                                                               
VR80     XC    KEY,KEY             ADD CURRENT BUYER PASSIVE KEY                
         MVI   BYRKTYP,BYRKTYPQ                                                 
         MVI   BYRKSUB,BYRKSUB2                                                 
         MVC   BYRKAM,BAGYMD                                                    
         MVC   BYRKMKT2,MYBMKT                                                  
         MVC   BYRKDAT2,NEWDATE                                                 
         MVC   BYRKBUY2,NEWBYR                                                  
         MVC   PREVKEY,KEY                                                      
*                                                                               
         OI    DMINBTS,X'08'                                                    
         GOTO1 HIGH                                                             
         NI    DMINBTS,X'FF'-X'08'                                              
         CLC   BYRKEY,KEYSAVE                                                   
         BNE   VR82                                                             
         NI    BYRKCNTL,X'FF'-X'80'                                             
         MVC   BYRKDA,DISKADD                                                   
         GOTO1 WRITE                                                            
         CLI   DMCB+8,0                                                         
         BE    VR84                                                             
         DC    H'0'                                                             
*                                                                               
VR82     MVC   KEY,PREVKEY                                                      
         MVC   BYRKDA,DISKADD                                                   
         GOTO1 ADD                                                              
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
VR84     CLI   RECFOUND,C'N'       DOES PREVIOUS BUYER EXIST?                   
         BE    VR89                                                             
         MVI   RECFOUND,C'N'       RESET FLAG                                   
         MVC   KEY,MYSVKEY         ADD PREVIOUS KEY WITH NO DATE                
         MVC   BYRKDAT2,=X'999999'   ADD IT BACK WITH NEW DATE                  
*                                                                               
         OI    DMINBTS,X'08'                                                    
         GOTO1 HIGH                                                             
         NI    DMINBTS,X'FF'-X'08'                                              
         CLC   BYRKEY,KEYSAVE                                                   
         BNE   VR86                                                             
         NI    BYRKCNTL,X'FF'-X'80'                                             
         GOTO1 WRITE                                                            
         CLI   DMCB+8,0                                                         
         BE    VR88                                                             
         DC    H'0'                                                             
*                                                                               
VR86     MVC   KEY,MYSVKEY         ADD PREVIOUS KEY WITH NO DATE                
         MVC   BYRKDAT2,=X'999999'   PASSIVE KEY                                
         GOTO1 ADD                                                              
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
VR88     MVC   KEY,MYSVKEY         ADD PREVIOUS KEY WITH NO DATE                
         GOTO1 HIGH                                                             
         OI    BYRKCNTL,X'80'      DELETE PASSIVE KEY                           
         GOTO1 WRITE                                                            
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
VR89     OI    4(R2),X'20'         SET VALIDITY BIT                             
*                                                                               
VR90     ZIC   R0,0(R2)            BUMP TO NEXT DISPLAY LINE                    
         AR    R2,R0                                                            
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         LA    RF,MRKEND1H                                                      
         CR    R2,RF               TEST END OF SCREEN                           
         BL    VR10                                                             
*                                                                               
VR100    CLI   RECCHANG,C'N'       IF NO RECORD FIELDS CHANGED                  
         BNE   VRX                                                              
         SR    RE,RE               THEN DISPLAY NEXT SET OF MARKETS             
         ICM   RE,3,ENDMKT                                                      
         LA    RE,1(RE)                                                         
         STCM  RE,3,STARTMKT                                                    
*                                                                               
VRX      B     DR                                                               
         DROP  R4                                                               
         EJECT                                                                  
* DISPLAY RECORD                                                                
*                                                                               
DR       GOTO1 CLEARF,DMCB,(0,MRKL1H),MRKEND2H                                  
         GOTO1 CLEARF,DMCB,(1,MRKL1H),MRKEND2H                                  
*                                                                               
         LA    R4,KEY              BUILD MARKET HEADER KEY                      
         USING MKTRECD,R4                                                       
         MVI   MKTKTYPE,C'M'                                                    
         MVC   MKTKMED,QMED                                                     
         EDIT  (2,STARTMKT),(4,MKTKMKT),FILL=0,ZERO=NOBLANK                     
         MVC   MKTKAGY,AGENCY                                                   
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI'),=C'STATION',KEY,AIO                  
         CLI   8(R1),0                                                          
         BNE   DR100                                                            
         DROP  R4                                                               
*                                                                               
         L     R6,AIO              R6 = A(IOAREA)                               
         USING MKTRECD,R6                                                       
         LA    R2,MRKL1H           R2 = A(FIRST SCREEN LINE)                    
*                                                                               
DR10     CLC   MKTKEY(2),KEY       WHILE NOT END OF MARKET RECORDS              
         BNE   DR100                                                            
*                                                                               
         CLC   MKTKAGY,AGENCY      SKIP RECORD WITH WRONG AGENCY                
         BNE   DR20                                                             
*                                                                               
         MVC   8(4,R2),MKTKMKT     DISPLAY MARKET NUMBER                        
         ZIC   R0,0(R2)                                                         
         AR    R2,R0               BUMP PAST                                    
*                                                                               
         MVC   8(L'MRKNAME,R2),MKTNAME    DISPLAY MARKET NAME                   
         ZIC   R0,0(R2)                                                         
         AR    R2,R0               BUMP PAST                                    
*                                                                               
         ZIC   R0,0(R2)            BUMP TO NEXT LINE                            
         AR    R2,R0                                                            
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
*                                                                               
         LA    RF,MRKEND1H                                                      
         CR    R2,RF               IF END OF SCREEN THEN DONE                   
         BNL   DR100                                                            
*                                  GET NEXT RECORD                              
DR20     GOTO1 DATAMGR,DMCB,(0,=C'DMRSEQ'),=C'STATION',KEY,AIO                  
         CLI   8(R1),0                                                          
         BE    DR10                                                             
         DROP  R6                                                               
         EJECT                                                                  
DR100    LA    R4,KEY              DISPLAY MARKET ASSIGNMENTS                   
         USING BYRKEY,R4                                                        
         XC    KEY,KEY                                                          
         MVI   BYRKTYP,BYRKTYPQ    BUYER RECORD TYPE                            
         MVI   BYRKSUB,BYRKSUB2    BUYER RECORD SUB-TYPE                        
         MVC   BYRKAM,BAGYMD                                                    
         PACK  DUB,MRKL1           CONVERT MARKET TO BINARY                     
         CVB   R1,DUB              START AT 1ST MARKET ON SCREEN                
         STCM  R1,3,BYRKMKT2                                                    
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                READ FORWARD TO FIRST RECORD                 
*                                                                               
         LA    R2,MRKL1H           POINT TO FIRST SCREEN LINE                   
*                                                                               
DR110    CLC   8(4,R2),=C'    '    TEST NO MORE MARKETS                         
         BNH   DR190                                                            
*                                                                               
         PACK  DUB,8(4,R2)         CONVERT MARKET TO BINARY                     
         CVB   R1,DUB                                                           
         STCM  R1,3,MYBMKT                                                      
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         ZIC   R0,0(R2)            BUMP TO ASSIGNMENT FIELD                     
         AR    R2,R0                                                            
         OI    4(R2),X'20'         SET VALIDITY BIT                             
*                                                                               
DR112    CLC   KEY(3),KEYSAVE      TEST SAME AGENCY                             
         BNE   DR150                                                            
         CLC   MYBMKT,BYRKMKT2     TEST SAME MARKET AS THIS SCREEN LINE         
         BE    DR115               IF SCREEN > ACTUAL ==>  DELETED MKT          
         BL    DR150               NO MKT ASSIGNMENTS FOR THIS MKT              
         MVC   KEY,KEYSAVE                                                      
         MVC   BYRKMKT2,MYBMKT                                                  
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         B     DR112                                                            
*                                                                               
DR115    DS    0H                                                               
*                                  DISPLAY CURRENT BUYER                        
         ZICM  R1,BYRKDAT2,3                                                    
         SLL   R1,4                                                             
         ST    R1,FULL                                                          
         OI    FULL+3,X'0F'                                                     
         SP    FULL(4),=P'999999'  UNCOMPLEMENT DATE                            
         OI    FULL+3,X'0F'                                                     
*                                  DISPLAY CURRENT BUYER                        
         GOTO1 DATCON,DMCB,(6,FULL),(11,8(R2))                                  
         MVI   16(R2),C'='                                                      
         MVC   17(L'BYRKBUY2,R2),BYRKBUY2                                       
         ZIC   R0,0(R2)                                                         
         AR    R2,R0               BUMP TO NEXT FIELD                           
*                                                                               
         GOTO1 SEQ                                                              
         CLC   KEY(3),KEYSAVE      IF PREVIOUS BUYER EXISTS                     
         BNE   DR155                                                            
         CLC   MYBMKT,BYRKMKT2     TEST SAME MARKET AS THIS SCREEN LINE         
         BNE   DR155                                                            
*                                  DISPLAY PREVIOUS BUYER                       
         MVC   8(L'BYRKBUY2,R2),BYRKBUY2                                        
         B     DR155                                                            
*                                                                               
DR150    ZIC   R0,0(R2)            BUMP TO NEXT LINE                            
         AR    R2,R0                                                            
DR155    ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
*                                                                               
DR160    DS    0H                                                               
         LA    RF,MRKEND1H                                                      
         CR    R2,RF               TEST END OF SCREEN                           
         BL    DR110                                                            
         MVC   ENDMKT,MYBMKT       SAVE LAST MARKET                             
         B     DRX                                                              
*                                                                               
DR190    XC    ENDMKT,ENDMKT       SET LAST MARKET TO ZERO                      
*                                                                               
DRX      DS    0H                                                               
         LA    RF,GETTXTCB                                                      
         USING GETTXTD,RF                                                       
         XC    GTBLOCK,GTBLOCK                                                  
         MVI   GTMSGNO1,2          'PLEASE ENTER FIELDS AS REQUIRED'            
         CLI   KEYCHANG,C'Y'                                                    
         BE    EXIT1                                                            
         MVI   GTMSGNO1,17         'ACTION COMPLETED - ENTER NEXT'              
EXIT1    MVI   GTMSYS,GTGENSYS                                                  
         DROP  RF                                                               
         OI    GENSTAT2,USMYOK+USGETTXT                                         
         LA    RF,MRKCEL1H                                                      
         ST    RF,ACURFORC                                                      
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
RELO     DS    A                                                                
*                                                                               
* ERROR MESSAGES                                                                
*                                                                               
MISSFLD  MVI   GERROR1,MISSING                                                  
         B     ERREXIT                                                          
*                                                                               
INVLFLD  MVI   GERROR1,INVALID                                                  
         B     ERREXIT                                                          
*                                                                               
INVLBYR  MVI   GERROR1,BYRCURR     BUYER ALREADY CURRENT                        
         B     ERREXIT                                                          
*                                                                               
ERREXIT  GOTO1 MYERR                                                            
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
MKTRECD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
         EJECT                                                                  
       ++INCLUDE SPADDSECTS                                                     
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDFLDIND                                                       
       ++INCLUDE FATIOB                                                         
       ++INCLUDE FAGETTXTD                                                      
       ++INCLUDE SPADDFFD                                                       
         PRINT ON                                                               
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPADDF2D                                                       
         EJECT                                                                  
       ++INCLUDE DDPERVALD                                                      
         EJECT                                                                  
       ++INCLUDE SPADDWORKD                                                     
         EJECT                                                                  
* WORK AREA                                                                     
*                                                                               
MYAREAD  DSECT                                                                  
KEYCHANG DS    C                                                                
RECCHANG DS    C                                                                
RECFOUND DS    C                                                                
MYBMKT   DS    H                                                                
STARTMKT DS    H                                                                
ENDMKT   DS    H                                                                
PREVBYR  DS    CL3                                                              
PREVDATE DS    XL3                                                              
NEWBYR   DS    CL3                                                              
THISDATE DS    CL6                                                              
NEWDATE  DS    CL3                                                              
DISKADD  DS    XL4                                                              
PREVKEY  DS    XL48                                                             
MYSVKEY  DS    XL48                                                             
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002SPADD02   07/23/91'                                      
         END                                                                    
