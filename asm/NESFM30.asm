*          DATA SET NESFM30    AT LEVEL 049 AS OF 04/18/18                      
*PHASE T31C30A                                                                  
***********************************************************************         
*                                                                               
*  TITLE: T31C30 - MAINTENANCE/LIST OF CLIENT GROUP (CGROUP)                    
*                                                                               
*  COMMENTS: MAINTAINS CLIENT GROUP ASSIGNMENTS                                 
*                                                                               
*  CALLED FROM: NET SFM CONTROLLER (T31C00), WHICH CALLS                        
*               DDGENCON (T00A30) WHICH CALLS THIS.                             
*                                                                               
*  CALLS TO:    DATAMGR                                                         
*                                                                               
*  INPUTS: SCREENS NESFMDC (T31CDC) -- MAINTENANCE                              
*                  NESFMDD (T31CDD) -- LIST                                     
*                                                                               
*  OUTPUTS: UPDATED OR NEW BUYERS                                               
*                                                                               
*  LOCALS: REGISTER USAGE                                                       
*          R0 - WORK                                                            
*          R1 - WORK                                                            
*          R2 - POINTER TO SCREEN FLDH, ERREX WILL POSITION CURSOR              
*          R3 - WORK                                                            
*          R4 - WORK                                                            
*          R5 - WORK                                                            
*          R6 - USED FOR GETEL ELEMENT DSECT POINTER                            
*          R7 - WORK                                                            
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
         TITLE 'NESFM30 - CLIENT GROUP ASSIGNMENTS'                             
T31C30   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,NETCGA                                                         
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING T31CFFD,RA                                                       
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LR                                                               
         CLI   MODE,PRINTREP       PRINT RECORDS                                
         BE    PR                                                               
         CLI   MODE,RECDEL         DELETE RECORDS                               
         BE    DELERR                                                           
         CLI   MODE,XRECADD        AFTER RECORD HAS BEEN ADDED                  
         BE    XU                                                               
         CLI   MODE,XRECPUT        AFTER RECORD HAS BEEN PUT                    
         BE    XU                                                               
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* VALIDATE KEY                                                                  
***********************************************************************         
VK       DS    0H                                                               
         XC    SVKEY,SVKEY                                                      
*                                                                               
         GOTO1 GETFACT,DMCB,0      GET SECURITY ALPHA                           
         USING FACTSD,R3                                                        
         L     R3,DMCB                                                          
         MVC   SECALPHA,FATAGYSC   SAVE SECURITY AGENCY                         
         MVC   PIDNUM,FAPASSWD     2 CHARACTER PID                              
         DROP  R3                                                               
*                                                                               
         LA    R3,SVKEY                                                         
         USING CLGRECD,R3                                                       
         MVC   CLGKTYP,=X'0D06'    RECORD ID                                    
*                                                                               
         LA    R2,SFMMEDH                                                       
         TM    4(R2),X'20'         VALIDATED PREVIOUSLY??                       
         BNZ   VK03                                                             
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
         GOTO1 VALIMED                                                          
         OI    4(R2),X'20'         VALIDATED                                    
VK03     MVC   CLGKAGMD,BAGYMD                                                  
*                                                                               
         LA    R2,SFMIDH                                                        
         CLI   ACTNUM,ACTLIST      OPTIONAL FOR LIST OR REPORT                  
         BE    *+12                                                             
         CLI   ACTNUM,ACTREP                                                    
         BNE   VK05                                                             
*                                                                               
         MVI   CLGKID,0                                                         
         MVI   FILTID,0                                                         
         MVI   IDFILTER,C'N'       NO LIST FILTER ON ID                         
         CLI   5(R2),0                                                          
         BE    VKX                                                              
         MVI   IDFILTER,C'Y'       LIST FILTER ON ID                            
         B     VK06                                                             
*                                                                               
VK05     DS    0H                                                               
         CLI   5(R2),0             MUST BE THERE IF NOT LIST                    
         BE    MISSFLD                                                          
*                                                                               
VK06     DS    0H                                                               
         NI    MYFLAG,X'FF'-ID2                                                 
         MVC   DUB(1),8(R2)                                                     
         CLI   9(R2),C' '          CHECK IF 1 CHARACTER ID                      
         BE    VK07                                                             
         CLI   9(R2),C'A'                                                       
         BL    VK07                                                             
         CLI   9(R2),C'Z'                                                       
         BH    VK07                                                             
         OI    MYFLAG,ID2                                                       
         MVC   DUB(2),8(R2)                                                     
*                                                                               
VK07     OI    DUB+1,C' '                                                       
         BAS   RE,TRANS21          GET 1 BYTE EQUATE                            
         MVC   CLGKID,BYTE                                                      
         MVC   FILTID,BYTE                                                      
*                                                                               
VK08     DS    0H                                                               
         MVC   KEY,SVKEY                                                        
         GOTO1 HIGH                                                             
         CLC   KEY(L'CLGKEY),KEYSAVE   GET GROUP DEFINITION RECORD              
         BNE   CDEFERR             NOT A GROUP DEFINITION RECORD                
*                                                                               
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         USING CLGDESD,R6                                                       
         MVI   ELCODE,CLGDESQ      GET GROUP BREAK DESCRIPTION ELEMENT          
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
*                                                                               
         ZIC   R1,CLGBK1LN         CHECK TO SEE IF TOTAL BREAK LENGTH           
         ZIC   R4,CLGBK2LN           IS EQUAL TO ID LENGTH                      
         AR    R1,R4                                                            
*                                                                               
         LA    R1,1(R1)            BUMP TO COMPENSATE FOR LETTER CODE           
         TM    MYFLAG,ID2                                                       
         BZ    *+8                                                              
         LA    R1,1(R1)            2 CHARACTER ID                               
*                                                                               
VK09     MVC   BYTE,5(R2)          IN ID                                        
         STC   R1,WORK                                                          
         CLC   BYTE,WORK                                                        
         BE    VK10                                                             
         CLI   ACTNUM,ACTLIST      LIST MIGHT BE FILTERING                      
         BE    VK10                                                             
         CLI   ACTNUM,ACTREP       REPORT MIGHT BE FILTERING                    
         BNE   INVLFLD                                                          
*                                                                               
VK10     XC    WORK,WORK                                                        
*                                                                               
         MVC   WORK(3),9(R2)       LEFT ALIGNED PWOS                            
         TM    MYFLAG,ID2                                                       
         BZ    *+10                                                             
         MVC   WORK(3),10(R2)                                                   
*                                                                               
         PACK  WORK+10(3),WORK(5)                                               
         MVC   CLGKGRP,WORK+10                                                  
*                                                                               
         CLI   ACTNUM,ACTADD       TEST ADD                                     
         BNE   VKX                                                              
*                                                                               
         MVC   SFMBK1(12),CLGBK1                                                
         OI    SFMBK1H+6,X'80'     XMIT                                         
*                                                                               
         MVC   SFMBK2(12),CLGBK2                                                
         OI    SFMBK2H+6,X'80'     XMIT                                         
*                                                                               
         MVC   SVBKLNS+0(1),CLGBK1LN                                            
         MVC   SVBKLNS+1(1),CLGBK2LN                                            
         MVC   SVBKLNS+2(1),CLGBK3LN                                            
*                                                                               
VKX      MVC   KEY,SVKEY                                                        
         B     EXIT                                                             
         DROP  R3,R6                                                            
         EJECT                                                                  
*                                                                               
TRANS21  DS    0H                                                               
         MVI   BYTE,0                                                           
         LA    R1,SPCGRTAB                                                      
         LHI   R0,(SPCGRTBX-SPCGRTAB)/3                                         
*                                                                               
TRANS21A CLC   DUB(2),0(R1)                                                     
         BNE   *+12                                                             
         MVC   BYTE,2(R1)                                                       
         BR    RE                                                               
*                                                                               
         LA    R1,3(R1)                                                         
         BCT   R0,TRANS21A                                                      
         B     INVID                                                            
*                                                                               
***********************************************************************         
* VALIDATE RECORD                                                               
***********************************************************************         
VR       DS    0H                                                               
         BRAS  RE,VRRTN                                                         
         B     EXIT                                                             
***********************************************************************         
* AFTER RECORD ADDED/PUT, UPDATE CLIENT RECORDS/PASSIVE KEYS                    
***********************************************************************         
XU       DS    0H                  UPDATE CLIENT RECORD                         
         LA    R2,SFMCLT1H                                                      
         B     XU40                                                             
*                                                                               
XU10     DS    0H                  VALIDATE CLT ON FILE                         
         OI    4(R2),X'20'         SET VALID                                    
         XC    KEY,KEY             BUILD CLIENT KEY                             
         LA    R3,KEY                                                           
         USING CLTRECD,R3                                                       
         MVC   CKEYAM,SAVEKEY+2    MOVE AGY/MED                                 
*                                                                               
         NI    MYFLAG,X'FF'-DELCLT                                              
*                                                                               
         MVC   QCLT,8(R2)                                                       
         CLI   8(R2),C'-'                                                       
         BNE   XU10A                                                            
****     CLI   T31CFFD+1,C'*'        DDS ONLY                                   
****     BNE   INVLFLD                                                          
         MVC   QCLT,9(R2)                                                       
         OI    MYFLAG,DELCLT                                                    
*                                                                               
XU10A    OC    QCLT,SPACES                                                      
         GOTO1 CLPACK,DMCB,QCLT,BCLT                                            
         CLI   0(R1),0                                                          
         BNE   INVLCLI                                                          
         MVC   CKEYCLT,BCLT        MOVE PACKED CLIENT CODE                      
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(L'CKEY),KEYSAVE                                              
         BNE   INVLCLI                                                          
*                                                                               
         MVC   DASAVE,KEY+14       SAVE CLTHDR DISK ADDRESS                     
         DROP  R3                                                               
*                                                                               
*        MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         TM    MYFLAG,DELCLT       DELETE THIS CLIENT?                          
         BZ    XU13                                                             
*                                                                               
         L     R3,AIO                                                           
         USING CLTRECD,R3                                                       
         XC    ELEM,ELEM           PUT ALL CGROUP ASSGNS IN ELEM+50             
*                                                                               
         LA    R6,ELEM+50          ONE LEVEL NOW                                
         LA    R5,CGRP1                                                         
         LA    RF,10                                                            
*                                                                               
XU10B    DS    0H                  FILL IN ALL 10 CLIENT GROUPS IN ELEM         
         OC    0(3,R5),0(R5)                                                    
         BZ    XU10X                                                            
         MVC   0(3,R6),0(R5)                                                    
*                                                                               
         LA    RE,CGRP5                                                         
         CR    RE,R5               FINISHED FIRST 5 CLIENT GROUPS?              
         BNE   XU10C                                                            
         LA    R5,CGRP6            YES, CHECK NEXT 5 CLIENT GROUPS              
         B     *+8                                                              
*                                                                               
XU10C    LA    R5,3(R5)                                                         
         LA    R6,3(R6)                                                         
         BCT   RF,XU10B                                                         
*                                                                               
XU10X    DS    0H                                                               
         LA    R6,ELEM             REBUILD CLIENT GROUP W/O DELETED             
         LA    R5,ELEM+50          CLIENT IN ELEM                               
         LA    RF,10                                                            
*                                                                               
XU11     DS    0H                                                               
         CLC   0(3,R5),SAVEKEY+3   REMOVE THIS CLIENT?                          
         BE    *+14                                                             
         MVC   0(3,R6),0(R5)       NO                                           
         LA    R6,3(R6)                                                         
         LA    R5,3(R5)            BUMP TO NEXT CGROUP                          
         BCT   RF,XU11                                                          
         B     XU18A                                                            
*                                                                               
XU13     DS    0H                                                               
         L     R3,AIO                                                           
         USING CLTRECD,R3                                                       
         XC    ELEM,ELEM           PUT ALL CGROUP ASSGNS IN ELEM                
*                                                                               
         LA    R6,ELEM             ONE LEVEL NOW                                
         LA    R5,CGRP1                                                         
         LA    RF,10                                                            
*                                                                               
XU13A    DS    0H                  FILL IN ALL 10 CLIENT GROUPS IN ELEM         
         OC    0(3,R5),0(R5)                                                    
         BZ    XU13X                                                            
         MVC   0(3,R6),0(R5)                                                    
*                                                                               
         LA    RE,CGRP5                                                         
         CR    RE,R5               FINISHED FIRST 5 CLIENT GROUPS?              
         BNE   XU13C                                                            
         LA    R5,CGRP6            YES, CHECK NEXT 5 CLIENT GROUPS              
         B     *+8                                                              
*                                                                               
XU13C    LA    R5,3(R5)                                                         
         LA    R6,3(R6)                                                         
         BCT   RF,XU13A                                                         
*                                                                               
XU13X    DS    0H                                                               
         LA    R6,ELEM             CHECK IF THERE'S A FREE ASSIGNMENT           
         LA    RF,10                                                            
*                                                                               
XU14     DS    0H                                                               
         XC    GROUP,GROUP                                                      
         CLC   0(1,R6),SAVEKEY+3   IF CLT ALREADY ASSIGNED TO ANOTHER           
         BNE   XU16                GRP WITH SAME SCHEME, MOVE CLIENT TO         
         MVC   GROUP,0(R6)         THIS SCHEME                                  
         B     XU18                                                             
*                                                                               
XU16     OC    0(3,R6),0(R6)       ROOM TO ADD NEW ASSIGNMENT??                 
         BZ    XU18                                                             
         LA    R6,3(R6)                                                         
         BCT   RF,XU14                                                          
         B     CFULLERR            CLIENT FULL, NO ROOM TO ADD                  
*                                                                               
XU18     MVC   0(3,R6),SAVEKEY+3   MOVE NEW CLTGRP INTO ELEM                    
XU18A    MVC   CGRP1(15),ELEM       RESTORE CGROUP ASSGNS (MAX 10)              
         MVC   CGRP6(15),ELEM+15                                                
         GOTO1 DATAMGR,DMCB,=C'PUTREC',=CL8'SPTFILE',KEY+14,AIO,MYDMWRK         
*        GOTO1 PUTREC                                                           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R3                                                               
*                                                                               
         XC    KEY,KEY             ADD/RESTORE PASSIVE KEY                      
         LA    R3,KEY                                                           
         USING CLGRECD,R3                                                       
         MVC   CLGCTYP,=X'0D86'                                                 
         MVC   CLGCAGMD,SAVEKEY+2                                               
         MVC   CLGCID(3),SAVEKEY+3                                              
         MVC   CLGCCLT(2),BCLT     MOVE IN CLIENT CODE                          
*                                  SEE IF ON FILE                               
         OI    DMINBTS,X'08'       PASS DELETES                                 
*        MVI   RDUPDATE,C'Y'                                                    
         GOTO1 HIGH                                                             
         MVI   DMINBTS,0           RESET                                        
*                                                                               
         CLC   KEY(13),KEYSAVE                                                  
         BNE   XU20                                                             
*                                                                               
         NI    KEY+13,X'FF'-X'80'  RESTORE DELETED PASSIVE KEY                  
*                                                                               
         TM    MYFLAG,DELCLT       DELETE THIS CLIENT?                          
         BZ    *+8                                                              
         OI    KEY+13,X'80'                                                     
*                                                                               
         MVC   KEY+14(4),DASAVE    INSERT DISK ADDR                             
         GOTO1 WRITE                                                            
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         B     XU25                                                             
*                                                                               
XU20     MVC   KEY,KEYSAVE         RESTORE KEY                                  
         MVC   KEY+14(4),DASAVE    INSERT DISK ADDR                             
         GOTO1 ADD                 ADD NEW PASSIVE POINTER                      
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
XU25     DS    0H                                                               
         OC    GROUP,GROUP         ARE WE MOVING THE CLIENT FROM                
         BZ    XU30                ONE GROUP TO ANOTHER??                       
         CLC   GROUP(3),SAVEKEY+3  SAME GROUP                                   
         BE    XU30                                                             
         XC    KEY,KEY             DELETE PASSIVE KEY FROM THE GROUP            
         LA    R3,KEY              WHICH PRODUCT HAS MOVED FROM                 
         USING CLGRECD,R3                                                       
         MVC   CLGCTYP,=X'0D86'                                                 
         MVC   CLGCAGMD,SAVEKEY+2                                               
         MVC   CLGCID(3),GROUP                                                  
         MVC   CLGCCLT(2),BCLT     MOVE IN CLIENT CODE                          
*                                  SEE IF ON FILE                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     NOT THERE, SO LEAVE                          
         BNE   XU30                                                             
         OI    KEY+13,X'80'        ELSE DELETE PASSIVE KEY                      
         GOTO1 WRITE                                                            
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
XU30     ZIC   R0,0(R2)            BUMP TO NEXT FIELD                           
         AR    R2,R0                                                            
         LA    RF,SFMCLTXH                                                      
         CR    R2,RF               LAST INPUT FIELD??                           
         BH    XUX                                                              
*                                                                               
XU40     CLI   5(R2),0             TEST DATA                                    
         BE    XU30                NO-TRY NEXT                                  
         TM    4(R2),X'20'         TEST PREVIOUSLY VALIDATED                    
         BO    XU30                                                             
         B     XU10                GO EDIT                                      
*                                                                               
*XUX      B     EXIT                                                            
XUX      MVC   KEY,SAVEKEY                                                      
         B     DR                                                               
         EJECT                                                                  
***********************************************************************         
* DISPLAY KEY                                                                   
***********************************************************************         
DK       DS    0H                  DISPLAY KEY                                  
         L     R3,AIO                                                           
         MVC   KEY(L'CLGKEY),0(R3)                                              
         MVC   SAVEKEY,KEY                                                      
         LA    R3,KEY                                                           
         USING CLGRECD,R3                                                       
         MVI   SFMMED,C'N'          MEDIA FOR NOW                               
         OI    SFMMEDH+6,X'80'      XMIT                                        
*                                                                               
         MVC   BYTE,CLGKID                                                      
         BAS   RE,TRANS12                                                       
         MVC   SFMID(2),DUB                                                     
*                                                                               
         XC    FULL,FULL                                                        
         MVC   FULL(2),CLGKGRP                                                  
         UNPK  WORK(5),FULL(3)                                                  
*                                                                               
         XC    CLGKGRP,CLGKGRP     GET DEFINITION RECORD                        
         GOTO1 HIGH                                                             
         CLC   KEY(L'CLGKEY),KEYSAVE                                            
         BNE   CDEFERR                                                          
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         USING CLGDESD,R6                                                       
         MVI   ELCODE,CLGDESQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         ZIC   R1,CLGBK1LN                                                      
         ZIC   R4,CLGBK2LN                                                      
         AR    R1,R4               GET LENGTH OF ID                             
         STC   R1,IDLEN            SO WE CAN DISPLAY ID CORRECTLY               
*                                                                               
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   DUB(0),WORK                                                      
*                                                                               
         OI    SFMIDH+6,X'80'      XMIT                                         
*                                                                               
         CLI   SFMID+1,C' '                                                     
         BNE   *+14                                                             
         MVC   SFMID+1(3),DUB                                                   
         B     DKX                                                              
*                                                                               
         MVC   SFMID+2(3),DUB                                                   
*                                                                               
DKX      MVC   KEY,SAVEKEY                                                      
         B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
*                                                                               
TRANS12  DS    0H                                                               
         XC    DUB,DUB                                                          
         LA    R1,SPCGRTAB                                                      
         LHI   R0,(SPCGRTBX-SPCGRTAB)/3                                         
*                                                                               
TRANS12A CLC   BYTE,2(R1)                                                       
         BNE   *+12                                                             
         MVC   DUB(2),0(R1)                                                     
         BR    RE                                                               
*                                                                               
         LA    R1,3(R1)                                                         
         BCT   R0,TRANS12A                                                      
         B     INVID                                                            
***********************************************************************         
* DISPLAY RECORD                                                                
***********************************************************************         
DR       DS    0H                                                               
         CLI   ACTNUM,ACTSEL                                                    
         BNE   DR05                                                             
         MVC   PREVKEY,KEY                                                      
         MVI   PREVFLAG,C'Y'                                                    
*                                                                               
DR05     MVC   SAVEKEY,KEY                                                      
         LA    R3,KEY                                                           
         USING CLGRECD,R3                                                       
         XC    CLGKGRP,CLGKGRP     GET DEFINITION RECORD                        
         GOTO1 HIGH                                                             
         CLC   KEY(L'CLGKEY),KEYSAVE                                            
         BNE   CDEFERR                                                          
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         USING CLGDESD,R6                                                       
         MVI   ELCODE,CLGDESQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   SFMBK1(12),CLGBK1                                                
         OI    SFMBK1H+6,X'80'     XMIT                                         
*                                                                               
         MVC   SFMBK2(12),CLGBK2                                                
         OI    SFMBK2H+6,X'80'     XMIT                                         
*                                                                               
         MVC   SVBKLNS+0(1),CLGBK1LN                                            
         MVC   SVBKLNS+1(1),CLGBK2LN                                            
         MVC   SVBKLNS+2(1),CLGBK3LN                                            
         DROP  R6                                                               
*                                                                               
         CLI   ACTNUM,ACTADD                                                    
         BE    DR10                                                             
*                                                                               
         XC    SFMNM1,SFMNM1                                                    
         OI    SFMNM1H+6,X'80'     XMIT                                         
*                                                                               
         XC    SFMNM2,SFMNM2                                                    
         OI    SFMNM2H+6,X'80'     XMIT                                         
*                                                                               
         MVC   KEY,SAVEKEY                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(L'CLGKEY),KEYSAVE                                            
         BNE   CASSERR                                                          
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         USING CLGMEMD,R6                                                       
         MVI   ELCODE,CLGMEMQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   SFMNM1,CLGNAM1                                                   
         OI    SFMNM1H+6,X'80'     XMIT                                         
*                                                                               
         MVC   SFMNM2,CLGNAM2                                                   
         OI    SFMNM2H+6,X'80'     XMIT                                         
         DROP  R6                                                               
*                                                                               
DR10     TWAXC SFMCLT1H,SFMCLTXH   CLEAR ADDED CLIENT FIELDS                    
         TWAXC SFMLST1H,PROT=Y     CLEAR CLIENT LIST                            
*                                                                               
         XC    KEY,KEY             READ PASSIVE KEY AND DISPLAY LIST            
         MVC   CLGCTYP,=X'0D86'                                                 
         MVC   CLGCAGMD,SAVEKEY+2    AGY/MED                                    
         MVC   CLGCID(3),SAVEKEY+3   GROUP ID                                   
*                                                                               
         MVC   FULL,AIO            SAVE AIO VALUE                               
         GOTO1 HIGH                                                             
*                                                                               
         LA    R2,SFMLST1H                                                      
*                                                                               
         LA    R4,SFMLST1                                                       
         ZAP   HALF,=P'18'         SET FOR 18 CLTS PER LINE                     
         B     DR40                                                             
*                                                                               
DR30     GOTO1 SEQ                                                              
*                                                                               
DR40     CLC   KEY(8),KEYSAVE      TEST SAME THRU CLTGRP                        
         BNE   DR50                                                             
*                                                                               
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         USING CLTRECD,R6                                                       
*                                                                               
         GOTO1 CLUNPK,DMCB,(CPROF+6,CLGCCLT),0(R4)                              
         LA    R4,4(R4)                                                         
         SP    HALF,=P'1'          ADJUST COUNTER                               
         BP    DR30                CONTINUE IF MORE ROOM                        
         OI    6(R2),X'80'         XMIT LINE                                    
*                                                                               
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0             TEST E-O-S                                   
         BE    DR50                                                             
         LA    R4,8(R2)                                                         
         ZAP   HALF,=P'18'                                                      
         B     DR30                                                             
*                                                                               
DR50     OI    6(R2),X'80'         XMIT LINE                                    
         MVC   KEY,SAVEKEY                                                      
         MVC   AIO,FULL            RESET AIO                                    
         DROP  R6                                                               
*                                                                               
         CLI   ACTNUM,ACTADD                                                    
         BNE   DRX                                                              
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(54),=C'RECORD ADDED, ENTER MORE CLIENTS OR ENTERX        
                NEXT REQUEST'                                                   
         OI    CONHEADH+6,X'80'    XMIT                                         
         XC    CONACT,CONACT                                                    
         MVC   CONACT(3),=C'CHA'                                                
         OI    CONACT+6,X'80'      XMIT                                         
         LA    R2,SFMCLT1H                                                      
         GOTO1 ERREX2                                                           
*                                                                               
DRX      B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
*   LIST RECORDS                                                                
***********************************************************************         
LR       DS    0H                                                               
         LA    R3,KEY                                                           
         USING CLGRECD,R3                                                       
         LA    R5,LISTAR                                                        
         USING PLINED,R5                                                        
*                                                                               
LR02     MVI   NLISTS,14           SET NUMBER OF LIST LINES                     
         CLI   PREVFLAG,C'Y'                                                    
         BE    LR05                                                             
         OC    KEY,KEY                                                          
         BNZ   LR08                                                             
         MVC   KEY(4),SVKEY                                                     
         B     LR08                                                             
*                                                                               
LR05     MVC   KEY,PREVKEY                                                      
         MVI   PREVFLAG,C'N'                                                    
*                                                                               
LR08     GOTO1 HIGH                                                             
         B     LR10                                                             
*                                                                               
LRSEQ    GOTO1 SEQ                                                              
*                                                                               
LR10     CLC   KEY(3),SVKEY                                                     
         BNE   LRX                                                              
         OC    KEY+3(2),KEY+3      NEED THIS FOR KEY CHANGE                     
         BZ    LRSEQ                                                            
         CLI   FILTID,0            ANY FILTER ID?                               
         BE    LR13                                                             
         CLC   CLGKID,FILTID                                                    
         BNE   LRSEQ                                                            
         CLI   IDFILTER,C'N'                                                    
         BE    LR13                                                             
         OC    CLGKGRP(9),CLGKGRP  CHK IF DEFINITION RECORD                     
         BNZ   LRSEQ                                                            
         MVI   IDFILTER,C'N'                                                    
         B     LR14                                                             
*                                                                               
LR13     OC    CLGKGRP(9),CLGKGRP  CHK IF DEFINITION RECORD                     
         BNZ   LR15                                                             
*                                                                               
LR14     DS    0H                                                               
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         USING CLGDESD,R6                                                       
         MVI   ELCODE,CLGDESQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         ZIC   R1,CLGBK1LN                                                      
         ZIC   R4,CLGBK2LN                                                      
         DROP  R6                                                               
         AR    R1,R4                                                            
         STC   R1,IDLEN                                                         
         B     LRSEQ                                                            
*                                                                               
LR15     DS    0H                                                               
         CLI   SFMIDH+5,1          FILTER ON MORE THAN LETTER                   
         BNH   LR20                                                             
         TM    MYFLAG,ID2                                                       
         BO    LR20                                                             
         CLI   FILTID,0                                                         
         BE    LR20                                                             
         CLC   CLGKID,FILTID                                                    
         BNE   LRSEQ                                                            
         XC    FULL,FULL                                                        
         MVC   FULL(2),CLGKGRP                                                  
         UNPK  WORK(5),FULL(3)     YES/UNPACK ID FIELD                          
*                                                                               
         ZIC   R1,SFMIDH+5         GET LENGTH OF COMPARE                        
         BCTR  R1,0                                                             
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   WORK(0),SFMID+1                                                  
         BNE   LRSEQ                                                            
*                                                                               
LR20     GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         USING CLGMEMD,R6                                                       
         MVI   ELCODE,CLGMEMQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   LISTAR,SPACES                                                    
*                                                                               
         MVC   BYTE,CLGKID                                                      
         BAS   RE,TRANS12                                                       
         MVC   PLGRPID(2),DUB                                                   
*                                                                               
         XC    FULL,FULL                                                        
         MVC   FULL(2),CLGKGRP                                                  
         UNPK  WORK(5),FULL(3)                                                  
         ZIC   R1,IDLEN                                                         
*                                                                               
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   DUB(0),WORK                                                      
*                                                                               
         OI    SFMIDH+6,X'80'      XMIT                                         
*                                                                               
         CLI   PLGRPID+1,C' '                                                   
         BNE   *+14                                                             
         MVC   PLGRPID+1(3),DUB                                                 
         B     *+10                                                             
         MVC   PLGRPID+2(3),DUB                                                 
*                                                                               
         MVC   PLLEV1,CLGNAM1                                                   
         MVC   PLLEV2,CLGNAM2                                                   
         GOTO1 LISTMON                                                          
         B     LRSEQ                                                            
*                                                                               
LRX      DS    0H                                                               
         B     EXIT                                                             
         DROP  R3,R5                                                            
         EJECT                                                                  
***********************************************************************         
* PRINT REPORT                                                                  
***********************************************************************         
PR       DS    0H                                                               
         LA    R1,HEADING                                                       
         ST    R1,SPECS                                                         
         LA    R1,HHOOK                                                         
         ST    R1,HEADHOOK                                                      
*                                                                               
         LA    R3,KEY                                                           
         USING CLGRECD,R3                                                       
         LA    R5,P                                                             
         USING RLINED,R5                                                        
         XC    KEY,KEY                                                          
         MVC   KEY(3),SVKEY                                                     
         MVI   PLFLAG,C'N'                                                      
*                                                                               
PR08     GOTO1 HIGH                                                             
         B     PR10                                                             
*                                                                               
PRSEQ    GOTO1 SEQ                                                              
*                                                                               
PR10     CLC   KEY(3),SVKEY                                                     
         BNE   PR80                                                             
         CLI   IDFILTER,C'N'                                                    
         BE    PR12                                                             
         CLC   CLGKID,FILTID                                                    
         BNE   PRSEQ                                                            
         OC    CLGKGRP(9),CLGKGRP  CHK IF DEFINITION RECORD                     
         BNZ   PRSEQ                                                            
         MVI   IDFILTER,C'N'                                                    
         B     PR13                                                             
*                                                                               
PR12     OC    CLGKGRP(9),CLGKGRP  CHK IF DEFINITION RECORD                     
         BNZ   PR20                                                             
*                                                                               
PR13     DS    0H                                                               
         CLI   PLFLAG,C'N'                                                      
         BE    PR14                                                             
         OC    ABOX,ABOX           PRINT BOTTOM OF BOX                          
         BZ    PR15                                                             
         L     R7,ABOX                                                          
         USING BOXD,R7                                                          
         ZIC   R1,LINE                                                          
         LA    R1,BOXROWS-1(R1)                                                 
         MVI   0(R1),C'B'                                                       
         MVI   BOXINIT,0                                                        
         DROP  R7                                                               
         BAS   RE,PRTLINE                                                       
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
PR14     DS    0H                                                               
         MVC   H2+60(1),CLGKID                                                  
*                                                                               
PR15     DS    0H                                                               
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         USING CLGDESD,R6                                                       
         MVI   ELCODE,CLGDESQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         ZIC   R1,CLGBK1LN                                                      
         ZIC   R4,CLGBK2LN                                                      
         AR    R1,R4                                                            
         STC   R1,IDLEN                                                         
*                                                                               
         BAS   RE,PRTLINE                                                       
         BAS   RE,PRTLINE                                                       
         MVC   RCODE,=C'NO. '                                                   
         MVC   RBREAK1(12),CLGBK1                                               
*        MVI   P+19,C'('                                                        
*        EDIT  (B1,CLGBK1LN),(1,P+20)                                           
*        MVI   P+21,C')'                                                        
         CLI   CLGBK2LN,0                                                       
         BE    PR18                                                             
*                                                                               
         MVC   RBREAK2(12),CLGBK2                                               
*        MVI   P+44,C'('                                                        
*        EDIT  (B1,CLGBK2LN),(1,P+45)                                           
*        MVI   P+46,C')'                                                        
*                                                                               
PR18     MVC   RCLT(7),=C'CLIENTS'                                              
         MVC   SVRLINE,P           SAVE REPORT HEADER                           
         BAS   RE,PRTLINE                                                       
         MVI   PLFLAG,C'Y'                                                      
         B     PRSEQ                                                            
*                                                                               
PR20     DS    0H                                                               
         CLI   SFMIDH+5,1          FILTER ON MORE THAN LETTER                   
         BNH   PR30                                                             
         CLC   CLGKID,SFMID                                                     
         BNE   PRSEQ                                                            
         XC    FULL,FULL                                                        
         MVC   FULL(2),CLGKGRP                                                  
         UNPK  WORK(5),FULL(3)     YES/UNPACK ID FIELD                          
         ZIC   R1,SFMIDH+5         GET LENGTH OF COMPARE                        
         BCTR  R1,0                                                             
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   WORK(0),SFMID+1                                                  
         BNE   PRSEQ                                                            
*                                                                               
PR30     DS    0H                                                               
         CLI   LINE,55                                                          
         BL    PRML                                                             
*                                                                               
         OC    ABOX,ABOX           PRINT BOTTOM OF BOX                          
         BZ    PR35                                                             
         L     R7,ABOX                                                          
         USING BOXD,R7                                                          
         ZIC   R1,LINE                                                          
         LA    R1,BOXROWS-1(R1)                                                 
         MVI   0(R1),C'B'                                                       
         MVI   BOXINIT,0                                                        
         DROP  R7                                                               
         BAS   RE,PRTLINE                                                       
         MVI   FORCEHED,C'Y'                                                    
         MVC   H2+60(1),CLGKID                                                  
         BAS   RE,PRTLINE                                                       
         BAS   RE,PRTLINE                                                       
         MVC   P,SVRLINE                                                        
         BAS   RE,PRTLINE                                                       
*                                                                               
PRML     OC    ABOX,ABOX           PRINT MIDDLE OF BOX                          
         BZ    PR35                                                             
         L     R7,ABOX                                                          
         USING BOXD,R7                                                          
         ZIC   R1,LINE                                                          
         LA    R1,BOXROWS-1(R1)                                                 
         MVI   0(R1),C'M'                                                       
         MVI   BOXINIT,0                                                        
         DROP  R7                                                               
         BAS   RE,PRTLINE                                                       
*                                                                               
PR35     DS    0H                                                               
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         USING CLGMEMD,R6                                                       
         MVI   ELCODE,CLGMEMQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    FULL,FULL                                                        
         MVC   FULL(2),CLGKGRP                                                  
         UNPK  WORK(5),FULL(3)                                                  
         ZIC   R1,IDLEN                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   RCODE(0),WORK                                                    
         MVC   RBREAK1,CLGNAM1                                                  
         MVC   RBREAK2,CLGNAM2                                                  
*                                                                               
         MVC   SAVEKEY,KEY                                                      
         XC    KEY,KEY             READ PASSIVE KEY AND PRINT LIST              
         MVC   CLGCTYP,=X'0D86'                                                 
         MVC   CLGCAGMD,SAVEKEY+2    AGY/MED                                    
         MVC   CLGCID(3),SAVEKEY+3   GROUP ID                                   
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         LA    R4,RCLT                                                          
         ZAP   HALF,=P'18'         SET FOR 18 CLTS PER LINE                     
         MVI   PLFLAG,C'Y'                                                      
         B     PR50                                                             
*                                                                               
PR40     GOTO1 SEQ                                                              
*                                                                               
PR50     CLC   KEY(8),KEYSAVE      TEST SAME THRU CLTGRP                        
         BNE   PR60                                                             
         MVI   PLFLAG,C'Y'                                                      
*                                                                               
         GOTO1 CLUNPK,DMCB,CLGCCLT,0(R4)                                        
         LA    R4,4(R4)                                                         
         SP    HALF,=P'1'          ADJUST COUNTER                               
         BP    PR40                CONTINUE IF MORE ROOM                        
*                                                                               
         BAS   RE,PRTLINE                                                       
         ZAP   HALF,=P'18'         CAN FIT 18 CLIENT CODES PER LINE             
         LA    R4,RCLT                                                          
         MVI   PLFLAG,C'N'                                                      
         B     PR40                                                             
*                                                                               
PR60     DS    0H                                                               
         CLI   PLFLAG,C'N'                                                      
         BE    PR75                                                             
*                                                                               
PR70     DS    0H                                                               
         BAS   RE,PRTLINE                                                       
*                                                                               
PR75     MVI   PLFLAG,C'Y'                                                      
         MVC   KEY,SAVEKEY                                                      
         GOTO1 HIGH                                                             
         B     PRSEQ                                                            
*                                                                               
PR80     DS    0H                                                               
         OC    ABOX,ABOX           PRINT BOTTOM OF BOX                          
         BZ    PRX                                                              
         L     R7,ABOX                                                          
         USING BOXD,R7                                                          
         ZIC   R1,LINE                                                          
         LA    R1,BOXROWS-1(R1)                                                 
         MVI   0(R1),C'B'                                                       
         MVI   BOXINIT,0                                                        
         DROP  R7                                                               
         BAS   RE,PRTLINE                                                       
*                                                                               
PRX      B     EXIT                                                             
         DROP  R3,R5                                                            
         EJECT                                                                  
***********************************************************************         
* PRINT LINE                                                                    
***********************************************************************         
PRTLINE  NTR1                                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     EXIT                                                             
***********************************************************************         
* HEADING AND HEAD HOOK ROUTINE                                                 
***********************************************************************         
HEADING  DS    0H                                                               
         SSPEC H1,3,REQUESTOR                                                   
         SSPEC H1,46,C'NETWORK CLIENT GROUP RECORDS'                            
         SSPEC H2,58,C'ID'                                                      
         SSPEC H3,46,C'----------------------------'                            
         SSPEC H1,93,AGYNAME                                                    
         SSPEC H2,93,AGYADD                                                     
         SSPEC H3,93,REPORT                                                     
         SSPEC H4,93,RUN                                                        
         SSPEC H5,103,PAGE                                                      
         DC    X'00'                                                            
         EJECT                                                                  
HHOOK    NTR1                                                                   
         L     R3,ABOX             A(BOX DSECT)                                 
         LTR   R3,R3                                                            
         BZ    HHOOKX                                                           
         USING BOXD,R3                                                          
         MVC   BOXROWS,SPACES                                                   
         MVI   BOXROWS+7,C'T'                                                   
         MVC   BOXCOLS,SPACES                                                   
         MVI   BOXCOLS,C'L'                                                     
         MVI   BOXCOLS+5,C'C'                                                   
         MVI   BOXCOLS+30,C'C'                                                  
         MVI   BOXCOLS+55,C'C'                                                  
         MVI   BOXCOLS+131,C'R'                                                 
         MVI   BOXWT,1                                                          
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXINIT,0                                                        
         MVI   BOXOFF,0                                                         
         DROP  R3                                                               
HHOOKX   B     EXIT                                                             
*                                                                               
TRAPERR  GOTO1 ERREX                                                            
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
CTDISP   DC    Y(L'SAPEKEY+L'SAPELEN+L'SAPESTAT)                                
         GETEL2 R6,CTDISP,ELCODE                                                
*                                                                               
ALPHERR  DS    0H                                                               
         MVI   ERROR,NOTALPHA                                                   
         J     TRAPERR                                                          
*                                                                               
MISSFLD  DS    0H                                                               
         MVI   ERROR,MISSING                                                    
         J     TRAPERR                                                          
INVLFLD  DS    0H                                                               
         MVI   ERROR,INVALID                                                    
         J     TRAPERR                                                          
INVLCLI  DS    0H                                                               
         MVI   ERROR,INVCLI                                                     
         J     TRAPERR                                                          
DELERR   DS    0H                  DELETE NOT ALLOWED                           
         MVI   ERROR,INVACT                                                     
         J     TRAPERR                                                          
CFULLERR DS    0H                                                               
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(33),=C'CANNOT ASSIGN, CLIENT RECORD FULL'                
         GOTO1 ERREX2                                                           
CDEFERR  DS    0H                                                               
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(27),=C'DEFINITION RECORD NOT FOUND'                      
         GOTO1 ERREX2                                                           
CASSERR  DS    0H                                                               
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(27),=C'ASSIGNMENT RECORD NOT FOUND'                      
         GOTO1 ERREX2                                                           
DUPCLT   DS    0H                                                               
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(30),=C'CLIENT ALREADY ON CLIENT GROUP'                   
         GOTO1 ERREX2                                                           
INVID    DS    0H                                                               
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(30),=C'*** INVALID GROUP ID          '                   
         GOTO1 ERREX2                                                           
         EJECT                                                                  
         LTORG                                                                  
       ++INCLUDE SPCGRTAB                                                       
         EJECT                                                                  
***********************************************************************         
*        USERCHK SUBROUTINE                                           *         
***********************************************************************         
USERCHK  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
**       OC    ASECBLK,ASECBLK                                                  
**       BZ    UCHKNO              NO SECURITY NO CHANGE                        
**       DC    H'0'                                                             
         L     R1,ASECBLK                                                       
         USING SECD,R1                                                          
         MVC   PIDNAME,SECPID                                                   
         DROP  R1                                                               
*                                                                               
*****    BRAS  RE,CHECKPID         NEED THE 2 BYTE PID NOW                      
*****    BNE   UCHKNO                                                           
*                                                                               
**********************************                                              
*                                                                               
*                                                                               
         LA    RE,KEY                                                           
         USING CLGRECD,RE                                                       
         CLC   KEY(2),=X'0D06'     RECNUM IS 34, MUST BE CLIENT GROUP           
         BE    *+6                                                              
         DC    H'0'                                                             
         XC    CLGKGRP,CLGKGRP                                                  
         DROP  RE                                                               
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(4),KEYSAVE      SHOULD BE THERE                              
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AIO,AIO2                                                         
         GOTO1 DATAMGR,DMCB,=C'GETREC',=CL8'SPTFILE',KEY+14,AIO,MYDMWRK         
*******  GOTO1 GETREC                                                           
         MVC   AIO,AIO1                                                         
*                                                                               
         USING CLGKEY,R6                                                        
         L     R6,AIO2             WE NEED THE GROUP DEFINITION RECORD          
         MVI   ELCODE,CLGSCMCQ     X'12' SECURITY MANAGER ID ELEMENT            
         BRAS  RE,GETEL                                                         
         BNE   UCHKYES             NO RESTRICTION, WE'RE GOOD TO GO             
         USING CLGSCMD,R6                                                       
         OC    CLGSCM(12),CLGSCM   ANYTHING HERE?                               
         BZ    UCHKYES              - NOPE, WE OK                               
*                                                                               
         LA    RF,6                BCT LOOP 6 TIMES                             
         LA    R2,CLGSCM1          START WITH 1ST MANAGER                       
*                                                                               
UCHK50   CLC   PIDNUM,0(R2)        SAME PERSON?                                 
         BE    UCHKYES              - YUP, WE GOOD                              
         LA    R2,L'CLGSCM(R2)     BUMP NOW                                     
         BCT   RF,UCHK50                                                        
*                                                                               
UCHKNO   LTR   RE,RE                                                            
         B     *+6                                                              
*                                                                               
UCHKYES  CR    RE,RE                                                            
         J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        VR ROUTINE                                                   *         
***********************************************************************         
VRRTN    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   SAVEKEY(L'KEY),KEY                                               
*                                                                               
         BRAS  RE,USERCHK  SEE IF USER HAS ACCESS                               
         JNE   BADUSER                                                          
* REPOSITION THE POINTER                                                        
                                                                                
*                                                                               
         MVC   KEY,SAVEKEY                                                      
         GOTO1 HIGH                                                             
*                                                                               
         CLI   ACTNUM,ACTADD       TEST ADD                                     
         BE    VR10                                                             
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,CLGMEMQ                                                   
         GOTO1 REMELEM                                                          
         CLI   DMCB+12,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
VR10     DS    0H                                                               
         LA    R6,ELEM                                                          
         USING CLGMEMD,R6                                                       
         MVI   CLGMEMCD,CLGMEMQ    ELEMENT CODE                                 
         MVI   CLGMEMLN,CLGMEMLQ   ELEMENT LENGTH                               
         LA    R2,SFMNM1H                                                       
         CLI   5(R2),0                                                          
         JE    MISSFLD                                                          
         MVC   CLGNAM1,SFMNM1                                                   
         OC    CLGNAM1,SPACES                                                   
*                                                                               
         LA    R2,SFMNM2H                                                       
         CLI   SVBKLNS+1,0                                                      
         BNE   VR20                                                             
         CLI   5(R2),0                                                          
         JNE   INVLFLD                                                          
         B     VR30                                                             
*                                                                               
VR20     CLI   5(R2),0                                                          
         JE    MISSFLD                                                          
         MVC   CLGNAM2,SFMNM2                                                   
         OC    CLGNAM2,SPACES                                                   
         DROP  R6                                                               
*                                                                               
VR30     L     R6,AIO                                                           
         GOTO1 ADDELEM                                                          
         CLI   DMCB+12,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
VRX      J     EXIT                                                             
BADUSER  DS    0H                                                               
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(38),=C'YOU ARE NOT AUTHORIZED TO MAKE CHANGES'           
         GOTO1 ERREX2                                                           
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE SEACSFILE                                                      
       ++INCLUDE FASECRETD                                                      
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE NESFMFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE NESFMDCD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NESFMDDD                                                       
         EJECT                                                                  
       ++INCLUDE NESFMWORKD                                                     
         ORG   SYSSPARE                                                         
         PRINT ON                                                               
*                           *******  T31C30 WORK AREA  *******                  
WORKAREA DS    0CL1                                                             
BYTE2    DS    CL1                                                              
SVBKLNS  DS    CL6                                                              
PREVFLAG DS    CL1                                                              
PREVKEY  DS    CL13                                                             
SVCLTAS  DS    CL15                                                             
SAVEKEY  DS    CL48                                                             
IDLEN    DS    CL1                                                              
IDFILTER DS    CL1                                                              
PIDNAME  DS    CL8                  USER PID NAME                               
PIDNUM   DS    CL2                  PID                                         
SECALPHA DS    CL2                  ALPHA AGENCY                                
PLFLAG   DS    CL1                 PRINT LINE FLAG                              
GROUP    DS    CL3                                                              
SVRLINE  DS    CL132                                                            
         DS    0D                                                               
MYDMWRK  DS    CL96                                                             
DASAVE   DS    XL4                                                              
*                                                                               
MYFLAG   DS    XL1                 FLAGS                                        
DELCLT   EQU   X'01'               DELETE THIS CLIENT                           
ID2      EQU   X'02'               2 CHAR ID                                    
*                                                                               
FILTID   DS    XL1                                                              
*                                                                               
WORKEND  EQU   *                                                                
*                                                                               
PLINED   DSECT                                                                  
         DS    CL2                                                              
PLGRPID  DS    CL5                                                              
         DS    CL4                                                              
PLLEV1   DS    CL24                                                             
         DS    CL2                                                              
PLLEV2   DS    CL24                                                             
*                                                                               
RLINED   DSECT                                                                  
         DS    CL1                                                              
RCODE    DS    CL4                                                              
         DS    CL1                                                              
RBREAK1  DS    CL24                                                             
         DS    CL1                                                              
RBREAK2  DS    CL24                                                             
         DS    CL1                                                              
RCLT     DS    CL70                                                             
         DS    CL6                                                              
         EJECT                                                                  
       ++INCLUDE SPGENCLG                                                       
CLTRECD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
 END                                                                            
