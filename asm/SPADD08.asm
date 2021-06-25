*          DATA SET SPADD08    AT LEVEL 017 AS OF 05/01/02                      
*PHASE T21208A                                                                  
***********************************************************************         
*                                                                               
*  TITLE: T21208 - MAINTENANCE/LIST OF AVAIL COMMENTS                           
*                                                                               
*  COMMENTS: MAINTAINS AVAIL COMMENTS                                           
*                                                                               
*  CALLED FROM: ADDS CONTROLLER (T21200), WHICH CALLS                           
*               DDGENCON (T00A30) WHICH CALLS THIS.                             
*                                                                               
*  CALLS TO:    DATAMGR                                                         
*                                                                               
*  INPUTS: SCREENS SPADDF8 (T212F8) -- MAINTENANCE                              
*                                                                               
*  OUTPUTS: UPDATED AVAIL RECORDS WITH COMMENTS                                 
*                                                                               
*  LOCALS: REGISTER USAGE                                                       
*          R0 - WORK                                                            
*          R1 - WORK                                                            
*          R2 - POINTER TO SCREEN FLDH, ERREX WILL POSITION CURSOR              
*          R3 - WORK                                                            
*          R4 - WORK                                                            
*          R5 - POINTS TO THE OVERLAY STORAGE AREA DSECT                        
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
         TITLE 'SPADD08 MAINTENANCE OF AVAIL COMMENTS'                          
T21208   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*T21208*,R7,RR=R3                                              
         L     RC,0(R1)            STANDARD CODING                              
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN + OUR SCREEN                     
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         LA    R5,SYSSPARE         R5 = A(OVERLAY STORAGE AREA)                 
         USING MYAREAD,R5                                                       
         ST    R3,RELO                                                          
*                                                                               
         XC    ACURSOR,ACURSOR     CLEAR CURSOR POSITION                        
*                                                                               
         LA    R2,PFTABLE          NO, JUST RETURN KEY ALLOWED                  
         CLI   PFKEY,12                                                         
         BNE   CKMODES                                                          
*                                                                               
T21208A  GOTO1 INITIAL,DMCB,(R2)   INITIALIZE THE PFKEYS                        
*                                                                               
CKMODES  CLI   MODE,VALKEY         VALIDATE KEY?                                
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD?                             
         BE    VR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY?                                 
         BE    DK                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD?                              
         BE    DR                                                               
*                                                                               
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* VALIDATE THE KEY                                                              
***********************************************************************         
*****                                                                           
* VALIDATE THE REFERENCE NUMBER                                                 
*****                                                                           
VK       DS    0H                                                               
         LA    R2,AVMREFNH         R2 = A(FLDHDR OF REFERENCE #)                
         CLI   ACTNUM,ACTLIST                                                   
         BNE   *+8                                                              
         LA    R2,AVLREFNH                                                      
*                                                                               
         TM    4(R2),X'80'         REFERENCE NUMBER CHANGED?                    
         BZ    *+8                                                              
         MVI   PREVFLAG,0          YES, DON'T USE OLD KEY                       
*                                                                               
         CLI   5(R2),1             IF NO REFERENCE NUMBER                       
         BL    MISSFLD             THEN INVALID, NEED A MEDIA                   
*                                                                               
         GOTO1 VALIREFN            VALIDATE REFERENCE NUMBER                    
*                                                                               
         CLI   ACTNUM,ACTLIST      IF ACTION LIST                               
         BE    VKXIT               THEN VALIDATE BUYER                          
*                                                                               
         CLI   5(R2),7             IF REFERENCE NUMBER IS FILLED                
         BNE   INVLFLD             THEN SEE IF REFERENCE # EXISTS               
*                                                                               
         PACK  DUB(4),9(6,R2)                                                   
         GOTO1 PAKTOREF,DMCB,DUB                                                
*                                                                               
         XC    KEY,KEY             CLEAN OUT THE KEY                            
         LA    R4,KEY                                                           
         USING AVARECD,R4          OVERLAY KEY WITH OUR TEMPLATE                
         MVI   AVAKTYP,AVAKTYPQ    LOAD UP THE IDENTIFIERS                      
         MVI   AVAKSUB,AVAKSUB2    USE PASSIVE KEY                              
         MVC   AVAKAM,BAGYMD                                                    
         MVC   AVAKREF2,DMCB+1                                                  
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(AVAKBYR2-AVAKEY),KEYSAVE    IF MATCH ON REFERENCE #          
         BE    *+14                                                             
         MVC   KEY,KEYSAVE                                                      
         B     VKXIT                                                            
*                                                                               
         BAS   RE,DISPINFO         THEN DISPLAY INFORMATION LINE                
*                                                                               
VKXIT    B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* DISPLAY THE AVAIL INFORMATION ON ONE LINE.                                    
***********************************************************************         
DISPINFO NTR1                                                                   
         LA    R4,KEY                                                           
         USING AVARECD,R4          OVERLAY KEY WITH OUR TEMPLATE                
         LA    R3,AVMINFOH         R3 = A(FLDHDR OF INFORMATION LINE)           
*                                                                               
         XC    8(L'AVMINFO,R3),8(R3)                                            
         MVI   5(R3),L'AVMINFO                                                  
         OI    6(R3),X'80'                                                      
         LA    R3,8(R3)                                                         
         USING INFOLIND,R3                                                      
*                                                                               
         MVC   LINBUYR,AVAKBYR2    DISPLAY THE BUYER                            
         MVI   LINSLSH,C'/'                                                     
* CODE COMMENTED OUT BECAUSE WE'RE GETTING OFFICE FROM ELEMENT                  
*        GOTO1 VALIBUYR,DMCB,LINBUYR                                            
*        BE    *+6                                                              
*        DC    H'0'                                                             
*        MVC   LINOFID,QOFFICE         AND ITS OFFICE                           
*                                                                               
         MVC   BCLT,AVAKCLT2                                                    
         GOTO1 CLUNPK,DMCB,AVAKCLT2,LINCLT    DISPLAY THE CLIENT                
*                                                                               
         CLI   AVAKEST2,0          DISPLAY ESTIMATE IF ANY                      
         BE    DINF10                                                           
         ZIC   R1,AVAKEST2                                                      
         CVD   R1,DUB                                                           
         UNPK  LINEST(3),DUB+6(2)                                               
         OI    LINEST+2,X'F0'                                                   
*                                                                               
DINF10   MVC   BPRD,AVAKPRD2       COPY THE PRODUCT CODE                        
         GOTO1 GETQPRD             DISPLAY THE EBCDIC PRODUCT                   
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   LINPRD,QPRD                                                      
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO              R6 = A(DESCRIPTION ELEMENT)                  
         MVI   ELCODE,AVARDCDQ                                                  
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING AVARDSCD,R6                                                      
         MVC   LINOFID,AVARDBYO    OFFICE ID                                    
         GOTO1 DATCON,DMCB,(8,AVARDFLS),(11,LINFLTS)                            
         MVI   LINDASH,C'-'                                                     
         GOTO1 DATCON,DMCB,(8,AVARDFLE),(11,LINFLTE)                            
         OC    AVARDDUE,AVARDDUE                                                
         BZ    DINF15                                                           
         GOTO1 DATCON,DMCB,(8,AVARDDUE),(11,LINDUED)                            
*                                                                               
DINF15   LA    R4,AVARDSNT         LAST STATUS WAS SENT?                        
         OC    AVARDSNT,AVARDSNT                                                
         BZ    *+14                NO                                           
         MVC   LINSTAT,=CL12'   LAST SENT'                                      
         B     DINF20                                                           
*                                                                               
         LA    R4,AVARDUPT         LAST STATUS WAS CHANGE?                      
         OC    AVARDUPT,AVARDUPT                                                
         BZ    *+14                NO                                           
         MVC   LINSTAT,=CL12'LAST CHANGED'                                      
         B     DINF20                                                           
*                                                                               
         LA    R4,AVARDCRE         LAST STATUS WAS CREATED                      
         MVC   LINSTAT,=CL12'   OPENED ON'                                      
*                                                                               
DINF20   GOTO1 DATCON,DMCB,(8,(R4)),(11,LINDATE)                                
         DROP  R3,R4,R6                                                         
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* DISPLAY THE KEY                                                               
***********************************************************************         
DK       DS    0H                                                               
         MVI   PREVFLAG,1          KEY BEING USED BY LIST                       
         MVC   PREVKEY,KEY                                                      
         LA    R4,PREVKEY          POINT TO THE KEY TO DISPLAY                  
         USING AVARECD,R4                                                       
*                                                                               
         MVC   AVMREFN(L'QMED),QMED    DISPLAY THE REFERENCE NUMBER             
         GOTO1 REFTOPAK,DMCB,AVAKREF                                            
         UNPK  AVMREFN+1(6),DMCB(4)                                             
         OI    AVMREFN+6,X'F0'                                                  
         OI    AVMREFNH+6,X'80'                                                 
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING AVARECD,R4          OVERLAY KEY WITH OUR TEMPLATE                
         MVC   KEY(L'PREVKEY),PREVKEY                                           
         MVI   AVAKSUB,AVAKSUB2                                                 
         MVC   AVAKREF2,PREVKEY+AVAKREF-AVAKEY                                  
         MVC   AVAKBYR2(AVAKCNTL-AVAKBYR2),PREVKEY+AVAKBYR-AVAKEY               
*                                                                               
         BAS   RE,DISPINFO         DISPLAY INFORMATION LINE                     
*                                                                               
DKXIT    MVC   KEY,PREVKEY                                                      
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* DISPLAY THE RECORD                                                            
***********************************************************************         
DR       DS    0H                                                               
         TWAXC AVMFLINH,AVMLLINH   CLEAR ALL THE LINES                          
*****                                                                           
* DISPLAY THE COMMENTS                                                          
*****                                                                           
         L     R6,AIO              POINT TO THE RECORD                          
         MVI   ELCODE,AVARCCEQ     GET DESCRIPTION ELEMENT                      
         BAS   RE,GETEL                                                         
         BNE   DRXIT                                                            
         USING AVARCOMD,R6                                                      
*                                                                               
DR10LP   ZIC   R1,AVARCLNM         CALCULATE WHERE TO PUT THE LINE              
         BCTR  R1,0                                                             
         MH    R1,=Y(L'AVMFLINH+L'AVMFLIN)                                      
         LA    R2,AVMFLINH                                                      
         AR    R2,R1                                                            
*                                                                               
         ZIC   R1,AVARCLEN         DISPLAY THE LINE                             
         SH    R1,=Y(AVARCOVQ+1)                                                
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),AVARCTXT                                                 
         MVI   6(R2),X'80'                                                      
*                                                                               
         BAS   RE,NEXTEL                                                        
         BE    DR10LP                                                           
*                                                                               
DRXIT    MVC   ACURFORC,ACURSOR                                                 
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE THE RECORD ON SCREEN                                                 
***********************************************************************         
VR       DS    0H                                                               
         CLI   PFKEY,3             ERASE LINE?                                  
         BE    *+12                                                             
         CLI   PFKEY,4             ADD LINE?                                    
         BNE   VR100                                                            
*                                                                               
         L     R7,ATIOB            A(TIOB)                                      
         USING TIOBD,R7                                                         
         SR    R1,R1                                                            
         ICM   R1,3,TIOBCURS       ABSOLUTE CURSOR ADDRESS                      
         SR    R0,R0                                                            
         D     R0,=F'80'                                                        
         MH    R1,=H'80'           ABSOLUTE ADDR OF BEGINNING OF LINE           
         DROP  R7                                                               
*                                                                               
         LA    R2,AVMFILLH         1ST FIELD WHICH COULD CONTAIN CURSOR         
VR10     SR    RF,RF                                                            
         ICM   RF,3,2(R2)          ABSOLUTE SCREEN ADDR OF THIS FIELD           
         SR    RE,RE                                                            
         D     RE,=F'80'                                                        
         MH    RF,=H'80'           ABSOLUTE SCREEN ADDR OF LINE START           
         LA    RE,79(RF)           ABSOLUTE SCREEN ADDR OF LINE END             
*                                                                               
         CR    RF,R1               WAS CURSOR ON THIS LINE?                     
         BH    VR100               NO - IT'S ABOVE THIS FIELD                   
         CR    RE,R1                                                            
         BNL   VR20                YES                                          
*                                                                               
         ZIC   RF,0(R2)            BUMP TO NEXT FIELD                           
         AR    R2,RF                                                            
         LA    RF,AVMLLINH                                                      
         CR    R2,RF               END OF SCREEN?                               
         BH    VR100               YES                                          
         B     VR10                                                             
*                                                                               
VR20     LA    RF,AVMLLINH         A(LAST TEXT FIELD)                           
         CLI   PFKEY,3             ERASE LINE?                                  
         BNE   VR50                NO, ADD LINE                                 
*                                                                               
         LA    R0,AVMFILLH                                                      
         CR    R2,R0               IS CURSOR ABOVE 1ST LINE?                    
         BE    VR100               YES -- ONLY ALLOWED FOR ADD                  
*                                                                               
         ST    R2,ACURSOR          KEEP CURSOR IN PLACE                         
         LR    R3,R2                                                            
VR30     CR    R2,RF               ARE WE UP TO LAST LINE OF TEXT?              
         BE    VR40                YES                                          
         ZIC   R0,0(R2)                                                         
         AR    R3,R0               R3 POINTS TO FOLLOWING LINE                  
         LA    R1,L'AVMFLIN                                                     
         BCTR  R1,0                FOR EX INSTRUCTIONS:  R1 = L'TEXT            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),8(R3)       MOVE LINE OF TEXT UP                         
         MVC   4(2,R2),4(R3)       MOVE INPUT INDICATORS AND LENGTH             
         LR    R2,R3                                                            
         B     VR30                                                             
*                                                                               
VR40     XC    4(2,R2),4(R2)       CLEAR INPUT INDICATORS AND LENGTH            
         LA    R1,L'AVMFLIN                                                     
         BCTR  R1,0                FOR EX INSTRUCTIONS:  R1 = L'TEXT            
         EX    R1,*+8                                                           
         B     *+10                                                             
         XC    8(0,R2),8(R2)       CLEAR LAST TEXT FIELD                        
         B     VR100                                                            
*                                                                               
VR50     CR    R2,RF               ARE THEY TRYING TO INSERT AFTER END?         
         BE    VR100               YES                                          
*                                                                               
         LR    RF,R2               SAVE A(INSERTION)                            
         LA    R3,AVMLLINH         LAST LINE OF TEXT                            
         LR    R2,R3                                                            
*                                                                               
VR60     ZIC   R0,0(R2)                                                         
         SR    R2,R0               R3 POINTS TO PREVIOUS LINE                   
         CR    R2,RF               ARE WE UP TO LAST LINE OF TEXT?              
         BE    VR70                YES                                          
         LA    R1,L'AVMFLIN                                                     
         BCTR  R1,0                FOR EX INSTRUCTIONS:  R1 = L'TEXT            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R3),8(R2)       MOVE LINE OF TEXT DOWN                       
         MVC   4(2,R3),4(R2)       MOVE INPUT INDICATORS AND LENGTH             
         LR    R3,R2                                                            
         B     VR60                                                             
*                                                                               
VR70     XC    4(2,R3),4(R3)       CLEAR INPUT INDICATORS AND LENGTH            
         LA    R1,L'AVMFLIN                                                     
         BCTR  R1,0                FOR EX INSTRUCTIONS:  R1 = L'TEXT            
         EX    R1,*+8                                                           
         B     *+10                                                             
         XC    8(0,R3),8(R3)       CLEAR TEXT FIELD (INSERT BLANK LINE)         
         ST    R3,ACURSOR          KEEP CURSOR IN PLACE                         
         EJECT                                                                  
VR100    L     R6,AIO                                                           
         MVI   ELCODE,AVARCCEQ     REMOVE OLD DESCRIPTION ELEMENTS              
         GOTO1 REMELEM                                                          
*                                                                               
         LA    R2,AVMFLINH         R2 = A(1ST LINE OF TEXT)                     
         LA    R3,1                R3 = LINE NUMBER                             
*                                                                               
         LA    R6,COMMELEM         SET BASIC ELEMENT INFORMATION FOR            
         USING AVARCOMD,R6             COMMENT ELEMENT                          
         XC    COMMELEM,COMMELEM   CLEAR THE COMMENT ELEMENT                    
         MVI   AVARCCDE,AVARCCEQ                                                
*                                                                               
VR110LP  CLI   5(R2),0             IF NO DATA IN THIS LINE                      
         BE    VR110NXT            THEN CHECK NEXT LINE                         
*                                                                               
         ZIC   R1,5(R2)            MOVE IN TEXT, LINE #, AND L'ELEMENT          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   AVARCTXT(0),8(R2)                                                
         STC   R3,AVARCLNM                                                      
         LA    R1,1+AVARCOVQ(R1)                                                
         STC   R1,AVARCLEN                                                      
*                                                                               
         MVC   ELEM(L'COMMELEM),COMMELEM                                        
         GOTO1 ADDELEM             ADD TEXT LINE ELEMENT                        
         CLI   DMCB+12,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
VR110NXT LA    R0,AVMLLINH         IF LINE IS LAST LINE                         
         CR    R2,R0                                                            
         BE    VR120               THEN DONE WITH TEXT LINE ELEMENTS            
*                                                                               
         LA    R2,L'AVMFLINH+L'AVMFLIN(R2)   R2 = A(NEXT LINE)                  
         LA    R3,1(R3)            R3 = NEXT LINE NUMBER                        
         B     VR110LP                                                          
*                                                                               
VR120    DS    0H                                                               
*                                                                               
VRXIT    B     DR                                                               
         EJECT                                                                  
RELO     DS    A                                                                
***********************************************************************         
* ERROR MESSAGES                                                                
***********************************************************************         
MISSFLD  MVI   GERROR1,MISSING                                                  
         B     ERREXIT                                                          
*                                                                               
INVLFLD  MVI   GERROR1,INVALID                                                  
         B     ERREXIT                                                          
*                                                                               
INVLREFN MVI   GERROR1,INVREFN                                                  
         B     ERREXIT                                                          
*                                                                               
ERREXIT  GOTO1 MYERR                                                            
*                                                                               
         GETEL R6,DATADISP,ELCODE  USED FOR THE GETEL OPERATIONS                
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* PFKEY TABLE DEFINITIONS                                                       
***********************************************************************         
PFTABLE  DS    0C                                                               
*                                                                               
* RETURN TO CALLER                                                              
         DC    AL1(PF12X-*,12,PFTRPROG,0,0,0)                                   
         DC    CL3' ',CL8' ',CL8' '                                             
PF12X    EQU   *                                                                
*                                                                               
         DC    X'FF'                                                            
         EJECT                                                                  
       ++INCLUDE DDSPOOLD          (GENERAL PRINT AREAS)                        
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD        (GENERAL CONTROLLER AREAS)                   
         EJECT                                                                  
       ++INCLUDE FATIOB                                                         
         EJECT                                                                  
       ++INCLUDE SPADDFFD          (BASE SCREEN FOR SYSTEM)                     
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPADDF8D          (OUR MAINTENANCE SCREEN OVERLAY)             
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPADDE8D          (OUR LIST SCREEN OVERLAY)                    
         EJECT                                                                  
       ++INCLUDE SPADDSECTS                                                     
         EJECT                                                                  
       ++INCLUDE DDPERVALD                                                      
         EJECT                                                                  
       ++INCLUDE SPADDWORKD        (SYSTEM AREAS)                               
         EJECT                                                                  
* MY STORAGE AREA                                                               
MYAREAD  DSECT                                                                  
ACURSOR  DS    A                                                                
PREVFLAG DS    XL1                 PREVIOUS KEY USED FLAG                       
PREVSEQN DS    XL1                 PREVIOUS SEQUENCE NUMBER                     
PREVKEY  DS    XL(L'AVAKEY)        SAVED KEYS                                   
*                                                                               
COMMELEM DS    XL80                COMMENT ELEMENT                              
         EJECT                                                                  
*                                                                               
* ONLINE LIST LINE                                                              
*                                                                               
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
LSTREFN  DS    CL7                                                              
         DS    C                                                                
LSTBUYR  DS    CL3                                                              
LSTSLASH DS    C                                                                
LSTOFF   DS    CL2                                                              
         DS    CL3                                                              
LSTCLT   DS    CL3                                                              
         DS    CL2                                                              
LSTPRD   DS    CL3                                                              
         DS    CL2                                                              
LSTEST   DS    CL3                                                              
         DS    CL2                                                              
LSTFLTD  DS    CL17                                                             
         DS    CL2                                                              
LSTDUED  DS    CL8                                                              
         DS    CL2                                                              
LSTSTAT  DS    CL13                                                             
*                                                                               
* ONLINE LIST LINE                                                              
*                                                                               
SPOOLD   DSECT                                                                  
         ORG   P                                                                
PRTSTARS DS    CL5                                                              
         DS    CL2                                                              
PRTMKT   DS    CL4                                                              
         DS    CL2                                                              
PRTMKTNM DS    CL24                                                             
         DS    CL2                                                              
PRTBUYR  DS    CL3                                                              
PRTSLASH DS    C                                                                
PRTBYROF DS    CL2                                                              
*                                                                               
* ONLINE INFORMATION LINE                                                       
*                                                                               
INFOLIND DSECT                                                                  
LINBUYR  DS    CL3                                                              
LINSLSH  DS    C                                                                
LINOFID  DS    CL2                                                              
         DS    CL2                                                              
LINCLT   DS    CL3                                                              
         DS    CL2                                                              
LINPRD   DS    CL3                                                              
         DS    CL2                                                              
LINEST   DS    CL3                                                              
         DS    CL2                                                              
LINFLTS  DS    CL8                                                              
LINDASH  DS    C                                                                
LINFLTE  DS    CL8                                                              
         DS    CL2                                                              
LINDUED  DS    CL8                                                              
         DS    CL2                                                              
LINSTAT  DS    CL12                                                             
         DS    C                                                                
LINDATE  DS    CL8                                                              
         DS    CL3                                                              
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'017SPADD08   05/01/02'                                      
         END                                                                    
