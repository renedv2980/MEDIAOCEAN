*          DATA SET CTMAD10    AT LEVEL 039 AS OF 05/01/02                      
*PHASE TA0C10A,*                                                                
*INCLUDE RECUP                                                                  
TA0C10   TITLE 'CTMAD10 - $MAD MEDIABASE - MEDLINE UPLOAD BUYLINES'             
TA0C10   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 LENFREE,TA0C10,RA,RR=R2                                          
*                                                                               
         USING CONTROLD,RC         CONTROLLER COMMON STORAGE                    
         USING APPLD,R7            FIRST APPLICATION COMMON STORAGE             
         USING USUKD,R8            US/UK APPLICATION COMMON STORAGE             
         USING OVERD,R9            OVERLAY SAVED STORAGE                        
*                                                                               
*        INITIALIZE OVERLAY WIDE REGISTERS                                      
*                                                                               
         LR    RF,RC               RF = A(OVERLAY'S SPARE MEMORY)               
*                                                                               
         L     RC,ACONTD           RC = A(CONTROLLER COMMON STORAGE)            
         L     R9,AOVER            R9 = A(OVERLAY SAVED STORAGE)                
*                                                                               
         ST    RD,SAVEDRD          SAVE STACK POINTER FOR RETURNING             
         ST    RF,ABLNTBL          SAVE A(BUYLINE TABLE)                        
         ST    R2,APRELO                                                        
         ST    RB,APBASE1          SAVE PROGRAM BASE REGISTER 1                 
         ST    RA,APBASE2          SAVE PROGRAM BASE REGISTER 2                 
         L     RE,=V(RECUP)                                                     
         AR    RE,R2                                                            
         ST    RE,RECUP                                                         
         L     RE,ACOMFACS                                                      
         USING COMFACSD,RE                                                      
         MVC   GLOBBER,CGLOBBER                                                 
         DROP  RE                                                               
*                                                                               
         EJECT                                                                  
***********************************************************************         
* MAIN DRIVER - CALLS THE THREE SUBROUTINES THAT HANDLE EACH OF THE             
* THREE MODES THAT THE CONTROLLER CALLS AN OVERLAY WITH (START, MIDDLE,         
* AND END).                                                                     
***********************************************************************         
MAIN     DS    0H                                                               
         BAS   RE,INIT             INITIALIZE OVERLAY                           
*                                                                               
         CLI   OVERMODE,C'S'       IF MODE IS START                             
         BNE   M10                                                              
         BAS   RE,PROCSTRT         THEN CALL PROCSTRT                           
         B     MX                                                               
*                                                                               
M10      CLI   OVERMODE,C'M'       ELSE IF MODE IS MIDDLE                       
         BNE   M20                                                              
         BAS   RE,PROCMID          THEN CALL PROCMID                            
         B     MX                                                               
*                                                                               
M20      BAS   RE,PROCEND          ELSE CALL PROCEND                            
*                                                                               
MX       B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE INITIALIZES VARIABLES IN THIS OVERLAY THAT NEED                  
* INITIALIZATION.                                                               
***********************************************************************         
INIT     NTR1                                                                   
*                                                                               
INX      B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE PROCESSES THE START MODE.  IT GETS THE HEADER OBJECT             
* AND THEN PROCEEDS TO GET THE INSERTION OBJECTS.                               
***********************************************************************         
PROCSTRT NTR1                                                                   
         MVI   MDLAST,C'N'                                                      
         MVI   CONFFLAG,0          HAVEN'T CONFIRM YET                          
         MVI   ENDOBJCT,C'N'       NOT END OF OBJECTS                           
*                                                                               
*                                  OPEN TEMP FILE FOR PUTS                      
         GOTO1 TMPOPEN,DMCB,(C'S',=C'PUT')                                      
         BNE   EXIT                                                             
*                                                                               
         BAS   RE,GETOBJCT         GET THE OBJECT                               
*                                                                               
         CLI   ENDOBJCT,C'Y'       IF END OF OBJECTS                            
         BNE   PSX                                                              
         GOTO1 TMPCLOSE            DONE WITH PUTTING INTO TEMPFILE              
*                                                                               
         BAS   RE,CALLBUY          CALL MEDLINE BUY PROGRAM                     
*                                                                               
PSX      B     XIT                                                              
*                                                                               
GLOBDELE MVC   MDACTION,=Y(ER10GDEL)  PROBLEM WITH GLOBBER DELETE               
         B     EXIT                EXIT $MAD WITH AN ERROR CODE                 
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE PROCESSES THE CONTINUATION MODE.  IT CONTINUES TO GET            
* ALL THE SCHEDULES UNTIL AN END-OF-DATA OBJECT OCCURS.                         
***********************************************************************         
PROCMID  NTR1                                                                   
         CLI   ENDOBJCT,C'Y'       IF END OF OBJECTS                            
         BE    PM10                THEN WE CAN CONFIRM                          
*                                                                               
         BAS   RE,GETOBJCT                                                      
*                                                                               
         CLI   ENDOBJCT,C'Y'       IF END OF OBJECTS                            
         BNE   PMX                                                              
         GOTO1 TMPCLOSE            DONE WITH PUTTING INTO TEMPFILE              
*                                                                               
         BAS   RE,CALLBUY          CALL MEDLINE BUY PROGRAM                     
         B     PMX                                                              
*                                                                               
PM10     CLI   CONFFLAG,X'FF'      CONFIRM USED BEFORE?                         
         BE    PM20                YES                                          
*                                                                               
*                                  DID WE GET CONTROL BACK FROM BUY?            
         GOTO1 GLOBBER,DMCB,=C'GETD',BLOCK,24,GLVXCTL                           
         CLI   DMCB+8,0                                                         
         BNE   PM20                                                             
*                                                                               
         GOTO1 GLOBBER,DMCB,=C'DELE',,,GLVXCTL                                  
         CLI   DMCB+8,0                                                         
         BNE   GLOBDELE            PROBLEM WITH GLOBBER DELETE                  
*                                                                               
         LA    R1,BLOCK                                                         
         USING GLVXFRSY,R1                                                      
         CLC   GLVXFRSY,=C'MED'    FROM MEDLINE                                 
         BNE   PM20                                                             
         CLC   GLVXFRPR,=C'BUY'         BUY?                                    
         BNE   PM20                                                             
         TM    GLVXFLG1,GLV1RETN   YES, IS IT A RETURN CALL?                    
         BZ    PM20                                                             
*                                                                               
         MVI   CONFFLAG,0          NOTHING IN OUR BUFFER YET                    
PM20     BAS   RE,CONFIRM          SEND OUT CONFIRMATIONS                       
         B     PMX                                                              
*                                                                               
PMX      B     XIT                                                              
*                                                                               
***********************************************************************         
* THIS ROUTINE PROCESSES THE END MODE.  IT CURRENTLY DOES NOTHING.              
***********************************************************************         
PROCEND  NTR1                                                                   
*                                                                               
PEX      B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE GETS THE STANDARD HEADER AND THE FOLLOWING ELEMENTS              
* FROM THE INPUT FRAME.                                                         
***********************************************************************         
GETOBJCT NTR1                                                                   
         L     R0,AIO              CLEAR OUR LOCAL STORAGE                      
         LA    R1,LENIO                                                         
         LR    RE,R0                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     R1,ATWA             USE INTERNAL TABLE ENTRY?                    
         USING TA0CFFD,R1                                                       
         LA    R1,MADDATA                                                       
         DROP  R1                                                               
         CLI   0(R1),C'X'          ONE OF OURS?                                 
         BNE   GOBJLOOP            NO                                           
*                                                                               
         CLI   1(R1),C'1'          SYNCH ENTRY?                                 
         BNE   GOBJ10              NO                                           
         MVI   COMMTYPE,C'S'                                                    
         L     R6,AIO                                                           
         ZICM  R3,SYNCTAB,2                                                     
         ST    R3,DATALEN                                                       
         BCTR  R3,R0                                                            
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R6),SYNCTAB+2                                                
         L     R3,DATALEN                                                       
         GOTO1 PUTTMP,DMCB,AIO,(R3)                                             
         BNE   EXIT                                                             
         B     GOBJDONE                                                         
*                                                                               
SYNCTAB  DS    0C                                                               
         DC    AL2(SYNCTABX-SYNCTAB-2)                                          
         DC    CL61'10061SP0000000219920630173030                    00X        
               0000000052'                                                      
         DC    CL52'20052TIM'                                                   
SYNCTABX DS    0C                                                               
*                                                                               
GOBJ10   CLI   1(R1),C'2'          INSERTION UPLOAD ENTRY?                      
         BNE   GOBJLOOP                                                         
         MVI   COMMTYPE,C'I'       YES                                          
         L     R6,AIO                                                           
         LA    R5,SCHDTAB                                                       
         LA    R0,3                3 OBJECTS                                    
GOBJ12   SR    R1,R1                                                            
         ICM   R1,3,0(R5)                                                       
         ST    R1,DATALEN                                                       
         BCTR  R1,R0                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R6),2(R5)                                                    
         L     R3,DATALEN          SEND OBJECT TO TEMPSTR                       
         GOTO1 PUTTMP,DMCB,AIO,(R3)                                             
         BNE   EXIT                                                             
         LA    R5,2(R3,R5)                                                      
         BCT   R0,GOBJ12                                                        
         B     GOBJDONE                                                         
*                                                                               
SCHDTAB  DS    0C                                                               
         DC    AL2(SCHDTABX-SCHDTAB-2)                                          
         DC    CL61'10061NP0000000019920818150000                    00X        
               0200000067'                                                      
         DC    CL52'20052TIMSCHED001SCHEDULE NUMBER 1'                          
         DC    CL15'30015T21  01092'                                            
SCHDTABX DS    0C                                                               
*                                                                               
SCHDTAB2 DS    0C                                                               
         DC    AL2(SCHDTA2X-SCHDTAB2-2)                                         
         DC    CL61'10061NP0000000019920818150000                    00X        
               0100000083'                                                      
         DC    CL83'40083TUE19920818100076HALF PAGE      ERHB/W 0001000X        
               00000000000            000000000'                                
SCHDTA2X DS    0C                                                               
*                                                                               
SCHDTAB3 DS    0C                                                               
         DC    AL2(SCHDTA3X-SCHDTAB3-2)                                         
         DC    CL61'10061NP0000000019920818150000                    00X        
               0100000083'                                                      
         DC    CL83'40083WED19920819100076HALF PAGE      ERHB/W 0002000X        
               00000000000            000000000'                                
SCHDTA3X DS    0C                                                               
*                                                                               
GOBJLOOP GOTO1 GETITEM             GET AN OBJECT                                
         BNE   EXIT                                                             
*                                                                               
         CLI   EIFFLAG,C'Y'        END OF FRAME?                                
         BE    GOBJX                                                            
*                                                                               
GOBJ20   CLC   TYPENUM,=A(ITEOD)   IF OBJECT IS END-OF-DATA                     
         BE    GOBJDONE                                                         
*                                                                               
GOBJ25   CLC   TYPENUM,=A(ITNWNSYN)  NET WORKS NOW OBJECT?                      
         BNE   *+12                                                             
         MVI   COMMTYPE,C'S'       YES, SYNCHRONISATION CHECK                   
         B     GOBJ30                                                           
*                                                                               
         CLC   TYPENUM,=A(ITNWNUPL)                                             
         BNE   INVLNWNO                                                         
         MVI   COMMTYPE,C'I'       YES, INSERTION UPLOAD                        
*                                                                               
GOBJ30   L     R0,AIO              COPY THE OBJECT TO HEADER OBJECT IN          
         L     R1,DATALEN              LOCAL STORAGE                            
         L     RE,ADATA                                                         
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
         BAS   RE,VALOBJCT         VALIDATE THE OBJECT                          
*                                                                               
GOBJ40   L     R3,DATALEN          SEND OBJECT TO TEMPSTR                       
         GOTO1 PUTTMP,DMCB,AIO,(R3)                                             
         BNE   EXIT                                                             
*                                                                               
         B     GOBJLOOP                                                         
*                                                                               
GOBJDONE MVI   ENDOBJCT,C'Y'       NO MORE OBJECTS TO READ                      
*                                                                               
GOBJX    B     XIT                 RETURN TO CALLER                             
*                                                                               
INVLNWNO MVC   MDACTION,=Y(ER10NWNO)  INVALID OBJECT FOR NET WORK NOW           
         B     EXIT                EXIT $MAD WITH AN ERROR CODE                 
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE VALIDATES THE OBJECT WE GOT                                      
***********************************************************************         
VALOBJCT NTR1                                                                   
         L     R6,AIO                                                           
         USING HDROBJCT,R6                                                      
*                                                                               
         CLC   =C'10',HDRELCOD     ERROR IF 1ST ELEM NOT STANDARD HDR           
         BNE   NEEDSHDR                                                         
*                                                                               
         CLI   HDRACTN,C'L'        CHECK IF VALID ACTION                        
         BE    VOBJX               LOOK-UP IS OK - NO MORE CHECKS               
         CLI   HDRACTN,C'N'                                                     
         BE    VOBJ20                                                           
         CLI   HDRACTN,C'S'        N & S ARE COMMON ACTIONS                     
         BE    VOBJ20                                                           
*                                                                               
         CLI   COMMTYPE,C'S'       UPDATE AND                                   
         BNE   VOBJ10                DELETE VALID ONLY FOR SYNCH CHECK          
         CLI   HDRACTN,C'U'                                                     
         BE    VOBJ20                                                           
         CLI   HDRACTN,C'D'                                                     
         BNE   INVLACTN                                                         
         B     VOBJ20                                                           
*                                                                               
VOBJ10   CLI   HDRACTN,C'A'        AMEND AND                                    
         BE    VOBJ20                CANCEL VALID ONLY FOR INS UPLOAD           
         CLI   HDRACTN,C'C'                                                     
         BNE   INVLACTN                                                         
*                                                                               
VOBJ20   CLC   HDRMNKEY,SPACES     ANY MAINFRAME KEY?                           
         BNE   VOBJ30                                                           
*                                                                               
         CLI   HDRACTN,C'N'        NO, DO WE NEED ONE?                          
         BNE   NEEDMKEY            YES WE DO                                    
*                                                                               
VOBJ30   CLC   HDRDTTIM,SPACES     ANY DATE/TIME UPDATED?                       
         BE    NEEDDTTM            NO, WE NEED IT                               
*                                                                               
VOBJX    B     XIT                 RETURN TO CALLER                             
*                                                                               
NEEDSHDR MVC   MDACTION,=Y(ER10SHDR)  NEED STANDARD HEADER ELEMENT              
         B     EXIT                EXIT $MAD WITH AN ERROR CODE                 
*                                                                               
INVLACTN MVC   MDACTION,=Y(ER10IACT)  INVALID ACTION CODE                       
         B     EXIT                EXIT $MAD WITH AN ERROR CODE                 
*                                                                               
NEEDMKEY MVC   MDACTION,=Y(ER10MKEY)  NEED MAINFRAME KEY                        
         B     EXIT                EXIT $MAD WITH AN ERROR CODE                 
*                                                                               
NEEDDTTM MVC   MDACTION,=Y(ER10DTTM)  NEED DATE/TIME UPDATED                    
         B     EXIT                EXIT $MAD WITH AN ERROR CODE                 
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE CALLS THE BUY PROGRAM WITH THE TRANSFER CONTROL ELEMENT          
* VIA GLOBBER.                                                                  
***********************************************************************         
CALLBUY  NTR1                                                                   
         GOTO1 SWITCH,DMCB,=C'MED',0                                            
         CLI   DMCB+4,0                                                         
         BNE   CANTSWTC            ERROR WHILE SWITCHING SYSTEMS                
*                                                                               
         XC    BLOCK(14),BLOCK                                                  
         LA    R1,BLOCK                                                         
         USING GLVXFRSY,R1                                                      
         MVC   GLVXFRSY,=C'CON'    FROM THE CONTROL SYSTEM                      
         MVC   GLVXFRPR,=C'MAD'    MAD PROGRAM                                  
         MVC   GLVXTOSY,=C'MED'    TO THE MEDLINE SYSTEM                        
         MVC   GLVXTOPR,=C'BUY'    BUY PROGRAM                                  
         OI    GLVXFLG1,GLV1GOTO   CALL BASE PROGRAM ON TRANSFER                
         DROP  R1                                                               
*                                  SET UP THE TRANSFER CONTROL BLOCK            
         GOTO1 GLOBBER,DMCB,=C'PUTD',BLOCK,22,GLVXCTL                           
         CLI   DMCB+8,0                                                         
         BNE   CBUYNOT                                                          
*                                                                               
CBUYX    B     XIT                 RETURN TO CALLER                             
*                                                                               
CANTSWTC MVC   MDACTION,=Y(ERSWITCH)  ERROR SWITCHING TO MEDIA SYSTEM           
         B     EXIT                EXIT $MAD WITH AN ERROR CODE                 
*                                                                               
CBUYNOT  MVC   MDACTION,=Y(ER10XCTL) GLOBBER COULDN'T PUT OUT XCTL ELEM         
         B     EXIT                EXIT $MAD WITH AN ERROR CODE                 
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE CONFIRMS THE BUILDING OF THE BUY RECORDS USING THE               
* SCHEDULE OBJECTS BY SENDING OUT CONFIRMATION OBJECTS.                         
***********************************************************************         
CONFIRM  NTR1                                                                   
         CLI   CONFFLAG,X'FF'      IF CONFIRM WAS USED BEFORE                   
         BE    CNF10               THEN PUT OUT CONFIRMATION OBJECT             
*                                                                               
         GOTO1 TMPOPEN,DMCB,=C'GET',LENIO                                       
         BNE   EXIT                                                             
*                                  GET INSERTION DATA FROM TEMPFILE             
CNFLOOP  GOTO1 GETTMP,DMCB,CNFOBJCT                                             
         BNE   EXIT                                                             
         MVC   DATALEN,DMCB+4      COPY LENGTH OF DATA                          
*                                                                               
         CLI   EOTFLAG,C'Y'        DONE IF NO MORE                              
         BNE   CNF10                                                            
         MVI   MDLAST,C'Y'         LAST OUTPUT DATA FRAME                       
*                                                                               
*                                  PUT END-OF-DATA OBJECT OUT                   
         GOTO1 PUTITEM,DMCB,ITEOD,0                                             
         BNE   EXIT                                                             
         B     CNFX                EXIT ROUTINE                                 
*                                                                               
CNF10    L     R3,=A(ITNWNSYR)     SYNCH RETURN OBJECT                          
         CLI   COMMTYPE,C'I'       INSERTION UPLOAD RETURN OBJECT?              
         BNE   *+8                                                              
         L     R3,=A(ITNWNUPR)     YES                                          
*                                                                               
         L     R2,DATALEN                                                       
         GOTO1 PUTITEM,DMCB,(R3),(R2),CNFOBJCT                                  
         BNE   EXIT                                                             
*                                                                               
         CLI   EOFFLAG,C'Y'        IF END OF FRAME FLAG SET                     
         BE    CNFX                THEN WE NEED MORE                            
*                                                                               
         B     CNFLOOP             LOOP BACK UNTIL NO MORE SCHEDULES            
*                                                                               
CNFX     MVI   CONFFLAG,X'FF'      WE HAVE SOMETHING IN OUR BUFFER              
         B     XIT                 RETURN TO THE CALLER                         
         EJECT                                                                  
SPACES   DC    132C' '                                                          
*                                                                               
EXIT     L     RD,SAVEDRD                                                       
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         SPACE 5                                                                
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE CTMADFFD                                                       
         EJECT                                                                  
       ++INCLUDE CTMADWORKD                                                     
         EJECT                                                                  
       ++INCLUDE CTMADEQUS                                                      
         EJECT                                                                  
       ++INCLUDE CTMADDSECT                                                     
         SPACE 3                                                                
* DDCOMFACS                                                                     
* DDCOREQUS                                                                     
* FAFACTS                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDCOREQUS                                                      
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE DDGLOBEQUS                                                     
       ++INCLUDE DDGLVXCTLD                                                     
         PRINT ON                                                               
         EJECT                                                                  
* OVERLAY WORKING STORAGE                                                       
*                                                                               
OVERD    DSECT                                                                  
APRELO   DS    A                                                                
APBASE1  DS    A                                                                
APBASE2  DS    A                                                                
SAVEDRD  DS    A                   RD UPON ENTRY TO OVERLAY                     
GLOBBER  DS    A                   A(ENTRY) FOR BUYLINE                         
RECUP    DS    V                   V(RECUP)                                     
DAYUNPK  DS    V                   V(DAYUNPK)                                   
PERVERT  DS    V                                                                
NUMBKEYS DS    A                   NUMBER OF DIFFERENT BUY KEYS                 
ABLNTBL  DS    A                   A(BUYLINE TABLE)                             
*                                                                               
*                   BINARY EQUIVALENTS                                          
*                                                                               
KEYPRD   DS    XL1                 PRODUCT CODE TO BE IN THE KEY                
BNUMWKS  DS    XL1                 # OF WEEKS WITHIN ESTIMATE PERIOD            
BPRVWKS  DS    XL1                 # OF WEEKS AS OF LAST TRANSFER               
BDAY1    DS    XL1                 BUY START DAY                                
*                                                                               
*                   MISCELLANEOUS                                               
CONFFLAG DS    XL1                                                              
ENDOBJCT DS    CL1                 END OF OBJECTS FLAG                          
COMMTYPE DS    CL1                 COMMUNICATION TYPE                           
*                                        C'S' - SYSCHRONISATION CHECK           
*                                        C'I' - INSERTION UPLOAD                
*                                                                               
CNFOBJCT DS    CL(LENIO)           DATA FOR CONFIRMATION OBJECT                 
*                                                                               
*                   ELEMENTS                                                    
*                                                                               
ELEMENTS DSECT                                                                  
HDROBJCT DS    0C                  STANDARD HEADER OBJECT STRUCTURE             
HDRELCOD DS    CL2            N    ELEMENT CODE (X'10')                         
HDRELLEN DS    CL3            N    ELEMENT LENGTH                               
HDRACTN  DS    CL1            C    ACTION CODE                                  
HDRMNKEY DS    CL9            C    MAINFRAME KEY                                
HDRDTTIM DS    0CL14          N    DATE/TIME UPDATED                            
HDRDATE  DS    CL8            N    DATE - YYYYMMDD                              
HDRTIME  DS    CL6            N    TIME - HHMMSS                                
HDRERROR DS    CL4            N    ERROR CODE                                   
HDRERFLD DS    CL3            N    ERROR FIELD ID - # OF FIELD IN ERROR         
HDRINSSN DS    CL4            N    NUMBER OF INSERTIONS SENT BY PC              
HDRINSAC DS    CL4            N    NUMBER OF INSERTIONS ACCEPTED                
HDRLNGTH DS    CL4            N    NUMBER OF BYTES OF DATA THAT FOLLOWS         
HDRLEN   EQU   *-HDROBJCT          LENGTH OF HEADER OBJECT                      
*                                                                               
INSD1OBJ DS    0C                  INSERTION DATA 1 OBJECT STRUCTURE            
ID1ELCOD DS    CL2            N    ELEMENT CODE (X'40')                         
ID1ELLEN DS    CL3            N    ELEMENT LENGTH                               
ID1DAYPD DS    CL3            C    DAY/PERIOD                                   
ID1DATE  DS    CL8            N    DATE - YYYYMMDD                              
ID1MPUBN DS    CL6            N    MEDLINE PUB NO.                              
ID1DESCR DS    CL15           C    SPACE DESCRIPTION                            
ID1POSCD DS    CL3            C    POSITION CODE                                
ID1COLCD DS    CL4            C    COLOUR CODE                                  
ID1BKRTE DS    CL9            N    BOOKED RATE (IN PENNIES)                     
ID1CRDRT DS    CL9            N    CARD RATE (IN PENNIES)                       
ID1ADVCD DS    CL8            C    ADVERT CODE                                  
ID1FIXLN EQU   *-INSD1OBJ          LENGTH OF FIXED PORTION                      
ID1CMNT  DS    0C             C    COMMENTS (VARIABLE 0-200 CHARACTERS)         
*                                                                               
INSD2OBJ DS    0C                  INSERTION DATA 2 OBJECT STRUCTURE            
ID2ELCOD DS    CL2            N    ELEMENT CODE (X'50')                         
ID2ELLEN DS    CL3            N    ELEMENT LENGTH                               
ID2XRTE1 DS    CL9            N    EXTRA RATE 1 (IN PENNIES)                    
ID2XRTE2 DS    CL9            N    EXTRA RATE 2 (IN PENNIES)                    
ID2XRTE3 DS    CL9            N    EXTRA RATE 3 (IN PENNIES)                    
ID2XRTE4 DS    CL9            N    EXTRA RATE 4 (IN PENNIES)                    
ID2XRTE5 DS    CL9            N    EXTRA RATE 5 (IN PENNIES)                    
ID2XRTE6 DS    CL9            N    EXTRA RATE 6 (IN PENNIES)                    
ID2XRTE7 DS    CL9            N    EXTRA RATE 7 (IN PENNIES)                    
ID2XRTE8 DS    CL9            N    EXTRA RATE 8 (IN PENNIES)                    
ID2XRTE9 DS    CL9            N    EXTRA RATE 9 (IN PENNIES)                    
ID2LEN   EQU   *-INSD2OBJ          LENGTH OF INSERTION DATA 2 OBJECT            
*                                                                               
INSSTOBJ DS    0C                  INSERTION STATUS OBJECT STRUCTURE            
ISTELCOD DS    CL2            N    ELEMENT CODE (X'60')                         
ISTELLEN DS    CL3            N    ELEMENT LENGTH                               
ISTBILLD DS    CL1            C    BILLED STATUS (C'B')                         
ISTPAID  DS    CL1            C    PAID STATUS (C'P')                           
ISTVOUCH DS    CL1            C    VOUCHER STATUS (C'V')                        
ISTCLTCS DS    CL9            N    CLIENT COST (IN PENNIES)                     
ISTSPARE DS    0C             ?    SPARE  ????                                  
ISTLEN   EQU   *-INSSTOBJ          LENGTH OF INSERTION STATUS OBJECT            
*                                                                               
*              EQUATES                                                          
*                                                                               
LENIO    EQU   2048                LENGTH OF AN IO AREA                         
LENBLNTB EQU   0                   SPARE MEMORY FOR BUYLINE TABLE               
LENFREE  EQU   LENBLNTB            AMOUNT OF SPARE MEMORY NEEDED                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'039CTMAD10   05/01/02'                                      
         END                                                                    
