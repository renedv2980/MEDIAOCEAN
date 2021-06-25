*          DATA SET SRASS00    AT LEVEL 024 AS OF 08/20/12                      
*PHASE T16C00A                                                                  
         TITLE '$ASSIST - ADD INITIALS FOR DARE NOTIFICATION'                   
ASSIST   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WRKL-WRKD,**$ASS**,CLEAR=YES                                     
         USING WRKD,RC             RC=A(W/S)                                    
*                                                                               
         USING SRPARMD,R1          R1=A(PARMS)                                  
         L     RA,SRQATWA                                                       
         USING SRASSFFD,RA         RA=A(TWA)                                    
         L     R9,SRQASYSF                                                      
         USING SYSFACD,R9          R9=A(SYSFACS)                                
         L     R8,SRQAUTL                                                       
         USING UTLD,R8             R8=A(UTL ENTRY)                              
         L     R2,SRQACOMF                                                      
         USING COMFACSD,R2         R2=A(COMFACS)                                
         MVC   VGETTXT,CGETTXT                                                  
         MVC   VSWITCH,CSWITCH                                                  
         DROP  R2                                                               
*                                                                               
         MVC   MSG,=AL2(SI$DIS)                                                 
*                                                                               
         CLI   SRVSAV,C'1'         TEST FIRST TIME                              
         BE    *+12                                                             
         OI    FLAG,FLAGNEW                                                     
         OI    TSVCREQ,X'02'                                                    
*                                                                               
*                                                                               
         BAS   RE,INIT             INITIALISE ALL VALUES                        
*                                                                               
         TM    FLAG,FLAGNEW        FIRST TIME?                                  
         BO    *+8                                                              
         BAS   RE,VAL              VALIDATE SCREEN DETAILS                      
*                                                                               
         BAS   RE,DISP             RE-DISPLAY TASK INFORMATION                  
*                                                                               
         BAS   RE,SETMSG           OUTPUT MESSAGE                               
*                                                                               
EXIT     XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* INITIALISE ALL VALUES                                               *         
***********************************************************************         
         SPACE 1                                                                
INIT     NTR1  ,                                                                
         MVI   SRVSAV,C'1'         SET NOT FIRST TIME                           
         OI    SRVI1H+FHOID,FHOICU SET CURSOR POSITION                          
         L     R1,VSSB                                                          
         MVC   ALET,SSBTBLET-SSBD(R1)                                           
         MVC   FACID,SSBSYSID-SSBD(R1)                                          
INITX    J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE INPUT FIELDS                                               *         
***********************************************************************         
         SPACE 1                                                                
VAL      NTR1  ,                                                                
*                                                                               
         BAS   RE,VKEYWORD         VALIDATE KEYWORD, IF ANY                     
*                                                                               
         XR    R0,R0                R0 HOLDS COUNT OF INPUT FIELDS              
         XC    ASSLOC,ASSLOC                                                    
*                                                                               
LOCAL    USING ASSISTD,ASSLOC                                                   
         MVC   LOCAL.ASSLUID,TLUID                                              
         MVC   LOCAL.ASSFAC,FACID                                               
         MVI   LOCAL.ASSTYPE,ASSTDARE                                           
         LA    R4,LOCAL.ASSINITS                                                
         LA    R1,SRVI1H            FIRST INPUT FIELD                           
         LA    RF,SRVI0H            LAST  INPUT FIELD                           
         LA    RE,SRVI2H-SRVI1H     DISPLACEMENT BETWEEN INPUT FIELDS           
         USING FHD,R1                                                           
*                                                                               
VAL02    CLI   FHIL,0               ANY INPUT?                                  
         BE    VAL04                NO                                          
         MVC   0(L'ASSINITS,R4),FHDA                                            
         CLC   0(L'ASSINITS,R4),SPACES                                          
         BNH   *+12                                                             
         LA    R4,L'ASSINITS(R4)                                                
         AHI   R0,1                                                             
         STC   R0,LOCAL.ASSCNT                                                  
*                                                                               
VAL04    BXLE  R1,RE,VAL02                                                      
*                                                                               
         LTR   R0,R0               ANYTHING TO ADD TO DATASPACE?                
         BNZ   VAL06               NO                                           
         OC    TAINITS,TAINITS     ASSIST ENTRY IN DATASPACE?                   
         BZ    DISPX               NO - THAT'S OK                               
*                                                                               
         XC    ASSLOC,ASSLOC       WRITE EMPTY LINE TO DATASPACE                
         B     VAL12                                                            
*                                                                               
VAL06    LR    RF,R0               SET FUNNIES TO SPACES                        
         MH    RF,=Y(L'ASSINITS)                                                
         LA    R1,LOCAL.ASSINITS                                                
*                                                                               
VAL08    CLI   0(R1),C' '                                                       
         BH    *+8                                                              
         MVI   0(R1),C' '                                                       
         LA    R1,1(R1)                                                         
         BCT   RF,VAL08                                                         
*                                                                               
         LR    RF,R0               SORT ASCENDING                               
         LA    R1,LOCAL.ASSINITS                                                
*                                                                               
VAL07    LR    RE,RF                                                            
         AHI   RE,-1                                                            
         BNP   VAL07C                                                           
         LA    R4,L'ASSINITS(R1)                                                
*                                                                               
VAL07A   CLC   0(L'ASSINITS,R4),SPACES                                          
         BNH   VAL07B                                                           
         CLC   0(L'ASSINITS,R1),0(R4)                                           
         BL    VAL07B              ALREADY SORTED                               
         BH    VAL07AA                                                          
*                                                                               
         AHI   R0,-1               REMOVE DUPLICATE                             
         STC   R0,LOCAL.ASSCNT                                                  
         MVC   0(L'ASSINITS,R4),FFS                                             
         B     VAL07B              ALREADY SORTED                               
*                                                                               
VAL07AA  XC    0(L'ASSINITS,R1),0(R4)                                           
         XC    0(L'ASSINITS,R4),0(R1)                                           
         XC    0(L'ASSINITS,R1),0(R4)                                           
*                                                                               
VAL07B   LA    R4,L'ASSINITS(R4)                                                
         BCT   RE,VAL07A                                                        
         LA    R1,L'ASSINITS(R1)                                                
         BCT   RF,VAL07                                                         
*                                                                               
VAL07C   LA    RF,ASSNDARE         REMOVE ANY DUPLICATES                        
         LA    R1,LOCAL.ASSINITS                                                
*                                                                               
VAL07D   CLC   0(L'ASSINITS,R1),FFS                                             
         BNE   *+10                                                             
         XC    0(L'ASSINITS,R1),0(R1)                                           
         LA    R1,L'ASSINITS(R1)                                                
         BCT   RF,VAL07D                                                        
*                                                                               
         OC    TAINITS,TAINITS     HAVE ENTRY IN DATASPACE?                     
         BNZ   VAL12               YES                                          
*                                                                               
         STAR  CLEAR=ARZERO,ARS=OFF                                             
         XC    DMCB(4*6),DMCB                                                   
         MVC   DMCB(4),=AL4(DTASS)                                              
         GOTO1 VLOCKSPC,DMCB       LOCK DATASPACE                               
         ICM   R2,15,4(R1)                                                      
         BNZ   *+6                                                              
         DC    H'0'                WHY NOT?                                     
*                                                                               
         USING DMSPACED,R2                                                      
         ICM   RF,15,DSPTEND       RF=A(END-1)                                  
         XR    RE,RE                                                            
         ICM   RE,3,DSPTWIDE       RE=WIDTH                                     
         ICM   R2,15,DSPTFRST      R2=A(START)                                  
         L     R1,VSSB                                                          
         LAM   AR2,AR2,SSBTBLET-SSBD(R1)                                        
         SAC   512                                                              
         USING ASSISTD,R2                                                       
*                                                                               
         OC    ASSLUID,ASSLUID     FIND FREE SLOT                               
         BZ    VAL10                                                            
         BXLE  R2,RE,*-10                                                       
         DC    H'0'                TABLE FULL                                   
*                                                                               
VAL10    MVC   ASSLUID,TLUID       USE LUID TO RESERVE SLOT                     
         MVC   ASSFAC,FACID        SET FACPAK IN CASE OF ABEND                  
         MVI   ASSTYPE,ASSTDARE    SET LIST TYPE                                
         STCM  R2,15,TAINITS       SAVE ADDRESS IN UTL                          
         REAR  ARS=OFF                                                          
*                                                                               
         XC    DMCB(4*6),DMCB                                                   
         MVC   DMCB(4),=AL4(DTASS)                                              
         MVI   DMCB,X'10'                                                       
         GOTO1 VLOCKSPC,DMCB       FREE DATASPACE                               
*                                                                               
VAL12    STAR  CLEAR=YES,ARS=ON    MOVE LINE INTO DATASPACE                     
         ICM   R2,15,TAINITS                                                    
         ICM   R1,15,VSSB                                                       
         LAM   AR2,AR2,SSBTBLET-SSBD(R1)                                        
         CLC   0(ASSISTLQ,R2),ASSLOC                                            
         BE    *+10                                                             
         MVC   MSG,=AL2(SI$CHA)                                                 
         MVC   0(ASSISTLQ,R2),ASSLOC                                            
         REAR  ARS=OFF                                                          
*                                                                               
         LTR   R0,R0               DID WE MOVE IN A BLANK LINE?                 
         BNZ   VALX                NO - IT HAD DATA                             
         XC    TAINITS,TAINITS     CLEAR NOTIFY ENTRY IN UTL                    
*                                                                               
VALX     J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* DISPLAY DEFAULT INITIALS FROM KEYWORD, IF PROVIDED                  *         
***********************************************************************         
         SPACE 1                                                                
VKEYWORD NTR1  ,                                                                
         CLI   SRVKYWDH+5,0                                                     
         BE    VKWX                                                             
*                                                                               
         MVI   DMCB,X'0A'          SWITCH TO CONTROL SYSTEM                     
         MVC   DMCB+1(3),=3X'FF'   DON'T CARE IF PROGRAM IS AUTHORIZED          
         GOTO1 VSWITCH,DMCB,,0                                                  
         CLI   DMCB+4,0            SUCCESSFUL?                                  
         BNE   VKWX                NO, SKIP KEYWORD PROCESSING                  
*                                                                               
         XC    KEY,KEY             SEE IF KEYWORD EXISTS                        
         LA    R6,KEY                                                           
         USING DASKEY,R6                                                        
         MVI   DASKTYP,C'D'                                                     
         MVC   DASKCPY,TAGY                                                     
         MVC   DASKINIT,SRVKYWD                                                 
         OC    DASKINIT,SPACES                                                  
         DROP  R6                                                               
*                                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 VDATAMGR,DMCB,=C'DMRDHI',=C'GENDIR',KEYSAVE,KEY                  
         CLC   KEYSAVE(L'DASKEY),KEY                                            
         BNE   VKWX                                                             
         GOTO1 VDATAMGR,DMCB,=C'GETREC',=C'GENFIL',KEY+36,IOAREA,DMWORK         
         CLI   DMCB+8,0                                                         
         BNE   VKWX                                                             
*                                                                               
         TWAXC SRVKYWDH             CLEAR SCREEN                                
*                                                                               
         LA    R6,IOAREA+DASFIRST  R6 = A(1ST ELEM)                             
*                                                                               
         SR    R0,R0                                                            
VKW10    CLI   0(R6),0                                                          
         BE    VKWX                                                             
         CLI   0(R6),X'03'         INITIALS ELEMENT?                            
         BE    VKW20                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     VKW10                                                            
*                                                                               
VKW20    DS    0H                                                               
         USING DASELD,R6                                                        
*                                                                               
         CLI   DASLN,2                                                          
         BNH   VKWX                                                             
         ZIC   R0,DASLN                                                         
         AR    R0,R6                                                            
         LA    RF,DASINIT                                                       
         LA    R1,SRVI1H                                                        
         USING FHD,R1                                                           
         XR    RE,RE                                                            
*                                                                               
VKW30    MVC   FHDA(DASINITL),0(RF)                                             
         OI    FHOI,FHOITR+FHOIMO                                               
         MVI   FHIL,DASINITL                                                    
*                                                                               
         LA    RF,DASINITL(RF)                                                  
         IC    RE,FHLN                                                          
         AR    R1,RE                                                            
         IC    RE,FHLN             PASS PROTECTED NUMBER FIELD                  
         AR    R1,RE                                                            
         LTR   RE,RE                                                            
         BZ    VKWX                                                             
         CR    RF,R0                                                            
         BL    VKW30                                                            
*                                                                               
VKWX     J     EXIT                                                             
         DROP  R1,R6                                                            
         EJECT                                                                  
***********************************************************************         
* DISPLAY INFORMATION FROM ASSIST BLOCK IN DATASPACE                  *         
***********************************************************************         
         SPACE 1                                                                
DISP     NTR1  ,                                                                
         TWAXC SRVKYWDH             CLEAR SCREEN FOR REDISPLAY                  
*                                                                               
         ICM   R2,15,TAINITS        ADDRESS OF ASSIST ENTRY?                    
         BNZ   DISP28               NO                                          
*                                                                               
* CHECK IF LUID ENTRY EXISTS IN DATASPACE                                       
*                                                                               
         STAR  CLEAR=ARZERO,ARS=OFF                                             
         XC    DMCB(4*6),DMCB                                                   
         MVC   DMCB(4),=AL4(DTASS)                                              
         MVI   DMCB,X'20'          ENQUIRE ONLY                                 
         GOTO1 VLOCKSPC,DMCB                                                    
         ICM   R2,15,4(R1)                                                      
         BNZ   *+6                                                              
         DC    H'0'                WHY NOT?                                     
*                                                                               
         USING DMSPACED,R2                                                      
         ICM   RF,15,DSPTEND       RF=A(END-1)                                  
         XR    RE,RE                                                            
         ICM   RE,3,DSPTWIDE       RE=WIDTH                                     
         ICM   R2,15,DSPTFRST      R2=A(START)                                  
         L     R1,VSSB                                                          
         LAM   AR2,AR2,SSBTBLET-SSBD(R1)                                        
         SLL   R2,2                CLEAR X'40' BIT IN HIGH ORDER BYTE           
         SRL   R2,2                                                             
         SAC   512                                                              
         USING ASSISTD,R2                                                       
*                                                                               
DISP10   DS    0H                                                               
         OC    ASSLUID,ASSLUID     NO MORE?                                     
         BZ    DISP25                                                           
         CLC   ASSLUID,TLUID       PREVIOUSLY ENTERED?                          
         BE    DISP20                                                           
         BXLE  R2,RE,DISP10                                                     
         B     DISP25                                                           
*                                                                               
DISP20   DS    0H                                                               
         STCM  R2,15,TAINITS       SAVE ADDRESS IN UTL                          
*                                                                               
DISP25   DS    0H                                                               
         REAR  ARS=OFF                                                          
*                                                                               
DISP28   DS    0H                                                               
         STAR  CLEAR=ARZERO,ARS=ON                                              
         ICM   RF,15,VSSB                                                       
         LAM   AR2,AR2,SSBTBLET-SSBD(RF)                                        
         MVC   ASSLOC,0(R2)         COPY LINE LOCALLY                           
         REAR  ARS=OFF                                                          
*                                                                               
LOCAL    USING ASSISTD,ASSLOC                                                   
         CLI   LOCAL.ASSTYPE,ASSTDARE                                           
         BNE   DISPX               NOT A LIST OF INITIALS                       
*                                                                               
         XR    R0,R0                                                            
         ICM   R0,1,LOCAL.ASSCNT                                                
         BZ    DISPX               NO ENTRIES IN LIST                           
         LA    RF,LOCAL.ASSINITS                                                
         LA    R1,SRVI1H                                                        
         USING FHD,R1                                                           
         XR    RE,RE                                                            
*                                                                               
DISP30   MVC   FHDA(L'ASSINITS),0(RF)                                           
         OI    FHOI,FHOITR+FHOIMO                                               
*                                                                               
         LA    RF,L'ASSINITS(RF)                                                
         IC    RE,FHLN                                                          
         AR    R1,RE                                                            
         IC    RE,FHLN             PASS PROTECTED NUMBER FIELD                  
         AR    R1,RE                                                            
         BCT   R0,DISP30                                                        
*                                                                               
DISPX    J     EXIT                                                             
         DROP  LOCAL                                                            
         EJECT                                                                  
***********************************************************************         
* SET MESSAGE IN HEADER FIELD                                         *         
***********************************************************************         
         SPACE 1                                                                
SETMSG   NTR1  ,                                                                
         LA    R1,PLIST                                                         
         USING GETTXTD,R1                                                       
         XC    GTBLOCK,GTBLOCK                                                  
         MVI   GTMSYS,1            SET SERVICE SYSTEM MESSAGES                  
         MVI   GTMTYP,GTMINF       SET INFO MESSAGE                             
         MVC   GTMSGNO,MSG         SET MESSAGE NUMBER                           
         L     RF,VGETTXT                                                       
         BASR  RE,RF                                                            
SETMSGX  J     EXIT                                                             
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS AND CONSTANTS                                              *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
*                                                                               
SI$DIS   EQU   310                 INITIALS DISPLAYED MESSAGE                   
SI$CHA   EQU   311                 INITIALS CHANGED MESSAGE                     
FF       EQU   X'FF'                                                            
SPACES   DC    32C' '                                                           
FFS      DC    16X'FF'                                                          
ARZERO   DC    16F'0'                                                           
         EJECT                                                                  
***********************************************************************         
* WORKING STORAGE DSECT                                               *         
***********************************************************************         
         SPACE 1                                                                
WRKD     DSECT                                                                  
DUB      DS    D                                                                
PLIST    DS    6F                                                               
VGETTXT  DS    V                                                                
VSWITCH  DS    A                                                                
ALET     DS    A                                                                
MSG      DS    H                                                                
FACID    DS    X                                                                
FLAG     DS    X                                                                
FLAGNEW  EQU   X'80'                                                            
ASSLOC   DS    XL(ASSISTLQ)                                                     
KEY      DS    XL64                                                             
KEYSAVE  DS    XL64                                                             
DMWORK   DS    12D                                                              
DMCB     DS    6F                                                               
IOAREA   DS    XL2000                                                           
WRKL     EQU   *                                                                
         SPACE 2                                                                
***********************************************************************         
* SCREEN DSECT                                                        *         
***********************************************************************         
         SPACE 1                                                                
SRASSFFD DSECT                                                                  
         DS    CL64                                                             
* SRASSFFD                                                                      
       ++INCLUDE SRASSFFD                                                       
         EJECT                                                                  
***********************************************************************         
* OTHER INCLUDED DSECTS                                               *         
***********************************************************************         
         SPACE 1                                                                
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
* FASRPARM                                                                      
         PRINT OFF                                                              
       ++INCLUDE FASRPARM                                                       
         PRINT ON                                                               
* FASYSFAC                                                                      
         PRINT OFF                                                              
       ++INCLUDE FASYSFAC                                                       
         PRINT ON                                                               
* FASSB                                                                         
         PRINT OFF                                                              
       ++INCLUDE FASSB                                                          
         PRINT ON                                                               
* FATCB                                                                         
         PRINT OFF                                                              
       ++INCLUDE FATCB                                                          
         PRINT ON                                                               
* FAUTL                                                                         
         PRINT OFF                                                              
       ++INCLUDE FAUTL                                                          
         PRINT ON                                                               
* DDFH                                                                          
         PRINT OFF                                                              
       ++INCLUDE DDFH                                                           
         PRINT ON                                                               
* DDASSISTD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDASSISTD                                                      
         PRINT ON                                                               
* GEGENASS                                                                      
         PRINT OFF                                                              
       ++INCLUDE GEGENASS                                                       
         PRINT ON                                                               
* FAGETTXTD                                                                     
         PRINT OFF                                                              
       ++INCLUDE FAGETTXTD                                                      
         PRINT ON                                                               
* FATABSDEQU                                                                    
         PRINT OFF                                                              
       ++INCLUDE FATABSDEQU                                                     
         PRINT ON                                                               
* DMSPACED                                                                      
         PRINT OFF                                                              
       ++INCLUDE DMSPACED                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'024SRASS00   08/20/12'                                      
         END                                                                    
