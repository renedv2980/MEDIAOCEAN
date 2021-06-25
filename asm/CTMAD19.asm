*          DATA SET CTMAD19    AT LEVEL 087 AS OF 05/04/05                      
*PHASE TA0C19A                                                                  
*INCLUDE RECUP                                                                  
*INCLUDE MEDGET                                                                 
         TITLE 'CTMAD19 - $MAD UPLOAD NET UNITS'                                
TA0C19   CSECT                                                                  
         PRINT NOGEN                                                            
         NMODL WORKX-WORKD,TA0C19,RA,R8,RR=R2,CLEAR=YES                         
*                                                                               
         USING APPLD,R7            FIRST APPLICATION COMMON STORAGE             
*                                                                               
         LR    RF,RC               RF = A(OVERLAY'S SPARE MEMORY)               
*                                                                               
         L     RC,ACONTD           RC = A(CONTROLLER COMMON STORAGE)            
         USING CONTROLD,RC         CONTROLLER COMMON STORAGE                    
         L     R9,AOVER            R9 = A(OVERLAY SAVED STORAGE)                
         USING OVERD,R9            OVERLAY SAVED STORAGE                        
*                                                                               
         ST    RD,SAVEDRD          SAVE STACK POINTER FOR RETURNING             
         ST    RF,AWORKSTR         SAVE A(UNSAVED WORKING STORAGE)              
*                                                                               
         ST    R2,APRELO                                                        
         ST    RB,APBASE1          SAVE PROGRAM BASE REGISTER 1                 
         ST    RA,APBASE2          SAVE PROGRAM BASE REGISTER 2                 
         EJECT                                                                  
***********************************************************************         
* MAIN DRIVER - CALLS THE THREE SUBROUTINES THAT HANDLE EACH OF THE             
* THREE MODES THAT THE CONTROLLER CALLS AN OVERLAY WITH (START, MIDDLE,         
* AND END).                                                                     
***********************************************************************         
MAIN     BAS   RE,INIT             INITIALIZE OVERLAY                           
*                                                                               
         CLI   OVERMODE,C'S'       IF MODE IS START                             
         BNE   MAIN10                                                           
         BAS   RE,PROCSTRT         THEN CALL PROCSTRT                           
         MVI   OVERMODE,C'M'       SET MODE IS MIDDLE                           
         B     MAIN10                                                           
         L     R1,ATWA             USE INTERNAL TABLE ENTRY?                    
         LA    R1,MADDATA-TA0CFFD(R1)                                           
         CLI   0(R1),C'X'          ONE OF OURS?                                 
         BNE   *+10                                                             
         MVC   TSTDATAF,1(R1)                                                   
         CLI   TSTDATAF,0          ARE WE TESTING?                              
         BE    MX                                                               
         BAS   RE,PROCMID          THEN CALL PROCMID                            
         B     MX                                                               
*                                                                               
MAIN10   CLI   OVERMODE,C'M'       ELSE IF MODE IS MIDDLE                       
         BNE   *+12                                                             
         BAS   RE,PROCMID          THEN CALL PROCMID                            
         B     MX                                                               
*                                                                               
         BAS   RE,PROCEND          ELSE CALL PROCEND                            
*                                                                               
MX       B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* INITIALIZE FIELDS                                                             
***********************************************************************         
INIT     NTR1                                                                   
*                                                                               
         MVC   SYSFIL,=CL8'UNTFIL'                                              
         MVC   SYSDIR,=CL8'UNTDIR'                                              
         MVI   ISDAFILE,C'Y'       SET DA FILE                                  
         MVI   DISPDSKA,21         DISPLACEMENT TO D/A                          
                                                                                
         L     RE,AWORKSTR         SAVE A(UNSAVED WORKING STORAGE)              
         USING WORKD,RE                                                         
         LA    RF,MINBLK           MINIO PARAMETER BLOCK                        
         ST    RF,AMINBLK                                                       
         LA    RF,MINBUFFS         MINIO BUFFERS                                
         ST    RF,AMINBUFF                                                      
         LA    RF,MINRECTB         MINIO RECORD TABLE                           
         ST    RF,AMINRECT                                                      
         DROP  RE                                                               
*                                                                               
         L     RE,ACOMFACS                                                      
         USING COMFACSD,RE                                                      
         MVC   GLOBBER,CGLOBBER    A(GLOBBER)                                   
         MVC   MINIO,CMINIO        A(MINIO)                                     
         DROP  RE                                                               
*                                                                               
         L     RE,=V(RECUP)                                                     
         AR    RE,R2                                                            
         ST    RE,RECUP            A(RECUP)                                     
         L     RE,=V(MEDGET)                                                    
         AR    RE,R2                                                            
         ST    RE,MEDGET           A(MEDGET)                                    
*                                                                               
         GOTO1 SWITCH,DMCB,=C'NET',0                                            
         CLI   DMCB+4,0                                                         
         BE    *+14                                                             
         MVC   MDACTION,=Y(ERSWITCH)  ERROR SWITCHING TO NET SYSTEM             
         B     EXIT                   EXIT $MAD WITH AN ERROR CODE              
*                                                                               
         BAS   RE,INITMIN          INITIALIZE MINIO                             
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* INITIALIZE MINIO                                                              
***********************************************************************         
INITMIN  NTR1                                                                   
         L     R5,AMINBLK                                                       
         USING MINBLKD,R5                                                       
         LR    RE,R5               CLEAR MINIO PARAMETER BLOCK                  
         LH    RF,=Y(MINBLKL)                                                   
         XCEFL                                                                  
         MVI   MINOPEN,C'N'        SET NOT OPEN                                 
         MVC   MINFIL,=C'UNTFIL  ' FILE NAME                                    
         MVC   MINDIR,=C'UNTDIR  ' DIR NAME                                     
         MVI   MINFKLEN,L'NEUPKEY  KEY LENGTH                                   
         MVI   MINNCTL,1           NUMBER OF CONTROL BYTES                      
         MVC   MINFRCLM,=Y(LENMINRC)  MAXIMUM RECORD LENGTH                     
         MVI   MINEKLEN,L'NEUPEKEY ELEMENT KEY LENGTH                           
         MVI   MINEKDSP,NEUPEKEY-NEUPREC  DISPLACEMENT TO ELEMENT KEY           
         MVC   MINBUFF,AMINBUFF    A(FIRST BUFFER)                              
         MVC   MINRTAB,AMINRECT    A(AREA FOR RECORD TABLE)                     
         LA    RF,ELEMENT                                                       
         ST    RF,MINELEM          A(AREA FOR MINIO ELEMENT)                    
         MVC   MINCOMF,ACOMFACS    A(COMFACS)                                   
         MVC   MINRECUP,RECUP      A(RECUP)                                     
         MVC   MINRTABL,=Y(MINRCTBL) LENGTH OF RECORD TABLE                     
         MVC   MINMAXEL,=H'250'    MAX LENGTH OF ELEMENT                        
         MVI   MINNBUF,NUMMNBUF    NUMBER OF AVAILABLE BUFFERS                  
         MVC   MINWRITE,SVWRTFLG   'N' = DON'T WRITE TO FILE                    
*                                                                               
         BAS   RE,SMINMKEY         SET MINIO MASTER KEY                         
*                                                                               
INITMX   B     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE PROCESSES THE START MODE.  IT GETS THE FILE HEADER               
* OBJECT, INITIALIZES MINIO.                                                    
***********************************************************************         
PROCSTRT NTR1                                                                   
         MVI   FRSTIME,C'Y'                                                     
         MVI   TMPOPEND,C'N'       HAVE NOT YET OPENED TEMPSTR                  
         MVI   MDLAST,C'N'         NOT YET LAST OUTPUT DATA FRAME               
         MVI   CONFFLAG,0          HAVEN'T CONFIRM YET                          
         MVI   MISCFLG1,0          CLEAR MISCELLANEOUS FLAG 1                   
         MVI   ENDOBJCT,C'N'       NOT END OF OBJECTS                           
         XC    RECNUM,RECNUM       NO RECORDS YET                               
         XC    UNTNUM,UNTNUM       NO UNITS YET                                 
         XC    TOTALDEL,TOTALDEL   NO UNITS DELETED YET                         
         XC    ABSRECN,ABSRECN     ABSOLUTE RECORD NUMBER                       
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE PROCESSES THE CONTINUATION MODE.  IT GETS ALL UNITS              
* FOR ALL STATIONS AND PUTS THEM IN THE TEMPSTR BUFFERS.                        
***********************************************************************         
PROCMID  NTR1                                                                   
         MVI   PUTINFO,C'N'                                                     
         CLI   FRSTIME,C'Y'        IF THIS IS THE FIRST TIME                    
         BNE   *+8                                                              
         BAS   RE,PROCHEAD         PROCESS THE HEADER                           
*                                                                               
         CLI   ENDOBJCT,C'Y'       IF END OF OBJECTS                            
         BE    PM30                THEN WE CAN CONFIRM                          
         CLI   ENDBLOCK,C'Y'       IF END OF BLOCK                              
         BE    PM30                THEN WE CAN CONFIRM                          
*                                                                               
         CLI   TMPOPEND,C'Y'       IS TEMPSTR ALREADY OPEN?                     
         BE    PM10                                                             
*                                                                               
         GOTO1 TMPOPEN,DMCB,(C'S',=C'PTX'),,(3,0)                               
         BNE   EXIT                                                             
         MVI   TMPOPEND,C'Y'       TEMPSTR IS OPEN                              
*                                                                               
PM10     BAS   RE,GETOBJCT                                                      
*                                                                               
         CLI   EIFFLAG,C'Y'        ANY MORE DATA FOR THIS INPUT FRAME?          
         BE    PM20                NO MORE                                      
*                                                                               
         CLI   ENDOBJCT,C'Y'       ANY MORE OBJECTS COMING?                     
         BE    PM15                                                             
         CLI   ENDBLOCK,C'Y'       IF END OF BLOCK                              
         BNE   PM10                                                             
*                                                                               
PM15     GOTO1 TMPCLOSE            DONE WITH PUTTING INTO TEMPFILE              
         MVI   TMPOPEND,C'N'       HAVE NOT YET OPENED TEMPSTR                  
*                                                                               
         BAS   RE,CALLBUY          CALL NET BUY PROGRAM                         
*                                                                               
PM20     CLI   TSTDATAF,0          ARE WE TESTING WITHOUT REAL UPLOAD?          
         BE    *+8                                                              
         MVI   OVERMODE,C'M'       YES                                          
         B     PMX                                                              
*                                                                               
PM30     CLI   CONFFLAG,X'FF'      CONFIRM USED BEFORE?                         
         BE    PM40                YES                                          
*                                  DID WE GET CONTROL BACK FROM NBUY?           
         GOTO1 GLOBBER,DMCB,=C'GETD',BLOCK,24,GLVXCTL                           
         CLI   DMCB+8,0                                                         
         BNE   PM40                                                             
*                                                                               
         GOTO1 GLOBBER,DMCB,=C'DELE',,,GLVXCTL                                  
         CLI   DMCB+8,0                                                         
         BE    PM40                                                             
         MVC   MDACTION,=Y(ER10GDEL)  PROBLEM WITH GLOBBER DELETE               
         B     EXIT                EXIT $MAD WITH AN ERROR CODE                 
*                                                                               
PM40     BAS   RE,CONFIRM          SEND OUT CONFIRMATIONS                       
         CLI   TSTDATAF,0          ARE WE TESTING WITHOUT REAL UPLOAD?          
         BNE   PM45                                                             
*                                                                               
PM45     CLI   ENDOBJCT,C'Y'       DID WE SEE THE END-OF-DATA OBJECT?           
         BNE   PM10                NO - GO GET NEXT                             
*                                                                               
         OC    TOTALDEL,TOTALDEL   ANY UNITS DELETED?                           
         BZ    PM50                                                             
         LA    R2,WORK             BUILD RECORD DELETED TOTAL OBJECT            
         XC    WORK,WORK                                                        
         USING DSTATUSD,R2                                                      
         MVC   DSTATYPE,=C'04'     RECORDS DELETED OBJECT                       
         L     R0,TOTALDEL         TOTAL NUMBER OF UNITS DELETED                
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  DSTATOTD,DUB+4(4)                                                
         L     R3,=A(ITNBYSTA)     STATUS OBJECT                                
         LH    R0,=Y(DSTAOKLQ)     L'OBJECT                                     
         GOTO1 PUTITEM,DMCB,(R3),(R0),WORK                                      
         BNE   EXIT                                                             
         DROP  R2                                                               
                                                                                
PM50     GOTO1 PUTITEM,DMCB,ITEOD,0   THAT'S ALL, FOLKS!                        
         BNE   EXIT                                                             
         MVI   MDLAST,C'Y'            LAST OUTPUT DATA FRAME                    
         B     PMX                                                              
                                                                                
                                                                                
PMX      B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE PROCESSES THE END MODE.  IT CURRENTLY DOES NOTHING.              
***********************************************************************         
PROCEND  NTR1                                                                   
*                                                                               
PEX      B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE PROCESSES THE HEADER RECORD                                      
***********************************************************************         
PROCHEAD NTR1                                                                   
         MVI   PUTINFO,C'N'                                                     
         MVI   FRSTIME,C'N'                                                     
         BAS   RE,GETDATA                                                       
*                                                                               
         CLC   TYPENUM,=A(ITNBYHDR) THIS MUST BE FILE HEADER OBJECT             
         BE    PH10                                                             
         MVC   MDACTION,=Y(ER11OBCD)  INVALID OBJECT CODE                       
         B     EXIT                EXIT $MAD WITH AN ERROR CODE                 
*                                                                               
PH10     BAS   RE,SETSAVE          SET SAVED DATA                               
*                                                                               
         BAS   RE,VALHEADR         VALIDATE HEADER RECORD FIELDS                
         CLI   ERFLDNUM,0          ANY ERROR ON HEADER?                         
         BE    PH20                                                             
         BAS   RE,HDRERR           HEADER RECORD IN ERROR                       
         B     EXIT                                                             
*                                                                               
PH20     LA    RE,MYOBJECT         BUILD HEADER OBJECT HERE                     
         USING NHDRODAT,RE                                                      
         L     RF,ADATA            OBJECT AS GIVEN TO ME                        
         XC    NHDRODAT(NHDRLENQ),NHDRODAT                                      
         MVC   NHDRTYPE,=C'HDR*'                                                
         MVC   NHDRSTRT(NHDRRLNQ),0(RF)   OBJECT DATA                           
         DROP  RE                                                               
*                                                                               
         GOTO1 TMPOPEN,DMCB,(C'S',=C'PTX'),,(3,0)                               
         BNE   EXIT                                                             
         MVI   TMPOPEND,C'Y'       TEMPSTR IS OPEN                              
*                                                                               
         LH    R0,=Y(NHDRLENQ)     WRITE FILE HEADER OBJECT TO TEMPSTR          
         GOTO1 PUTTMP,DMCB,MYOBJECT,(R0)                                        
         BNE   EXIT                                                             
         MVI   PUTINFO,C'Y'                                                     
*                                                                               
PHX      B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE GETS THE UNITS FROM THE INPUT FRAMES                             
***********************************************************************         
GETOBJCT NTR1                                                                   
         MVI   MINCHG,C'N'         NO CHANGES TO MINIO RECORD YET               
*                                                                               
GOBJLOOP BAS   RE,GETDATA                                                       
*                                                                               
         MVI   SEQNUM,0            RESET SEQUENCE NUMBER                        
         CLI   EIFFLAG,C'Y'        END OF FRAME?                                
         BE    GOBJ50              NO                                           
         CLC   TYPENUM,=A(ITEOD)   IF OBJECT IS END-OF-DATA                     
         BNE   GOB10                                                            
         MVI   ENDOBJCT,C'Y'       NO MORE OBJECTS TO READ                      
         BAS   RE,SETEOF                                                        
         B     GOBJ50                                                           
*                                                                               
GOB10    CLC   TYPENUM,=A(ITEOB)   IF OBJECT IS END-OF-BLOCK                    
         BNE   GOB20                                                            
***      CLI   PUTINFO,C'Y'        IF NOTHING WAS PUT TO TEMPSTR YET            
***      BNE   GOBJLOOP            GET NEXT PAGE                                
         MVI   ENDBLOCK,C'Y'                                                    
         BAS   RE,SETEOF                                                        
         B     GOBJ50                                                           
*                                                                               
GOB20    L     R1,AIO              COPY THE OBJECT TO LOCAL STORAGE             
         LA    R1,4(R1)            LEAVE ROOM FOR OBJECT IDENTIFIER             
         MVI   0(R1),C' '                                                       
         MVC   1(255,R1),0(R1)     PRE-FILL WITH BLANKS                         
         LR    R0,R1                                                            
         L     R1,DATALEN                                                       
         L     RE,ADATA                                                         
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
         BAS   RE,VALOBJCT         VALIDATE THE OBJECT                          
         B     GOBJLOOP                                                         
*                                                                               
GOBJ50   CLI   MINCHG,C'Y'         ANY CHANGE TO THIS MINIO RECORD?             
         BNE   GOBJX                                                            
*                                                                               
         L     R5,AMINBLK          YES -- SAVE IT AT END OF FRAME               
         USING MINBLKD,R5                                                       
         BAS   RE,CLSMIN                                                        
                                                                                
GOBJX    B     XIT                 RETURN TO CALLER                             
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE TESTS IF WE NEED TEST OR REAL DATA & GETS IT                     
***********************************************************************         
GETDATA  NTR1                                                                   
         MVI   TSTDATAF,0                                                       
         L     R1,ATWA             USE INTERNAL TABLE ENTRY?                    
         LA    R1,MADDATA-TA0CFFD(R1)                                           
         CLI   0(R1),C'X'          ONE OF OURS?                                 
         BNE   GD20                                                             
         MVC   TSTDATAF,1(R1)                                                   
         CLI   1(R1),C'1'                                                       
         BE    GD10                                                             
         CLI   1(R1),C'2'                                                       
         BE    GD10                                                             
         CLI   1(R1),C'3'                                                       
         BNE   GD20                                                             
*                                                                               
GD10     GOTO1 GETTITEM            GET TEST TABLE ITEM                          
         B     GD30                                                             
*                                                                               
GD20     GOTO1 GETITEM             GET FIRST OBJECT                             
         BNE   EXIT                                                             
*                                                                               
GD30     DS    0H                                                               
         CLI   EIFFLAG,C'Y'                                                     
         BE    GDX                                                              
         TM    MISCFLG1,MF1MVU     WE USING NEW FORMAT (MVU)?                   
         BO    GD40                                                             
***  WE'RE USING OLDER FORMAT (NBU)                                             
***  NBU  ***                                                                   
         CLC   TYPENUM,=A(ITNBYHDR) HEADER OBJECT                               
         BE    GDX                                                              
         CLC   TYPENUM,=A(ITNBYEPK) END PACKAGE OBJECT?                         
         BE    GDX                                                              
         CLC   TYPENUM,=A(ITNBYEPG) END PROG OBJECT?                            
         BE    GDX                                                              
         CLC   TYPENUM,=A(ITNBYEUN) END OF UNIT OBJECT?                         
         BE    GDX                                                              
         CLC   TYPENUM,=A(ITNBYEOD) END OF DEAL OBJECT?                         
         BE    GDX                                                              
         CLC   TYPENUM,=A(ITEOB)    END-OF-BLOCK OBJECT?                        
         BE    GDX                                                              
         CLC   TYPENUM,=A(ITEOD)    END-OF-DATA OBJECT?                         
         BE    GDX                                                              
         B     GD50                                                             
***  NBU  ***                                                                   
*                                                                               
***  WE'RE USING NEWER FORMAT (MVU)                                             
***  MVU  ***                                                                   
GD40     CLC   TYPENUM,=A(ITNBYHDR) HEADER OBJECT                               
         BE    GDX                                                              
         CLC   TYPENUM,=A(ITNBYEPK) END PACKAGE OBJECT?                         
         BE    GDX                                                              
         CLC   TYPENUM,=A(ITNBYEUN) END UNIT UPDATE OBJECT?                     
         BE    GDX                                                              
         CLC   TYPENUM,=A(ITNBYEOD) END OF DEAL OBJECT?                         
         BE    GDX                                                              
         CLC   TYPENUM,=A(ITEOB)    END-OF-BLOCK OBJECT?                        
         BE    GDX                                                              
         CLC   TYPENUM,=A(ITEOD)    END-OF-DATA OBJECT?                         
         BE    GDX                                                              
***  MVU  ***                                                                   
*                                                                               
GD50     L     R1,ABSRECN           ABSOLUTE RECORD NUMBER                      
         LA    R1,1(R1)                                                         
         ST    R1,ABSRECN                                                       
*                                                                               
GDX      B     XIT                  RETURN TO CALLER                            
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE VALIDATES THE OBJECT WE GOT                                      
***********************************************************************         
VALOBJCT NTR1                                                                   
*                                                                               
         L     R5,AMINBLK                                                       
         USING MINBLKD,R5                                                       
*                                                                               
         TM    MISCFLG1,MF1MVU     IS IT NEW FORMAT MVU?                        
         BO    VOMVU00              - YUP, LET'S GO THERE                       
*                                                                               
***  NBU  ***                                                                   
         TM    MYSTAT,DEALERR      IF THERE WAS A DEAL ERROR                    
         BNO   VO05                                                             
         CLC   TYPENUM,=A(ITNBYDL)  SKIP TO NEXT DEAL                           
         BNE   VOBJX                                                            
         NI    MYSTAT,X'FF'-(DEALERR+PKGERR+PGMERR)                             
*                                                                               
VO05     CLC   TYPENUM,=A(ITNBYDL)  DEAL OBJECT?                                
         BNE   VO10                                                             
         BAS   RE,VALDEAL                                                       
         B     VOBJX                                                            
*                                                                               
VO10     TM    MYSTAT,PKGERR       IF THERE WAS A PKG ERROR                     
         BNO   VO15                                                             
         CLC   TYPENUM,=A(ITNBYPKG)  SKIP TO NEXT PACKAGE                       
         BNE   VOBJX                                                            
         NI    MYSTAT,X'FF'-(PKGERR+PGMERR)                                     
*                                                                               
VO15     CLC   TYPENUM,=A(ITNBYPKG) PACKAGE OBJECT?                             
         BNE   VO20                                                             
         BAS   RE,VALPKG                                                        
         B     VOBJX                                                            
*                                                                               
VO20     TM    MYSTAT,PGMERR       IF THERE WAS A PGM ERROR                     
         BNO   VO25                                                             
         CLC   TYPENUM,=A(ITNBYPGM)  SKIP TO NEXT PROGRAM                       
         BNE   VOBJX                                                            
         NI    MYSTAT,X'FF'-PGMERR                                              
*                                                                               
VO25     CLC   TYPENUM,=A(ITNBYPGM) PROGRAM OBJECT?                             
         BNE   VO30                                                             
         BAS   RE,VALPGM                                                        
         B     VOBJX                                                            
*                                                                               
VO30     CLC   TYPENUM,=A(ITNBYPDE) PROG DEMO EST OBJECT?                       
         BNE   VO40                                                             
         BAS   RE,SETPDE                                                        
         B     VOBJX                                                            
*                                                                               
VO40     CLC   TYPENUM,=A(ITNBYPDA) PROG DEMO ACT OBJECT?                       
         BNE   VO50                                                             
         BAS   RE,SETPDA                                                        
         B     VOBJX                                                            
*                                                                               
VO50     CLC   TYPENUM,=A(ITNBYEPK) END PACKAGE OBJECT?                         
         BNE   VO52                                                             
         BAS   RE,SETEPK                                                        
         B     VOBJX                                                            
*                                                                               
VO52     CLC   TYPENUM,=A(ITNBYEPG) END PROG OBJECT?                            
         BNE   VO55                                                             
         BAS   RE,SETEPG                                                        
         B     VOBJX                                                            
*                                                                               
VO55     CLC   TYPENUM,=A(ITNBYUNT) UNIT OBJECT?                                
         BNE   VO60                                                             
         BAS   RE,VALUNT                                                        
         B     VOBJX                                                            
*                                                                               
VO60     CLC   TYPENUM,=A(ITNBYUDE) UNIT DEMO EST OBJECT?                       
         BNE   VO70                                                             
         BAS   RE,SETUDE                                                        
         B     VOBJX                                                            
*                                                                               
VO70     CLC   TYPENUM,=A(ITNBYUDA) UNIT DEMO ACT OBJECT?                       
         BNE   VO80                                                             
         BAS   RE,SETUDA                                                        
         B     VOBJX                                                            
*                                                                               
VO80     CLC   TYPENUM,=A(ITNBYUSC) UNIT SPECIAL CHARGES OBJECT?                
         BNE   VO90                                                             
         BAS   RE,SETUSP                                                        
         B     VOBJX                                                            
*                                                                               
VO90     CLC   TYPENUM,=A(ITNBYUPR) UNIT PRODUCT OBJECT?                        
         BNE   VO100                                                            
         BAS   RE,SETUPR                                                        
         B     VOBJX                                                            
*                                                                               
VO100    CLC   TYPENUM,=A(ITNBYUCO) UNIT COMMENT OBJECT?                        
         BNE   VO110                                                            
         BAS   RE,SETUCO                                                        
         B     VOBJX                                                            
*                                                                               
VO110    CLC   TYPENUM,=A(ITNBYUCM) UNIT COMMERCAIL OBJECT?                     
         BNE   VO120                                                            
         BAS   RE,SETUCM                                                        
         B     VOBJX                                                            
*                                                                               
VO120    CLC   TYPENUM,=A(ITNBYEUN) END OF UNIT OBJECT?                         
         BNE   VO130                                                            
         BAS   RE,SETEUN                                                        
         B     VOBJX                                                            
*                                                                               
VO130    CLC   TYPENUM,=A(ITNBYEOD) END OF DEAL OBJECT?                         
         BNE   VO140                                                            
         BAS   RE,SETEOD                                                        
         B     VOBJX                                                            
*                                                                               
VO140    B     VOBJX                                                            
***  NBU  ***                                                                   
*                                                                               
***  MVU  ***                                                                   
VOMVU00  TM    MYSTAT,DEALERR      IF THERE WAS A DEAL ERROR                    
         BNO   VOMVU05                                                          
         CLC   TYPENUM,=A(ITNBYDL)  SKIP TO NEXT DEAL                           
         BNE   VOBJX                                                            
         NI    MYSTAT,X'FF'-(DEALERR+PKGERR+CHUERR)                             
*                                                                               
VOMVU05  CLC   TYPENUM,=A(ITNBYDL)  DEAL OBJECT?                                
         BNE   VOMVU10                                                          
         BAS   RE,VALDEAL                                                       
         B     VOBJX                                                            
*                                                                               
VOMVU10  TM    MYSTAT,PKGERR       IF THERE WAS A PKG ERROR                     
         BNO   VOMVU15                                                          
         CLC   TYPENUM,=A(ITNBYPKG)  SKIP TO NEXT PACKAGE                       
         BNE   VOBJX                                                            
         NI    MYSTAT,X'FF'-(PKGERR+CHUERR)                                     
*                                                                               
VOMVU15  CLC   TYPENUM,=A(ITNBYPKG) PACKAGE OBJECT?                             
         BNE   VOMVU20                                                          
         BAS   RE,VALPKG                                                        
         B     VOBJX                                                            
*                                                                               
VOMVU20  TM    MYSTAT,CHUERR       IF THERE WAS A UNIT UPDATE ERROR             
         BNO   VOMVU25                                                          
         CLC   TYPENUM,=A(ITNBYCHU)  SKIP TO NEXT UNIT UPDATE                   
         BNE   VOBJX                                                            
         NI    MYSTAT,X'FF'-CHUERR                                              
*                                                                               
VOMVU25  CLC   TYPENUM,=A(ITNBYCHU) UNIT UPDATE OBJECT?                         
         BNE   VOMVU30                                                          
         BAS   RE,VALCHU                                                        
         B     VOBJX                                                            
*                                                                               
VOMVU30  CLC   TYPENUM,=A(ITNBYUPR) UNIT PRODUCT OBJECT?                        
         BNE   VOMVU40                                                          
         BAS   RE,SETUPR                                                        
         B     VOBJX                                                            
*                                                                               
VOMVU40  CLC   TYPENUM,=A(ITNBYEUN) END OF UNIT OBJECT?                         
         BNE   VOMVU50                                                          
         BAS   RE,SETEUN                                                        
         B     VOBJX                                                            
*                                                                               
VOMVU50  CLC   TYPENUM,=A(ITNBYEPK) END PACKAGE OBJECT?                         
         BNE   VOMVU60                                                          
         BAS   RE,SETEPK                                                        
         B     VOBJX                                                            
*                                                                               
VOMVU60  CLC   TYPENUM,=A(ITNBYEOD) END OF DEAL OBJECT?                         
         BNE   VOMVU100                                                         
         BAS   RE,SETEOD                                                        
         B     VOBJX                                                            
*                                                                               
VOMVU100 DS    0H                                                               
***  MVU  ***                                                                   
*                                                                               
VOBJX    B     XIT                 RETURN TO CALLER                             
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE VALIDATES THE DEAL OBJECT                                        
***********************************************************************         
VALDEAL  NTR1                                                                   
         XC    MYSTAT,MYSTAT                                                    
         L     R6,AIO              DEAL OBJECT                                  
         USING UDLD,R6                                                          
         MVC   UDLTYPE,=C'DEAL'                                                 
         XC    MYOBJLEN,MYOBJLEN   NOTHING IN OBJECT YET                        
         XC    SUBNUM,SUBNUM       RESET SUB-RECORD NUMBER                      
*                                                                               
         LA    R4,MYOBJECT         BUILD OBJECT HERE                            
         USING NDLODAT,R4                                                       
         XC    NDLODAT(NDLLENQ),NDLODAT                                         
         MVC   NDLRECN,ABSRECN+2                                                
         MVC   NDLSTRT(NDLRLNQ),UDLD   OBJECT DATA                              
*                                                                               
         MVC   SVMED,NDLMED        SET MEDIA                                    
*                                                                               
         MVC   ERFLDNUM,NDLMEDQ    FIELD NUMBER                                 
         MVC   ERRNUM,=H'2'        INVALID INPUT FIELD                          
         BAS   RE,GETMED                                                        
         TM    MYSTAT,DEALERR                                                   
         BO    VDX                                                              
         MVC   SVCLT,NDLCLT             CLT                                     
         MVC   SVDEAL,NDLDEAL           DEAL                                    
         MVC   SVNET,NDLNET             NETWORK                                 
         DROP  R4                                                               
         GOTO1 CLPACK,DMCB,SVCLT,SVBCLT                                         
*                                                                               
         BAS   RE,DEALMIN          SET UP MINIO RECORD FOR THIS DEAL            
*                                                                               
         LH    R0,=Y(NDLLENQ)      L'OBJECT                                     
         GOTO1 PUTTMP,DMCB,MYOBJECT,(R0)                                        
         BNE   EXIT                                                             
         MVI   PUTINFO,C'Y'                                                     
*                                                                               
VDX      B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE VALIDATES THE PACKAGE OBJECT                                     
***********************************************************************         
VALPKG   NTR1                                                                   
         L     R6,AIO              PACKAGE OBJECT                               
         USING UPKGD,R6                                                         
         MVC   UPKGTYPE,=C'PKG*'                                                
         XC    MYOBJLEN,MYOBJLEN   NOTHING IN OBJECT YET                        
         XC    SUBNUM,SUBNUM       RESET SUB-RECORD NUMBER                      
         PACK  DUB,UPKGEST(3)                                                   
         CVB   R1,DUB                                                           
         STC   R1,SVEST            ESTIMATE CODE                                
         MVC   SVDPT,UPKGDPT       DAYPART                                      
*                                                                               
         LA    RE,MYOBJECT         BUILD OBJECT HERE                            
         USING NPKGODAT,RE                                                      
         XC    NPKGODAT(NPKGLENQ),NPKGODAT                                      
         MVC   NPKGRECN,ABSRECN+2                                               
         MVC   NPKGSTRT(NPKGRLNQ),UPKGD OBJECT DATA                             
*                                                                               
         LH    R0,=Y(NPKGLENQ)     L'OBJECT                                     
         GOTO1 PUTTMP,DMCB,MYOBJECT,(R0)                                        
         BNE   EXIT                                                             
         MVI   PUTINFO,C'Y'                                                     
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE VALIDATES THE PROGRAM OBJECT                                     
***********************************************************************         
VALPGM   NTR1                                                                   
         L     R6,AIO              PROGRAM OBJECT                               
         USING UPGMD,R6                                                         
         MVC   UPGMTYPE,=C'PROG'                                                
         XC    SUBNUM,SUBNUM       RESET SUB-RECORD NUMBER                      
         XC    MYOBJLEN,MYOBJLEN   NOTHING IN OBJECT YET                        
*                                                                               
         LA    RE,MYOBJECT         BUILD OBJECT HERE                            
         USING NPGMD,RE                                                         
         XC    NPGMODAT(NPGMLENQ),NPGMODAT                                      
         MVC   NPGMRECN,ABSRECN+2                                               
         MVC   NPGMSTRT(NPGMRLNQ),UPGMD OBJECT DATA                             
         MVC   NPGMLEN,=Y(NPGMLENQ) L'RECORD                                    
         LH    RF,NPGMLEN          RECORD LENGTH                                
         LA    RF,2(RF)                                                         
         STH   RF,MYOBJLEN         L'OBJECT FOR TEMPSTR SO FAR                  
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE VALIDATES THE UNIT UPDATE OBJECT                                 
***********************************************************************         
VALCHU   NTR1                                                                   
         L     R6,AIO              PROGRAM OBJECT                               
         USING UCHUD,R6                                                         
         MVC   UCHUTYPE,=C'UNUP'                                                
         XC    SUBNUM,SUBNUM       RESET SUB-RECORD NUMBER                      
         XC    MYOBJLEN,MYOBJLEN   NOTHING IN OBJECT YET                        
*                                                                               
         LA    RE,MYOBJECT         BUILD OBJECT HERE                            
         USING NCHUD,RE                                                         
         XC    NCHUODAT(150),NCHUODAT                                           
         XC    NCHUODAT+150(NCHTLENQ-150),NCHUODAT+150                          
         MVC   NCHURNUM,ABSRECN+2                                               
         MVC   NCHUSTRT(150),UCHUD OBJECT DATA                                  
         MVC   NCHUSTRT+150(NCHTRLNQ-150),UCHUD+150 OBJECT DATA                 
         MVC   NCHULEN,=Y(NCHTLENQ) L'RECORD                                    
         LH    RF,NCHULEN          RECORD LENGTH                                
         LA    RF,2(RF)                                                         
         STH   RF,MYOBJLEN         L'OBJECT FOR TEMPSTR SO FAR                  
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE SETS A PROGRAM DEMO EST OBJECT IN MYOBJECT                       
***********************************************************************         
SETPDE   NTR1                                                                   
         L     R6,AIO                                                           
         USING UPDED,R6                                                         
         MVC   UPDETYPE,=C'PDRE'                                                
         LH    R1,=Y(NPDELENQ)     LENGTH OF OBJECT                             
         BAS   RE,SETINFO                                                       
         USING NPDED,R3                                                         
         MVC   NPDELEN,=Y(NPDELENQ)     LENGTH OF OBJECT                        
         MVC   NPDERECN,ABSRECN+2                                               
         B     XIT                                                              
         DROP  R3,R6                                                            
         SPACE 3                                                                
***********************************************************************         
* THIS ROUTINE SETS A PROGRAM DEMO ACT OBJECT IN MYOBJECT                       
***********************************************************************         
SETPDA   NTR1                                                                   
         L     R6,AIO                                                           
         USING UPDAD,R6                                                         
         MVC   UPDATYPE,=C'PDRA'                                                
         LH    R1,=Y(NPDALENQ)     LENGTH OF OBJECT                             
         BAS   RE,SETINFO                                                       
         USING NPDAD,R3                                                         
         MVC   NPDALEN,=Y(NPDALENQ)     LENGTH OF OBJECT                        
         MVC   NPDARECN,ABSRECN+2                                               
         B     XIT                                                              
         DROP  R3,R6                                                            
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE SETS AN EPKG IN TEMPSTR                                          
***********************************************************************         
SETEPK   NTR1                                                                   
         LA    RE,MYOBJECT         BUILD END-OF-PACKAGE OBJECT HERE             
         USING NEOPDAT,RE                                                       
         XC    NEOPDAT(NEOPLENQ),NEOPDAT                                        
         MVC   NEOPTYPE,=C'EPKG'                                                
         DROP  RE                                                               
         LH    R0,=Y(NEOPLENQ)     SEND END-OF-PACK OBJECT TO TEMPSTR           
         GOTO1 PUTTMP,DMCB,MYOBJECT,(R0)                                        
         BNE   EXIT                                                             
         MVI   PUTINFO,C'Y'                                                     
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* AT END OF PROGRAM - PUT OBJECT TO TEMPSTR                                     
***********************************************************************         
SETEPG   NTR1                                                                   
         LA    RE,MYOBJECT         BUILD END-OF-PROGRAM OBJECT HERE             
         AH    RE,MYOBJLEN         BUMP PAST PREVIOUS OBJECTS                   
         USING NEPGD,RE                                                         
         XC    NEPGODAT(NEPGLENQ),NEPGODAT                                      
         MVC   NEPGLEN,=Y(NEPGLENQ) L'RECORD                                    
         LH    RF,MYOBJLEN         OBJECT LENGTH SO FAR                         
         AH    RF,=Y(NEPGLENQ)     PLUS L'EBY                                   
         LA    RF,2(RF)                                                         
         STH   RF,MYOBJLEN         TOTAL LENGTH OF OBJECT                       
*                                                                               
         MVC   NEPGTYPE,=C'EPGM'                                                
         DROP  RE                                                               
*                                                                               
         LH    R0,MYOBJLEN         SEND OBJECT TO TEMPSTR                       
         CH    R0,=Y(MYOBJLNQ)                                                  
         BNH   *+6                                                              
         DC    H'0'                NEED MORE SPACE TO BUILD OBJECT              
         GOTO1 PUTTMP,DMCB,MYOBJECT,(R0)                                        
         BNE   EXIT                                                             
         MVI   PUTINFO,C'Y'                                                     
*                                                                               
EPGX     B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE SETS AN EOF IN TEMPSTR                                           
***********************************************************************         
SETEOF   NTR1                                                                   
         LA    RE,MYOBJEOF         BUILD END-OF-FILE OBJECT HERE                
         USING NEOFDAT,RE                                                       
         XC    NEOFDAT(NEOFLENQ),NEOFDAT                                        
         MVC   NEOFTYPE,=C'EOF*'                                                
         DROP  RE                                                               
         LH    R0,=Y(NEOFLENQ)     SEND END-OF-FILE OBJECT TO TEMPSTR           
         GOTO1 PUTTMP,DMCB,MYOBJEOF,(R0)                                        
         BNE   EXIT                                                             
         MVI   PUTINFO,C'Y'                                                     
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE VALIDATES THE UNIT OBJECT                                        
***********************************************************************         
VALUNT   NTR1                                                                   
         MVI   SVACT,C' '                                                       
         NI    MYSTAT,X'FF'-MYSDEL                                              
         L     R6,AIO              UNIT OBJECT                                  
         USING UUNTD,R6                                                         
         MVC   UUNTTYPE,=C'UNIT'                                                
         XC    SUBNUM,SUBNUM       RESET SUB-RECORD NUMBER                      
         XC    MYOBJLEN,MYOBJLEN   NOTHING IN OBJECT YET                        
*                                                                               
         LA    R2,MYOBJECT         BUILD UNIT OBJECT HERE                       
         USING NUNTD,R2                                                         
         XC    NUNTODAT(NUNTLENQ),NUNTODAT                                      
         MVC   NUNTLEN,=Y(NUNTLENQ) L'RECORD                                    
         SR    R1,R1                                                            
         ICM   R1,3,UNTNUM                                                      
         LA    R1,1(R1)                                                         
         STCM  R1,3,UNTNUM                                                      
         MVC   NUNTRNUM,UNTNUM                                                  
         MVC   NUNTSTRT(NUNTRLNQ),UUNTD  OBJECT DATA                            
         L     R1,AGYSEQ                                                        
         STCM  R1,15,NUNTASEQ      AGY SEQUENCE NUMBER                          
         L     R1,ABSRECN                                                       
         STCM  R1,15,NUNTAREC                                                   
*                                                                               
         MVI   ENDBLOCK,C'N'       NOT END OF BLOCK                             
*                                                                               
         ZIC   R1,SEQNUM           INCREMENT SEQUENCE NUMBER                    
         LA    R1,1(R1)                                                         
         STC   R1,SEQNUM                                                        
*                                                                               
         MVI   SVACT,C'A'          SET DEFAULT ACTION TO ADD                    
****     MVI   UPLSER,C'Y'                                                      
****     CLC   UUNTSER,SPACES      IF NOT UPLOADING W/SER NUM                   
****     BH    VU05                                                             
         MVI   UPLSER,C'N'                                                      
         B     VU20                IGNORE MINIO                                 
*                                                                               
VU05     XC    MINEKEY,MINEKEY     IS THIS UNIT ON THE FILE?                    
         MVI   MINEKEY,X'90'                                                    
         MVC   MINEKEY+1(12),UUNTSER                                            
         MVC   SVSER,UUNTSER                                                    
         MVI   MINFILTL,13                                                      
         BAS   RE,HIGHMIN                                                       
         CLI   MINERR,MINEEOF                                                   
         BE    VU10                NO -- ADD THIS UNIT                          
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   SVACT,C'C'          YES IT'S A CHANGE                            
         LA    R3,ELEMENT                                                       
         USING NEUPMEL,R3                                                       
         OI    NEUPUSRC,X'40'      KEEP THIS UNIT                               
         BAS   RE,WRTMIN                                                        
         MVI   MINCHG,C'Y'         MINIO RECORD CHANGED                         
         MVC   SVLINE,NEUPSLN1     NETPAK LINE NUMBER                           
         MVC   SVLINE2,NEUPSLN2    NETPAK LINE NUMBER                           
         MVC   NUNTLINE,SVLINE     NETPAK LINE NUMBER (FOR CHANGE)              
         MVC   NUNTLIN2,SVLINE2    NETPAK LINE NUMBER 2                         
         MVC   NUNTPROG,NEUPPROG   PROGRAM CODE                                 
         B     VU20                                                             
         DROP  R3                                                               
*                                                                               
VU10     MVC   NUNTSEQ,SEQNUM      SEQUENCE # (FOR ADD)                         
         BAS   RE,SETUPEL          ADD NEUPEL TO MINIO                          
*                                                                               
VU20     MVC   NUNTACTN,SVACT      ACTION                                       
         LH    R1,NUNTLEN          RECORD LENGTH                                
         LA    R1,2(R1)                                                         
         STH   R1,MYOBJLEN         L'OBJECT FOR TEMPSTR SO FAR                  
*                                                                               
         B     XIT                                                              
         DROP  R2,R6                                                            
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE SETS UP THE UNIT ELEMENT FOR MINIO & ADDS IT                     
***********************************************************************         
SETUPEL  NTR1                                                                   
         XC    ELEMENT,ELEMENT                                                  
         LA    R3,ELEMENT                                                       
         USING NEUPMEL,R3                                                       
         MVI   NEUPEL90,X'90'      ELEMENT CODE                                 
         MVI   NEUPEL9L,NEUPEL9Q   ELEMENT LENGTH                               
         MVC   NEUPSERN,SVSER      SERIAL NUMBER                                
         MVC   NEUPCLT,SVBCLT      CLIENT                                       
         MVC   NEUPNET,SVNET       NETWORK                                      
         MVC   NEUPDATE,SVADATE    SIR DATE                                     
         GOTO1 DATCON,DMCB,(5,0),(2,NEUPDATE)   UPLOAD DATE                     
         MVC   NEUPEST,SVEST       ESTIMATE                                     
         MVC   NEUPDPT,SVDPT       DAYPART                                      
         MVC   NEUPSQN,SEQNUM      SEQUENCE NUMBER (AVOID DUPS)                 
         GOTO1 DATCON,DMCB,(5,0),(0,NEUPUDAT)   UPLOAD DATE                     
         OI    NEUPUSRC,X'41'      FLAGS: CAME FROM UPLOAD / KEEP IT            
*                                                                               
         BAS   RE,ADDMIN                                                        
         MVI   MINCHG,C'Y'         MINIO RECORD CHANGED                         
*                                                                               
SUPX     B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE SETS A UNIT DEMO EST OBJECT IN MYOBJECT                          
***********************************************************************         
SETUDE   NTR1                                                                   
         L     R6,AIO                                                           
         USING UUDED,R6                                                         
         MVC   UUDETYPE,=C'UDRE'                                                
         LH    R1,=Y(NUDELENQ)     LENGTH OF OBJECT                             
         BAS   RE,SETINFO                                                       
         USING NUDED,R3                                                         
         MVC   NUDELEN,=Y(NUDELENQ)     LENGTH OF OBJECT                        
         B     XIT                                                              
         DROP  R3,R6                                                            
         SPACE 3                                                                
***********************************************************************         
* THIS ROUTINE SETS A UNIT DEMO ACT OBJECT IN MYOBJECT                          
***********************************************************************         
SETUDA   NTR1                                                                   
         L     R6,AIO                                                           
         USING UUDAD,R6                                                         
         MVC   UUDATYPE,=C'UDRA'                                                
         LH    R1,=Y(NUDALENQ)     LENGTH OF OBJECT                             
         BAS   RE,SETINFO                                                       
         USING NUDAD,R3                                                         
         MVC   NUDALEN,=Y(NUDALENQ)     LENGTH OF OBJECT                        
         B     XIT                                                              
         DROP  R3,R6                                                            
         SPACE 3                                                                
***********************************************************************         
* THIS ROUTINE SETS A SPECIAL CHARGES OBJECT IN MYOBJECT                        
***********************************************************************         
SETUSP   NTR1                                                                   
         L     R6,AIO                                                           
         USING USPCD,R6                                                         
         MVC   USPCTYPE,=C'UPSC'                                                
         LH    R1,=Y(NSPCLENQ)     LENGTH OF OBJECT                             
         BAS   RE,SETINFO                                                       
         USING NSPCD,R3                                                         
         MVC   NSPCLEN,=Y(NSPCLENQ)     LENGTH OF OBJECT                        
         B     XIT                                                              
         DROP  R3,R6                                                            
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE SETS A PRODUCT OBJECT IN MYOBJECT                                
***********************************************************************         
SETUPR   NTR1                                                                   
         L     R6,AIO                                                           
         USING UPRDD,R6                                                         
         MVC   UPRDTYPE,=C'PROD'                                                
         LH    R1,=Y(NPRDLENQ)     LENGTH OF OBJECT                             
         BAS   RE,SETINFO                                                       
         USING NPRDD,R3                                                         
         MVC   NPRDLEN,=Y(NPRDLENQ)     LENGTH OF OBJECT                        
         B     XIT                                                              
         DROP  R3,R6                                                            
         SPACE 3                                                                
***********************************************************************         
* THIS ROUTINE SETS A COMMENT OBJECT IN MYOBJECT                                
***********************************************************************         
SETUCO   NTR1                                                                   
         L     R6,AIO                                                           
         USING UCOMD,R6                                                         
         MVC   UCOMTYPE,=C'CMMT'                                                
         LH    R1,=Y(NCOMLENQ)     LENGTH OF OBJECT                             
         BAS   RE,SETINFO                                                       
         USING NCOMD,R3                                                         
         MVC   NCOMLEN,=Y(NCOMLENQ)     LENGTH OF OBJECT                        
         B     XIT                                                              
         DROP  R3,R6                                                            
         SPACE 3                                                                
***********************************************************************         
* THIS ROUTINE SETS A COMMERCIAL OBJECT IN MYOBJECT                             
***********************************************************************         
SETUCM   NTR1                                                                   
         L     R6,AIO                                                           
         USING UCMLD,R6                                                         
         MVC   UCMLTYPE,=C'COMM'                                                
         LH    R1,=Y(NCMLLENQ)     LENGTH OF OBJECT                             
         BAS   RE,SETINFO                                                       
         USING NCMLD,R3                                                         
         MVC   NCMLLEN,=Y(NCMLLENQ)     LENGTH OF OBJECT                        
         B     XIT                                                              
         DROP  R3,R6                                                            
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE MOVES THE INFO TO THE OBJECT                                     
* R1 - L'OBJECT (W/O LENGTH)                                                    
* R6 - OBJECT                                                                   
* RETURN - R3 CURRENT OBJECT                                                    
***********************************************************************         
SETINFO  NTR1                                                                   
*                                                                               
         LA    R3,MYOBJECT         BUILD ADDITIONAL DATA OBJECT HERE            
         AH    R3,MYOBJLEN         BUMP PAST PREVIOUS OBJECTS                   
*                                                                               
         LH    RF,MYOBJLEN         OBJECT LENGTH SO FAR                         
         AR    RF,R1               PLUS L'THIS OBJECT                           
         LA    RF,2(RF)                                                         
         STH   RF,MYOBJLEN         TOTAL LENGTH OF OBJECT SO FAR                
*                                                                               
         BCTR  R1,0                                                             
         EX    R1,*+8              CLEAR ENOUGH AT END OF OBJECT                
         BE    *+10                                                             
         XC    0(0,R3),0(R3)                                                    
*                                                                               
         LH    RF,SUBNUM           INCREMENT SUB-RECORD NUMBER                  
         LA    RF,1(RF)                                                         
         STH   RF,SUBNUM                                                        
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  5(2,R3),DUB+4(4)                                                 
*                                                                               
         L     RF,DATALEN                                                       
         AH    RF,=H'4'            4 MORE FOR OBJECT IDENTIFIER                 
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   12(0,R3),0(R6)                                                   
         XIT1  REGS=(R3)                                                        
         EJECT                                                                  
***********************************************************************         
* AT END OF UNIT - PUT OBJECT TO TEMPSTR                                        
***********************************************************************         
SETEUN   NTR1                                                                   
         TM    MYSTAT,MYSDEL       IF THIS WAS A DELETE - ALREADY PUT           
         BO    EUNX                DEL OBJECT                                   
         LA    RE,MYOBJECT         BUILD END-OF-UNIT OBJECT HERE                
         AH    RE,MYOBJLEN         BUMP PAST PREVIOUS OBJECTS                   
         USING NEUND,RE                                                         
         XC    NEUNODAT(NEUNLENQ),NEUNODAT                                      
         MVC   NEUNLEN,=Y(NEUNLENQ) L'RECORD                                    
         MVC   NEUNSTRT(NEUNRLNQ),=C'EUNT'                                      
         LH    RF,MYOBJLEN         OBJECT LENGTH SO FAR                         
         AH    RF,=Y(NEUNLENQ)     PLUS L'EBY                                   
         LA    RF,2(RF)                                                         
         STH   RF,MYOBJLEN         TOTAL LENGTH OF OBJECT                       
         DROP  RE                                                               
*                                                                               
         LH    R0,MYOBJLEN         SEND OBJECT TO TEMPSTR                       
         CH    R0,=Y(MYOBJLNQ)                                                  
         BNH   *+6                                                              
         DC    H'0'                NEED MORE SPACE TO BUILD OBJECT              
         GOTO1 PUTTMP,DMCB,MYOBJECT,(R0)                                        
         BNE   EXIT                                                             
         MVI   PUTINFO,C'Y'                                                     
*                                                                               
EUNX     B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* AT END OF DEAL - CLEAN UP MINIO RECORD                                        
***********************************************************************         
SETEOD   NTR1                                                                   
         L     R5,AMINBLK                                                       
         USING MINBLKD,R5                                                       
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,X'90'       LOOK FOR FIRST UNIT                          
         MVI   MINFILTL,1                                                       
         BAS   RE,HIGHMIN                                                       
         CLI   MINERR,MINEEOF      ANY HISTORY                                  
         BE    SE30                NO                                           
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
SE10     LA    R3,ELEMENT                                                       
         USING NEUPMEL,R3                                                       
         CLI   UPLOC,C'Y'          IF UPDATING BY LOCATION                      
         BNE   SE15                                                             
         CLC   SVLOCID,NEUPSLOC    & THIS UNIT IS NOT BY SAME LOCATION          
         BNE   SE25                IGNORE IT                                    
*                                                                               
SE15     TM    NEUPUSRC,X'40'      IS THIS MARKED FOR KEEPING?                  
         BZ    SE16                                                             
         NI    NEUPUSRC,X'FF'-X'40' TURN IT OFF (IT'S A WORK FLAG)              
         B     SE20                                                             
*                                                                               
SE16     TM    NEUPUSRC,X'80'      IS THIS MARKED FOR DELETION                  
         BO    SE17                                                             
         CLI   SVUPDFLG,C'Y'       UPDATES ONLY?                                
         BE    SE20                                                             
         OI    NEUPUSRC,X'80'      NO -- MARK IT FOR DELETION                   
*                                                                               
SE17     BAS   RE,PROCDEL          PUT DELETE OBJECTS TO TEMPSTR                
*                                                                               
SE20     MVI   WORK,X'90'          SAVE ELEMENT KEY                             
         MVC   WORK+1(12),NEUPSERN                                              
         BAS   RE,WRTMIN                                                        
         MVI   MINCHG,C'Y'         MINIO RECORD CHANGED                         
*                                                                               
         MVC   MINEKEY(13),WORK     RESTORE ELEMENT KEY                         
         BAS   RE,READMIN                                                       
         BE    *+6                                                              
         DC    H'0'                BUT WE JUST READ IT!                         
*                                                                               
SE25     BAS   RE,SEQMIN                                                        
         CLI   MINERR,MINEEOF      ANY MORE UNITS?                              
         BE    SE30                NO                                           
         CLI   MINERR,0                                                         
         BE    SE10                                                             
         DC    H'0'                                                             
         DROP  R3                                                               
*                                                                               
SE30     CLI   MINCHG,C'Y'         ANY CHANGE TO THIS MINIO RECORD?             
         BNE   SE40                                                             
         BAS   RE,CLSMIN                                                        
*                                                                               
SE40     LA    RE,MYOBJECT         BUILD END-OF-DEAL OBJECT HERE                
         USING NEODDAT,RE                                                       
         XC    NEODDAT(NEODLENQ),NEODDAT                                        
         MVC   NEODTYPE,=C'EOD*'                                                
         DROP  RE                                                               
         LH    R0,=Y(NEODLENQ)     SEND END-OF-DEAL OBJECT TO TEMPSTR           
         GOTO1 PUTTMP,DMCB,MYOBJECT,(R0)                                        
         BNE   EXIT                                                             
         MVI   PUTINFO,C'Y'                                                     
*                                                                               
SEX      B     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* PUT DELETE OBJECTS TO TEMPSTR FOR THE NET NBUY PROGRAM                        
***********************************************************************         
PROCDEL  NTR1                                                                   
*                                                                               
         LA    R3,ELEMENT          MINIO ELEMENT BEING DELETED                  
         USING NEUPMEL,R3                                                       
         LA    R2,MYOBJECT         BUILD DELETE OBJECT HERE                     
         USING NDELODAT,R2                                                      
         XC    NDELODAT(NDELLENQ),NDELODAT                                      
         MVC   NDELTYPE,=C'DEL*'                                                
*                                                                               
         MVC   NDELCLT,NEUPCLT     CLT                                          
         MVC   NDELNET,NEUPNET     NETWORK                                      
         MVC   NDELPROG,NEUPPROG   PROGRAM CODE                                 
         MVC   NDELDATE,NEUPDATE   DATE                                         
         MVC   NDELEST,NEUPEST     EST                                          
         MVC   NDELDPT,NEUPDPT     DAYPART                                      
         MVC   NDELSLN1,NEUPSLN1   NETPAK LINE NUMBER                           
         MVC   NDELSLN2,NEUPSLN2   NETPAK LINE NUMBER                           
         MVC   NDELSERN,NEUPSERN   SERIAL NUMBER                                
         LH    R0,=Y(NDELLENQ)     SEND DELETE OBJECT TO TEMPSTR                
         GOTO1 PUTTMP,DMCB,MYOBJECT,(R0)                                        
         BNE   EXIT                                                             
         MVI   PUTINFO,C'Y'                                                     
*                                                                               
         B     XIT                                                              
         DROP  R3,R2                                                            
         EJECT                                                                  
***********************************************************************         
* SET SAVED STORAGE FIELDS                                                      
***********************************************************************         
SETSAVE  NTR1                                                                   
         L     R2,ADATA            OBJECT AS GIVEN TO ME                        
         USING UHDRD,R2                                                         
         MVC   SVSYS,UHDRSYS       SYSTEM                                       
         MVC   SVTYPE,UHDRUTYP     UPLOAD TYPE                                  
         MVC   SVAGY,UHDRAGID      BUYING AGENCY ID                             
         MVC   SVLOCID,UHDRUID2    LOCATION ID - 1ST 2 CHARS-UNIQUE ID          
*                                                                               
         MVI   SVWRTFLG,C'Y'       DEFAULT TO WRITE=YES                         
         CLI   UHDRTSTR,C'Y'       WRITE INHIBIT FLAG GIVEN?                    
         BNE   *+8                                                              
         MVI   SVWRTFLG,C'N'       CORRECT -- WRITE=NO                          
         L     R1,AMINBLK                                                       
         MVC   MINWRITE-MINBLKD(,R1),SVWRTFLG 'N' = DON'T WRITE TO FILE         
*                                                                               
         MVI   SVUPDFLG,C'N'       DEFAULT TO NOT UPDATES ONLY                  
         CLI   UHDRUPDO,C'Y'       UPDATES ONLY?                                
         BNE   *+8                                                              
         MVI   SVUPDFLG,C'Y'       YES                                          
         B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE THE HEADER RECORD FIELDS, SO WE DON'T UPLOAD A SHITLOAD OF           
* BOGUS UNITS FOR NOTHING.                                                      
***********************************************************************         
VALHEADR NTR1                                                                   
*                                                                               
         MVI   ERFLDNUM,NHDRSYSQ   SYSTEM                                       
         CLI   SVSYS,C'N'                                                       
         BNE   VHINV                                                            
         MVI   ERFLDNUM,NHDRUTYQ   UPLOAD TYPE                                  
         CLC   SVTYPE,=C'NBU'                                                   
         BE    VH10                                                             
***  MVU  ***                                                                   
         CLC   SVTYPE,=C'MVU'      NEW FORMAT TYPE NAME                         
         BNE   VHINV                                                            
         OI    MISCFLG1,MF1MVU      - IT'S MVU!                                 
***  MVU  ***                                                                   
VH10     MVI   ERFLDNUM,NHDRAGIQ   AGENCY                                       
         CLC   SVAGY,SPACES                                                     
         BNH   VHMISS                                                           
         MVI   ERFLDNUM,0                                                       
*                                                                               
VHX      B     XIT                 HEADER RECORD FIELDS ARE VALID               
*                                                                               
VHMISS   MVC   ERRNUM,=H'1'        MISSING FIELD                                
         B     VHX                                                              
*                                                                               
VHINV    MVC   ERRNUM,=H'2'        INVALID INPUT FIELD                          
         B     VHX                                                              
         EJECT                                                                  
***********************************************************************         
* STOP PROCESSING IF HEADER IN ERROR                                            
***********************************************************************         
HDRERR   NTR1                                                                   
         LA    RF,WORK                                                          
         USING DSTATUSD,RF                                                      
         MVC   DSTATYPE,=C'01'     ERROR STATUS                                 
         MVC   DSTAUNTN,=C'00000'  RECORD ZERO = HEADER                         
         MVC   DSTASUBR,=C'00'     NO SUB-RECORD                                
         ZIC   R0,ERFLDNUM         FIELD NUMBER                                 
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  DSTAFLDN,DUB+4(4)                                                
         SR    R0,R0                                                            
         ICM   R0,3,ERRNUM         ERROR NUMBER                                 
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  DSTAMSGN,DUB+4(4)                                                
*                                                                               
         L     R0,RECNUM           ABSOLUTE RECORD NUMBER                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  DSTAFREC,DUB+4(4)                                                
         L     R3,=A(ITNBYSTA)     STATUS OBJECT                                
         LH    R0,=Y(DSTAERLQ)     L'OBJECT                                     
*                                                                               
         DROP  RF                                                               
         GOTO1 PUTITEM,DMCB,(R3),(R0),WORK                                      
         BNE   EXIT                                                             
*                                                                               
         GOTO1 PUTITEM,DMCB,ITEOB,0                                             
         BNE   EXIT                                                             
         GOTO1 PUTITEM,DMCB,ITEOD,0                                             
         BNE   EXIT                                                             
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE CALLS THE BUY PROGRAM WITH THE TRANSFER CONTROL ELEMENT          
* VIA GLOBBER.                                                                  
***********************************************************************         
CALLBUY  NTR1                                                                   
*                                                                               
         XC    BLOCK(24),BLOCK                                                  
         LA    R1,BLOCK                                                         
         USING GLVXFRSY,R1                                                      
         MVC   GLVXFRSY,=C'CON'    FROM THE CONTROL SYSTEM                      
         MVC   GLVXFRPR,=C'MAD'    MAD PROGRAM                                  
         MVC   GLVXTOSY,=C'NET'    TO THE NET SYSTEM                            
         MVC   GLVXTOPR,=C'NBU'    BUY PROGRAM                                  
         DROP  R1                                                               
*                                  SET UP THE TRANSFER CONTROL BLOCK            
         GOTO1 GLOBBER,DMCB,=C'PUTD',BLOCK,24,GLVXCTL                           
         CLI   DMCB+8,0                                                         
         BE    XIT                                                              
         MVC   MDACTION,=Y(ER10XCTL) GLOBBER COULDN'T PUT OUT XCTL ELEM         
         B     EXIT                EXIT $MAD WITH AN ERROR CODE                 
*                                                                               
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE RETURNS THE STATUS OF THE UPLOADED UNITS TO THE PC.              
***********************************************************************         
CONFIRM  NTR1                                                                   
*                                                                               
         L     R5,AMINBLK                                                       
         USING MINBLKD,R5                                                       
*                                                                               
         CLI   CONFFLAG,X'FF'      IF CONFIRM WAS USED BEFORE                   
         BE    CNF10               THEN PUT OUT CONFIRMATION OBJECT             
*                                                                               
         GOTO1 TMPOPEN,DMCB,(C'S',=C'GTX'),MYOBJLNQ,(3,0)                       
         BNE   EXIT                                                             
*                                                                               
CNFLOOP  GOTO1 GETTMP,DMCB,MYOBJGET GET BUY DATA FROM TEMPSTR                   
         BNE   EXIT                                                             
*                                                                               
         MVC   DATALEN,DMCB+4      COPY LENGTH OF DATA                          
*                                                                               
         CLI   EOTFLAG,C'Y'        DONE IF NO MORE                              
         BNE   CNF10                                                            
*                                                                               
         CLI   ENDOBJCT,C'Y'       END-OF-DATA OBJECT?                          
         BE    CNF05                                                            
*                                  PUT END-OF-BLOCK OBJECT OUT                  
         GOTO1 PUTITEM,DMCB,ITEOB,0                                             
         BNE   EXIT                                                             
*                                                                               
CNF05    MVI   ENDBLOCK,C'N'                                                    
         MVI   CONFFLAG,0                                                       
         GOTO1 TMPCLOSE            DONE WITH READING TEMPFILE                   
         MVI   TMPOPEND,C'N'       HAVE NOT YET OPENED TEMPSTR                  
         B     CNFX                EXIT ROUTINE                                 
*                                                                               
CNF10    LA    R4,MYOBJGET         RETURNED OBJECT                              
         CLC   =C'HDR*',10(R4)     HEADER OBJECT?                               
         BNE   CNF20                                                            
         BAS   RE,CNFHDR                                                        
         B     CNFCHK                                                           
*                                                                               
CNF20    TM    MYSTAT,DEALERR      IF THERE WAS A DEAL ERROR                    
         BNO   CNF25                                                            
         CLC   =C'DEAL',10(R4)                                                  
         BNE   CNFLOOP             IGNORE REST UNTIL NEXT DEAL                  
         NI    MYSTAT,X'FF'-(DEALERR+PKGERR+PGMERR+CHUERR)                      
*                                                                               
CNF25    CLC   =C'DEAL',10(R4)     DEAL OBJECT?                                 
         BNE   CNF30                                                            
         BAS   RE,CNFITEM                                                       
         OC    0(2,R4),0(R4)       AN ERROR IN DEAL                             
         BZ    CNFCHK              NO                                           
         OI    MYSTAT,DEALERR                                                   
         B     CNFCHK                                                           
*                                                                               
CNF30    TM    MYSTAT,PKGERR       IF THERE WAS A PKG ERROR                     
         BNO   CNF35                                                            
         CLC   =C'PKG*',10(R4)                                                  
         BNE   CNFLOOP             IGNORE REST UNTIL NEXT PACKAGE               
         NI    MYSTAT,X'FF'-(PKGERR+PGMERR+CHUERR)                              
*                                                                               
CNF35    CLC   =C'PKG*',10(R4)     PACKAGE OBJECT?                              
         BNE   CNF40                                                            
         BAS   RE,CNFITEM                                                       
         OC    0(2,R4),0(R4)       AN ERROR IN PACKAGE                          
         BZ    CNFCHK              NO                                           
         OI    MYSTAT,PKGERR                                                    
         B     CNFCHK                                                           
*                                                                               
CNF40    TM    MYSTAT,PGMERR       IF THERE WAS A PGM ERROR                     
         BNO   CNF45                                                            
         CLC   =C'PROG',12(R4)                                                  
         BNE   CNFLOOP             IGNORE REST UNTIL NEXT PROGRAM               
         NI    MYSTAT,X'FF'-PGMERR                                              
*                                                                               
CNF45    CLC   =C'PROG',12(R4)     PROGRAM OBJECT?                              
         BNE   CNF46                                                            
         BAS   RE,CNFPROG                                                       
         B     CNFCHK                                                           
*                                                                               
CNF46    CLC   =C'PRDE',10(R4)     PROGRAM DEMO EST OBJECT?                     
         BNE   CNF47                                                            
         BAS   RE,CNFITEM                                                       
         OC    0(2,R4),0(R4)       AN ERROR IN PROGRAM                          
         BZ    CNFCHK              NO                                           
         OI    MYSTAT,PGMERR                                                    
         B     CNFCHK                                                           
*                                                                               
CNF47    CLC   =C'PRDA',10(R4)     PROGRAM DEMO ACT OBJECT?                     
         BNE   CNF50                                                            
         BAS   RE,CNFITEM                                                       
         OC    0(2,R4),0(R4)       AN ERROR IN PROGRAM                          
         BZ    CNFCHK              NO                                           
         OI    MYSTAT,PGMERR                                                    
         B     CNFCHK                                                           
*                                                                               
***  MVU  ***                                                                   
CNF50    TM    MYSTAT,CHUERR       IF THERE WAS A PGM ERROR                     
         BNO   CNF55                                                            
         CLC   =C'UNUP',12(R4)                                                  
         BNE   CNFLOOP             IGNORE REST UNTIL NEXT PROGRAM               
         NI    MYSTAT,X'FF'-CHUERR                                              
*                                                                               
CNF55    CLC   =C'UNUP',12(R4)     UNIT UPDATE OBJECT?                          
         BNE   CNF56                                                            
         BAS   RE,CNFUNUP                                                       
         B     CNFCHK                                                           
*                                                                               
CNF56    CLC   =C'PROD',12(R4)     UNIT PRODUCT OBJECT?                         
         BNE   CNF60                                                            
         BAS   RE,CNFITEM                                                       
         OC    0(2,R4),0(R4)       AN ERROR IN UNIT UPDATE                      
         BZ    CNFCHK              NO                                           
         OI    MYSTAT,CHUERR                                                    
         B     CNFCHK                                                           
***  MVU  ***                                                                   
*                                                                               
CNF60    CLC   =C'UNIT',12(R4)     UNIT OBJECT?                                 
         BNE   CNF70                                                            
         BAS   RE,CNFUNIT                                                       
         B     CNFCHK                                                           
*                                                                               
CNF70    CLC   =C'DEL*',10(R4)     DELETE OBJECT?                               
         BNE   CNF80                                                            
         BAS   RE,CNFDEL                                                        
         B     CNFCHK                                                           
*                                                                               
CNF80    CLC   =C'EPKG',10(R4)     END OF PACKAGE?                              
         BE    CNF90                                                            
         CLC   =C'EPGM',10(R4)     END OF PROGRAM?                              
         BE    CNF90                                                            
         CLC   =C'EUN*',10(R4)     END-OF-UNIT OBJECT?                          
         BE    CNF90                                                            
         CLC   =C'EOD*',10(R4)     END-OF-DATA OBJECT?                          
         BE    CNF90                                                            
         CLC   =C'EOF*',10(R4)     END-OF-FILE OBJECT?                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
CNF90    CLI   MINCHG,C'Y'         ANY CHANGE TO THIS ESTIMATE                  
         BNE   CNFCHK                                                           
         BAS   RE,CLSMIN                                                        
         B     CNFCHK                                                           
*                                                                               
CNFCHK   CLI   EOFFLAG,C'Y'        IF END OF FRAME FLAG NOT SET,                
         BNE   CNFLOOP             LOOP BACK UNTIL NO MORE OBJECTS              
         MVI   CONFFLAG,X'FF'      WE HAVE SOMETHING IN OUR BUFFER              
*                                                                               
CNFX     B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE RETURNS THE STATUS OF ANY OBJECT                                 
***********************************************************************         
CNFITEM  NTR1                                                                   
         OC    0(2,R4),0(R4)       ANY ERROR IN OBJECT?                         
         BZ    CIX                 NO                                           
         XC    UNTNUM,UNTNUM                                                    
         XC    SUBNUM,SUBNUM                                                    
         MVC   ERFLDNUM,2(R4)      FIELD NUMBER                                 
         MVC   ERRNUM,0(R4)        ERROR NUMBER                                 
         SR    R1,R1                                                            
         ICM   R1,3,3(R4)                                                       
         ST    R1,RECNUM                                                        
         BAS   RE,SETERR           SET ERROR                                    
*                                                                               
CIX      B     XIT                 RETURN TO CALLER                             
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE RETURNS THE STATUS OF A PROG OBJECT                              
***********************************************************************         
         USING NPGMD,R4                                                         
CNFPROG  NTR1                                                                   
         OC    2(2,R4),2(R4)       ANY ERROR IN OBJECT?                         
         BZ    CPX                 NO                                           
         XC    UNTNUM,UNTNUM                                                    
         XC    SUBNUM,SUBNUM                                                    
         SR    R1,R1                                                            
         ICM   R1,3,5(R4)                                                       
         ST    R1,RECNUM                                                        
         MVC   ERFLDNUM,4(R4)      FIELD NUMBER                                 
         MVC   ERRNUM,2(R4)        ERROR NUMBER                                 
         BAS   RE,SETERR           SET ERROR                                    
         OI    MYSTAT,PGMERR                                                    
         B     CPX                                                              
*                                                                               
CPX      B     XIT                 RETURN TO CALLER                             
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE RETURNS THE STATUS OF A PROG OBJECT                              
***********************************************************************         
         USING NCHUD,R4                                                         
CNFUNUP  NTR1                                                                   
         OC    2(2,R4),2(R4)       ANY ERROR IN OBJECT?                         
         BZ    CUNUPX              NO                                           
         XC    UNTNUM,UNTNUM                                                    
         XC    SUBNUM,SUBNUM                                                    
         SR    R1,R1                                                            
         ICM   R1,3,5(R4)                                                       
         ST    R1,RECNUM                                                        
         MVC   ERFLDNUM,4(R4)      FIELD NUMBER                                 
         MVC   ERRNUM,2(R4)        ERROR NUMBER                                 
         BAS   RE,SETERR           SET ERROR                                    
         OI    MYSTAT,CHUERR                                                    
         B     CUNUPX                                                           
*                                                                               
CUNUPX   B     XIT                 RETURN TO CALLER                             
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE RETURNS THE STATUS OF THE UNIT OBJECT                            
***********************************************************************         
         USING NUNTD,R4                                                         
CNFUNIT  NTR1                                                                   
         XC    SUBNUM,SUBNUM       SUB RECORD NUMBER                            
         MVC   SVLINE,NUNTLINE     SAVE LINE NUMBER FROM NETPAK                 
         MVC   SVLINE2,NUNTLIN2                                                 
         MVC   SEQNUM,NUNTSEQ      SEQUENCE NUMBER (FROM $MAD)                  
         MVC   SVACT,NUNTACTN      ACTION                                       
         MVC   SVSERN,NUNTSERN     USER-SUPPLIED SERIAL NUMBER                  
         MVC   SVPROG,NUNTPROG     PROGRAM CODE                                 
         MVC   SVADATE,NUNTADAT    AIR DATE                                     
         MVC   UNTNUM,NUNTRNUM     UNIT NUMBER                                  
         ICM   R1,15,NUNTASEQ                                                   
         ST    R1,AGYSEQ           AGENCY SEQUENCE NUMBER                       
*                                                                               
         BAS   RE,SMINMKEY         SET MINIO MASTER KEY                         
*                                                                               
         CLI   UPLSER,C'N'         ARE WE ADDING BY SERIAL NUMBER               
         BE    CUNIT10             YES - IGNORE MINIO                           
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,X'90'                                                    
         MVC   MINEKEY+1(12),SVSERN    SERIAL NUMBER                            
         MVI   MINFILTL,13             L'COMPARE OF ELEMENT KEY (CODE)          
*                                                                               
         BAS   RE,READMIN                                                       
         BE    *+6                                                              
         DC    H'0'                DIDN'T WE ADD/SEE THIS EARLIER?              
*                                                                               
CUNIT10  ICM   R1,15,NUNTAREC                                                   
         ST    R1,RECNUM                                                        
         OC    NUNTERNO,NUNTERNO   ANY ERROR IN UNIT RECORD?                    
         BZ    CUNIT30                                                          
*                                                                               
CUNITERR MVC   ERRNUM,NUNTERNO     SAVE ERROR NUMBER                            
         MVC   ERFLDNUM,NUNTERF    SAVE FIELD NUMBER IN ERROR                   
         CLI   SVACT,C'A'          DID WE ADD THIS EARLIER?                     
         BNE   CUNIT60                                                          
         CLI   UPLSER,C'N'         ARE WE ADDING BY SERIAL NUMBER               
         BE    CUNIT60             YES - IGNORE MINIO                           
         BAS   RE,DELMIN                                                        
         B     CUNIT60             WRITE ERROR OBJECT                           
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
CUNIT30  SR    R0,R0                                                            
         ICM   R0,3,0(R4)          LENGTH OF SUB-OBJECT                         
         AR    R4,R0               BUMP TO NEXT PORTION                         
         LA    R4,2(R4)            BUMP PAST LENGTH                             
*                                                                               
         CLC   =C'UDRE',12(R4)                                                  
         BE    CUNIT40                                                          
         CLC   =C'UDRA',12(R4)                                                  
         BE    CUNIT40                                                          
         CLC   =C'UPSC',12(R4)                                                  
         BE    CUNIT40                                                          
         CLC   =C'PROD',12(R4)                                                  
         BE    CUNIT40                                                          
         CLC   =C'CMMT',12(R4)                                                  
         BE    CUNIT40                                                          
         CLC   =C'COMM',12(R4)                                                  
         BE    CUNIT40                                                          
         CLC   =C'EUNT',12(R4)                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   SVACT,C'A'                                                       
         BE    CUNIT70                                                          
         BAS   RE,CNFEUNT                                                       
         L     R3,=A(ITNBYSTA)     STATUS OBJECT                                
         LH    R0,=Y(DSTAOKLQ)     L'OBJECT                                     
         B     CUNITPUT                                                         
*                                                                               
CUNIT40  OC    2(2,R4),2(R4)       ANY ERROR IN OBJECT?                         
         BZ    CUNIT30             NO                                           
         MVC   SUBNUM,5(R4)        SUBRECORD NUMBER                             
*                                                                               
CUNIT60  MVC   ERFLDNUM,4(R4)      FIELD NUMBER                                 
         MVC   ERRNUM,2(R4)        ERROR NUMBER                                 
         BAS   RE,SETERR           SET ERROR                                    
         CLI   SVACT,C'A'          DID WE DELETE THIS EARLIER?                  
         BE    CBX                                                              
         BAS   RE,DELMIN                                                        
         B     CBX                                                              
*                                                                               
CUNITPUT GOTO1 PUTITEM,DMCB,(R3),(R0),WORK                                      
         BNE   EXIT                                                             
*                                                                               
CUNIT70  CLI   SVACT,C'A'          IF THIS WAS AN ADD                           
         BNE   CBX                                                              
         BAS   RE,CNFUADD          PUT OUT CONFIRM THAT UNIT WAS ADDED          
*                                                                               
CBX      B     XIT                 RETURN TO CALLER                             
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE PUTS A STATUS OBJECT THAT IS IN ERROR                            
***********************************************************************         
SETERR   NTR1                                                                   
         LA    R2,WORK             BUILD ERROR OBJECT                           
         USING DSTATUSD,R2                                                      
         MVC   DSTATYPE,=C'01'     ERROR STATUS                                 
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,UNTNUM         UNIT NUMBER                                  
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  DSTAUNTN,DUB+4(4)                                                
*                                                                               
         LH    R0,SUBNUM                                                        
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  DSTASUBR,DUB+4(4)                                                
*                                                                               
         L     R0,RECNUM           ABSOLUTE RECORD NUMBER                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  DSTAFREC,DUB+4(4)                                                
*                                                                               
         ZIC   R0,ERFLDNUM         FIELD NUMBER                                 
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  DSTAFLDN,DUB+4(4)                                                
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,ERRNUM         ERROR NUMBER                                 
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  DSTAMSGN,DUB+4(4)                                                
*                                                                               
         L     R3,=A(ITNBYSTA)     STATUS OBJECT                                
         LH    R0,=Y(DSTAERLQ)     L'OBJECT                                     
         GOTO1 PUTITEM,DMCB,(R3),(R0),WORK                                      
         BNE   EXIT                                                             
         B     XIT                 RETURN TO CALLER                             
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE RETURNS THE STATUS OF THE DEL OBJECT                             
***********************************************************************         
         USING NDELODAT,R4                                                      
CNFDEL   NTR1                                                                   
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,X'90'                                                    
         MVC   MINEKEY+1(12),NDELSERN                                           
         BAS   RE,READMIN                                                       
         BE    *+6                                                              
         DC    H'0'                DIDN'T WE ADD THIS EARLIER?                  
*                                                                               
         OC    NDELERNO,NDELERNO   ANY ERROR ON DELETE?                         
         BZ    CD20                                                             
         LA    R3,ELEMENT                                                       
         USING NEUPMEL,R3                                                       
         NI    NEUPUSRC,X'FF'-X'80' YES -- TAKE OFF DELETE BIT                  
         OI    NEUPUSRC,X'40'      KEEP THIS UNIT                               
         BAS   RE,WRTMIN                                                        
         MVI   MINCHG,C'Y'         MINIO RECORD CHANGED                         
*                                                                               
         MVC   ERRNUM,NDELERNO     SAVE ERROR NUMBER                            
         MVC   ERFLDNUM,NDELERF    FIELD NUMBER                                 
         XC    SUBNUM,SUBNUM       SUB RECORD NUMBER                            
         BAS   RE,SETERR           SET ERROR                                    
         B     CDELX                                                            
*                                                                               
CD20     BAS   RE,DELMIN                                                        
*                                                                               
         L     R1,TOTALDEL         INCREMENT DELETE COUNTER                     
         LA    R1,1(R1)                                                         
         ST    R1,TOTALDEL                                                      
*                                                                               
CDELX    B     XIT                                                              
         DROP  R3,R4                                                            
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE RETURNS THE STATUS OF THE HDR OBJECT                             
***********************************************************************         
         USING NHDRODAT,R4                                                      
CNFHDR   NTR1                                                                   
         OC    NHDRERNO,NHDRERNO   ANY ERROR ON HEADER?                         
         BZ    CHDRX               NO                                           
*                                                                               
         LA    R2,WORK             YES -- BUILD ERROR OBJECT                    
         USING DSTATUSD,R2                                                      
         MVC   DSTATYPE,=C'01'     ERROR STATUS                                 
         MVC   DSTAUNTN,=C'00000'  RECORD ZERO = HEADER                         
         MVC   DSTASUBR,=C'00'     NO SUB-RECORD                                
         ZIC   R0,NHDRERF          FIELD NUMBER                                 
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  DSTAFLDN,DUB+4(4)                                                
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,NHDRERNO       ERROR NUMBER                                 
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  DSTAMSGN,DUB+4(4)                                                
*                                                                               
         L     R0,RECNUM           ABSOLUTE RECORD NUMBER                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  DSTAFREC,DUB+4(4)                                                
         DROP  R2                                                               
                                                                                
         L     R3,=A(ITNBYSTA)     STATUS OBJECT                                
         LH    R0,=Y(DSTAERLQ)     L'OBJECT                                     
         GOTO1 PUTITEM,DMCB,(R3),(R0),WORK                                      
         BNE   EXIT                                                             
                                                                                
         GOTO1 PUTITEM,DMCB,ITEOB,0                                             
         BNE   EXIT                                                             
         GOTO1 PUTITEM,DMCB,ITEOD,0                                             
         BNE   EXIT                                                             
                                                                                
CHDRX    B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* CONFIRM IF UNIT ADDED                                                         
***********************************************************************         
CNFUADD  NTR1                                                                   
         LA    R2,WORK             UNIT WAS OK                                  
         USING DSTATUSD,R2                                                      
         L     R0,RECNUM           ABSOLUTE RECORD NUMBER                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  DSTAFREC,DUB+4(4)                                                
*                                                                               
         CLI   SVACT,C'A'                                                       
         BNE   CADDX                                                            
*                                                                               
         MVC   DSTATYPE,=C'03'     RECORD ADDED STATUS                          
         LA    R1,ELEMENT                                                       
         USING NEUPMEL,R1                                                       
         MVC   NEUPSLN1,SVLINE     PUT NETPAK LINE NUMBER IN ELEMENT            
         MVC   NEUPSLN2,SVLINE2                                                 
         MVC   NEUPPROG,SVPROG     PROGRAM CODE                                 
         MVC   NEUPDATE,SVADATE    AIR DATE                                     
         MVI   NEUPSQN,0           REMOVE THE $MAD SEQUENCE NUMBER              
         DROP  R1                                                               
         CLI   UPLSER,C'N'         ARE WE ADDING BY SERIAL NUMBER               
         BE    CADD10              YES - IGNORE MINIO                           
         BAS   RE,WRTMIN                                                        
         MVI   MINCHG,C'Y'         MINIO RECORD CHANGED                         
*                                                                               
CADD10   SR    R0,R0                                                            
         ICM   R0,3,UNTNUM                                                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  DSTAUNTN,DUB+4(4)                                                
         L     R3,=A(ITNBYSTA)     STATUS OBJECT                                
         LH    R0,=Y(DSTAOKLQ)     L'OBJECT                                     
         GOTO1 PUTITEM,DMCB,(R3),(R0),WORK                                      
         BNE   EXIT                                                             
*                                                                               
CADDX    B     XIT                 RETURN TO THE CALLER                         
         DROP  R2                                                               
         EJECT                                                                  
*                                                                               
***********************************************************************         
* CONFIRM END OF UNIT OBJECT                                                    
***********************************************************************         
CNFEUNT  NTR1                                                                   
         LA    R2,WORK             UNIT WAS OK                                  
         USING DSTATUSD,R2                                                      
         CLI   SVACT,C'C'                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   DSTATYPE,=C'02'     RECORD MODIFIED STATUS                       
         SR    R0,R0                                                            
         ICM   R0,3,UNTNUM                                                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  DSTAUNTN,DUB+4(4)                                                
*                                                                               
CEINX    B     XIT                 RETURN TO THE CALLER                         
         DROP  R2                                                               
         EJECT                                                                  
*                                                                               
GETTITEM NTR1                                                                   
*                                                                               
         SR    R2,R2                                                            
         CLI   TSTDATAF,C'1'                                                    
         BNE   GT01                                                             
         LA    R3,TSTX1                                                         
         B     GT10                                                             
*                                                                               
GT01     CLI   TSTDATAF,C'2'                                                    
         BNE   GT02                                                             
****     LA    R3,TSTX2                                                         
         B     GT10                                                             
*                                                                               
GT02     CLI   TSTDATAF,C'3'                                                    
         BNE   *+12                                                             
         LA    R3,TSTX3                                                         
         B     GT10                                                             
         DC    H'0'                                                             
*                                                                               
GT10     CLI   0(R3),X'FF'                                                      
         BNE   *+14                                                             
         MVC   TYPENUM,=A(ITEOD)                                                
         B     XIT                                                              
*                                                                               
         CLI   0(R3),X'80'                                                      
         BNE   *+16                                                             
         ICM   R2,3,1(R3)                                                       
         LA    R3,3(R2,R3)                                                      
         B     GT10                                                             
*                                                                               
         CLC   =C'NOP*',3(R3)                                                   
         BNE   GT20                                                             
         MVI   0(R3),X'80'                                                      
         ICM   R2,3,1(R3)                                                       
         LA    R3,3(R2,R3)                                                      
         B     GT10                                                             
*                                                                               
GT20     TM    MISCFLG1,MF1MVU     IS IT NEW MVU FORMAT?                        
         BO    GT30                 - YUP IT IS!                                
****  THIS IS FOR OLDER FORMAT (NBU)  ****                                      
***  NBU  ***                                                                   
         MVC   TYPENUM,=A(ITNBYHDR)                                             
         CLC   =C'HDR*',3(R3)                                                   
         BE    GTX                                                              
         MVC   TYPENUM,=A(ITNBYDL)                                              
         CLC   =C'DEAL',3(R3)                                                   
         BE    GTX                                                              
         MVC   TYPENUM,=A(ITNBYPKG)                                             
         CLC   =C'PKG*',3(R3)                                                   
         BE    GTX                                                              
         MVC   TYPENUM,=A(ITNBYPGM)                                             
         CLC   =C'PROG',3(R3)                                                   
         BE    GTX                                                              
         MVC   TYPENUM,=A(ITNBYPDE)                                             
         CLC   =C'PRDE',3(R3)                                                   
         BE    GTX                                                              
         MVC   TYPENUM,=A(ITNBYPDA)                                             
         CLC   =C'PRDA',3(R3)                                                   
         BE    GTX                                                              
         MVC   TYPENUM,=A(ITNBYUNT)                                             
         CLC   =C'UNIT',3(R3)                                                   
         BE    GTX                                                              
         MVC   TYPENUM,=A(ITNBYUDE)                                             
         CLC   =C'URDE',3(R3)                                                   
         BE    GTX                                                              
         MVC   TYPENUM,=A(ITNBYUDA)                                             
         CLC   =C'URDA',3(R3)                                                   
         BE    GTX                                                              
         MVC   TYPENUM,=A(ITNBYUSC)                                             
         CLC   =C'SPCH',3(R3)                                                   
         BE    GTX                                                              
         MVC   TYPENUM,=A(ITNBYUPR)                                             
         CLC   =C'PROD',3(R3)                                                   
         BE    GTX                                                              
         MVC   TYPENUM,=A(ITNBYUCO)                                             
         CLC   =C'CMMT',3(R3)                                                   
         BE    GTX                                                              
         MVC   TYPENUM,=A(ITNBYUCM)                                             
         CLC   =C'COMM',3(R3)                                                   
         BE    GTX                                                              
         MVC   TYPENUM,=A(ITNBYEPK)                                             
         CLC   =C'EPKG',3(R3)                                                   
         BE    GTX                                                              
         MVC   TYPENUM,=A(ITNBYEPG)                                             
         CLC   =C'EPGM',3(R3)                                                   
         BE    GTX                                                              
         MVC   TYPENUM,=A(ITNBYEUN)                                             
         CLC   =C'EUNT',3(R3)                                                   
         BE    GTX                                                              
         MVC   TYPENUM,=A(ITNBYEOD)                                             
         CLC   =C'EOD*',3(R3)                                                   
         BE    GTX                                                              
         DC    H'0'                                                             
***  NBU  ***                                                                   
*                                                                               
****  THIS IS FOR NEWER FORMAT (MVU)  ****                                      
***  MVU  ***                                                                   
GT30     MVC   TYPENUM,=A(ITNBYHDR)                                             
         CLC   =C'HDR*',3(R3)                                                   
         BE    GTX                                                              
         MVC   TYPENUM,=A(ITNBYDL)                                              
         CLC   =C'DEAL',3(R3)                                                   
         BE    GTX                                                              
         MVC   TYPENUM,=A(ITNBYPKG)                                             
         CLC   =C'PKG*',3(R3)                                                   
         BE    GTX                                                              
         MVC   TYPENUM,=A(ITNBYCHU)                                             
         CLC   =C'UNUP',3(R3)                                                   
         BE    GTX                                                              
         MVC   TYPENUM,=A(ITNBYUPR)                                             
         CLC   =C'PROD',3(R3)                                                   
         BE    GTX                                                              
         MVC   TYPENUM,=A(ITNBYEPK)                                             
         CLC   =C'EPKG',3(R3)                                                   
         BE    GTX                                                              
         MVC   TYPENUM,=A(ITNBYEUN)                                             
         CLC   =C'EUNT',3(R3)                                                   
         BE    GTX                                                              
         MVC   TYPENUM,=A(ITNBYEOD)                                             
         CLC   =C'EOD*',3(R3)                                                   
         BE    GTX                                                              
         DC    H'0'                                                             
***  MVU  ***                                                                   
*                                                                               
GTX      SR    R0,R0                                                            
         ICM   R0,3,1(R3)                                                       
         SH    R0,=H'4'                                                         
         ST    R0,DATALEN                                                       
         MVI   0(R3),X'80'                                                      
         LA    R3,7(R3)                                                         
         ST    R3,ADATA                                                         
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* GET MEDIA BYTE FROM AGENCY RECORD                                             
***********************************************************************         
GETMED   NTR1                                                                   
         XC    KEY,KEY                                                          
         MVI   KEY,X'06'                                                        
         MVC   KEY+1(2),SVAGY                                                   
         GOTO1 DATAMGR,DMCB,(0,=C'DMREAD'),=C'SPTDIR',KEY,KEY                   
         CLI   DMCB+8,0                                                         
         BNE   GMMDERR                                                          
         GOTO1 DATAMGR,DMCB,(0,=C'GETREC'),=C'SPTFIL',KEY+14,AIO2,     X        
               DMWORK                                                           
         CLI   DMCB+8,0                                                         
         BNE   GMMDERR                                                          
*                                                                               
GM10     L     R4,AIO2                                                          
         USING AGYHDRD,R4                                                       
*                                                                               
         LA    R4,AGYEL            LOCATE MEDIA ELEMENT                         
         SR    R0,R0                                                            
*                                                                               
GM20     CLI   0(R4),0             TEST E-O-R                                   
         BE    GMMDERR                                                          
         CLI   0(R4),X'02'         MEDIA CODE ELEMENT                           
         BNE   GM30                                                             
         CLI   2(R4),C'N'          GET NETWORK CODE                             
****     CLC   2(1,R4),SVMED       CHECK MEDIA CODE                             
         BE    GM40                                                             
*                                                                               
GM30     IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     GM20                                                             
*                                                                               
GM40     MVC   SVBAM,3(R4)         SAVE AGY/MED                                 
*                                                                               
GMX      B     XIT                                                              
*                                                                               
GMMDERR  BAS   RE,SETERR           SET ERROR                                    
         OI    MYSTAT,DEALERR                                                   
         B     GMX                                                              
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* SET MINIO RECORD FOR DEAL                                                     
***********************************************************************         
DEALMIN  NTR1                                                                   
         BAS   RE,GETSEQ           GET SEQ NUM FOR THIS AGY/CLT/DEAL            
         BAS   RE,SMINMKEY         SET MINIO MASTER KEY                         
         MVI   UPLOC,C'N'                                                       
         MVI   ERFLDNUM,0          ALL FIELDS ARE OK                            
*                                                                               
         L     R5,AMINBLK                                                       
         USING MINBLKD,R5                                                       
*                                                                               
         LA    R3,MINMKEY                                                       
         USING NEUPKEY,R3                                                       
         MVI   NEUPTYPE,X'32'      RECORD TYPE                                  
         MVC   NEUPAM,SVBAM        AGENCY/MEDIA                                 
         XC    MINEKEY,MINEKEY     SEE IF RECORD EXISTS                         
         MVI   MINEKEY,X'01'       STATUS ELEMENT                               
****     MVI   MINFILTL,1                                                       
         BAS   RE,READMIN                                                       
         BNE   DM50                                                             
*                                  CHECK IF UPLOADED BY LOCATION                
         LA    R3,ELEMENT                                                       
         USING NEUPEL01,R3                                                      
         TM    NEUPSTAT,NEUPSLOC   IF FIRST ONE WAS BY LOCATION                 
         BNO   DM10                                                             
         CLC   SVLOCID,SPACES      ALL OTHERS MUST BE                           
         BNH   DM20                                                             
         B     DM30                EXIT                                         
*                                                                               
DM10     CLC   SVLOCID,SPACES      IF 1ST ONE NOT BY LOCATION                   
         BNH   DM40                NO ON ELSE CAN BE                            
*                                                                               
DM20     XC    MINEKEY,MINEKEY     UNLESS NO RECORDS ON FILE                    
         MVI   MINEKEY,X'90'       LOOK FOR FIRST UNIT OF ESTIMATE              
         MVI   MINFILTL,1          L'COMPARE OF ELEMENT KEY (CODE)              
         BAS   RE,HIGHMIN                                                       
         CLI   MINERR,MINEEOF      THERE ARE UNITS                              
         BNE   DMINV               ERROR                                        
         XC    MINEKEY,MINEKEY     GET ACTIVITY ELEMENT AGAIN                   
         MVI   MINEKEY,X'01'                                                    
         MVI   MINFILTL,1                                                       
         BAS   RE,READMIN                                                       
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                WE JUST READ IT                              
         NI    NEUPSTAT,X'FF'-NEUPSLOC                                          
         CLC   SVLOCID,SPACES                                                   
         BNH   *+8                                                              
         OI    NEUPSTAT,NEUPSLOC   SET BY LOCATION                              
         BAS   RE,WRTMIN                                                        
*                                                                               
DM30     MVI   UPLOC,C'Y'                                                       
*                                                                               
DM40     B     DM60                                                             
*                                                                               
DM50     XC    ELEMENT,ELEMENT                                                  
         LA    R3,ELEMENT          RECORD DOESN'T EXIST YET                     
         USING NEUPEL01,R3         ADD IT - WITH AN ACTIVITY ELEMENT            
         MVI   NEUPEL01,X'01'      ELEMENT CODE                                 
         MVI   NEUPEL1L,NEUPEL1Q   ELEMENT LENGTH                               
         GOTO1 DATCON,DMCB,(5,0),(3,NEUPUDT)   UPLOAD DATE                      
         MVI   NEUPSEQN,0          FIRST UPLOAD OF THIS DEAL                    
         CLC   SVLOCID,SPACES      UPLOADED BY LOCATION                         
         BNH   *+8                                                              
         OI    NEUPSTAT,NEUPSLOC                                                
         DROP  R3                                                               
*                                                                               
         BAS   RE,ADDMIN           ADD MINIO                                    
*                                                                               
         BAS   RE,CLSMIN                                                        
*                                                                               
DM60     CLI   FRSTIME,C'Y'        ONLY DO THIS ONCE                            
         BNE   DMX                                                              
         BAS   RE,UPDACTV          UPDATE MINIO ACTIVITY ELEMENT                
*                                                                               
         BAS   RE,CLEANUP          CLEAN UP REC IF BAD RETURN FROM $NBU         
*                                                                               
DMX      B     XIT                                                              
*                                                                               
DMINV    MVC   ERRNUM,=H'2'        INVALID INPUT FIELD                          
         MVI   ERFLDNUM,NHDRUIDQ   LOCATION ID                                  
         BAS   RE,SETERR                                                        
         B     DMX                                                              
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE READS THE NET FILE TO FIND THE CORRECT SEQUENCE NUMBER           
* FOR THE MINIO RECORD                                                          
***********************************************************************         
GETSEQ   NTR1                                                                   
         MVI   SEQADD,C'Y'         NEED TO ADD NESA RECORD                      
         MVI   RDUPDATE,C'Y'                                                    
         MVC   AIO,AIO2                                                         
         XC    KEY,KEY             GET THE SEQUENCE ASSIGNMENT NUMBER           
         LA    R2,KEY                                                           
         USING NESAREC,R2                                                       
         MVI   NESATYPE,X'31'      SET RECORD TYPE                              
         MVC   NESAAM,SVBAM        SET AGY/MEDIA                                
         MVC   NESACLT,SVBCLT      SET CLIENT                                   
         MVC   NESADEAL,SVDEAL     SET THE DEAL                                 
         GOTO1 HIGH                                                             
         CLC   KEY(L'NESAKEY),KEYSAVE   RECORD ON FILE                          
         BNE   GSQ30                                                            
         MVI   SEQADD,C'N'         RECORD EXISTS - ONLY ADD ELEMENT             
         GOTO1 GETREC              FIND NETWORK ELEMENT                         
         L     R2,AIO                                                           
         USING NESAREC,R2                                                       
         LA    R4,NESAELEM         POINT TO 1ST ELEMENT                         
         MVI   ELCODE,X'01'                                                     
         USING NEASEL01,R4                                                      
         B     GSQ20                                                            
*                                                                               
GSQ10    BAS   RE,NEXTEL                                                        
         BNE   GSQ30                                                            
*                                                                               
GSQ20    CLC   NEASNET,SVNET       IS THIS THE NETWORK                          
         BNE   GSQ10                                                            
         MVC   AGYSEQ,NEASSEQ      SAVE SEQUENCE NUMBER                         
         B     GSQX                  AND EXIT                                   
*                                                                               
GSQ30    XC    KEY,KEY             FIND HIGHEST SEQUENCE NUMBER                 
         LA    R2,KEY                                                           
         SR    R3,R3                                                            
         USING NESQREC,R2                                                       
         MVI   NESQTYPE,X'30'      SET RECORD TYPE                              
         MVC   NESQAM,SVBAM        SET AGY/MEDIA                                
         GOTO1 HIGH                                                             
         B     GSQ50                                                            
*                                                                               
GSQ40    L     R3,NESQNUM          SAVE LAST SEQUENCE NUMBER USED               
         GOTO1 SEQ                                                              
*                                                                               
GSQ50    CLC   KEY(3),KEYSAVE      IS THIS AGENCY ON FILE                       
         BE    GSQ40                                                            
         LA    R3,1(R3)            INCREMENT IT                                 
         XC    KEY,KEY                                                          
         LA    R2,KEY                                                           
         MVI   NESQTYPE,X'30'      SET RECORD TYPE                              
         MVC   NESQAM,SVBAM        SET AGY/MEDIA                                
         ST    R3,NESQNUM          SET SEQUENCE NUMBER                          
         ST    R3,AGYSEQ           SAVE AGY SEQUENCE NUMBER                     
         L     R2,AIO                                                           
         XC    0(250,R2),0(R2)                                                  
         MVC   0(L'NESQKEY,R2),KEY                                              
         MVI   NESQLEN+1,31        SET RECORD LENGTH                            
         LA    R2,NESQELEM                                                      
         MVC   0(2,R2),=X'0104'    SET UP ELEMENT                               
         GOTO1 ADDREC              ADD THE RECORD                               
*                                                                               
         XC    KEY,KEY             SET THE SEQUENCE ASSIGNMENT REC KEY          
         LA    R2,KEY                                                           
         USING NESAREC,R2                                                       
         MVI   NESATYPE,X'31'      SET RECORD TYPE                              
         MVC   NESAAM,SVBAM        SET AGY/MEDIA                                
         MVC   NESACLT,SVBCLT      SET CLIENT                                   
         MVC   NESADEAL,SVDEAL     SET THE DEAL                                 
         CLI   SEQADD,C'Y'         NEED TO ADD RECORD?                          
         BE    GSQ60                                                            
         GOTO1 HIGH                                                             
         CLC   KEY(L'NESAKEY),KEYSAVE   RECORD ON FILE                          
         BE    *+6                 WE JUST READ IT                              
         DC    H'0'                                                             
         GOTO1 GETREC                                                           
         B     GSQ70                                                            
*                                                                               
GSQ60    L     R2,AIO                                                           
         MVC   0(L'NESAKEY,R2),KEY                                              
         MVI   NESALEN+1,27        SET RECORD LENGTH                            
*                                                                               
GSQ70    XC    ELEM,ELEM                                                        
         LA    R4,ELEM                                                          
         MVI   NEASEL01,X'01'      SET ELCODE                                   
         MVI   NEASEL1L,NEASEL1Q   SET LENGTH                                   
         MVC   NEASNET,SVNET       SET THE NETWORK                              
         MVC   NEASSEQ,AGYSEQ      SET THE SEQUENCE NUMBER                      
         GOTO1 ADDELEM,DMCB,ELEM                                                
*                                                                               
         CLI   SEQADD,C'Y'         NEED TO ADD RECORD?                          
         BE    GSQ80                                                            
         GOTO1 PUTREC              WRITE BACK THE RECORD                        
         B     GSQX                                                             
*                                                                               
GSQ80    GOTO1 ADDREC                                                           
*                                                                               
GSQX     B    XIT                                                               
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE UPDATES THE ACTIVITY ELEMENT IN THE MINIO RECORD                 
***********************************************************************         
UPDACTV  NTR1                                                                   
         L     R5,AMINBLK                                                       
         USING MINBLKD,R5                                                       
         XC    MINEKEY,MINEKEY     RECORD MUST EXIST                            
         MVI   MINEKEY,X'01'       STATUS ELEMENT                               
         MVI   MINFILTL,1                                                       
         BAS   RE,READMIN                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R3,ELEMENT                                                       
         USING NEUPEL01,R3                                                      
         MVC   NEUPLDT,NEUPUDT     SAVE PREVIOUS UPLOAD DATE                    
         GOTO1 DATCON,DMCB,(5,0),(3,NEUPUDT)   NEW UPLOAD DATE                  
         CLC   NEUPLDT,NEUPUDT     SAME DAY AS PREVIOUS UPLOAD?                 
         BNE   UPDA10                                                           
         SR    R1,R1               YES -- INCREMENT COUNTER                     
         IC    R1,NEUPSEQN                                                      
         LA    R1,1(R1)                                                         
         STC   R1,NEUPSEQN                                                      
*                                                                               
UPDA10   BAS   RE,WRTMIN                                                        
         B     XIT                                                              
         DROP  R3,R5                                                            
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE WILL CLEAN UP THE MINIO RECORD IF WE RETURNED FROM               
* $NBUY WITH A DUMP. IT WILL UNMARK RECORDS THAT WERE MARKED FOR                
* DELETION IF THE RECORD WAS NEVER DELETED FROM THE SPOTFILE AND                
* IT WILL CHECK IF ELEMENTS WITH A LINE NUMBER OF ZERO, WERE ADDED              
* TO THE SPOTFILE (VIA SERIAL NUMBER'S)                                         
***********************************************************************         
CLEANUP  NTR1                                                                   
         L     R5,AMINBLK                                                       
         USING MINBLKD,R5                                                       
         XC    WORK,WORK                                                        
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,X'90'       LOOK FOR FIRST UNIT OF SEQUENCE              
         MVI   MINFILTL,1          L'COMPARE OF ELEMENT KEY (CODE)              
         BAS   RE,HIGHMIN                                                       
         CLI   MINERR,MINEEOF      ANY HISTORY FOR THIS DEAL (SEQ NUM)          
         BE    CUX                 NO                                           
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
CU10     LA    R3,ELEMENT                                                       
         USING NEUPMEL,R3                                                       
         CLI   NEUPSLN1,0          IF THE NETPAK LINE NUMBER = 0                
         BNE   CU12                                                             
         BAS   RE,CHECKNET         AND ITS NOT ON THE NET FILE                  
         BNE   CU15                DELETE THE ELEMENT                           
         B     CU20                                                             
*                                                                               
CU12     TM    NEUPUSRC,X'80'      IS THIS MARKED FOR DELETION?                 
         BZ    CU20                                                             
         BAS   RE,CHECKNET         SEE IF ITS ON THE NET FILE                   
         BNE   CU15                                                             
         CLC   KEY(12),KEYSAVE                                                  
         BNE   CU15                                                             
         NI    NEUPUSRC,X'FF'-X'80' YES -- TAKE OFF DELETE BIT                  
         MVI   WORK,X'90'          SAVE ELEMENT KEY                             
         MVC   WORK+1(12),NEUPSERN                                              
         BAS   RE,WRTMIN                                                        
         MVI   MINCHG,C'Y'         MINIO RECORD CHANGED                         
*                                                                               
         MVC   MINEKEY(13),WORK    RESTORE ELEMENT KEY                          
         BAS   RE,READMIN                                                       
         BE    CU20                                                             
         DC    H'0'                BUT WE JUST READ IT!                         
*                                                                               
CU15     MVC   WORK(12),MINEKEY    SAVE ELEMENT KEY                             
         BAS   RE,DELMIN                                                        
         MVC   MINEKEY(12),WORK    NOW FIND THE NEXT ONE                        
*                                                                               
CU20     BAS   RE,SEQMIN                                                        
         CLI   MINERR,MINEEOF      ANY MORE UNITS                               
         BE    CUX                 NO                                           
         CLI   MINERR,0                                                         
         BE    CU10                                                             
         DC    H'0'                                                             
*                                                                               
CUX      CLI   MINCHG,C'Y'                                                      
         BNE   *+8                                                              
         BAS   RE,CLSMIN                                                        
         B     XIT                                                              
         DROP  R3,R5                                                            
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE CHECKS THE SPOT FILE TO SEE IF A UNIT EXIST ON THE FILE          
***********************************************************************         
CHECKNET NTR1                                                                   
         MVI   BYTE,1                                                           
         L     R5,AMINBLK                                                       
         USING MINBLKD,R5                                                       
         LA    R4,KEY                                                           
         USING NURECD,R4                                                        
         XC    NUKEY,NUKEY         BUILD UNIT RECORD KEY                        
         LA    R3,ELEMENT                                                       
         USING NEUPMEL,R3          FROM ELEMENT                                 
         MVC   NUKPSUB,NEUPSLN1    LINE 1                                       
*                                                                               
CN10     MVI   NUKPKEY,X'84'       RECORD TYPE                                  
         MVC   NUKPAM,SVBAM        A/M                                          
         MVC   NUKPCLT,NEUPCLT     CLT                                          
         MVC   NUKPNET,NEUPNET     NETWORK                                      
         MVC   NUKPPROG,NEUPPROG   PROGRAM CODE                                 
         MVC   NUKPDATE,NEUPDATE   DATE                                         
         MVC   NUKPEST,NEUPEST     EST                                          
         MVC   NUKPDP,NEUPDPT      DAYPART                                      
         CLI   NUKPSUB,0           WAS THERE A LINE NUMBER                      
         BNE   CN20                                                             
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI'),=C'UNTDIR',KEY,KEY                   
         B     CN15                                                             
*                                                                               
CN12     GOTO1 DATAMGR,DMCB,(0,=C'DMRSEQ'),=C'UNTDIR',KEY,KEY                   
*                                                                               
CN15     CLC   KEY(17),KEYSAVE                                                  
         BE    CN40                                                             
         B     NO                                                               
*                                                                               
CN20     MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,(0,=C'DMREAD'),=C'UNTDIR',KEY,KEY                   
*                                                                               
CN30     CLI   DMCB+8,0                                                         
         BNE   NO                  RECORD NOT FOUND                             
*                                                                               
CN40     BAS   RE,CHECKREC         GET RECORD & COMPARE SERIAL NUMBER           
         BE    CN50                NOT FOUND - DELETE ELEMENT                   
         CLI   NUKPSUB,0           WAS THERE A LINE NUMBER                      
         BE    CN12                NO TRY NEXT                                  
         BNE   NO                                                               
*                                                                               
CN50     LA    R4,KEY                                                           
         USING NURECD,R4                                                        
         CLI   BYTE,1                                                           
         BE    CN60                                                             
         MVC   NEUPSLN2,NUKPSUB    YES - SET LINE #                             
         B     *+10                                                             
*                                                                               
CN60     MVC   NEUPSLN1,NUKPSUB    YES - SET LINE #                             
         BAS   RE,WRTMIN                                                        
         MVI   MINCHG,C'Y'         MINIO RECORD CHANGED                         
         CLI   BYTE,2                                                           
         BE    YES                                                              
         CLI   NEUPSLN2,0          IS THERE A LINE 2                            
         BE    YES                 NO - THEN OK                                 
         XC    NUKEY,NUKEY         BUILD UNIT RECORD KEY                        
         MVC   NUKPSUB,NEUPSLN2    LINE 2                                       
         MVI   BYTE,2                                                           
         B     CN10                                                             
         DROP  R3,R4,R5                                                         
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE READS ALL THE RECORD & COMPARES SERIAL NUMBER'S                  
***********************************************************************         
CHECKREC NTR1                                                                   
         L     R5,AMINBLK                                                       
         USING MINBLKD,R5                                                       
         LA    R3,ELEMENT                                                       
         USING NEUPMEL,R3                                                       
*                                                                               
CR10     GOTO1 DATAMGR,DMCB,(0,=C'GETREC'),=C'UNTFIL',KEY+21,AIO                
         L     R4,AIO                                                           
         USING NURECD,R4                                                        
         LA    R4,NUDATA           GET SERIAL NUMBER ELEMENT                    
*                                                                               
CR20     ZIC   R1,1(R4)                                                         
         AR    R4,R1                                                            
         CLI   0(R4),0                                                          
         BE    NO                                                               
         CLI   0(R4),X'75'         SERIAL NUMBER ELEMENT                        
         BNE   CR20                                                             
         USING NUSQD,R4                                                         
         CLC   NUSQSER,NEUPSERN     DO THE SERIAL NUMBER'S MATCH                
         BNE   NO                                                               
         DROP  R4                                                               
         L     R4,AIO                                                           
         USING NURECD,R4                                                        
         LA    R4,NUDATA           GET MAIN DATA ELEMENT                        
*                                                                               
CR30     ZIC   R1,1(R4)                                                         
         AR    R4,R1                                                            
         CLI   0(R4),0                                                          
         BE    CR40                                                             
         CLI   0(R4),X'01'         MAIN DATA ELEMENT                            
         BNE   CR30                                                             
         USING NUMAINEL,R4                                                      
         TM    NUUNITST,X'40'      IF PRE-EMT BIT IS ON                         
         BO    NO                  THEN WE CAN DELETE ELEMENT                   
*                                                                               
CR40     DS    0H                                                               
         B     YES                                                              
*&&DO                                                                           
         LA    R4,KEY                                                           
         USING NURECD,R4                                                        
         MVC   NEUPSLN1,NUKPSUB     YES - SET LINE #                            
         BAS   RE,WRTMIN                                                        
         MVI   MINCHG,C'Y'         MINIO RECORD CHANGED                         
         B     YES                                                              
*&&                                                                             
*                                                                               
CRNO     B     NO                                                               
         DROP  R5                                                               
         EJECT                                                                  
*                                                                               
*        MINIO I/O ROUTINES                                                     
*                                                                               
***********************************************************************         
* THIS ROUTINE SETS MINIO MASTER KEY                                            
***********************************************************************         
SMINMKEY NTR1                                                                   
         L     R5,AMINBLK                                                       
         USING MINBLKD,R5                                                       
         LA    R3,MINMKEY          SET MASTER KEY                               
         USING NEUPKEY,R3                                                       
         MVI   NEUPTYPE,X'32'      RECORD TYPE                                  
         MVC   NEUPAM,SVBAM        AGENCY/MEDIA                                 
         MVC   NEUPSER,SVSERN      SERIAL NUMBER                                
         L     R1,AGYSEQ                                                        
         STCM  R1,15,NEUPSEQ       AGY SEQUENCE NUMBER                          
         B     XIT                                                              
         SPACE                                                                  
***********************************************************************         
* THIS ROUTINE READ HIGH FOR A MINIO ELEMENT                                    
***********************************************************************         
HIGHMIN  NTR1                                                                   
         GOTO1 MINIO,DMCB,('MINHI',(R5))                                        
         B     XIT                                                              
         SPACE                                                                  
***********************************************************************         
* THIS ROUTINE READS SEQUENTIAL A MINIO ELEMENT                                 
***********************************************************************         
SEQMIN   NTR1                                                                   
         GOTO1 MINIO,DMCB,('MINSEQ',(R5))                                       
         B     XIT                                                              
         SPACE                                                                  
***********************************************************************         
* THIS ROUTINE READS A MINIO ELEMENT                                            
***********************************************************************         
READMIN  NTR1                                                                   
         GOTO1 MINIO,DMCB,('MINRD',(R5))                                        
         CLI   MINERR,0                                                         
         BE    YES                 YES EXIT                                     
         B     NO                                                               
         SPACE                                                                  
***********************************************************************         
* THIS ROUTINE ADDS A MINIO ELEMENT                                             
***********************************************************************         
ADDMIN   NTR1                                                                   
         GOTO1 MINIO,DMCB,('MINADD',(R5))                                       
         CLI   MINERR,0                                                         
         BE    XIT                                                              
         DC    H'0'                                                             
         SPACE                                                                  
***********************************************************************         
* THIS ROUTINE CLOSES A MINIO RECORD                                            
***********************************************************************         
CLSMIN   NTR1                                                                   
         GOTO1 MINIO,DMCB,('MINCLS',(R5))                                       
         CLI   MINERR,0                                                         
         BE    XIT                 CONTINUE                                     
         DC    H'0'                                                             
         SPACE                                                                  
***********************************************************************         
* THIS ROUTINE WRITES A MINIO ELEMENT                                           
***********************************************************************         
WRTMIN   NTR1                                                                   
         GOTO1 MINIO,DMCB,('MINWRT',(R5))                                       
         CLI   MINERR,0                                                         
         BE    XIT                                                              
         DC    H'0'                                                             
         SPACE                                                                  
***********************************************************************         
* THIS ROUTINE DELETES A MINIO ELEMENT                                          
***********************************************************************         
DELMIN   NTR1                                                                   
         GOTO1 MINIO,DMCB,('MINDEL',(R5))                                       
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   MINCHG,C'Y'         MINIO RECORD CHANGED                         
         B     XIT                                                              
         EJECT                                                                  
NEXTEL   CLI   0(R4),0                                                          
         BE    NEXTELX                                                          
         ZIC   R0,1(R4)                                                         
         LTR   R0,R0                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R4,R0                                                            
*                                                                               
NEXTEL2  CLI   0(R4),0                                                          
         BE    NEXTELX                                                          
         CLC   ELCODE,0(R4)                                                     
         BNE   NEXTEL                                                           
         CR    RB,RB                                                            
         B     *+6                                                              
*                                                                               
NEXTELX  LTR   RB,RB                                                            
         BR    RE                                                               
         EJECT                                                                  
         EJECT                                                                  
EXIT     L     RD,SAVEDRD                                                       
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         SPACE 3                                                                
XFF      DC    64X'FF'                                                          
SPACES   DC    64C' '                                                           
         SPACE 5                                                                
         LTORG                                                                  
         EJECT                                                                  
*        PRINT OFF                                                              
       ++INCLUDE CTMAD19TES                                                     
*        PRINT OFF                                                              
       ++INCLUDE CTMAD19TST                                                     
         EJECT                                                                  
         PRINT ON                                                               
       ++INCLUDE CTMADFFD                                                       
         EJECT                                                                  
       ++INCLUDE CTMADWORKD                                                     
         EJECT                                                                  
       ++INCLUDE CTMADDSECT                                                     
         EJECT                                                                  
       ++INCLUDE DDMINBLK                                                       
         EJECT                                                                  
       ++INCLUDE NEUPLOADD                                                      
         EJECT                                                                  
       ++INCLUDE NEGENUPL                                                       
         EJECT                                                                  
       ++INCLUDE NEGENSEQ                                                       
         SPACE 3                                                                
       ++INCLUDE NEGENSEQA                                                      
         EJECT                                                                  
* DDCOMFACS                                                                     
* DDGLOBEQUS                                                                    
* CTMADEQUS                                                                     
* DDGLVXCTLD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDGLOBEQUS                                                     
       ++INCLUDE CTMADEQUS                                                      
       ++INCLUDE DDGLVXCTLD                                                     
AGYHDRD  DSECT                                                                  
       ++INCLUDE SPGENAGY                                                       
       ++INCLUDE NEGENUNIT                                                      
         PRINT ON                                                               
         EJECT                                                                  
* OVERLAY WORKING STORAGE                                                       
*                                                                               
OVERD    DSECT                                                                  
APRELO   DS    A                                                                
APBASE1  DS    A                                                                
APBASE2  DS    A                                                                
SAVEDRD  DS    A                   RD UPON ENTRY TO OVERLAY                     
GLOBBER  DS    A                   A(GLOBBER)                                   
MINIO    DS    A                   A(MINIO)                                     
RECUP    DS    A                   A(RECUP)                                     
MEDGET   DS    A                   A(MEDGET)                                    
AWORKSTR DS    A                   A(UNSAVED WORKING STORAGE)                   
AMINBLK  DS    A                   A(MINIO PARAMETER BLOCK)                     
AMINBUFF DS    A                   A(MINIO I/O BUFFERS)                         
AMINRECT DS    A                   A(MINIO RECORD TABLE)                        
*                                                                               
AGYSEQ   DS    F                   THIS AGENCY'S SEQUENCE NUMBER                
*                                                                               
ABSRECN  DS    F                   ABSOLUTE RECORD NUMBER                       
TOTALDEL DS    F                   TOTAL NUMBER UNITS DELETED                   
RECNUM   DS    F                   UNIT NUMBER                                  
SUBNUM   DS    H                   SUB-RECORD NUMBER                            
MYOBJLEN DS    H                   LENGTH OF OBJECTS I BUILD                    
ERRNUM   DS    H                   ERROR NUMBER                                 
*                                                                               
CONFFLAG DS    X                                                                
*                                                                               
MISCFLG1 DS    X                   MISCELLANEOUS FLAG                           
MF1MVU   EQU   X'80'                - IT'S THE NEW UPLOAD FORMAT (MVU)          
*                                                  (NOT NBU)                    
TSTDATAF DS    CL1                 WHICH TEST RECORDS TO USE                    
ENDOBJCT DS    CL1                 END OF OBJECTS FLAG                          
ENDBLOCK DS    CL1                 END OF BLOCK FLAG                            
ERFLDNUM DS    X                   FIELD NUMBER IN ERROR                        
FRSTIME  DS    CL1                 'Y' = THIS IS FIRST TIME THROUGH             
TMPOPEND DS    CL1                 'Y' = TEMPSTR IS OPEN                        
MINCHG   DS    CL1                 'Y' = MINIO RECORD CHANGED                   
UPLOC    DS    CL1                 'Y' = UPLOAD BY LOCATION                     
UNTNUM   DS    XL2                 UNIT NUMBER                                  
SVLOCID  DS    CL2                 LOCATION ID                                  
SVLINE   DS    X                   NETPAK LINE NUMBER                           
SVLINE2  DS    X                   NETPAK LINE 2 NUMBER                         
SVACT    DS    CL1                 ACTION CODE                                  
SVSER    DS    CL12                SERIAL NUMBER                                
SVPROG   DS    CL6                 PROGRAM CODE                                 
SVADATE  DS    XL2                 AIR DATE                                     
SVEST    DS    XL1                 ESTIMATE                                     
SVDPT    DS    CL1                 DAYPART                                      
*                                                                               
UPLSER   DS    CL1                 UPLOAD BY SERIAL NUMBER                      
PUTINFO  DS    CL1                 PUT SOMETHING TO TEMPSTR                     
*                                                                               
ELCODE   DS    X                   ELEMENT CODE                                 
ELEM     DS    XL255               ELEMENT                                      
*                                                                               
MYSTAT   DS    X                   INTERNAL STATUS BYTE                         
MYSDEL  EQU    X'80'               THIS IS A DELETE ERROR                       
DEALERR EQU    X'40'               THIS IS A DEAL ERROR                         
PKGERR  EQU    X'20'               THIS IS A PKG ERROR                          
PGMERR  EQU    X'10'               THIS IS A PROGRAM ERROR                      
CHUERR  EQU    X'08'               THIS IS A UNIT UPDATE ERROR                  
*                                                                               
SVSYS    DS    CL1                 SYSTEM                                       
SVTYPE   DS    CL3                 UPLOAD TYPE                                  
SVAGY    DS    CL2                 AGENCY                                       
SVMED    DS    CL1                 MEDIA                                        
SVBAM    DS    X                   AGY/MED                                      
SVCLT    DS    CL3                 CLIENT                                       
SVBCLT   DS    XL2                 CLIENT (PACKED)                              
SVDEAL   DS    CL10                DEAL NUMBER                                  
SVNET    DS    CL4                 NETWORK                                      
SVSERN   DS    CL12                SERIAL NUMBER                                
SVWRTFLG DS    CL1                 WRITE ENABLE FLAG                            
SVUPDFLG DS    CL1                 UPDATES ONLY FLAG                            
SEQNUM   DS    X                   SEQUENCE NUMBER (FOR TIES ON DATE)           
SEQADD   DS    C                   ADD SEQUENCE RECORD (Y/N)                    
*                                                                               
MYOBJEOF DS    CL50                BUILD A TWA EOF OBJECT HERE                  
*                                                                               
MYOBJGET DS    (MYOBJLNQ)CL1       BUILD A TWA OBJECT HERE                      
*                                                                               
MYOBJECT DS    (MYOBJLNQ)CL1       BUILD A TWA OBJECT HERE                      
MYOBJLNQ EQU   1500                MAXIMUM OBJECT LENGTH                        
*                                                                               
         DS    (4096-(*-OVERD))X   $MAD CONTROLLER ONLY SAVES 4K                
         EJECT                                                                  
*                   ELEMENTS                                                    
*                                                                               
UHDRD    DSECT                                                                  
UHDRSYS  DS    CL1                 SYSTEM (ALWAYS C'N')                         
UHDRUTYP DS    CL3                 UPLOAD TYPE (ALWAYS 'NBU')                   
UHDRAGID DS    CL2                 BUYING AGENCY ID                             
UHDRUID2 DS    CL2                 LOCATION ID-1ST 2 CHARS UNIQUE ID            
UHDRUPDO DS    CL1                 UPDATES ONLY? (Y/N)                          
UHDRTST  DS    CL1                 TEST DATA                                    
UHDRTSTR DS    CL1                 TEST RUN - DON'T UPDATE                      
         DS    CL30                SPARE                                        
         SPACE 3                                                                
*                                                                               
UDLD     DSECT                                                                  
UDLTYPE  DS    CL4                 OBJECT TYPE (ALWAYS C'DEAL')                 
UDLMED   DS    CL1                 MEDIA CODE                                   
UDLTPC   DS    CL8                 TRADING PARTNER CODE                         
UDLNET   DS    CL4                 NETWORK CODE                                 
UDLCLT   DS    CL3                 CLIENT CODE                                  
UDLDEAL  DS    CL10                DEAL NUMBER                                  
UDLEDF   DS    CL1                 EST DEMO FORM                                
UDLADF   DS    CL1                 ACT DEMO FORM                                
UDLDEL   DS    CL1                 DEAL IS DELETED                              
         DS    CL7                                                              
UDLDEMOS DS    18CL7               DEMOS                                        
UDLLENQ EQU    *-UDLD              L'OBJECT FOR $NBUY                           
         SPACE 3                                                                
*                                                                               
UPKGD    DSECT                                                                  
UPKGTYPE DS    CL4                 OBJECT TYPE (ALWAYS C'PKG*')                 
UPKGEST  DS    CL3                 ESTIMATE CODE                                
UPKGPKG  DS    CL3                 PACKAGE CODE                                 
UPKGDESC DS    CL16                PACKAGE DESCRIPTION                          
UPKGDPT  DS    CL1                 DAYPART CODE                                 
UPKGCOST DS    CL8                 PACKAGE COST                                 
UPKGUNV  DS    CL4                 UNIVERSE CODE                                
UPKGREP  DS    CL3                 SPECIAL REP                                  
UPKGLENQ EQU   *-UPKGD             L'OBJECT FOR $NBUY                           
         SPACE 3                                                                
*                                                                               
UPGMD    DSECT                                                                  
UPGMTYPE DS    CL4                 OBJECT TYPE (ALWAYS C'PROG')                 
UPGMEST  DS    CL3                 ESTIMATE CODE                                
UPGMPKG  DS    CL3                 PACKAGE CODE                                 
UPGMROT  DS    CL7                 ROTATION                                     
UPGMSTIM DS    CL4                 START TIME                                   
UPGMETIM DS    CL4                 END TIME                                     
UPGMPROG DS    CL16                PROGRAM                                      
UPGMPGCD DS    CL6                 PROGRAM CODE                                 
UPGMULN  DS    CL3                 UNIT LENGTH                                  
UPGMULNT DS    CL1                 UNIT LENGTH TYPE                             
UPGMTRT  DS    CL9                 TIME RATE                                    
UPGMTRTQ DS    CL1                 TIME RATE QUALIFIER                          
UPGMMSTM DS    CL4                 MIRROR START TIME                            
         DS    CL20                                                             
UPGMIRT  DS    CL8                 INTEGRATION RATE                             
UPGMIRTQ DS    CL1                 INTEGRATION RATE QUALIFIER                   
UPGMCOST DS    CL9                 ASSIGNED COST                                
UPGMLENQ EQU   *-UPGMD             L'OBJECT FOR $NBUY                           
         SPACE 3                                                                
*                                                                               
***  MVU  ***                                                                   
* UNIT UPDATE OBJECT IS PRESENT IF UPLOAD TYPE IS MVU IN FILE HEADER *          
UCHUD    DSECT                                                                  
UCHUTYPE DS    CL4                 OBJECT TYPE (ALWAYS C'UNUP')                 
UCHUACTN DS    CL2                 ACTION TYPE (ALPHABETIC)                     
UCHUUID  DS    CL36                DDS UNIT ID (18 BYTE HEX IN TEXT)            
UCHUSERN DS    CL12                SERIAL NUMBER                                
UCHURSN  DS    CL3                 DDS REASON CODE                              
UCHUCON  DS    CL10                CONTRACT NUMBER                              
UCHUPGCD DS    CL6                 NETPAK PROGRAM CODE                          
UCHUDATE DS    CL6                 BUY DATE (YYMMDD)                            
UCHUROT  DS    CL7                 ROTATION (DAYS OF WEEK)                      
UCHUTSTR DS    XL4                 START TIME (0001-2400)                       
UCHUTEND DS    XL4                 END TIME (0001-2400)                         
UCHUPGNM DS    CL16                PROGRAM NAME                                 
UCHUULN  DS    XL3                 UNIT LENGTH                                  
UCHUTRTQ DS    XL10                TIME RATE & (OPT) QUALFR. (CENTS)            
UCHUITRQ DS    XL9                 INTEGRATN RATE & (OPT) QLFR (CENTS)          
UCHUCOST DS    XL9                 ASSIGNED COST (CENTS)                        
UCHUBLAT DS    CL1                 BILLBOARDS ATTACHED (Y/N)                    
UCHUADU  DS    CL1                 ADU FLAG (Y/N)                               
UCHUNPAY DS    CL1                 NO PAY UNIT FLAG (Y/N)                       
UCHUNIMP DS    CL1                 NO IMPRESSION UNIT FLAG (Y/N)                
UCHUPREE DS    CL1                 PREEMPT UNIT? (Y/N)                          
UCHUPROD DS    CL7                 PRODUCT                                      
UCHUPCT  DS    CL6                 1ST PRODUCT PERCENT                          
UCHUCOM1 DS    CL60                COMMENT 1                                    
UCHUCOM2 DS    CL60                COMMENT 2                                    
UCHULENQ EQU   *-UCHUD             L'OBJECT FOR $NBUY                           
* UNIT UPDATE OBJECT IS PRESENT IF UPLOAD TYPE IS MVU IN FILE HEADER *          
***  MVU  ***                                                                   
         SPACE 3                                                                
*                                                                               
UPDED    DSECT                                                                  
UPDETYPE DS    CL4                 OBJECT TYPE (ALWAYS C'PDRE')                 
UPDEEHI  DS    CL6                 EST HOMES IMP                                
UPDEEDV  DS    18CL6               EST DEMO VALUES                              
UPDELENQ EQU   *-UPDED             L'OBJECT FOR $NBUY                           
         SPACE 3                                                                
*                                                                               
UPDAD    DSECT                                                                  
UPDATYPE DS    CL4                 OBJECT TYPE (ALWAYS C'PDRA')                 
UPDAEHI  DS    CL6                 ACT HOMES IMP                                
UPDAEDV  DS    18CL6               ACT DEMO VALUES                              
UPDALENQ EQU   *-UPDAD             L'OBJECT FOR $NBUY                           
*                                                                               
UUNTD    DSECT                                                                  
UUNTTYPE DS    CL4                 OBJECT TYPE (ALWAYS C'UNIT')                 
UUNTSER  DS    CL12                SERIAL NUMBER                                
UUNTWOD  DS    CL6                 WEEK OF DATE                                 
UUNTDAYN DS    CL1                 DAY NUMBER                                   
         DS    CL3                                                              
UUNTBLAT DS    CL4                 BILLBOARDS ATTACHED                          
UUNTADU  DS    CL1                 ADU FLAG                                     
UUNTMG   DS    CL1                 MAKEGOOD                                     
UUNTPM   DS    CL1                 PRE-EMPTED/MISSED                            
UUNTMGS  DS    8CL12               MAKEGOOD SERIAL NUMBERS                      
UUNPROG  DS    CL6                 PROGRAM CODE - RETURNED BY NBUY              
UUNADAT  DS    XL2                 AIR DATE - RETURNED BY NBUY                  
UUNASEQ  DS    XL4                 AGENCY SEQUENCE NUMBER                       
UUNTLENQ EQU   *-UUNTD             L'OBJECT FOR $NBUY                           
         SPACE 3                                                                
*                                                                               
UUDED    DSECT                                                                  
UUDETYPE DS    CL4                 OBJECT TYPE (ALWAYS C'UDRE')                 
UUDEEHI  DS    CL6                 EST HOMES IMP                                
UUDEEDV  DS    18CL6               EST DEMO VALUES                              
UUDELENQ EQU   *-UUDED             L'OBJECT FOR $NBUY                           
         SPACE 3                                                                
*                                                                               
UUDAD    DSECT                                                                  
UUDATYPE DS    CL4                 OBJECT TYPE (ALWAYS C'UDRA')                 
UUDAEHI  DS    CL6                 ACT HOMES IMP                                
UUDAEDV  DS    18CL6               ACT DEMO VALUES                              
UUDALENQ EQU   *-UUDAD             L'OBJECT FOR $NBUY                           
*                                                                               
*                                                                               
USPCD    DSECT                                                                  
USPCTYPE DS    CL4                 OBJECT TYPE (ALWAYS C'SPCH')                 
USPCCODE DS    CL2                 SPECIAL CHARGE CODE                          
USPCAMT  DS    CL9                 SPECIAL CHARGE AMOUNT                        
USPCDATX EQU   *                                                                
USPCLENQ EQU   *-USPCD             L'OBJECT FOR $NBUY                           
         EJECT                                                                  
*                                                                               
UPRDD    DSECT                                                                  
UPRDTYPE DS    CL4                 OBJECT TYPE (ALWAYS C'PROD')                 
UPRDPROD DS    CL3                 PRODUCT                                      
UPRDPPRD DS    CL3                 PARTNER PRODUCT (P/B)                        
UPRDPLN  DS    CL3                 1ST PRODUCT LENGTH                           
UPRDPCST DS    CL3                 1ST PRODUCT COST PERCENT                     
UPRDDATX EQU   *                                                                
UPRDLENQ EQU   *-USPCD             L'OBJECT FOR $NBUY                           
         SPACE 3                                                                
*                                                                               
UCOMD    DSECT                                                                  
UCOMTYPE DS    CL4                 OBJECT TYPE (ALWAYS C'CMMT')                 
UCOMDATA DS    CL70                COMMENT DATA                                 
UCOMDATX EQU   *                                                                
UCOMLENQ EQU   *-USPCD             L'OBJECT FOR $NBUY                           
         SPACE 3                                                                
*                                                                               
UCMLD    DSECT                                                                  
UCMLTYPE DS    CL4                 OBJECT TYPE (ALWAYS C'COMM')                 
UCMLISC  DS    CL8                 ISCII CODE                                   
UCMLISCN DS    CL24                ISCII NAME                                   
UCMLCLAS DS    CL5                 COMMERCIAL CLASS                             
UCMLPISC DS    CL8                 PARTNER ISCII CODE                           
UCMLPISN DS    CL24                PARTNER ISCII NAME                           
UCMLPCLA DS    CL5                 PARTNER COMMERCIAL CLASS                     
UCMLBISC DS    CL8                 BB ISCII CODE                                
UCMLBISN DS    CL24                BB ISCII NAME                                
UCMLBCLA DS    CL5                 BB COMMERCIAL CLASS                          
UCMLDATX EQU   *                                                                
UCMLLENQ EQU   *-USPCD             L'OBJECT FOR $NBUY                           
         EJECT                                                                  
*                                                                               
DSTATUSD DSECT                                                                  
DSTATYPE DS    CL2                 STATUS OBJECT TYPE                           
*                                   C'01' = RECORD IN ERROR                     
*                                   C'02' = RECORD MODIFIED OK                  
*                                   C'03' = RECORD RECORD ADDED OK              
*                                   C'04' = TOTAL NUMBER DELETED                
DSTAUNTN DS    CL5                 UNIT NUMBER (ONE BASED)                      
         ORG   DSTAUNTN                                                         
DSTATOTD DS    CL5                 TOTAL NUMBER OF UNITS DELETED                
*                                   ONLY WHEN TYPE = C'04'                      
DSTAFREC DS    CL5                 ABSOLUTE RECORD NUMBER ON FILE               
DSTAOKLQ EQU   *-DSTATUSD          L'OBJECT WHEN TYPE = C'02','03','04'         
DSTASUBR DS    CL2                 SUB-RECORD NUMBER (ZERO-BASED)               
DSTAFLDN DS    CL2                 FIELD NUMBER IN ERROR                        
DSTAMSGN DS    CL5                 ERROR MESSAGE NUMBER                         
DSTAERLQ EQU   *-DSTATUSD          L'OBJECT WHEN TYPE = C'01'                   
         EJECT                                                                  
*              EQUATES                                                          
*                                                                               
LENMINRC EQU   1976                LENGTH OF A SPOT MINIO RECORD                
NUMMNBUF EQU   2                   NUMBER OF MINIO RECORD BUFFERS               
MINBUFSZ EQU   NUMMNBUF*LENMINRC   TOTAL SPACE FOR MINIO BUFFERS                
MINRCTBL EQU   2000                MINIO RECORD TABLE SIZE                      
         SPACE 3                                                                
WORKD    DSECT                                                                  
*                                                                               
MINBLK   DS    (MINBLKL)X          MINIO PARAMETER BLOCK                        
*                                                                               
MINRECTB DS    (MINRCTBL)X         MINIO RECORD TABLE                           
MINBUFFS DS    (MINBUFSZ)X         MINIO I/O BUFFERS                            
*                                                                               
WORKX    EQU   *                                                                
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'087CTMAD19   05/04/05'                                      
         END                                                                    
