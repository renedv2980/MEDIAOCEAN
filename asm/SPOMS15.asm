*          DATA SET SPOMS15    AT LEVEL 002 AS OF 08/27/13                      
*PHASE T23415A                                                                  
T23415   TITLE 'SPOMS15 - ORDER PHISTORY'                                       
T23415   CSECT                                                                  
         PRINT NOGEN                                                            
BGN      NMOD1 0,*T23415*,R7,RR=R3                                              
*                                                                               
         L     RC,0(R1)                   STANDARD CODING                       
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA             BASE SCREEN + OUR SCREEN              
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         LA    R5,SYSSPARE                R5=A(LOCAL SAVED STORAGE)             
         USING LSSD,R5                                                          
*                                                                               
         LA    R2,CONACTH                 CURSOR TO ACTION FIELD                
         CLI   CALLSP,0                   DID WE PFKEY INTO PMKGD?              
         BE    INVLFLD                    NO - ERROR                            
*                                                                               
         ST    R3,RELO                    RELO                                  
         ST    RC,BASERC                  BASE RC                               
         BAS   RE,GETPF                   GET PFKEYS                            
*                                                                               
         L     RF,ACOMFACS                A(COMFACS)                            
         MVC   VGLOBBER,CGLOBBER-COMFACSD(RF)                                   
*                                                                               
         OI    OPSMEDH+1,X'20'            PROTECT                               
         OI    OPSORDRH+1,X'20'           PROTECT                               
         MVI   NLISTS,15                  15 RECORDS PER LIST SCREEN            
*                                                                               
         CLI   MODE,LISTRECS              LIST RECORDS?                         
         BE    LR                         YES                                   
*                                                                               
XIT      XIT1                             EXIT                                  
***********************************************************************         
* LIST THE RECORD                                                     *         
***********************************************************************         
LR       OC    KEY,KEY                    FIRST TIME IN?                        
         BZ    *+12                       YES                                   
         L     R6,SVAELEM                 CONTINUE LISTING FROM HERE            
         B     LR00                       CONTINUE LISTING ELEMENTS             
*                                                                               
         LA    R2,OPSMEDH                 CURSOR AT MED IN CASE OF ERR          
         LA    R6,KEY                     R6 = KEY                              
         XC    KEY,KEY                    CLEAR THE KEY                         
         USING DOKEY,R6                   DARE ORDER PATCH KEY DSECT            
*                                                                               
         GOTO1 VGLOBBER,DMCB,=C'GETD',WORK,24,GLVSPREQ                          
*                                                                               
         GOTO1 HEXIN,DMCB,WORK,KEY,24     SET THE KEY WITH THE ORDER            
*                                                                               
         MVI   DOKCMT,DOKPSTAT            X'07'                                 
*                                                                               
         GOTO1 HIGH                       READ HIGH                             
         CLC   KEY(13),KEYSAVE            FOUND THE PATCH RECORD?               
         BNE   ERRNOORD                   NO - ERROR                            
*                                                                               
         GOTO1 GETREC                     GET THE ORDER PATCH RECORD            
*                                                                               
         L     R6,AIO                     A(ORDER PATCH)                        
         LA    R6,24(R6)                  A(FIRST ELEMENT)                      
*                                                                               
LR00     CLI   0(R6),0                    END OF RECORD?                        
         BE    LRX                        YES - DONE                            
         XC    LISTAR,LISTAR              CLEAR LIST LINE                       
         ST    R6,SVAELEM                 SAVE A(X'05') ELEM                    
         CLI   0(R6),DOPSELQ              STATUS PATCH ELEMENT?                 
         BE    LR15                       YES                                   
         CLI   0(R6),DOPNELQ              SEND METHOD ELEMENT?                  
         BE    LR20                       YES                                   
         CLI   0(R6),DOPPELQ              SUPPLEMENTARY PATCH ELEMENT?          
         BE    LR25                       YES                                   
         CLI   0(R6),DOPMELQ              MAKEGOOD PATCH ELEMENT?               
         BE    LR30                       YES                                   
         B     LR10                       UNKNOWN ELEMENT                       
*                                                                               
LR05     GOTO1 LISTMON                    LIST THE RECORD                       
*                                                                               
LR10     LLC   R1,1(R6)                   ELEEMENT LENGTH                       
         AR    R6,R1                      BUMP TO NEXT ELEMENT                  
         B     LR00                       NEXT ELEMENT                          
*                                                                               
         USING DOPSTATD,R6                STATUS PATCH ELEMENT DSECT            
LR15     MVC   LEVENT,=C'STAT'            STATUS PATCH                          
         MVC   PASSWD,DOPSPID             USER ID                               
         BAS   RE,AUTH                    GET USER ID                           
         MVC   LUSERID,PRSNLID            USER-ID                               
*                                                                               
         GOTO1 DATCON,DMCB,(8,DOPSDAT),(11,LDATE)                               
*                                                                               
         MVC   HALF,DOPSTIM               TIME                                  
         BAS   RE,DISTIME                 DISPLAY TIME                          
*                                                                               
         CLI   DOPSLEN,DOPSLNQ2           HAVE TICKET IN THE ELEMENT?           
         BL    LR05                       NO, LIST THE ELEMENT                  
         MVC   LTICKET,DOPSTIC            TICKET                                
         B     LR05                       LIST THE ELEMENT                      
         DROP  R6                         DROP R6                               
*                                                                               
         USING DOPSENDD,R6                SEND METHOD ELEMENT DSECT             
LR20     MVC   LEVENT,=C'SEND'            SEND METHOD PATCH                     
         MVC   PASSWD,DOPNPID             USER ID                               
         BAS   RE,AUTH                    GET USER ID                           
         MVC   LUSERID,PRSNLID            USER-ID                               
*                                                                               
         GOTO1 DATCON,DMCB,(8,DOPNDAT),(11,LDATE)                               
*                                                                               
         MVC   HALF,DOPNTIM               TIME                                  
         BAS   RE,DISTIME                 DISPLAY TIME                          
*                                                                               
         CLI   DOPNLEN,DOPNLNQ2           HAVE TICKET IN THE ELEMENT?           
         BL    LR05                       NO, LIST THE ELEMENT                  
         MVC   LTICKET,DOPNTIC            TICKET                                
         B     LR05                       LIST THE ELEMENT                      
         DROP  R6                         DROP R6                               
*                                                                               
         USING DOPSUPPD,R6                SUPP PATCH ELEM DSECT                 
LR25     MVC   LEVENT,=C'SUPP'            SUPP PATCH                            
         MVC   PASSWD,DOPPPID             USER ID                               
         BAS   RE,AUTH                    GET USER ID                           
         MVC   LUSERID,PRSNLID            USER-ID                               
*                                                                               
         GOTO1 DATCON,DMCB,(8,DOPPDAT),(11,LDATE)                               
*                                                                               
         MVC   HALF,DOPPTIM               TIME                                  
         BAS   RE,DISTIME                 DISPLAY TIME                          
*                                                                               
         CLI   DOPPLEN,DOPPLNQ2           HAVE TICKET IN THE ELEMENT?           
         BL    LR05                       NO, LIST THE ELEMENT                  
         MVC   LTICKET,DOPPTIC            TICKET                                
         B     LR05                       LIST THE ELEMENT                      
         DROP  R6                         DROP R6                               
*                                                                               
         USING DOPMKGDD,R6                MAKEGOOD ELEM DSECT                   
LR30     MVC   LEVENT,=C'MKGD'            MKGD PATCH                            
         MVC   LMKGD,DOPMMKG              MAKEGOOD                              
         MVC   PASSWD,DOPMPID             USER ID                               
         BAS   RE,AUTH                    GET USER ID                           
         MVC   LUSERID,PRSNLID            USER-ID                               
*                                                                               
         GOTO1 DATCON,DMCB,(8,DOPMDAT),(11,LDATE)                               
*                                                                               
         MVC   HALF,DOPMTIM               TIME                                  
         BAS   RE,DISTIME                 DISPLAY TIME                          
*                                                                               
         CLI   DOPMLEN,DOPMLNQ2           HAVE TICKET IN THE ELEMENT?           
         BL    LR05                       NO, LIST THE ELEMENT                  
         MVC   LTICKET,DOPMTIC            TICKET                                
         B     LR05                       LIST THE ELEMENT                      
         DROP  R6                         DROP R6                               
*                                                                               
LRX      B     XIT                        EXIT                                  
***********************************************************************         
* CONVERT THE TIME                                                    *         
***********************************************************************         
DISTIME  LR    R0,RE                      SAVE RE                               
         GOTO1 HEXOUT,DMCB,HALF,WORK,L'HALF                                     
         MVC   LTIME(2),WORK              HOUR                                  
         MVI   LTIME+2,C'.'               "."                                   
         MVC   LTIME+3(2),WORK+2          MINUTE                                
         LR    RE,R0                      RESTORE RE                            
         BR    RE                         RETURN TO CALLER                      
***********************************************************************         
* GET THE USER ID                                                     *         
***********************************************************************         
AUTH     NTR1                                                                   
*                                                                               
         CLC   SVPASSWD,PASSWD            PASSWORD IS THE SAME AS LAST?         
         BE    AUTHX                      YES - LEAVE PRSNLID AS IS             
*                                                                               
         MVC   SAVEKEY,KEY                SAVE OFF THE KEY                      
         MVC   AIO,AIO2                   USE AIO2                              
*                                                                               
         XC    PRSNLID,PRSNLID            CLEAR PERSONAL ID                     
         OC    PASSWD,PASSWD              HAVE CHANGED BY FIELD?                
         BZ    AUTHX                      NO - EXIT                             
*                                                                               
         XC    KEY,KEY                    CLEAR THE KEY                         
         LA    R6,KEY                     R6 = KEY                              
         USING CT0REC,R6                  USER ID RECORD DSECT                  
         MVI   CT0KTYP,CT0KTEQU           RECORD TYPE '0'                       
         MVC   CT0KAGY,AGENCY             AGENCY                                
         MVC   CT0KNUM,PASSWD             PERSON AUTH NUMBER                    
*                                                                               
         L     R6,AIO                     R6 = AIO2                             
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'CTFILE',KEY,(R6)                      
         CLC   CT0KEY,KEY                 FOUND OUR KEY?                        
         BNE   AUTHX                      NO - DONE                             
*                                                                               
         LA    RE,28(R6)                  A(FIRST ELEMENT)                      
         XR    R0,R0                      CLEAR R0                              
*                                                                               
AUTH10   CLI   0(RE),0                    END OF RECORD?                        
         BE    AUTHX                      YES - DONE                            
         CLC   =X'C30A',0(RE)             HAVE THE PERSON ELEMENT?              
         BE    AUTH20                     YES                                   
         IC    R0,1(RE)                   ELEMENT LENGTH                        
         AR    RE,R0                      BUMP TO NEXT ELEMENT                  
         B     AUTH10                     CHECK NEXT ELEMENT                    
*                                                                               
AUTH20   MVC   PRSNLID,2(RE)              PERSONAL ID                           
*                                                                               
AUTHX    MVC   AIO,AIO1                   RESTORE AIO                           
         MVC   KEY,SAVEKEY                RESTORE KEY                           
         MVC   SVPASSWD,PASSWD            SAVE PASSWORD                         
         B     XIT                        RETURN TO CALLER                      
         DROP  R6                                                               
***********************************************************************         
* GET THE PFKEY INFORMATION                                           *         
***********************************************************************         
GETPF    NTR1                                                                   
*                                                                               
         GOTO1 INITIAL,DMCB,PFTABLE       INITIALIZE THE PFKEYS                 
*                                                                               
         B     XIT                        RETURN                                
***********************************************************************         
* LIST PFKEY TABLE DEFINITIONS                                        *         
***********************************************************************         
PFRTNQ   EQU   12                         RETURN                                
*                                                                               
PFTABLE  DS    0C                                                               
* RETURN TO CALLER                                                              
         DC    AL1(PF12X-*,PFRTNQ,PFTRPROG,0,0,0)                               
         DC    CL3' ',CL8' ',CL8' '                                             
PF12X    EQU   *                                                                
         DC    X'FF'                                                            
*                                                                               
         LTORG                            LTORG                                 
         DROP  R7,RB                                                            
***********************************************************************         
* GENERAL ERROR MESSAGES                                                        
***********************************************************************         
INVLFLD  MVI   GERROR1,INVALID                                                  
         J     ERREXIT                                                          
***********************************************************************         
* ERROR MESSAGES (SYSTEM 23)                                                    
***********************************************************************         
ERRNOORD MVI   GERROR1,215         CANNOT FIND THAT ORDER                       
         J     ERREXIT                                                          
*                                                                               
INVLDPF  MVI   GERROR1,ERINVPFK    INVALID PF KEY                               
         J     ERREXIT                                                          
*                                                                               
ERREXIT  GOTO1 MYERR                                                            
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
***********************************************************************         
* LOCAL SAVED STORAGE                                                           
***********************************************************************         
LSSD     DSECT                                                                  
BASERC   DS    A                   BASE RC                                      
RELO     DS    A                   A(RELO)                                      
VGLOBBER DS    A                   A(GLOBBER)                                   
SVAELEM  DS    A                   A(NEXT X'05' ELEM)                           
SAVEKEY  DS    XL13                SAVED KEY                                    
SECRAGY  DS    XL2                 SECURITY AGENCY                              
PASSWD   DS    XL2                 AUTHORIZATION NUMBER                         
SVPASSWD DS    XL2                 SAVED AUTHORIZATION NUMBER                   
PRSNLID  DS    CL8                 PERSONAL ID                                  
*                                                                               
         ORG   LSSD+L'SYSSPARE                                                  
*                                                                               
       ++INCLUDE SPOMSFFD          (BASE SCREEN FOR SYSTEM)                     
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPOMSB5D          (OUR PHISTORY SCREEN)                        
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
         PRINT ON                                                               
* DDCOMFACSD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
* DDGLOBEQUS                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDGLOBEQUS                                                     
         PRINT ON                                                               
* DDSPLWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
* DDSPOOLD                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         PRINT ON                                                               
* FAFACTS                                                                       
         PRINT OFF                                                              
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
* FAGETTXTD                                                                     
         PRINT OFF                                                              
       ++INCLUDE FAGETTXTD                                                      
         PRINT ON                                                               
* FATIOB                                                                        
         PRINT OFF                                                              
       ++INCLUDE FATIOB                                                         
         PRINT ON                                                               
* SPOMSWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE SPOMSWORKD                                                     
         PRINT ON                                                               
* SPGENDRORD                                                                    
         PRINT OFF                                                              
       ++INCLUDE SPGENDRORD                                                     
         PRINT ON                                                               
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
LEVENT   DS    CL4                 EVENT                                        
         DS    CL3                                                              
LMKGD    DS    CL3                 MAKEGOOD CODE                                
         DS    CL3                                                              
LUSERID  DS    CL8                 USER-ID                                      
         DS    CL2                                                              
LDATE    DS    CL8                 DATE                                         
         DS    CL2                                                              
LTIME    DS    CL5                 TIME                                         
         DS    CL1                                                              
LTICKET  DS    CL8                 TICKET NUMBER                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002SPOMS15   08/27/13'                                      
         END                                                                    
