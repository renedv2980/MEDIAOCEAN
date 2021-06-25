*          DATA SET SRDTI00A   AT LEVEL 005 AS OF 05/08/98                      
*          DATA SET SRDTI00    AT LEVEL 013 AS OF 10/07/97                      
*                                                                               
*        test  version please do not use this for anything - aatk               
*        used  for testing shipit - routine for binary transfers                
*                                                                               
*                                                                               
*                                                                               
*                                                                               
*                                                                               
*                                                                               
*&&      SET   NOP=N                                                            
*PHASE T16900A                                                                  
         TITLE '$DTI - DATASPACE TASK INFORMATION'                              
DTI      CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WRKL,**$DTI**,CLEAR=YES                                          
         USING WRKD,RC             RC=A(W/S)                                    
*                                                                               
         ST    R1,CALLR1                                                        
         USING SRPARMD,R1          R1=A(PARMS)                                  
         L     RA,SRPARM6                                                       
         USING SRDTIFFD,RA         RA=A(TWA)                                    
         L     R9,SRPARM1                                                       
         USING SYSFACD,R9          R9=A(SYSFACS)                                
*                                                                               
         L     R2,SRPARM4                                                       
         USING COMFACSD,R2         R2=A(COMFACS)                                
         MVC   VHEXOUT,CHEXOUT     SAVE USEFUL ENTRY POINTS                     
         DROP  R2,R1               FINISHED WITH COMFACS                        
*                                                                               
HDR      USING FHD,SRVMSGH                                                      
*                                                                               
         TIME  TU                                                               
         ST    R0,TIMENOW          TIME IN MVS TU'S (1/38400 SEC)               
*                                                                               
         BAS   RE,INIT             INITIALISE                                   
*                                                                               
         BAS   RE,INPUT            VALIDATE INPUT PARAMETERS                    
         BNE   EXIT                                                             
*                                                                               
         BAS   RE,DISP             DISPLAY TASK INFORMATION                     
*                                                                               
         MVI   SRVMSG,C'('         SET MESSAGE                                  
         MVC   SRVMSG+1(L'SYSNAME),SYSNAME                                      
         LA    RF,SRVMSG+L'SYSNAME+1                                            
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVI   1(RF),C')'                                                       
         LA    RF,3(RF)                                                         
         MVC   0(L'MSG1,RF),MSG1                                                
         OI    HDR.FHOI,FHOITR     TRANSMIT IT                                  
*                                                                               
         L     R3,CALLR1                                                        
         USING SRPARMD,R3          R1=A(PARMS)                                  
         XC    WORK,WORK                                                        
CCC      GOTO1 VSHIPIT,DMCB,SRPARM6,SRPARM8,WORK                                
         BE    CCC                                                              
         DROP  R3                                                               
*                                                                               
EXIT     XMOD1 ,                                                                
         EJECT                                                                  
***********************************************************************         
* INITIALISE ALL VALUES                                               *         
***********************************************************************         
         SPACE 1                                                                
INIT     NTR1  ,                                                                
         L     R2,VSSB                                                          
         USING SSBD,R2                                                          
         MVC   SYSNAME,SSBSYSN4    GET SYSTEM NAME                              
INITX    XIT1  ,                                                                
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE INPUT PARAMETERS                                           *         
***********************************************************************         
         SPACE 1                                                                
INPUT    NTR1  ,                                                                
         LA    R2,SRVP1H           P1 = FACPAK NUMBER                           
         USING FHD,R2                                                           
         CLI   FHIL,0              INPUT?                                       
         BNE   INP02               YES                                          
         MVC   FACID,=H'1'         SET DEFAULT TO TOR FACPAK                    
         MVI   FHDA,C'1'                                                        
         OI    FHOI,(FHOITR+FHOIMO)                                             
         LA    RF,1                                                             
         B     INP06                                                            
*                                                                               
INP02    TM    FHII,FHIINU         VALID NUMERIC FIELD?                         
         BO    INP04               YES                                          
         MVC   SRVMSG,ERR1         MUST BE NUMERIC                              
         OI    HDR.FHOI,FHOITR                                                  
         OI    FHOI,FHOICU                                                      
         B     INPNE                                                            
*                                                                               
INP04    XR    RF,RF                                                            
         IC    RF,FHIL                                                          
         BCTR  RF,0                                                             
         EX    RF,*+8              PACK NUMERIC INPUT                           
         B     *+10                                                             
         PACK  DUB,FHDA(0)                                                      
*                                                                               
         CVB   RF,DUB                                                           
         CH    RF,=H'1'                                                         
         BL    *+12                                                             
         CH    RF,=Y(SBFACMAX)                                                  
         BNH   INP06                                                            
         MVC   SRVMSG,ERR1         MUST BE NUMERIC                              
         OI    HDR.FHOI,FHOITR                                                  
         OI    FHOI,FHOICU                                                      
         B     INPNE                                                            
*                                                                               
INP06    STH   RF,FACID            SET FACPAK ID                                
*                                                                               
         L     RE,VSSB             GET V(SSB)                                   
         USING SSBD,RE                                                          
         LAM   R1,R1,SSBALET       GET ALET FOR DMGR DATASPACE                  
         L     R1,SSBATOR          GET DISPLACEMENT TO TOR                      
         LA    R1,TORFACLQ(R1)                                                  
         SAC   512                                                              
         LH    R4,0(,R1)                                                        
         L     R5,2(,R1)                                                        
         LA    R1,6(,R1)                                                        
         USING SBEXCHD,R1                                                       
         CLC   SBFACID,FACID       REQUESTED FACPAK ID?                         
         BE    INP10                                                            
         BXLE  R1,R4,*-10                                                       
         SAC   0                                                                
         LAM   R0,RF,ARZERO                                                     
         MVC   SRVMSG,ERR2         FACPAK ID NOT FOUND                          
         OI    HDR.FHOI,FHOITR                                                  
         OI    FHOI,FHOICU                                                      
         B     INPNE                                                            
*                                                                               
INP10    OC    SBSTOKEN,SBSTOKEN   FACPAK IS INITIALISED                        
         BNZ   INP12               YES                                          
         SAC   0                                                                
         LAM   R0,RF,ARZERO                                                     
         MVC   SRVMSG,ERR3         FACPAK IS NOT INITIALISED                    
         OI    HDR.FHOI,FHOITR                                                  
         OI    FHOI,FHOICU                                                      
         B     INPNE                                                            
*                                                                               
INP12    LA    R0,SBMXTSK          R0 = MAX ALLOWED TASKS                       
         XR    RF,RF               RF = COUNT OF TASKS IN FACPAK                
         LA    R1,SBTSKBLK                                                      
         USING EXCHNGD,R1                                                       
*                                                                               
INP14    OC    EXCIDENT,EXCIDENT   TASK IDENTIFIER?                             
         BZ    INP16                                                            
         LA    RF,1(RF)                                                         
         LA    R1,EXCHNGLQ(,R1)                                                 
         BCT   R0,INP14                                                         
         DROP  R1                                                               
*                                                                               
INP16    SAC   0                                                                
         LAM   R0,RF,ARZERO                                                     
         LTR   RF,RF               ANY ACTIVE TASKS IN THIS FACPAK              
         BNZ   INP18                                                            
*                                                                               
         MVC   SRVMSG,ERR4         FACPAK HAS NO TASKS                          
         OI    HDR.FHOI,FHOITR                                                  
         OI    FHOI,FHOICU                                                      
         B     INPNE                                                            
*                                                                               
INP18    STH   RF,TSKMAX           SET MAX TASKS FOR THIS FACPAK                
*                                                                               
         LA    R2,SRVP2H           P2=START TASK NUMBER                         
         CLI   FHIL,0              INPUT?                                       
         BNE   INP20               YES                                          
         MVC   STRTTSK,=H'1'       SET DEFAULT TO FIRST TASK                    
         MVI   FHDA,C'1'                                                        
         OI    FHOI,(FHOITR+FHOIMO)                                             
         LA    R2,SRVP3H                                                        
         OI    FHOI,FHOICU                                                      
         B     INPOK                                                            
*                                                                               
INP20    TM    FHII,FHIINU         VALID NUMERIC FIELD?                         
         BO    INP22               YES                                          
         MVC   SRVMSG,ERR5         MUST BE NUMERIC IN RANGE 1 - ??              
         LH    RF,TSKMAX                                                        
         CVD   RF,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  SRVMSG+ERR5NUM(2),DUB                                            
         OI    HDR.FHOI,FHOITR                                                  
         OI    FHOI,(FHOITR+FHOICU+FHOIMO)                                      
         B     INPNE                                                            
*                                                                               
INP22    XR    RF,RF                                                            
         IC    RF,FHIL                                                          
         BCTR  RF,0                                                             
         EX    RF,*+8              PACK NUMERIC INPUT                           
         B     *+10                                                             
         PACK  DUB,FHDA(0)                                                      
*                                                                               
         CVB   RF,DUB                                                           
         CH    RF,=H'1'                                                         
         BL    *+12                                                             
         CH    RF,TSKMAX                                                        
         BNH   INP24                                                            
         MVC   SRVMSG,ERR5         MUST BE NUMERIC IN RANGE 1 - ??              
         LH    RF,TSKMAX                                                        
         CVD   RF,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  SRVMSG+ERR5NUM(2),DUB                                            
         OI    HDR.FHOI,FHOITR                                                  
         OI    FHOI,(FHOITR+FHOICU+FHOIMO)                                      
         B     INPNE                                                            
*                                                                               
INP24    STH   RF,STRTTSK          SET START TASK COUNT                         
         OI    FHOI,(FHOITR+FHOIMO)                                             
*                                                                               
         LA    R2,SRVP3H                                                        
         OI    FHOI,FHOICU                                                      
         B     INPOK                                                            
*                                                                               
INPNE    CLI   *,FF                                                             
         B     INPUTX                                                           
*                                                                               
INPOK    CR    RB,RB                                                            
         B     INPUTX                                                           
*                                                                               
INPUTX   XIT1  ,                                                                
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* DISPLAY TASK INFORMATION FROM DATASPACE                             *         
***********************************************************************         
         SPACE 1                                                                
DISP     NTR1  ,                                                                
         L     RE,VSSB             GET V(SSB)                                   
         USING SSBD,RE                                                          
         LAM   R2,R2,SSBALET       GET ALET FOR DMGR DATASPACE                  
         L     R2,SSBATOR          GET DISPLACEMENT TO TOR                      
         LA    R2,TORFACLQ(R2)                                                  
         SAC   512                                                              
         LH    R4,0(,R2)                                                        
         L     R5,2(,R2)                                                        
         LA    R2,6(,R2)                                                        
         USING SBEXCHD,R2                                                       
         CLC   SBFACID,FACID       REQUESTED FACPAK ID?                         
         BE    DISP02                                                           
         BXLE  R2,R4,*-10                                                       
         B     DISPX                                                            
*                                                                               
DISP02   LA    R4,SRVL1H           FIRST DISPLAY LINE                           
         USING SCDSECT,R4                                                       
         USING FHD,SCDHDR                                                       
         LA    R6,SCDLENQ                                                       
         LA    R7,SRVLAST-1                                                     
*                                                                               
         CPYA  R3,R2                                                            
         LA    R3,SBTSKBLK                                                      
         USING EXCHNGD,R3                                                       
*                                                                               
         LH    R0,STRTTSK          GET START TASK                               
         SH    R0,=H'1'            ZERO BASE IT                                 
         BNP   DISP04                                                           
         LA    R3,EXCHNGLQ(,R3)                                                 
         BCT   R0,*-4                                                           
*                                                                               
DISP04   XC    SCDLINE,SCDLINE     CLEAR AND TRANSMIT LINE                      
         OI    FHOI,FHOITR                                                      
*                                                                               
         OC    EXCIDENT,EXCIDENT   TASK INFORMATION?                            
         BZ    DISP18              NO                                           
         MVI   SCDTID,C'T'                                                      
         MVC   SCDTID+1(1),EXCIDENT+6                                           
*                                                                               
         CLI   EXCFLAG,EXCFREE     TASK IS BUSY?                                
         BE    DISP18              NO                                           
*                                                                               
         MVC   SCDTSTAT,EXCFLAG    SET TASK STATUS                              
*                                                                               
         ICM   R0,15,EXCTIME       GET TASK ARRIVAL TIME (TU)                   
         BZ    DISP06                                                           
         L     R1,TIMENOW          GET CURRENT TIME (TU)                        
         SLR   R1,R0                                                            
         M     R0,=F'1000'                                                      
         D     R0,=F'38400'        SET ELAPSED TIME IN MILLISECONDS             
*                                                                               
         CVD   R1,DUB              EDIT OUT ELAPSED TIME                        
         MVC   WORK(11),=XL11'402020202021204B202020'                           
         ED    WORK(11),DUB+3                                                   
         MVC   SCDELAPS,WORK+11-L'SCDELAPS                                      
*                                                                               
DISP06   XR    R0,R0               GET UTL NUMBER                               
         ICM   R0,3,EXCUTL                                                      
         BZ    DISP18                                                           
*                                                                               
         C     R0,=A(UTLZERO)      SPECIAL UTL NUMBERS                          
         BNE   *+14                                                             
         MVC   SCDUTL,DUTLZERO                                                  
         B     DISP07                                                           
*                                                                               
         C     R0,=A(UTLZERA)                                                   
         BNE   DISP07A                                                          
         MVC   SCDUTL,DUTLZERA                                                  
*                                                                               
DISP07   L     R1,VTCB                                                          
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
         USING TCBD,R1                                                          
*                                                                               
         CLC   TCBID,EXCIDENT                                                   
         BE    *+12                                                             
         BXLE  R1,RE,*-10                                                       
         B     DISP18                                                           
*                                                                               
         ICM   R5,15,TCBUTL        GET V(UTL) FROM TCB                          
         B     DISP10                                                           
         DROP  R1                                                               
*                                                                               
DISP07A  CVD   R0,DUB              OUTPUT UTL NUMBER                            
         UNPK  SCDUTL,DUB                                                       
         OI    SCDUTL+L'SCDUTL-1,C'0'                                           
*                                                                               
DISP08   L     RE,VSSB                                                          
         LAM   R5,R5,SSBALET       GET A(UTL BLOCK IN TOR)                      
         ICM   R5,15,SSBATOR                                                    
         ICM   R5,15,TORUTL-TORFACD(R5)                                         
         LAM   R5,R5,SSBTLET       R5 = TOR FACPAK ALET (OR 0)                  
*                                                                               
         XR    RF,RF                                                            
         ICM   RF,3,EXCUTL                                                      
         BZ    DISP18                                                           
         BCTR  RF,0                                                             
         MH    RF,0(,R5)                                                        
         LA    R5,6(RF,R5)                                                      
         USING UTLD,R5                                                          
*                                                                               
         SAC   0                                                                
         ST    R5,FULL                                                          
         GOTO1 VHEXOUT,DMCB,FULL,SCDAUTL,L'FULL,0                               
         SAC   512                                                              
*                                                                               
         MVC   SCDLUID,TLUID       SET LUID OF TERMINAL                         
*                                                                               
DISP10   TM    TSTAT1,TSTATDDS     DDS TERMINAL                                 
         BZ    *+8                                                              
         MVI   SCDDDS,C'*'                                                      
*                                                                               
         OC    TSVCREQ,TSVCREQ     RUNNING SERVICE REQUEST?                     
         BNZ   DISP12              YES                                          
*                                                                               
         XR    RF,RF               EDIT OUT SYS/PROG                            
         ICM   RF,7,TARSYS                                                      
         BZ    DISP18                                                           
*                                                                               
         MVC   SCDSYSPR(L'SENAME),SENAME-SELISTD(RF)                            
         LA    RE,SCDSYSPR+L'SENAME                                             
         CLI   0(RE),C' '                                                       
         BH    *+8                                                              
         BCT   RE,*-8                                                           
         MVI   1(RE),C'/'                                                       
         LA    RE,2(RE)                                                         
*                                                                               
         ICM   R0,15,SEPGMS-SELISTD(RF)                                         
         XR    RF,RF                                                            
         ICM   RF,7,TAPRG                                                       
         BZ    DISP18                                                           
         AR    RF,R0                                                            
*                                                                               
         LA    R1,SCDSYSPR+L'SCDSYSPR                                           
         SR    R1,RE                                                            
         BNP   DISP18                                                           
         CH    R1,=Y(L'PGMNAME)                                                 
         BNH   *+8                                                              
         LA    R1,L'PGMNAME                                                     
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   0(0,RE),PGMNAME-PGMLSTD(RF)                                      
*                                                                               
         CLC   TASYS,TARSYS        SWITCHED INTO ANOTHER SYSTEM?                
         BE    DISP18              NO                                           
*                                                                               
         XR    RF,RF               EDIT OUT SWITCHED SYSTEM                     
         ICM   RF,7,TASYS                                                       
         BZ    DISP18                                                           
         MVC   SCDSWCH(L'SENAME),SENAME-SELISTD(RF)                             
         B     DISP18                                                           
*                                                                               
DISP12   L     R1,VSELIST          SERVICE REQUESTS HERE                        
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
         USING SELISTD,R1                                                       
         CLI   SESYS,1                                                          
         BE    DISP14                                                           
         BXLE  R1,RE,*-8                                                        
         DC    H'0'                                                             
*                                                                               
DISP14   MVC   SCDSYSPR(L'SENAME),SENAME                                        
         LA    RE,SCDSYSPR+L'SENAME                                             
         CLI   0(RE),C' '                                                       
         BH    *+8                                                              
         BCT   RE,*-8                                                           
         MVI   1(RE),C'/'                                                       
         LA    RE,2(RE)                                                         
         LR    R0,RE               PRESERVE RE                                  
*                                                                               
         ICM   R1,15,SEPGMS                                                     
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
         USING PGMLSTD,R1                                                       
         CLC   PGMNUM,TSVCREQ+1                                                 
         BE    DISP16                                                           
         BXLE  R1,RE,*-10                                                       
*                                                                               
         LA    R1,XTRATAB                                                       
         LA    RF,XTRATABX-1                                                    
         LA    RE,L'XTRATAB                                                     
         LA    R1,XTRATAB                                                       
         CLC   TSVCREQ+1(1),1(R1)  LOOK FOR MATCH IN EXTRA PROGRAM LIST         
         BE    DISP14A                                                          
         BXLE  R1,RE,*-10                                                       
         B     DISP15                                                           
*                                                                               
DISP14A  LR    RE,R0               RESTORE RE                                   
         LA    RF,SCDSYSPR+L'SCDSYSPR-1                                         
         SR    RF,RE                                                            
         BM    DISP18                                                           
         EX    RF,*+4                                                           
         MVC   0(0,RE),2(R1)                                                    
         B     DISP18                                                           
*                                                                               
DISP15   XR    RF,RF                                                            
         IC    RF,TSVCREQ+1                                                     
         CVD   RF,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         LR    RE,R0                                                            
         UNPK  0(3,RE),DUB                                                      
         B     DISP18                                                           
*                                                                               
DISP16   LR    RE,R0               RESTORE RE                                   
         LA    RF,SCDSYSPR+L'SCDSYSPR-1                                         
         SR    RF,RE                                                            
         BM    DISP18                                                           
         EX    RF,*+4                                                           
         MVC   0(0,RE),PGMNAME                                                  
         DROP  R1                                                               
*                                                                               
DISP18   LA    R3,EXCHNGLQ(,R3)                                                 
         BXLE  R4,R6,DISP04                                                     
*                                                                               
DISPX    SAC   0                                                                
         LAM   R0,RF,ARZERO                                                     
         XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* LITERALS AND CONSTANTS                                              *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
*                                                                               
FF       EQU   X'FF'                                                            
ARZERO   DC    16F'0'                                                           
UTLZERO  EQU   X'FFFF'                                                          
UTLZERA  EQU   X'FFFE'                                                          
DUTLZERO DC    CL(L'SCDUTL)'UZERO'                                              
DUTLZERA DC    CL(L'SCDUTL)'UZERA'                                              
*                                                                               
MSG1     DC    CL50'Dataspace information displayed'                            
*                                                                               
ERR1     DC    CL60'Facpak ID must be numeric in the range 1 - 32'              
ERR2     DC    CL60'Facpak ID number cannot be found within dataspace'          
ERR3     DC    CL60'Facpak is not initialised'                                  
ERR4     DC    CL60'Facpak has no tasks'                                        
ERR5     DS    0CL60                                                            
         DC    C'Start Task must be numeric in the range 1 - '                  
ERR5NUM  EQU   *-ERR5                                                           
         ORG   ERR5+L'ERR5                                                      
*                                                                               
XTRATAB  DS    0XL10                                                            
         DC    X'0102',CL8'$TOP(A)'                                             
         DC    X'0103',CL8'$SYSNOP'                                             
         DC    X'0104',CL8'$T2'                                                 
         DC    X'0105',CL8'$TOP'                                                
         DC    X'0107',CL8'$SUB'                                                
         DC    X'0108',CL8'$SRGRF'                                              
         DC    X'0109',CL8'$HELP'                                               
         DC    X'010A',CL8'$EXT'                                                
         DC    X'010C',CL8'$VTL'                                                
         DC    X'010D',CL8'$WKR'                                                
         DC    X'010F',CL8'$OPS'                                                
         DC    X'0112',CL8'$DONE'                                               
         DC    X'0113',CL8'$EOJ'                                                
         DC    X'0117',CL8'$DISP'                                               
         DC    X'0118',CL8'$LOAD'                                               
         DC    X'011F',CL8'$SCR'                                                
         DC    X'01FC',CL8'$CAN'                                                
         DC    X'01FD',CL8'$PSWD'                                               
         DC    X'01FE',CL8'$ERR'                                                
         DC    X'0120',CL8'$RE'                                                 
         DC    X'012A',CL8'$SEARCH'                                             
         DC    X'012D',CL8'$TIMEOUT'                                            
         DC    X'0136',CL8'$TEST'                                               
         DC    X'0144',CL8'$BC'                                                 
         DC    X'0147',CL8'$KWX'                                                
         DC    X'0156',CL8'$CC'                                                 
         DC    X'015D',CL8'$DUMP'                                               
         DC    X'0160',CL8'$CFI'                                                
         DC    X'0165',CL8'$INDFILE'                                            
         DC    X'01EE',CL8'$BYE'                                                
         DC    X'01FB',CL8'$UNWIND'                                             
         DC    X'01FF',CL8'$ABEND'                                              
*&&US                                                                           
         DC    X'0161',CL8'SPTDARE'                                             
         DC    X'0162',CL8'REPDARE'                                             
         DC    X'0163',CL8'MKGDARE'                                             
*&&                                                                             
*                                                                               
XTRATABX DS    XL2'0000'                                                        
         EJECT                                                                  
***********************************************************************         
* WORKING STORAGE DSECT                                               *         
***********************************************************************         
         SPACE 1                                                                
WRKD     DSECT                                                                  
DUB      DS    D                                                                
CALLR1   DS    A                                                                
FULL     DS    F                                                                
DMCB     DS    6F                                                               
FACID    DS    H                                                                
FACMAX   DS    H                                                                
STRTTSK  DS    H                                                                
TSKMAX   DS    H                                                                
VHEXOUT  DS    A                                                                
TIMENOW  DS    F                                                                
SYSNAME  DS    CL(L'SSBSYSNA)                                                   
WORK     DS    CL20                                                             
WRKL     EQU   *-WRKD              SHOULD BE LESS THAN 256                      
         SPACE 2                                                                
***********************************************************************         
* DSECT TO COVER SCREEN DISPLAY LINE                                  *         
***********************************************************************         
         SPACE 1                                                                
SCDSECT  DSECT                     DEFINE SCREEN LINE                           
SCDHDR   DS    CL8                 FIELD HEADER                                 
SCDLINE  DS    0CL78                                                            
SCDTID   DS    CL2                 TASK ID 'TN'                                 
         DS    C                                                                
SCDTSTAT DS    CL1                 TASK STATUS                                  
         DS    C                                                                
SCDELAPS DS    CL7                 ELAPSED TIME                                 
         DS    C                                                                
SCDUTL   DS    XL5                 UTL NUMBER                                   
         DS    C                                                                
SCDAUTL  DS    CL8                 A(UTL) IN TOR                                
SCDDDS   DS    C                   '*' IF DDS TERMINAL                          
SCDLUID  DS    CL8                 TERMINAL ID                                  
         DS    C                                                                
SCDSYSPR DS    CL15                SYS/PROG                                     
         DS    C                                                                
SCDSWCH  DS    CL7                 SWITCHED SYSTEM                              
         ORG   SCDLINE+L'SCDLINE                                                
SCDLENQ  EQU   *-SCDSECT           SHOULD BE 78 BYTES                           
         EJECT                                                                  
***********************************************************************         
* SCREEN DSECT                                                        *         
***********************************************************************         
         SPACE 1                                                                
SRDTIFFD DSECT                                                                  
         DS    CL64                                                             
* SRDTIFFD                                                                      
       ++INCLUDE SRDTIFFD                                                       
         EJECT                                                                  
***********************************************************************         
* OTHER INCLUDED DSECTS                                               *         
***********************************************************************         
         SPACE 1                                                                
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
* FAPGMLST                                                                      
         PRINT OFF                                                              
       ++INCLUDE FAPGMLST                                                       
         PRINT ON                                                               
* FASRPARM                                                                      
         PRINT OFF                                                              
       ++INCLUDE FASRPARM                                                       
         PRINT ON                                                               
* FASELIST                                                                      
         PRINT OFF                                                              
       ++INCLUDE FASELIST                                                       
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
* FAPIGFACD                                                                     
         PRINT OFF                                                              
       ++INCLUDE FAPIGFACD                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005SRDTI00A  05/08/98'                                      
         END                                                                    
