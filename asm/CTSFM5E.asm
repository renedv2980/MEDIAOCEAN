*          DATA SET CTSFM5E    AT LEVEL 125 AS OF 06/03/20                      
*PHASE TA0A5EA                                                                  
*INCLUDE FAXPTAB                                                                
*INCLUDE RECUP                                                                  
         TITLE 'TA0A5E  SDR VALUES MAINTENANCE'                                 
***********************************************************************         
*                                                                     *         
*  TITLE        TA0A5E - STATION RATING SOURCE RECORD MAINT/LIST      *         
*                                                                     *         
*  CALLED FROM  GENCON VIA TA0A00 (SFM CTFILE CONTROLLER)             *         
*                                                                     *         
*  COMMENTS     SUPPORTS ADD, DISPLAY, CHANGE, LIST                   *         
*                                                                     *         
*  INPUTS       SCREEN TA0A9B (MAINTENANCE)                           *         
*               SCREEN TA0A9E (LIST)                                  *         
*                                                                     *         
*  OUTPUTS      SDR VALUES                                                      
*                                                                     *         
*  LOCALS: REGISTER USAGE                                             *         
*          R0 - WORK                                                  *         
*          R1 - WORK                                                  *         
*          R2 - POINTER TO SCREEN FLDH, ERREX WILL POSITION CURSOR    *         
*          R3 - WORK                                                  *         
*          R4 - WORK                                                  *         
*          R5 - WORK                                                  *         
*          R6 - GETEL REGISTER/WORK                                   *         
*          R7 - WORK                                                  *         
*          R8 - SPOOLD/WORK                                           *         
*          R9 - SYSD                                                  *         
*          RA - TWA                                                   *         
*          RB - FIRST BASE                                            *         
*          RC - GEND                                                  *         
*          RD - SYSTEM                                                *         
*          RE - SYSTEM/WORK                                           *         
*          RF - SYSTEM/WORK                                           *         
*                                                                     *         
***********************************************************************         
TA0A5E   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,TA0A5E,R7,RR=R3                                                
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R3,RELO                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     RE,=V(FAXPTPC)      A(PC PROGRAM TBL) - SEE FAXPTAB              
         A     RE,RELO                                                          
         ST    RE,VFXPCTBL                                                      
         L     RE,=V(RECUP)        A(RECUP)                                     
         A     RE,RELO                                                          
         ST    RE,VRECUP                                                        
*                                                                               
*   STAY ON SAME PAGE OF LIST + RETURN SAME SELECTION NEXT TIME                 
         MVI   ACTELOPT,C'N'                                                    
         OI    GENSTAT2,DISTHSPG+RETEQSEL                                       
         OI    GENSTAT4,NODELLST   DELETE FROM LIST NOT ALLOWED                 
         OI    GENSTAT5,NODLST     NO DELETING FROM THE LIST SCREEN             
*                                                                               
         CLI   PFKEY,12            IF PFKEY 12 HIT                              
         JE    CKMDPF12                                                         
*                                                                               
         CLI   MODE,PROCPFK        PFKEY PRESSED DURING SELECT                  
         JNE   CKMODE10                                                         
         CLI   PFKEY,8             USER WANTS TO PAGE DOWN?                     
         JNE   CKMODE10            NO                                           
CKMODE05 OC    SVSDRKEY,SVSDRKEY                                                
         JZ    CKMODE10                                                         
         XC    KEY,KEY                                                          
         MVC   KEY(L'SDRKEY),SVSDRKEY                                           
         GOTO1 HIGH                                                             
         GOTO1 GETREC                                                           
         BRAS  RE,DR               DISPLAY RECORD                               
         B     CKMODEX                                                          
*                                                                               
CKMODE10 DS    0H                                                               
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BNE   *+12                                                             
         BRAS  RE,VK                                                            
         B     CKMODEX                                                          
*                                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BNE   *+12                                                             
         BRAS  RE,VR                                                            
         B     CKMODE90                                                         
*                                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BNE   *+12                                                             
         BRAS  RE,DK                                                            
         B     CKMODEX                                                          
*                                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         JNE   CKMODE20                                                         
         CLI   PFKEY,8             DID WE ALREADY PROCESSED PAGE DOWN?          
         JE    CKMODE90            YES, WE'RE DONE                              
         BRAS  RE,DR                                                            
         B     CKMODE90                                                         
*                                                                               
CKMODE20 CLI   MODE,LISTRECS       LIST RECORDS                                 
         BNE   *+12                                                             
         BRAS  RE,LR                                                            
         B     CKMODEX                                                          
*                                                                               
         CLI   MODE,RECDEL                                                      
         BNE   CKMODEX                                                          
*                                                                               
         LA    R2,CONACTH           NO DELETES                                  
         MVI   ERROR,INVRCACT                                                   
         GOTOR ERREX                                                            
*                                                                               
CKMODE90 DS    0H                                                               
*                                                                               
         CLI   ACTNUM,ACTSEL        SKIP IF NOT SELECT FROM LIST                
         BNE   CKMODEX                                                          
*                                                                               
         CLI   PFKEY,0             IF NO PF KEY HIT                             
         BNZ   CKMODE95                                                         
*&&DO                                                                           
         BRAS  RE,TSTNTRY             TEST IF ANY FIELD ENTERED                 
         BZ    *+12                   NO                                        
         OI    GENSTAT2,RETEQSEL      YES RETURN TO THIS SCREEN                 
         B     CKMODE95                                                         
*                                                                               
**       OI    GENSTAT2,NEXTSEL          GO TO NEXT SELECT                      
*&&                                                                             
CKMODE95 DS    0H                                                               
*                                                                               
CKMODEX  DS    0H                  straight to exit                             
*                                                                               
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
EXIT     XIT1                                                                   
*                                                                               
CKMDPF12 CLI   ACTNUM,ACTSEL        SKIP IF NOT SELECT FROM LIST                
         BNE   CKPF12_50                                                        
*                                                                               
         OC    SDMMOD,SDMMOD        PF12 IN A CHG/INS MODE?                     
         JZ    CKPF12_10              NOT IN A MODE                             
         XC    SDMMOD,SDMMOD        WAS IN A MODE, BUT CLEAR IT NOW             
         OI    SDMMODH+6,X'80'                                                  
         J     CKMODE05                                                         
*                                                                               
CKPF12_10 DS   0H                                                               
         OI    GENSTAT2,NEXTSEL          GO TO NEXT SELECT                      
         NI    GENSTAT2,X'FF'-RETEQSEL   GO BACK TO LIST SCREEN                 
         B     CKMODEX                                                          
*                                                                               
CKPF12_50 DS   0H                                                               
         CLI   ACTNUM,ACTLIST       ARE WE ON THE LIST SCREEN?                  
         JE    CKMODE10             YES, PROCESS AS NORMAL                      
         OC    SDMMOD,SDMMOD        ARE WE IN A CHG/INS MODE?                   
         JZ    CKMODE10             NO, PROCESS AS NORMAL                       
         XC    SDMMOD,SDMMOD        YES, CLEAR THE MODE SO WE CAN               
         OI    SDMMODH+6,X'80'        REDISPLAY                                 
         J     CKMODE10                                                         
*                                                                               
RELO     DS    A                                                                
***********************************************************************         
*                                                                     *         
*     VALIDATE KEY ROUTINE                                            *         
*                                                                     *         
***********************************************************************         
VK       NTR1                                                                   
         XC    SVSDRKEY,SVSDRKEY                                                
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING SDRKEY,R6                                                        
         MVI   SDRKMAJ,SDRKMAJQ    SET RECORD MAJOR TYPE                        
         MVI   SDRKMIN,SDRKMINQ    SET RECORD SECONDARY TYPE                    
*********                                                                       
* VALIDATE SYSTEM                                                               
*********                                                                       
VKFPK    MVI   SVFPKHEX,0                                                       
         LA    R2,SDMFPKH          POINT TO FACPAK KEY FIELD                    
         CLI   5(R2),0             ANY INPUT?                                   
         JNE   VKFPK10             YES                                          
*                                                                               
         CLI   ACTNUM,ACTLIST      NONE, OK IF LIST                             
         JE    VKFPKX                                                           
         B     VKMISS              MISSING INPUT ERROR                          
*                                                                               
VKFPK10  GOTO1 VALFPK,DMCB,8(R2),SVFPKHEX                                       
         JNE   VKINV                                                            
         MVC   SDRKFFL,SVFPKHEX    ADD FACPAK FLAG TO KEY                       
VKFPKX   DS    0H                                                               
*********                                                                       
* VALIDATE SYSTEM                                                               
*********                                                                       
VKSYS    MVI   SVSYSHEX,0                                                       
         LA    R2,SDMSYSH          POINT TO SYSTEM KEY FIELD                    
         CLI   5(R2),0             ANY INPUT?                                   
         JNE   VKSYS10             YES                                          
*                                                                               
         CLI   ACTNUM,ACTLIST      NONE, OK IF LIST                             
         JE    VKSYSX                                                           
         B     VKMISS              MISSING INPUT ERROR                          
*                                                                               
VKSYS10  GOTO1 VALSYS,DMCB,8(R2),SVSYSHEX                                       
         JNE   VKINV                                                            
         MVC   SDRKSYS,SVSYSHEX    ADD SYSTEM FLAG TO KEY                       
VKSYSX   DS    0H                                                               
*********                                                                       
* VALIDATE PROGRAM                                                              
*********                                                                       
VKPRG    XC    SVPRGHEX,SVPRGHEX                                                
         LA    R2,SDMPRGH          POINT TO PROGRAM KEY FIELD                   
         CLI   5(R2),0             ANY INPUT?                                   
         JNE   VKPRG10             YES                                          
*                                                                               
         CLI   ACTNUM,ACTLIST      NONE, OK IF LIST                             
         JE    VKPRG20                                                          
         B     VKMISS              MISSING INPUT ERROR                          
*                                                                               
VKPRG10  GOTO1 VALXPN,DMCB,8(R2),SVPRGHEX                                       
         JNE   VKINV                                                            
         MVC   SDRKAPP,SVPRGHEX    ADD PROGRAM TO KEY                           
*                                                                               
VKPRG20  CLC   =C'=X',8(R2)                                                     
         JNE   VKPRGX                                                           
         GOTO1 OUTXPN,DMCB,SVPRGHEX,8(R2)                                       
         OI    6(R2),X'80'         TRANSMIT THIS FIELD TO REDISPLAY             
*                                                                               
VKPRGX   DS    0H                                                               
*********                                                                       
* VALIDATE FREEFORM KEY                                                         
*********                                                                       
VKFFK    XC    SVFFKHEX,SVFFKHEX                                                
         CLI   ACTNUM,ACTLIST      LIST ACTION?                                 
         JE    VKFFKX              YES, NO FREEFORM KEY FIELD                   
*                                                                               
         LA    R2,SDMFFKH          POINT TO FREEFORM KEY FIELD                  
         CLI   5(R2),0             ANY INPUT?                                   
         JE    VKFFKNNE            NONE                                         
*                                                                               
         LLC   R0,5(R2)                                                         
         ST    R0,DMCB+8           SET LENGTH OF INPUT                          
         GOTO1 HEXIN,DMCB,8(R2),SVFFKHEX,,0                                     
         OC    12(4,R1),12(R1)     P4 IS ZERO IF SOURCE IS INVALID              
         JZ    VKINV                                                            
*                                                                               
         OC    SVFFKHEX,SVFFKHEX   IF NULLS, ALSO CLEAR CHAR VERSION            
         JZ    VKFFKNNE                                                         
         MVC   SDRKKEY,SVFFKHEX    ADD FREEFORM TO KEY                          
         GOTO1 OUTFFK,DMCB,SVFFKHEX,SDMFFC  OTHERWISE SHOW CHAR VERSION         
         J     VKFFKXMT                                                         
*                                                                               
VKFFKNNE XC    SDMFFC,SDMFFC       NO INPUT, SO CLEAR CHAR VERSION TOO          
VKFFKXMT OI    SDMFFCH+6,X'80'     TRANSMIT THE FIELD                           
*                                                                               
VKFFKX   DS    0H                                                               
*********                                                                       
* VALIDATE SEQUENCE NUMBER                                                      
*********                                                                       
VKSQN    XC    SVSQNHEX,SVSQNHEX                                                
         CLI   ACTNUM,ACTLIST      LIST ACTION?                                 
         JE    VKSQNX              YES, NO SEQUENCE KEY FIELD                   
*                                                                               
         LA    R2,SDMSEQNH         POINT TO SYSTEM KEY FIELD                    
         CLI   5(R2),0             ANY INPUT?                                   
         JE    VKSQNX              NONE                                         
         TM    4(R2),X'08'         NUMERIC FIELD?                               
         JZ    VKINV               NO, INVALID NUMERIC                          
*                                                                               
         LLC   RE,5(R2)            WHAT NUMBER WAS ENTERED?                     
         BCTR  RE,0                                                             
         EX    RE,VKPKSEQN                                                      
         CVB   R1,DUB              CONVERT TO BINARY                            
         CHI   R1,255              HAS TO FIT IN A BYTE                         
         JH    VKINV                                                            
         STC   R1,SVSQNHEX         SAVE OFF THE SEQUENCE NUMBER                 
         STC   R1,SDRKSEQ          ADD SEQ NUMBER TO KEY                        
VKSQNXMT OI    SDMFFCH+6,X'80'     TRANSMIT THE FIELD                           
*                                                                               
VKSQNX   DS    0H                                                               
***********************************                                             
VKX      DS    0H                                                               
         J     EXIT                                                             
*                                                                               
VKPKSEQN PACK  DUB,8(0,R2)                                                      
*                                                                               
VKMISS   LA    RF,MISSING          REQUIRED FIELD                               
         B     VKERR                                                            
*                                                                               
VKINV    LA    RF,INVALID          INVALID INPUT                                
*                                                                               
VKERR    DS    0H                                                               
*                                                                               
*        MOVE ERROR NUMBER IN ERROR TO ERROR2CD IF REQUIRED                     
*                                                                               
         OI    GENSTAT2,USGETTXT   GENCON MUST CALL GETTXT, NOT GETMSG          
*                                                                               
         LA    RE,GETTXTCB                                                      
         USING GETTXTD,RE                                                       
         MVI   GTMSYS,X'08'        REP MESSAGE SYSTEM                           
         STCM  RF,3,GTMSGNO        SET ERROR CODE                               
         DROP  RE                                                               
*                                                                               
         GOTOR ERREX                                                            
*                                                                               
TOOSMALL EQU   1029                INPUT TOO SMALL                              
NOKEYFLD EQU   1030                NO OTHER KEY FIELDS ALLOWED                  
TOOBIG   EQU   1031                INPUT TOO BIG                                
PTRNOTFD EQU   1032                PARTNER NOT FOUND                            
OFCNOTOK EQU   1033                OFFICE NOT ALLOWED                           
ARSERR   EQU   1034                ONLY ONE OF PTR/STA/AGY                      
NOUSERID EQU   1035                USERID NOT ON FILE                           
***********************************************************************         
*                                                                     *         
*     DISPLAY  KEY ROUTINE                                            *         
*                                                                     *         
***********************************************************************         
DK       NTR1                                                                   
         L     R6,AIO                                                           
         USING SDRKEY,R6           ESTABLISH STATION SOURCE RECORD KEY          
*                                                                               
         CLC   SVSDRKEY,SDRKEY     SAME SDR RECORD FROM BEFORE?                 
         JE    DK010               YES, DON'T MESS  WITH SVELDISP               
         XC    SVELDISP,SVELDISP   NEW RECORD TO DISPLAY                        
*                                                                               
DK010    LA    R2,SDMFPKH                                                       
         GOTO1 OUTFPK,DMCB,SDRKFFL,8(R2)                                        
*                                                                               
         LA    R2,SDMSYSH                                                       
         GOTO1 OUTSYS,DMCB,SDRKSYS,8(R2)                                        
*                                                                               
         LA    R2,SDMPRGH                                                       
         GOTO1 OUTXPN,DMCB,SDRKAPP,8(R2)                                        
         LA    R2,SDMXPNH                                                       
         GOTO1 HEXOUT,DMCB,SDRKAPP,8(R2),L'SDRKAPP                              
*                                                                               
         OC    SDRKKEY,SDRKKEY     ANY FREE FORM KEY?                           
         JZ    DK020               NONE                                         
         LA    R2,SDMFFKH                                                       
         GOTO1 HEXOUT,DMCB,SDRKKEY,8(R2),L'SDRKKEY                              
         LA    R2,SDMFFCH                                                       
         MVC   8(L'SDRKKEY,R2),SDRKKEY                                          
         TR    8(L'SDRKKEY,R2),DISPTBLL                                         
*                                                                               
DK020    OC    SDRKSEQ,SDRKSEQ     ANY SEQUENCE NUMBER?                         
         JZ    DK030                                                            
         EDIT  (B1,SDRKSEQ),(3,SDMSEQN),FILL=0                                  
*                                                                               
DK030    GOTO1 HEXOUT,DMCB,DMDSKADD,SDMDSKA,L'DMDSKADD                          
*                                                                               
DKX      DS    0H                                                               
         DROP  R6                                                               
         J     EXIT                EXIT                                         
***********************************************************************         
*                                                                     *         
*     VALIDATE RECORD ROUTINE                                         *         
*                                                                     *         
***********************************************************************         
VR       NTR1                                                                   
         MVI   BITFLAG1,0          CLEAR THE MISC BIT FLAGS                     
         MVC   AIO,AIO1                                                         
         L     R6,AIO                                                           
         USING SDRKEY,R6                                                        
         CLI   ACTNUM,ACTADD                                                    
         JNE   VR010                                                            
         MVC   SDRRLEN,=Y(SDRRFRST-SDRKEY)                                      
         J     VRX                                                              
*                                                                               
VR010    CLI   SDMMOD,C'C'         CHANGE MAP CODE?                             
         JE    VRMOD000                                                         
         CLI   SDMMOD,C'I'         INSERT A NEW MAP CODE?                       
         JE    VRMOD000                                                         
*********                                                                       
* USER WAS IN DISPLAY MODE - WE CHECK THE SELECT CODES FOR                      
*                                                                               
* I - INSERT HERE                                                               
* D - DELETE THIS MAP CODE                                                      
* C - CHANGE THIS MAP CODE                                                      
*********                                                                       
VRDIS000 LA    R2,SDMSEL1H         R2 = A(FIRST LINE'S SELECT FLDHDR)           
         USING DRLINED,R2                                                       
         LA    R5,DISPLIST         R5 = DISPLIST                                
*                                                                               
VRDIS010 CLI   DRSELH+5,0          ANYTHING IN THE SELECT FIELD?                
         JNE   VRDIS030            YES                                          
VRDIS020 LA    R0,SDMPFLNH         DUMP TO NEXT LINE                            
         LA    R2,SDMSEL2H-SDMSEL1H(R2)                                         
         CR    R2,R0                                                            
         JNL   VRDISX                                                           
         LA    R5,2(R5)                 R5 = A(NEXT ENTRY IN DISPLIST)          
         J     VRDIS010                                                         
*                                                                               
VRDIS030 TM    BITFLAG1,BF1HADAC   ALREADY PERFORMED AN ACTION?                 
         JNZ   VRINV               CAN'T PROCESS 2 ACTIONS                      
*                                                                               
         CLI   DRSEL,C'D'          DELETE THIS MAP CODE?                        
         JE    VRDISD00            YES                                          
         CLI   DRSEL,C'I'          INSERT A NEW MAP CODE?                       
         JE    VRDISI00            YES                                          
         CLI   DRSEL,C'C'          CHANGE CURRENT MAP CODE?                     
         JE    VRDISC00            YES                                          
         J     VRINV               INVALID, NONE OF THE ABOVE                   
*********                                                                       
* DELETE THIS MAP CODE                                                          
*********                                                                       
VRDISD00 DS    0H                                                               
         XR    R0,R0                                                            
         ICM   R0,3,0(R5)          GET DISPLACEMENT INTO THE RECORD             
         JZ    VRINV               INVALID IF NO DISPLACEMENT                   
         L     R6,AIO                                                           
         AR    R6,R0                                                            
         GOTO1 VRECUP,DMCB,(X'FE',AIO),(R6),(R6),GENFCNFG                       
         OI    BITFLAG1,BF1HADAC   WE PROCESSED THIS ACTION                     
*                                                                               
         MVC   SVELDISP,DISPLIST   REDISPLAY FROM SAME STARTING ELEM            
         J     VRDIS020                                                         
*********                                                                       
* CHANGE CURRENT MAP CODE                                                       
*********                                                                       
VRDISC00 DS    0H                                                               
         MVI   SDMMOD,C'C'         CHANGE                                       
         MVC   SVELDISP,0(R5)      REDISPLAY FROM THIS MAP CODE                 
         MVC   SVACDISP,0(R5)      REDISPLAY FROM THIS MAP CODE                 
         J     VRDISI10                                                         
*********                                                                       
* INSERT A MAP CODE HERE                                                        
*********                                                                       
VRDISI00 DS    0H                                                               
         MVI   SDMMOD,C'I'         CHANGE                                       
*                                                                               
         MVC   SVELDISP,0(R5)      REDISPLAY FROM THIS MAP CODE                 
         MVC   SVACDISP,0(R5)      REDISPLAY FROM THIS MAP CODE                 
         OC    SVELDISP,SVELDISP   INSERTING WITH A NULL DISPLACMENT?           
         JNZ   VRDISI10            NO                                           
         LA    R0,DISPLIST         YES, INSERTING ON A BRAND NEW REC?           
         CR    R5,R0                                                            
         JNE   VRINV    <=======   NO, TO THE END OF RECORD THEN                
*                                                                               
         MVC   SVELDISP,=Y(SDRRFRST-SDRKEY)  YES, DISP TO FIRST ELEM            
***************                                                                 
* COMMON CODE BETWEEN CHANGE AND INSERT                                         
***************                                                                 
VRDISI10 GOTO1 HEXOUT,DMCB,SVELDISP,SDMMOD+1,L'SDRRLEN                          
         OI    SDMMODH+6,X'80'                                                  
*                                                                               
         OI    BITFLAG1,BF1HADAC   WE PROCESSED THIS ACTION                     
         J     VRDIS020                                                         
*********                                                                       
* DONE VALIDATING THE SELECT FIELDS                                             
*********                                                                       
VRDISX   CLC   SDMMOD,SPACES       ANY ELEMENT ACTION?                          
         JNH   VRDSPREC            NONE, CONTINUE REDISPLAYING RECORD           
*                                                                               
         TWAXC SDMSEL1H,SDMPFLNH,PROT=Y  CLEAR THE ENTIRE DISP SCREEN           
*                                                                               
         MVC   SDMPFLN(15),=C'** INSERTING **'                                  
         CLI   SDMMOD,C'I'         insert mode?                                 
         JE    *+10                                                             
         MVC   SDMPFLN(15),=C'** CHANGING ** '                                  
*                                                                               
         MVC   SDMPFLN+20(12),=C'PF12=Return'                                   
         OI    SDMPFLNH+6,X'80'                                                 
*                                                                               
         LA    R2,SDMSEL1H                                                      
         CLI   SDMMOD,C'I'         INSERTING?                                   
         JE    VRDISX05            NOW THERE IS ROOM TO ADD                     
*                                                                               
         L     R6,AIO              POINT TO ELEM USER WANTS TO CHANGE           
         XR    RE,RE                                                            
         ICM   RE,3,SVACDISP                                                    
         AR    R6,RE                                                            
*                                                                               
         BRAS  RE,DISPELEM         DISPLAY, BUT IT PROTECTS FIELDS              
*                                                                               
         LA    R2,SDMSEL1H         UNPROTECT WHAT WE NEED                       
         NI    DRDLNH+6,X'FF'-X'20'      DATA LENGTH &                          
         NI    DRDATH+6,X'FF'-X'20'      DATA                                   
         J     VRDISX10                                                         
*                                                                               
VRDISX05 OI    DRSELH+6,X'20'+X'80' PROTECT THE SELECT FIELD                    
         OI    DRMAPH+6,X'80'      BUT UNPROTECT MAP, TYPE, LEN, DATA           
         OI    DRDTYH+6,X'80'        ON FIRST LINE ONLY                         
         OI    DRDLNH+6,X'80'                                                   
         OI    DRDATH+6,X'80'                                                   
*                                                                               
VRDISX10 LA    R0,SDMPFLNH         BUMP TO NEXT LINE                            
         LA    R2,SDMSEL2H-SDMSEL1H(R2)                                         
         CR    R2,R0               BOUNDARY CHECK                               
         JNL   VRDISX20                                                         
* ON SUBSEQUENT LINES, ONLY DATA FIELD IS UNPROTECTED                           
         OI    DRSELH+6,X'20'+X'80' PROTECT THE SELECT FIELD                    
         OI    DRMAPH+6,X'20'+X'80'             MAP CODE                        
         OI    DRDTYH+6,X'20'+X'80'             DATA TYPE                       
         OI    DRDLNH+6,X'20'+X'80'             DATA LENGTH                     
         NI    DRDATH+6,X'FF'-X'20'      DATA                                   
         J     VRDISX10                                                         
*                                                                               
VRDISX20 J     VRX                 CHANGE, WE HAVE TO DISPLAY IT FIRST          
*********                                                                       
* USER WAS IN CHANGE/INSERT MODE?                                               
*********                                                                       
VRMOD000 LA    R2,SDMSEL1H                                                      
         USING DRLINED,R2                                                       
         LA    R6,ELEM                                                          
         USING SDELD,R6                                                         
         XC    ELEM,ELEM                                                        
*                                                                               
VRMMAP00 CLI   DRMAPH+5,0          WE NEED A MAP CODE                           
         JE    VRMMMISS                                                         
         TM    DRMAPH+4,X'08'      MAP CODE NEEDS TO BE A VALID NUMBER          
         JZ    VRMMINVL                                                         
         LLC   R1,DRMAPH+5                                                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,DRMAP(0)        CONVERT THE MAP CODE TO BINARY               
         CVB   R0,DUB                                                           
         CHI   R0,X'FE'            CAN'T HAVE AN 255 OR HIGHER                  
         JH    VRMMINVL                                                         
         STC   R0,SDEEL            WE HAVE OUR ELEMENT CODE                     
*                                                                               
VRMDTY00 CLI   DRDTYH+5,0          WE NEED A DATA TYPE                          
         JE    VRMTMISS                                                         
         GOTO1 VALDTY,DMCB,DRDTY,SDETYPE  VALIDATE THE DATA TYPE                
         JNE   VRMTINVL                   NOT VALID                             
*                                                                               
VRMDLN00 CLI   DRDLNH+5,0          ANY DATA LENGTH?                             
         JE    VRMDAT00            NONE, IMPLICIT LEN FROM THE DATA             
*                                                                               
         TM    DRDLNH+4,X'08'      YES, NEEDS TO BE A VALID NUMBER              
         JZ    VRMLINVL                                                         
         LLC   R1,DRDLNH+5                                                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,DRDLN(0)        CONVERT THE LENGTH TO BINARY                 
         CVB   R0,DUB                                                           
*                                                                               
         CLI   SDETYPE,12          TYPE IS UNSIGNED BINARY?                     
         JNE   VRMDLN10            NO                                           
         CHI   R0,4                CAN'T STORE MORE THAN A FULLWORD             
         JH    VRMLINVL                                                         
*                                                                               
VRMDLN10 CHI   R0,255-SDELEN1Q     CAN'T HAVE AN ELEM LENGTH > 255              
         JH    VRMLINVL                                                         
*  WE'LL ADD THE OVERHEAD TO SDELEN JUST BEFORE THE RECUP                       
         STC   R0,SDELEN           WE HAVE OUR ELEMENT LENGTH                   
*                                                                               
VRMDAT00 CLI   DRDATH+5,0          ANY DATA?                                    
         JE    VRMDMISS            WE NEED DATA                                 
*                                                                               
         MVI   CURRDLEN,0          NO DATA YET                                  
         CLI   SDETYPE,08          HEXADECIMAL STRING?                          
         JE    VMHEXD00                                                         
         CLI   SDETYPE,12          UNSIGNED BINARY?                             
         JE    VMUBIN00                                                         
         CLI   SDETYPE,03          CHARACTER STRING?                            
         JNE   *+2                                                              
*********                                                                       
* INSERT A CHARACTER STRING                                                     
*********                                                                       
VMCHAR00 LA    R4,SDEDATA          WHERE TO START STORING CHARACTERS            
         LA    R3,DRDATH                                                        
         MVC   ELEM+SDELEN1Q(L'ELEM-SDELEN1Q),=255C' '                          
VMCHAR10 CLI   5(R3),0             CURRENT LINE HAS NO DATA?                    
         JE    VMCHAR30            YES, BUT WE HAVE DATA (VRMDAT00)             
*                                                                               
         LLC   RE,CURRDLEN         ADJUST CURRENT DATA LENGTH                   
         LLC   RF,5(R3)                                                         
         AR    RE,RF                                                            
         CHI   RE,255-SDELEN1Q     CAN'T HAVE AN ELEM LENGTH > 255              
         JH    VRMDINVL                                                         
         STC   RE,CURRDLEN                                                      
*                                                                               
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),8(R3)       SAVE OFF THE CHARACTERS                      
*&&DO                                                                           
* A STRING CAN HAVE A SPACE IN THE LAST CHARACTER AND CONTINUE ON THE           
* NEXT LINE, WE CAN'T USE THE L'DRDAT ON CHAR STRING                            
         CLI   5(R3),L'DRDAT       WHOLE LINE WAS FILLED?                       
         JNE   VMCHAR20            NO, NEXT LINE SHOULD BE EMPTY THEN           
*&&                                                                             
         LA    R4,1(R4,RF)                                                      
         LA    R3,SDMSEL2H-SDMSEL1H(R3)                                         
         J     VMCHAR10            PROCESS THE NEXT LINE OF DATA                
*&&DO                                                                           
* NO NEED TO CHECK IF THE LINE AFTER AN EMPTY LINE IS ALSO EMPTY                
VMCHAR20 LA    R3,SDMSEL2H-SDMSEL1H(R3)                                         
         CLI   5(R3),0             WE SHOULD HAVE AN EMPTY LINE                 
         JNE   VIDATINV            ERROR IF WE DON'T                            
*&&                                                                             
VMCHAR30 CLI   SDELEN,0            DID USER SET THE DATA LENGTH?                
         JNE   VMCHAR50            NO, WE MIGHT HAVE TO ADJUST                  
VMCHAR35 LLC   RE,CURRDLEN                                                      
         LA    RE,SDELEN1Q(RE)                                                  
         STC   RE,SDELEN           WE NOW HAVE OUR ELEMENT LENGTH               
*                                                                               
         L     R6,AIO                                                           
         XR    R0,R0                                                            
         ICM   R0,3,SVACDISP      SAVED DISPLACEMENT FOR ACTION                 
         AR    R6,R0              LOCATION FOR THE INSERT                       
*                                                                               
         CLI   SDMMOD,C'I'        INSERT MODE?                                  
         BE    VMCHAR40           YES, NO NEED FOR A DELETE                     
* DELETE OLD ELEMENT                                                            
         GOTO1 VRECUP,DMCB,(X'FE',AIO),(R6),(R6),GENFCNFG                       
* INSERT NEWLY CHANGED ONE                                                      
VMCHAR40 GOTO1 VRECUP,DMCB,(X'FE',AIO),ELEM,(R6),GENFCNFG                       
         J     VRMODX                                                           
*                                                                               
VMCHAR50 CLC   SDELEN,CURRDLEN    SPECIFIED LEN = CHARACTER COUNT?              
         JE    VMCHAR35           YES, SAME                                     
         JL    VRMDINVL           ERROR IF LESS                                 
         MVC   CURRDLEN,SDELEN    HIGHER, USE THE SPACE FILL                    
         J     VMCHAR35             AND USE THE NEW LENGTH                      
*********                                                                       
* INSERT AN UNSIGNED BINARY - ONLY ONE LINE OF DATA                             
*********                                                                       
VMUBIN00 LA    R4,SDEDATA          WHERE TO START STORING DATA                  
         LA    R3,DRDATH                                                        
*                                                                               
         TM    4(R3),X'08'         MAKE SURE VALID NUMERIC                      
         JZ    VIDATINV            IT IS NOT                                    
         CLI   5(R3),15            MORE THAN 15 DIGITS?                         
         JH    VIDATINV            WON'T BE ABLE TO PACK THE # INTO DUB         
*                                                                               
         LLC   R1,5(R3)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R3)         CONVERT THE NUMBER TO BINARY                 
         CP    DUB,=P'4294967295'  MORE THAN FIT IN A FULLWORD?                 
         JH    VIDATINV            ERROR IF SO                                  
         CVB   R0,DUB                                                           
         ST    R0,FULL             BINARY AS A FULLWORD                         
*                                                                               
         LA    R3,SDMSEL2H-SDMSEL1H(R3)  CHECK NEXT DATA LINE                   
         CLI   5(R3),0             WE SHOULD HAVE AN EMPTY LINE                 
         JNE   VIDATINV            ERROR IF WE DON'T                            
*                                                                               
         LA    RE,4                FULLWORD HAS LENGTH OF 4                     
         LA    RF,FULL             FULLWORD HAS LENGTH OF 4                     
VMUBIN10 CLI   0(RF),0             HIGH ORDER BYTE IS A 0?                      
         JNE   VMUBIN20            WE NOW HAVE OUR ELEMENT LENGTH               
         LA    RF,1(RF)            SHIFT OVER 1 BYTE                            
         BCT   RE,VMUBIN10                                                      
         LA    RE,1                WE HAVE A VALUE OF 0, NOT A NULL             
*                                                                               
VMUBIN20 CLI   SDELEN,0            DID USER SET THE DATA LENGTH?                
         JNE   VMUBIN50            NO, WE MIGHT HAVE TO ADJUST                  
VMUBIN25 LA    RE,SDELEN1Q(RE)     CALCULATE PROPER ELEMENT LENGTH              
         STC   RE,SDELEN                                                        
*                                                                               
         MVC   SDEDATA(4),0(RF)                                                 
         L     R6,AIO                                                           
         XR    R0,R0                                                            
         ICM   R0,3,SVACDISP      SAVED DISPLACEMENT FOR ACTION                 
         AR    R6,R0              LOCATION FOR THE INSERT                       
*                                                                               
         CLI   SDMMOD,C'I'        INSERT MODE?                                  
         BE    VMUBIN40           YES, NO NEED FOR A DELETE                     
* DELETE OLD ELEMENT                                                            
         GOTO1 VRECUP,DMCB,(X'FE',AIO),(R6),(R6),GENFCNFG                       
* INSERT NEWLY CHANGED ONE                                                      
VMUBIN40 GOTO1 VRECUP,DMCB,(X'FE',AIO),ELEM,(R6),GENFCNFG                       
         J     VRMODX                                                           
*                                                                               
VMUBIN50 CLM   RE,1,SDELEN        CALCULATED LEN SAME AS SPECIFIED LEN?         
         JE    VMUBIN25           YES                                           
         JH    VRMDINVL           IT IS MORE, ERROR                             
         LLC   R1,SDELEN          LET'S SEE HOW MANY BYTES TO GO BACK           
         SR    R1,RE                                                            
VMUBIN55 BCTR  RF,0               DECREMENT POINTER TO FULL BACK 1 BYTE         
         BCT   R1,VMUBIN55                                                      
         LLC   RE,SDELEN                                                        
         J     VMUBIN25                                                         
*********                                                                       
* INSERT A HEXADECIMAL STRING                                                   
*********                                                                       
VMHEXD00 LA    R4,SDEDATA          WHERE TO START STORING CHARACTERS            
         LA    R3,DRDATH                                                        
VMHEXD10 CLI   5(R3),0             CURRENT LINE HAS NO DATA?                    
         JE    VMHEXD30            YES, BUT WE HAVE DATA (VRMDAT00)             
         TM    4(R3),X'02'         MAKE SURE VALID HEXADECIMAL                  
         JZ    VIDATINV            IT IS NOT                                    
*                                                                               
         LLC   RE,CURRDLEN         ADJUST CURRENT DATA LENGTH                   
         LLC   RF,5(R3)                                                         
         ST    RF,DMCB+8           # OF HEX CHARACTERS TO CONVERT               
         SRL   RF,1                DIVIDE BY 2 FOR ACTUAL BYTES                 
         AR    RE,RF                                                            
         CHI   RE,255-SDELEN1Q     CAN'T HAVE AN ELEM LENGTH > 255              
         JH    VRMDINVL                                                         
         STC   RE,CURRDLEN                                                      
* # OF HEX CHARACTERS TO CONVERT SET IN DMCB+8                                  
         GOTO1 HEXIN,DMCB,8(R3),0(R4),,0  SAVE OFF THE HEX                      
*                                                                               
         CLI   5(R3),L'DRDAT       WHOLE LINE WAS FILLED?                       
         JNE   VMHEXD20            NO, NEXT LINE SHOULD BE EMPTY THEN           
         LA    R4,L'DRDAT/2(R4)                                                 
         LA    R3,SDMSEL2H-SDMSEL1H(R3)                                         
         J     VMHEXD10            PROCESS THE NEXT LINE OF DATA                
*                                                                               
VMHEXD20 LA    R3,SDMSEL2H-SDMSEL1H(R3)                                         
         CLI   5(R3),0             WE SHOULD HAVE AN EMPTY LINE                 
         JNE   VIDATINV            ERROR IF WE DON'T                            
*                                                                               
VMHEXD30 CLI   SDELEN,0            DID USER SET THE DATA LENGTH?                
         JNE   VMHEXD50            NO, WE MIGHT HAVE TO ADJUST                  
VMHEXD35 LLC   RE,CURRDLEN                                                      
         LA    RE,SDELEN1Q(RE)                                                  
         STC   RE,SDELEN           WE NOW HAVE OUR ELEMENT LENGTH               
*                                                                               
         L     R6,AIO                                                           
         XR    R0,R0                                                            
         ICM   R0,3,SVACDISP      SAVED DISPLACEMENT FOR ACTION                 
         AR    R6,R0              LOCATION FOR THE INSERT                       
*                                                                               
         CLI   SDMMOD,C'I'        INSERT MODE?                                  
         BE    VMHEXD40           YES, NO NEED FOR A DELETE                     
* DELETE OLD ELEMENT                                                            
         GOTO1 VRECUP,DMCB,(X'FE',AIO),(R6),(R6),GENFCNFG                       
* INSERT NEWLY CHANGED ONE                                                      
VMHEXD40 GOTO1 VRECUP,DMCB,(X'FE',AIO),ELEM,(R6),GENFCNFG                       
         J     VRMODX                                                           
*                                                                               
VMHEXD50 CLC   SDELEN,CURRDLEN    SPECIFIED LEN = CHARACTER COUNT?              
         JE    VMHEXD35           YES, SAME                                     
         JL    VRMDINVL           ERROR IF LESS                                 
         MVC   CURRDLEN,SDELEN    HIGHER, USE THE ZERO FILL                     
         J     VMHEXD35             AND USE THE NEW LENGTH                      
*                                                                               
VRMODX   J     VRX                                                              
*                                                                               
VRMMMISS LA    R2,DRMAPH          MISSING INPUT FIELD FOR MAP CODE              
         J     VRMISS                                                           
VRMTMISS LA    R2,DRDTYH                                  DATA TYPE             
         J     VRMISS                                                           
VRMDMISS LA    R2,DRDATH                                  DATA                  
         J     VRMISS                                                           
*                                                                               
VRMMINVL LA    R2,DRMAPH          INVALID INPUT FIELD FOR MAP CODE              
         J     VRINV                                                            
VRMTINVL LA    R2,DRDTYH                                  DATA TYPE             
         J     VRINV                                                            
VRMLINVL LA    R2,DRDLNH                                  DATA LENGTH           
         J     VRINV                                                            
VRMDINVL LA    R2,DRDATH                                  DATA                  
         J     VRINV                                                            
VIDATINV LR    R2,R3               INVALID DATA LINE                            
         J     VRINV                                                            
*********                                                                       
VRDSPREC CLI   PFKEY,12                                                         
         JE    VRX                                                              
         OI    SDMSEL1H+6,X'81'    MODIFIED FIELD                               
         BRAS  RE,DR                                                            
VRX      J     EXIT                                                             
*                                                                               
GENFCNFG DC    AL2(42,32,2000)                                                  
*                                                                               
VRINV    MVI   ERROR,INVALID       INVALID FIELD                                
         B     VRERR                                                            
*                                                                               
VRDUP    MVI   ERROR,93            DUPLICATE ENTRY                              
         B     VRERR                                                            
*                                                                               
VRMISS   MVI   ERROR,MISSING       REQUIRED FIELD                               
         B     VRERR                                                            
*                                                                               
VRERR    DS    0H                                                               
         GOTOR ERREX                                                            
***********************************************************************         
*                                                                     *         
*     DISPLAY  RECORD ROUTINE                                         *         
*                                                                     *         
***********************************************************************         
DR       NTR1                                                                   
         XC    DISPLIST,DISPLIST                                                
         LA    R5,DISPLIST                                                      
*                                                                               
         TWAXC SDMSEL1H                                                         
         LA    R2,SDMSEL1H                                                      
         USING DRLINED,R2                                                       
         GOTO1 CALCDLNS,DMCB,(R2)  DLNSLEFT <-- # OF DISPLAY LINES LEFT         
*                                                                               
         CLI   ACTNUM,ACTSEL        SHOW USER HOW TO GET BACK TO LIST           
         JNE   DR005                                                            
         XC    SDMPFLN,SDMPFLN                                                  
         MVC   SDMPFLN(53),=C'C=Change I=Insert                PF8=Down+        
                PF12=Return'                                                    
         OI    SDMPFLNH+6,X'80'                                                 
*                                                                               
DR005    L     R6,AIO                                                           
         MVC   SVSDRKEY,0(R6)                                                   
         XR    RE,RE                                                            
         ICM   RE,3,SVELDISP                                                    
         OC    SVELDISP,SVELDISP                                                
         JNZ   DR010                                                            
         LA    RE,SDRRFRST-SDRKEY                                               
DR010    AR    R6,RE                                                            
*                                                                               
         USING SDELD,R6                                                         
DRELLOOP LR    RF,R6               CALCULATE DISP TO THIS ELEM                  
         L     R0,AIO                                                           
         SR    RF,R0                                                            
         STCM  RF,3,SVELDISP       SAVE IT OFF IN CASE ELEM WON'T FIT           
*                                                                               
         CLI   SDEEL,0             EOR?                                         
         JE    DREOR                                                            
*                                                                               
         CLI   DLNSLEFT,1          DO WE HAVE AT LEAST ONE LINE LEFT?           
         JL    DRX                 NO, DISPLAYED AS MUCH AS WE COULD            
*                                                                               
         BRAS  RE,GOTROOM          DO WE HAVE ROOM ON THE SCREEN?               
         JNE   DRX                                                              
*                                                                               
         BRAS  RE,DISPELEM         DISPLAY THIS MAP ELEM                        
*                                                                               
DRELNXEL LLC   R0,1(R6)                  R6 = A(NEXT ELEMENT)                   
         AR    R6,R0                                                            
         LA    R2,SDMSEL2H-SDMSEL1H(R2)  POINT TO NEXT DISPLAY LINE             
         LLC   R1,DLNSLEFT                                                      
         BCTR  R1,0                                                             
         STC   R1,DLNSLEFT                                                      
         LA    R5,2(R5)                 R5 = A(NEXT ENTRY IN DISPLIST)          
         J     DRELLOOP                                                         
*                                                                               
DREOR    MVC   0(2,R5),SVELDISP    DISPLACEMENT TO END OF REC                   
         XC    SVELDISP,SVELDISP   START FROM BEGINNING NEXT TIME               
*                                                                               
DRX      J     EXIT                                                             
***********************************************************************         
* GOT ENOUGH ROOM ON THE SCREEN FOR THIS MAP ELEMENT?                           
*                                                                               
* ON ENTRY:    (R6)                A(MAP ELEMENT)                               
*              DLNSLEFT            # OF DISPLAY LINES LEFT                      
*                                                                               
* ON EXIT:     CC                  CONDITION CODE                               
***********************************************************************         
GOTROOM  NTR1                                                                   
         USING SDELD,R6                                                         
         CLI   SDETYPE,03          CHARACTER STRING?                            
         JE    GCHAR00                                                          
         CLI   SDETYPE,12          NUMERIC DATA?                                
         JE    GUBIN00                                                          
         CLI   SDETYPE,08          HEXADECIMAL STRING?                          
         JE    GHEXD00                                                          
         J     GUNKN00             UNKNOWN, SO FAR                              
***************                                                                 
* CHARACTER STRING                                                              
***************                                                                 
GCHAR00  LLC   RF,SDELEN           R3 = L(DATA)                                 
         SHI   RF,SDELEN1Q                                                      
         XR    RE,RE                                                            
         LHI   R1,L'DRDAT                                                       
         DR    RE,R1               RF = # OF LINES NEEDED, ROUNDED DOWN         
         LTR   RE,RE               ANY REMAINDER?                               
         JZ    *+8                                                              
         LA    RF,1(RF)            YES, NEED AN ADDITIONAL LINE                 
*                                                                               
         CLM   RF,1,DLNSLEFT       WILL DATA FIT ON THE LINES LEFT?             
         JH    GOTROOMN            NO, WAIT FOR NEXT HIT OF ENTER               
         J     GOTROOMY                                                         
***************                                                                 
* NUMERIC DATA - WILL FIT ON ONE LINE                                           
***************                                                                 
GUBIN00  DS    0H                                                               
         J     GOTROOMY                                                         
***************                                                                 
* HEXADECIMAL STRING                                                            
***************                                                                 
GHEXD00  LLC   RF,SDELEN           R3 = L(DATA)                                 
         SHI   RF,SDELEN1Q                                                      
         SLL   RF,1                TIMES 2 AS EACH BYTE IS 2 CHAR               
         XR    RE,RE                                                            
         LHI   R1,L'DRDAT                                                       
         DR    RE,R1               RF = # OF LINES NEEDED, ROUNDED DOWN         
         LTR   RE,RE               ANY REMAINDER?                               
         JZ    *+8                                                              
         LA    RF,1(RF)            YES, NEED AN ADDITIONAL LINE                 
*                                                                               
         CLM   RF,1,DLNSLEFT       WILL DATA FIT ON THE LINES LEFT?             
         JH    GOTROOMN            NO, WAIT FOR NEXT HIT OF ENTER               
         J     GOTROOMY                                                         
***************                                                                 
* UNKNOWN DATA - WE'LL FIT IT ON ONE LINE                                       
***************                                                                 
GUNKN00  DS    0H                                                               
         J     GOTROOMY                                                         
*                                                                               
GOTROOMY J     YES                                                              
GOTROOMN J     NO                                                               
*                                                                               
***********************************************************************         
*                                                                     *         
*     LIST RECORDS ROUTINE                                            *         
*                                                                     *         
***********************************************************************         
LR       NTR1                                                                   
         MVI   NLISTS,17                                                        
         LA    R6,KEY                                                           
         USING SDRKEY,R6           ESTABLISH RATING SOURCE KEY                  
         MVC   AIO,AIO1                                                         
*                                                                               
         OC    KEY,KEY             TEST FIRST TIME                              
         BNZ   LR040               KEY IS LAST RECORD READ                      
*                                  SO GO CHECK VS. KEYSAVE                      
*                                                                               
         MVI   SDRKMAJ,SDRKMAJQ    SET RECORD MAJOR TYPE                        
         MVI   SDRKMIN,SDRKMINQ    SET RECORD SECONDARY TYPE                    
*                                                                               
         CLI   SVFPKHEX,0                                                       
         JE    LR010                                                            
         MVC   SDRKFFL,SVFPKHEX    SET FACPAK FLAG                              
*                                                                               
LR010    CLI   SVSYSHEX,0                                                       
         JE    LR015                                                            
         MVC   SDRKSYS,SVSYSHEX    SET SYSTEM                                   
*                                                                               
LR015    OC    SVPRGHEX,SVPRGHEX                                                
         JE    LR020                                                            
         MVC   SDRKAPP,SVPRGHEX    SET PROGRAM                                  
*                                                                               
LR020    DS    0H                                                               
*                                                                               
LRRDHI   GOTO1 HIGH                                                             
         B     LR040                                                            
*                                                                               
LRRDSQ   GOTO1 SEQ                                                              
*                                                                               
LR040    CLC   KEY(2),KEYSAVE      TEST FOR ALL DONE                            
         BNE   LR900                                                            
*                                                                               
         GOTO1 GETREC              READ IN FOUND RECORD                         
*                                                                               
         L     R6,AIO              POINT TO RECORD                              
         CLI   SVFPKHEX,0          ANY FACPAK FILTER?                           
         JE    LR045               NONE                                         
         CLC   SDRKFFL,SVFPKHEX    YES, WE HAVE A MATCH?                        
         JNE   LRRDSQ                   NO                                      
*                                                                               
LR045    CLI   SVSYSHEX,0          ANY SYSTEM FILTER?                           
         JE    LR050               NONE                                         
         CLC   SDRKSYS,SVSYSHEX    YES, WE HAVE A MATCH?                        
         JNE   LRRDSQ                   NO                                      
*                                                                               
LR050    OC    SVPRGHEX,SVPRGHEX   ANY PROGRAM FILTER?                          
         JZ    LR060               NONE                                         
         CLC   SDRKAPP,SVPRGHEX    YES, WE HAVE A MATCH?                        
         JNE   LRRDSQ                   NO                                      
*                                                                               
LR060    LA    R2,LISTAR           ESTABLISH LIST                               
         USING LSLINED,R2                                                       
         MVC   0(L'LSLINE,R2),SPACES                                            
*                                                                               
         GOTO1 OUTFPK,DMCB,SDRKFFL,LSFPK                                        
         GOTO1 OUTSYS,DMCB,SDRKSYS,LSSYS                                        
         GOTO1 OUTXPN,DMCB,SDRKAPP,LSPRGC                                       
         GOTO1 HEXOUT,DMCB,SDRKAPP,LSPRGH,L'SDRKAPP                             
*                                                                               
         OC    SDRKKEY,SDRKKEY     ANY FREEFORM KEY?                            
         JZ    LR070               NONE                                         
         GOTO1 OUTFFK,DMCB,SDRKKEY,LSFFKC                                       
         GOTO1 HEXOUT,DMCB,SDRKKEY,LSFFKH,L'SDRKKEY                             
*                                                                               
LR070    CLI   SDRKSEQ,0           ANY SEQUENCE NUMBER                          
         JE    LR080               NONE                                         
         EDIT  (B1,SDRKSEQ),(3,LSSEQN),FILL=0                                   
*                                                                               
LR080    GOTO1 LISTMON                                                          
         B     LRRDSQ                                                           
         DROP  R2                                                               
*                                                                               
LR900    XC    KEY,KEY                                                          
*                                                                               
LRX      DS    0H                                                               
         J     EXIT                                                             
         DROP  R6                                                               
***********************************************************************         
* CALCULATE THE NUMBER OF DISPLAY LINES (60 CHAR LINES) LEFT                    
*                                                                               
* ON ENTRY:    (P1)                CURRENT LINE TO START DISPLAYING             
*                                                                               
* ON EXIT:     DLNSLEFT            CALCULATED NUMBER                            
***********************************************************************         
CALCDLNS NTR1                                                                   
         L     R2,0(R1)            CURRENT LINE'S SELECT FLDHDR                 
         LA    RF,SDMPFLNH         POINT AFTER THE LAST LINE (PFLN)             
         SR    RF,R2               # BYTES BETWEEN THEM                         
         SR    RE,RE               SET UP FOR DIVIDE                            
*                                                                               
         LHI   R3,SDMSEL2H-SDMSEL1H  R3 = # BYTES PER LINE                      
         DR    RE,R3               RF = # OF LINES BTWN THEM                    
         STC   RF,DLNSLEFT                                                      
CDLNSX   J     EXIT                                                             
***********************************************************************         
* VALIDATE FACPAK CHAR BASED                                                    
*                                                                               
* ON ENTRY:    (P1)                A(FACPAK) 3 CHARACTER INPUT                  
*              (P2)                A(FACPAK FLAG) OUTPUT                        
***********************************************************************         
VALFPK   NTR1                                                                   
         LM    R2,R3,0(R1)                                                      
         LA    RE,FPKTAB                                                        
         USING FPKTABD,RE                                                       
VFPK010  CLI   FPKNAME,X'FF'       EOT?                                         
         JE    NO                                                               
*                                                                               
         CLC   FPKNAME,0(R2)       MATCH ON INPUT FACPAK FLAG?                  
         JE    VFPK020                                                          
         LA    RE,FPKTABLQ(RE)     CHECK NEXT FACPAK FLAG                       
         J     VFPK010                                                          
*                                                                               
VFPK020  MVC   0(L'FPKHEX,R3),FPKHEX  SAVE THE FACPAK FLAG                      
*                                                                               
VALFPKX  J     YES                                                              
         DROP  RE                                                               
                                                                                
***********************************************************************         
* VALIDATE SYSTEM CHAR BASED                                                    
*                                                                               
* ON ENTRY:    (P1)                A(SYSTEM) 3 CHARACTER INPUT                  
*              (P2)                A(SYSTEM HEX)                                
***********************************************************************         
VALSYS   NTR1                                                                   
         LM    R2,R3,0(R1)                                                      
         LA    RE,STMTAB                                                        
         USING STMTABD,RE                                                       
VSYS010  CLI   STMNAME,X'FF'       EOT?                                         
         JE    NO                                                               
*                                                                               
         CLC   STMNAME,0(R2)       MATCH ON INPUT FACPAK FLAG?                  
         JE    VSYS020                                                          
         LA    RE,STMTABLQ(RE)     CHECK NEXT FACPAK FLAG                       
         J     VSYS010                                                          
*                                                                               
VSYS020  MVC   0(L'STMHEX,R3),STMHEX  SAVE THE FACPAK FLAG                      
*                                                                               
VALSYSX  J     YES                                                              
         DROP  RE                                                               
                                                                                
***********************************************************************         
* VALIDATE EXTERNAL PROGRAM CHAR BASED                                          
*                                                                               
* ON ENTRY:    (P1)                A(PROGRAM) 10 CHARACTER INPUT                
*              (P2)                A(EXTERNAL PROGRAM NUMBER)                   
***********************************************************************         
VALXPN   NTR1                                                                   
         LM    R2,R3,0(R1)                                                      
         CLC   =C'=X',0(R2)        USER USING HEX FORMAT?                       
         JNE   VXPN010             NO, CHECK 10 CHARACTER FORM                  
         GOTO1 HEXIN,DMCB,2(R2),0(R3),4  YES, NEXT 4 CHARACTERS IS HEX          
         OC    12(4,R1),12(R1)     P4 IS ZERO IF SOURCE IS INVALID              
         JZ    NO                                                               
         J     VALXPNX                                                          
*                                                                               
VXPN010  L     RE,VFXPCTBL         A(PC PROGRAM TBL) - SEE FAXPTAB              
         USING FAXPTABD,RE                                                      
         OC    0(L'FAXPNEXE,R2),SPACES  SPACE FILL AND CAPS                     
*                                                                               
VXPN020  OC    0(FAXPNEXE-FAXPTYPE,RE),0(RE)                                    
         JZ    NO                  YES, NO ACTUAL NAME                          
*                                                                               
         CLC   FAXPNEXE,0(R2)      MATCH ON EXTERNAL PROGRAM NAME?              
         JE    VXPN030                                                          
         LA    RE,L'FAXPNTRY(RE)                                                
         J     VXPN020                                                          
*                                                                               
VXPN030  MVC   0(L'FAXPNUM,R3),FAXPNUM  SAVE THE FACPAK FLAG                    
*                                                                               
VALXPNX  J     YES                                                              
         DROP  RE                                                               
                                                                                
***********************************************************************         
* VALIDATE DATA TYPE                                                            
*                                                                               
* ON ENTRY:    (P1)                A(DATA TYPE) 4 CHARACTER DATA TYPE           
*              (P2)                A(OUTPUT FIELD) 1 BYTE BINARY TYPE           
***********************************************************************         
VALDTY   NTR1                                                                   
         LM    R2,R3,0(R1)                                                      
         LA    RE,DTYTAB                                                        
         USING DTYTABD,RE                                                       
VDTY010  CLI   DTYNAME,X'FF'       EOT?                                         
         JE    VALDTYN             SHOW NOTHING, TYPE NOT IN TABLE              
*                                                                               
         CLC   DTYNAME,0(R2)       MATCH ON INPUT?                              
         JE    VDTY020                                                          
         LA    RE,DTYTABLQ(RE)     CHECK NEXT TYPE                              
         J     VDTY010                                                          
*                                                                               
VDTY020  MVC   0(L'DTYHEX,R3),DTYHEX   OUTPUT THE BINARY TYPE                   
*                                                                               
VALDTYY  J     YES                                                              
VALDTYN  J     NO                                                               
         DROP  RE                                                               
***********************************************************************         
* OUTPUT FACPAK CHAR BASED ON FLAG INPUT                                        
*                                                                               
* ON ENTRY:    (P1)                A(FACPAK FLAG)                               
*              (P2)                A(FACPAK) 3 CHARACTER OUTPUT                 
***********************************************************************         
OUTFPK   NTR1                                                                   
         LM    R2,R3,0(R1)                                                      
         LA    RE,FPKTAB                                                        
         USING FPKTABD,RE                                                       
OFPK010  CLI   FPKNAME,X'FF'       EOT?                                         
         JE    *+2                                                              
*                                                                               
         CLC   FPKHEX,0(R2)        MATCH ON INPUT FACPAK FLAG?                  
         JE    OFPK020                                                          
         LA    RE,FPKTABLQ(RE)     CHECK NEXT FACPAK FLAG                       
         J     OFPK010                                                          
*                                                                               
OFPK020  MVC   0(L'FPKNAME,R3),FPKNAME   OUTPUT THE NAME                        
*                                                                               
OUTFPKX  J     EXIT                                                             
         DROP  RE                                                               
***********************************************************************         
* OUTPUT SYSTEM CHAR BASED ON INPUT                                             
*                                                                               
* ON ENTRY:    (P1)                A(SYSTEM HEX)                                
*              (P2)                A(SYSTEM) 3 CHARACTER OUTPUT                 
***********************************************************************         
OUTSYS   NTR1                                                                   
         LM    R2,R3,0(R1)                                                      
         LA    RE,STMTAB                                                        
         USING STMTABD,RE                                                       
OSYS010  CLI   STMNAME,X'FF'       EOT?                                         
         JE    *+2                                                              
*                                                                               
         CLC   STMHEX,0(R2)        MATCH ON INPUT SYSTEM?                       
         JE    OSYS020                                                          
         LA    RE,STMTABLQ(RE)     CHECK NEXT SYSTEM                            
         J     OSYS010                                                          
*                                                                               
OSYS020  MVC   0(L'STMNAME,R3),STMNAME   OUTPUT THE NAME                        
*                                                                               
OUTSYSX  J     EXIT                                                             
         DROP  RE                                                               
***********************************************************************         
* OUTPUT EXTERNAL PROGRAM NAME FOR THE GIVEN FAXPEQUS                           
*                                                                               
* ON ENTRY:    (P1)                A(EXTERNAL PROGRM NUMBER)                    
*              (P2)                A(PROGRAM) 10 CHARACTER OUTPUT               
***********************************************************************         
OUTXPN   NTR1                                                                   
         LM    R2,R3,0(R1)                                                      
         L     RE,VFXPCTBL         A(PC PROGRAM TBL) - SEE FAXPTAB              
         USING FAXPTABD,RE                                                      
         LA    RE,L'FAXPNTRY(RE)   SKIP FIRST ENTRY (LEN AND MAX)               
*                                  EOT?  SEE FAXPTPCX IN FAXPTAB                
OXPN010  OC    0(FAXPNEXE-FAXPTYPE,RE),0(RE)                                    
         JZ    OUTXPNX             YES, NO ACTUAL NAME                          
*                                                                               
         CLC   FAXPNUM,0(R2)       MATCH ON EXTERNAL PROGRAM NUMBER?            
         JE    OXPN020                                                          
         LA    RE,L'FAXPNTRY(RE)                                                
         J     OXPN010                                                          
*                                                                               
OXPN020  MVC   0(L'FAXPNEXE,R3),FAXPNEXE  OUTPUT THE NAME                       
*                                                                               
OUTXPNX  J     EXIT                                                             
         DROP  RE                                                               
***********************************************************************         
* OUTPUT FREEFORM KEY                                                           
*                                                                               
* ON ENTRY:    (P1)                A(10 BYTE FREE FORM KEY)                     
*              (P2)                A(OUTPUT FIELD)                              
***********************************************************************         
OUTFFK   NTR1                                                                   
         LM    R2,R3,0(R1)                                                      
         MVC   0(L'SDRKKEY,R3),0(R2)  OUTPUT THE NAME                           
         TR    0(L'SDRKKEY,R3),DISPTBLL                                         
*                                                                               
OUTFFKX  J     EXIT                                                             
*                                                                               
* SAME TRT TABLE IN GEPFM00                                                     
DISPTBLL DC    XL16'6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F'  00-0F                    
         DC    XL16'6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F'  10-1F                    
         DC    XL16'6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F'  20-2F                    
         DC    XL16'6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F'  30-3F                    
         DC    XL16'406F6F6F6F6F6F6F6F6F4A4B4C4D4E4F'  40-4F                    
         DC    XL16'506F6F6F6F6F6F6F6F6F5A5B5C5D5E5F'  50-5F                    
         DC    XL16'60616F6F6F6F6F6F6F6F6A6B6C6D6E6F'  60-6F                    
         DC    XL16'6F6F6F6F6F6F6F6F6F797A7B7C7D7E7F'  70-7F                    
         DC    XL16'6F8182838485868788896F6F6F6F6F6F'  80-8F                    
         DC    XL16'6F9192939495969798996F6F6F6F6F9F'  90-9F                    
         DC    XL16'6FA1A2A3A4A5A6A7A8A96F6F6F6F6F6F'  A0-AF                    
         DC    XL16'6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F'  B0-BF                    
         DC    XL16'C0C1C2C3C4C5C6C7C8C96F6F6F6F6F6F'  C0-CF                    
         DC    XL16'D0D1D2D3D4D5D6D7D8D96F6F6F6F6F6F'  D0-D1                    
         DC    XL16'E06FE2E3E4E5E6E7E8E96F6F6F6F6F6F'  E0-EF                    
         DC    XL16'F0F1F2F3F4F5F6F7F8F96F6F6F6F6F6F'  F0-FF                    
***********************************************************************         
* TABLE FOR VALIDATING FACPAK                                                   
***********************************************************************         
FPKTAB   DS    0D                                                               
         DC    C'ADV',X'00'                                                     
         DC    C'TST',X'80'                                                     
         DC    C'FQA',X'84'                                                     
         DC    C'CSC',X'88'                                                     
         DC    C'REP',X'20'                                                     
         DC    X'FF'                  END OF TABLE                              
***********************************************************************         
* TABLE FOR VALIDATING SYSTEM                                                   
***********************************************************************         
STMTAB   DS    0D                                                               
         DC    C'SPT',X'02'                                                     
         DC    C'NET',X'03'                                                     
         DC    C'PRT',X'04'                                                     
         DC    C'ACC',X'06'                                                     
         DC    C'REP',X'08'                                                     
         DC    C'CON',X'0A'                                                     
         DC    X'FF'                  END OF TABLE                              
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
         LTORG                                                                  
***********************************************************************         
* DISPLAY THE CURRENT MAP ELEMENT                                               
*                                                                               
* ON ENTRY:    (R2)                A(CURRENT LINE'S SELECT FLDHDR)              
*              (R6)                A(MAP ELEMENT)                               
*              (R5)                A(ENTRY IN DISPLIST)                         
*              DLNSLEFT            # OF DISPLAY LINES LEFT                      
*                                                                               
* ON EXIT:     (R2)                A(CURRENT LINE COULD BE MODIFIED)            
*              DLNSLEFT            THIS CAN BE MODIFIED IS STRING               
***********************************************************************         
DISPELEM NTR1  BASE=*,LABEL=*                                                   
         USING DRLINED,R2                                                       
         USING SDELD,R6                                                         
         CLI   SDETYPE,03          CHARACTER STRING?                            
         JE    DCHAR00                                                          
         CLI   SDETYPE,12          NUMERIC DATA?                                
         JE    DUBIN00                                                          
         CLI   SDETYPE,08          HEXADECIMAL STRING?                          
         JE    DHEXD00                                                          
         J     DUNKN00             UNKNOWN                                      
***************                                                                 
* CHARACTER STRING                                                              
***************                                                                 
DCHAR00  BRAS  RE,DATADTLS         SHOW DATA DETAILS (R3 = DATA LEN)            
*                                                                               
         LA    R4,SDEDATA          R4 = A(DATA)                                 
DCHAR10  OI    DRMAPH+6,X'20'+X'80'           PROTECT FIELDS                    
         OI    DRDTYH+6,X'20'+X'80'                                             
         OI    DRDLNH+6,X'20'+X'80'                                             
         OI    DRDATH+6,X'20'+X'80'                                             
         MVC   0(2,R5),SVELDISP                                                 
*                                                                               
         CHI   R3,L'DRDAT          MORE DATA THAN CAN FIT ON A LINE?            
         BH    DCHAR20                                                          
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   DRDAT(0),0(R4)      NO, DISPLAY WHAT WE HAVE                     
         J     DSPELEMY                AND THEN PROCESS NEXT ELEMENT            
*                                                                               
DCHAR20  MVC   DRDAT,0(R4)         YES, SHOW WHAT WILL FIT ON THE LINE          
         LA    R4,L'DRDAT(R4)          & DISPLAY MORE ON NEXT LINE(S)           
         LLC   R1,DLNSLEFT              DECR # OF LINES LEFT                    
         BCTR  R1,0                                                             
         STC   R1,DLNSLEFT                                                      
         SHI   R3,L'DRDAT               DECR # OF BYTES LEFT TO DISPLAY         
         LA    R5,2(R5)                 R5 = A(NEXT ENTRY IN DISPLIST)          
*                                                                               
         LA    R2,SDMSEL2H-SDMSEL1H(R2) R2=A(NEXT LINE'S SELECT FLDHDR)         
         OI    DRSELH+6,X'20'+X'80'     PROTECT FIELDS AND TRANSMIT             
         J     DCHAR10                                                          
***************                                                                 
* NUMERIC DATA - WILL FIT ON ONE LINE                                           
***************                                                                 
DUBIN00  DS    0H                                                               
         BRAS  RE,DATADTLS        SHOW DATA DETAILS (R3 = DATA LEN)             
*                                                                               
         OI    DRMAPH+6,X'20'+X'80'                                             
         OI    DRDTYH+6,X'20'+X'80'                                             
         OI    DRDLNH+6,X'20'+X'80'                                             
         OI    DRDATH+6,X'20'+X'80'                                             
         MVC   0(2,R5),SVELDISP                                                 
*                                                                               
         LLC   R4,SDEDATA         ASSUME 1 BYTE NUMERIC FIRST                   
         CHI   R3,4               MORE THAN 4 BYTE BINARY?                      
         JH    DUBINERR           WE NOT DISPLAYING SUCH A NUMBER               
         CHI   R3,1                                                             
         JE    DUBINOUT                                                         
         ICM   R4,3,SDEDATA                                                     
         CHI   R3,2                                                             
         JE    DUBINOUT                                                         
         ICM   R4,7,SDEDATA                                                     
         CHI   R3,3                                                             
         JE    DUBINOUT                                                         
         ICM   R4,15,SDEDATA                                                    
*                                                                               
DUBINOUT EDIT  (R4),(10,DRDAT),ALIGN=LEFT                                       
         J     DSPELEMY                                                         
*                                                                               
DUBINERR MVC   DRDAT(26),=C'!! MORE THAN A FULLWORD !!'                         
         J     DSPELEMY                                                         
***************                                                                 
* HEXADECIMAL STRING                                                            
***************                                                                 
DHEXD00  BRAS  RE,DATADTLS        SHOW DATA DETAILS (R3 = DATA LEN)             
*                                                                               
         LA    R4,SDEDATA          R4 = A(DATA)                                 
DHEXD10  OI    DRMAPH+6,X'20'+X'80'           PROTECT FIELDS                    
         OI    DRDTYH+6,X'20'+X'80'                                             
         OI    DRDLNH+6,X'20'+X'80'                                             
         OI    DRDATH+6,X'20'+X'80'                                             
         MVC   0(2,R5),SVELDISP                                                 
*                                                                               
         CHI   R3,L'DRDAT/2        MORE DATA THAN CAN FIT ON A LINE?            
         BH    DHEXD20                                                          
         ST    R3,DMCB+8                                                        
         GOTO1 HEXOUT,DMCB,0(R4),DRDAT,,0                                       
         J     DSPELEMY                AND THEN PROCESS NEXT ELEMENT            
*                                                                               
*                                  YES, SHOW WHAT WILL FIT ON THE LINE          
DHEXD20  GOTO1 HEXOUT,DMCB,0(R4),DRDAT,L'DRDAT/2                                
         LA    R4,L'DRDAT/2(R4)        & DISPLAY MORE ON NEXT LINE(S)           
         LLC   R1,DLNSLEFT              DECR # OF LINES LEFT                    
         BCTR  R1,0                                                             
         STC   R1,DLNSLEFT                                                      
         SHI   R3,L'DRDAT/2             DECR # OF BYTES LEFT TO DISPLAY         
         LA    R5,2(R5)                 R5 = A(NEXT ENTRY IN DISPLIST)          
*                                                                               
         LA    R2,SDMSEL2H-SDMSEL1H(R2) R2=A(NEXT LINE'S SELECT FLDHDR)         
         OI    DRSELH+6,X'20'+X'80'     PROTECT FIELDS AND TRANSMIT             
         J     DHEXD10                                                          
***************                                                                 
* UNKNOWN DATA - WE'LL FIT IT ON ONE LINE                                       
***************                                                                 
DUNKN00  DS    0H                                                               
         BRAS  RE,DATADTLS        SHOW DATA DETAILS (R3 = DATA LEN)             
*                                                                               
         OI    DRMAPH+6,X'20'+X'80'                                             
         OI    DRDTYH+6,X'20'+X'80'                                             
         OI    DRDLNH+6,X'20'+X'80'                                             
         OI    DRDATH+6,X'20'+X'80'                                             
         MVC   0(2,R5),SVELDISP                                                 
*                                                                               
         MVC   DRDAT(18),=C'!! UNKNOWN TYPE !!'                                 
         J     DSPELEMY                                                         
*                                                                               
DSPELEMY XIT1  REGS=(R2,R5)          SEND (R2 & R5) BACK TO CALLER              
         DROP  R2,R6                                                            
***********************************************************************         
* OUTPUT DATA TYPE                                                              
*                                                                               
* ON ENTRY:    (P1)                A(DATA TYPE) 1 BYTE                          
*              (P2)                A(OUTPUT FIELD) 4 CHAR                       
***********************************************************************         
OUTDTY   NTR1                                                                   
         LM    R2,R3,0(R1)                                                      
         LA    RE,DTYTAB                                                        
         USING DTYTABD,RE                                                       
ODTY010  CLI   DTYNAME,X'FF'       EOT?                                         
         JE    ODTY0FF             SHOW NOTHING, TYPE NOT IN TABLE              
*                                                                               
         CLC   DTYHEX,0(R2)        MATCH ON INPUT SYSTEM?                       
         JE    ODTY020                                                          
         LA    RE,DTYTABLQ(RE)     CHECK NEXT SYSTEM                            
         J     ODTY010                                                          
*                                                                               
ODTY0FF  MVC   0(L'DTYNAME,R3),=C'????'  OUTPUT ???? FOR UNKNOWN                
         J     EXIT                                                             
*                                                                               
ODTY020  MVC   0(L'DTYNAME,R3),DTYNAME   OUTPUT THE NAME                        
*                                                                               
OUTDTYX  J     EXIT                                                             
***********************************************************************         
* TABLE FOR DDLINK DATA TYPES USED IN THE SELF DEFINING RECORD                  
***********************************************************************         
DTYTAB   DS    0D                                                               
         DC    C'CHAR',AL1(03)        CHARACTER STRING                          
         DC    C'UBIN',AL1(12)        UNSIGNED NUMBER                           
         DC    C'HEXD',AL1(08)        HEXD STRING (LESS COMMON)                 
         DC    C'UBIN',AL1(01)        ANOTHER UNSIGNED NUMBER                   
         DC    X'FF'                  END OF TABLE                              
***********************************************************************         
* DISPLAY DATA DETAILS                                                          
*                                                                               
* ON ENTRY     (R2)                A(CURRENT LINE'S SELECT FLDHDR)              
*              (R6)                A(CURRENT DATA ELEMENT)                      
*                                                                               
* ON EXIT      (R3)                L(DATA)                                      
***********************************************************************         
         USING DRLINED,R2                                                       
         USING SDELD,R6                                                         
DATADTLS NTR1                                                                   
         EDIT  (B1,SDEEL),(3,DRMAP),FILL=0    SHOW MAP CODE IN DEC              
         GOTO1 OUTDTY,DMCB,SDETYPE,DRDTY      SHOW DATA TYPE                    
         LLC   R3,SDELEN                      R3 = L(DATA)                      
         SHI   R3,SDELEN1Q                                                      
         EDIT  (R3),(3,DRDLN),FILL=0          SHOW THE DATA LENGTH              
         XIT1  REGS=(R3)                      SEND L(DATA) TO CALLER            
         DROP  R2,R6                                                            
         LTORG                                                                  
***********************************************************************         
* DSECT FOR TABLE FOR VALIDATING FACPAK                                         
***********************************************************************         
FPKTABD  DSECT                                                                  
FPKNAME  DS    CL3                 SHORT NAME FOR HUMAN READABLE                
FPKHEX   DS    XL1                 VALUE USED IN SELF DEFINING RECORD           
FPKTABLQ EQU  *-FPKTABD            LENGTH OF TABLE ENTRY                        
***********************************************************************         
* DSECT FOR TABLE FOR VALIDATING SYSTEM                                         
***********************************************************************         
STMTABD  DSECT                                                                  
STMNAME  DS    CL3                 SHORT NAME FOR HUMAN READABLE                
STMHEX   DS    XL1                 VALUE USED IN SELF DEFINING RECORD           
STMTABLQ EQU  *-STMTABD            LENGTH OF TABLE ENTRY                        
***********************************************************************         
* DSECT FOR TABLE FOR VALIDATING DDLINK DATA TYPE USED IN SDR                   
***********************************************************************         
DTYTABD  DSECT                                                                  
DTYNAME  DS    CL4                 SHORT NAME FOR TYPE                          
DTYHEX   DS    XL1                 ENUM (SEE LD_TYPE  IN  DDLINKD)              
DTYTABLQ EQU   *-DTYTABD           LENGTH OF TABLE ENTRY                        
*                                                                               
       ++INCLUDE FAXPTABD                                                       
         EJECT                                                                  
*                                                                               
       ++INCLUDE CTSFMFFD                                                       
*                                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE CTSFM9BD                                                       
*                                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE CTSFM9ED                                                       
*                                                                               
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE CTSFMWORKD                                                     
*                                                                               
         ORG   SYSSPARE                                                         
         DS    0D                                                               
VFXPCTBL DS    A                                                                
VRECUP   DS    A                                                                
*                                                                               
SVSDRKEY DS    XL(L'SDRKEY)        SAVED KEY                                    
SVFPKHEX DS    XL1                 FACPAK                                       
SVSYSHEX DS    XL1                 SYSTEM                                       
SVPRGHEX DS    XL2                 PROGRAM                                      
SVFFKHEX DS    XL10                FREEFORM KEY                                 
SVSQNHEX DS    XL1                 SEQUENCE NUMBER                              
SVACDISP DS    XL2                 ACTION ELEMENT DISPLACEMENT                  
SVELDISP DS    XL2                 LAST ELEMENT DISPLACEMENT                    
CURRDLEN DS    XL1                 CURRENT DATA LENGTH                          
DLNSLEFT DS    XL1                 NUMBER OF DISPLAY LINES LEFT                 
DISPLIST DS    0XL(16*2)           LIST OF DISPLACEMENTS TO ELEMS               
         DS    16XL2               FFFF=CONTINUED  0000=END OF LIST             
BITFLAG1 DS    XL1                 MISC BIT FLAGS SET 1                         
BF1HADAC EQU   X'80'               - WE ALREADY PROCESSED AN ACTION             
*                                                                               
         EJECT                                                                  
       ++INCLUDE GEGENSDR                                                       
*********************                                                           
* DISPLAY RECORD LINE                                                           
*********************                                                           
DRLINED  DSECT                                                                  
DRSELH   DS    XL(L'SDMSEL1H)                                                   
DRSEL    DS    CL(L'SDMSEL1)                                                    
DRMAPH   DS    XL(L'SDMMAP1H)                                                   
DRMAP    DS    CL(L'SDMMAP1)                                                    
DRDTYH   DS    XL(L'SDMDTY1H)                                                   
DRDTY    DS    CL(L'SDMDTY1)                                                    
DRDLNH   DS    XL(L'SDMDLN1H)                                                   
DRDLN    DS    CL(L'SDMDLN1)                                                    
DRDATH   DS    XL(L'SDMDAT1H)                                                   
DRDAT    DS    CL(L'SDMDAT1)                                                    
DRLINEX  DS    0X                                                               
*********************                                                           
* ON-SCREEN LIST LINE                                                           
*********************                                                           
LSLINED  DSECT                                                                  
LSLINE   DS    0CL73                                                            
LSFPK    DS    CL3                 Facpak                                       
         DS    CL1                                                              
LSSYS    DS    CL3                 System                                       
         DS    CL2                                                              
LSPRGC   DS    CL10                Program Name                                 
         DS    CL2                                                              
LSPRGH   DS    CL4                 Program Hex FAXPEQUS                         
         DS    CL3                                                              
LSFFKC   DS    CL10                Free Form Key in Char                        
         DS    CL2                                                              
LSFFKH   DS    CL20                Free Form Key in Hex                         
         DS    CL2                                                              
LSSEQN   DS    CL3                                                              
         EJECT                                                                  
* GEGENSTSRC                                                                    
* CTGENFILE                                                                     
* DDSPOOLD                                                                      
* DDCOMFACS                                                                     
* DDFLDIND                                                                      
* FAGETTXTD                                                                     
         PRINT OFF                                                              
       ++INCLUDE GEGENSTSRC                                                     
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDFLDIND                                                       
       ++INCLUDE FAGETTXTD                                                      
       ++INCLUDE DDACTIVD                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'125CTSFM5E   06/03/20'                                      
         END                                                                    
