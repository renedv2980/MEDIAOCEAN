*          DATA SET PPEZF08    AT LEVEL 075 AS OF 05/01/02                      
*PHASE T43008A,*                                                                
*INCLUDE BINSRCH2                                                               
*        TITLE 'T43008 - EPIC INVOICE DETAILS'                                  
         TITLE 'T43008 - EPIC INVOICE DETAILS'                                  
***********************************************************************         
*                                                                     *         
*  TITLE: T43008 - PRINT EPIC INVOICE DETAILS MAINT                   *         
*  COMMENTS: THIS PROGRAM ALLOWS USER TO ENTER THE PRODUCT AND/OR     *         
*            ESTIMATE NUMBER FOR AN INVOICE DETAIL                    *         
*            IT USES THE SAME LIST SCREEN AS THE CONVERT RECORD AND   *         
*            IS ACCESSED BY ENTERING A SPECIAL SELECT CODE.           *         
*                                                                     *         
*  OUTPUTS: UPDATED INVOICE DETAILS                                   *         
*  LOCALS: REGISTER USAGE                                             *         
*          R2 - POINTER TO SCREEN FLDH, ERREX WILL POSITION CUSOR     *         
*          R3 - WORK REG                                              *         
*          R4 - WORK REG & KEY DSECT POINTER                          *         
*          R5 - WORK REG & POINTER TO INVOICE RECORD                  *         
*          R6 - USED FOR GETEL ELEMENT DSECT POINTER, PZBLOCK         *         
*          R7 - SECOND BASE                                           *         
*          R8 - POINTER TO SPOOLD                                     *         
*          R9 - POINTER TO SYSD                                       *         
*          RA - POINTER TO ATWA                                       *         
*          RB - FIRST BASE                                            *         
*          RC - POINTER TO GEND                                       *         
* AIO USAGE - AIO1 - VALI RTNS IN CONTROLLER (PPPZF00-T43000)         *         
*                  - IN AIO FROM HYPER CONTROLLER (T00A30)            *         
*             AIO2     - WORKER RECORD                                *         
*             AIO3     - PZBLOCK                                      *         
*             WRKFBUFR - WORKER BUFFER                                *         
*                      - INDEX RECORD                                 *         
*                                                                     *         
***********************************************************************         
         TITLE 'T43008 - INVOICE DETAILS - INITIALIZATION'                      
***********************************************************************         
*                                                                     *         
*        INITIALIZATION                                               *         
*                                                                     *         
***********************************************************************         
T43008   CSECT                                                                  
         PRINT NOGEN                                                            
         NMODL (ELTMAX*ELTABL),**3008**,RR=R2                                   
         LR    RF,RC               SAVE A(WORKING STORAGE)                      
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    RF,ELTABA           SAVE A(ELEMENT TABLE)                        
         ST    RC,SVRC                                                          
         ST    R2,RELO                                                          
         MVC   AIO,AIO1                                                         
         MVI   IOOPT,C'Y'          USER DOING ALL I/O                           
         CLC   =A(WRKFEND-SYSD),LSYSD                                           
         BNH   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R1,=A(WRKFBUFR-SYSD)   SET UP WORKER BUFFER ADDRESS              
         LA    R1,SYSD(R1)                                                      
         ST    R1,WRKFBUFA                                                      
*                                                                               
         L     R7,=A(SUBROUTS)     SET UP ADDRESSABILITY TO SUBROUTINES         
         A     R7,RELO             RELOCATE ADDRESS                             
         USING SUBROUTS,R7         ESTABLISH ADDRESSABILITY                     
         MVI   IPSTAT,0            INIT INPUT STATISTICS                        
         MVI   SAVMSGNO,0          INIT MESSAGE NUMBER SAVEAREA                 
         MVI   ERROR,0             INIT MESSAGE NUMBER                          
*                                                                               
*        CHECK TO SEE IF IN MIDDLE OF HELP CALL                                 
*                                                                               
         LA    R6,HELPCBLK         ESTABLISH HELP CONTROL BLOCK                 
         USING HELPCB,R6                                                        
*                                                                               
         XC    HELPCB(HELPCBL),HELPCB  INIT HELP CONTROL BLOCK                  
*                                                                               
         L     RF,=A(HELPSAVE-(CONHEADH-64))                                    
         LA    RF,0(RF,RA)                                                      
         ST    RF,HCBASAVE         PASS ADDRESS                                 
         MVC   HCBMTIC,=AL2(PRQHLPMN)  USE PRINT HELP MENUS                     
         MVI   HCBSCRN,X'D2'       SCREEN TO DISPLAY MENUS ON                   
         MVI   HCBPAGE,1           SAVE IN FIRST TWA PAGE                       
         ST    RA,HCBATWA          SET SYSTEM ADDRESSES                         
         MVC   HCBACOM,ACOMFACS                                                 
         L     R1,SYSPARMS         POINT TO TIOBD                               
         MVC   HCBATIOB,0(R1)                                                   
         MVC   HCBASYRD,SYSRD                                                   
         MVC   HCBCTRY,CTRY        SET COUNTRY                                  
         MVC   HCBLANG,LANG        SET LANGUAGE                                 
         MVI   HCBSYST,4           SET SYSTEM AS PRINT                          
         MVI   HCBSEGS,10          DDVAL TAB AREA IS 10*256 LONG                
         MVC   HCBATAB,WRKFBUFA    SET A(DDVAL TABLE)                           
*                                                                               
         GOTO1 VPRHELP,DMCB,0,0,HELPCBLK GO CHECK IF IN MIDDLE OF MENU          
*                                                                               
         TM    HCBRTRN,HCBRSELQ    IF SELECTION WAS MADE                        
         BNO   INIT10                                                           
*                                                                               
         GOTO1 ERREX2                 EXIT FOR USER TO COMPLETE ENTRY           
*                                                                               
         DROP  R6                                                               
*                                                                               
INIT10   DS    0H                                                               
*                                                                               
*===>                                                                           
         MVI   CONSERVH+6,X'81'    FORCE SRV REQ FIELD MODIFIED                 
*===>                                                                           
         TITLE 'T43008 - INVOICE DETAILS - MODE'                                
***********************************************************************         
*                                                                     *         
*        DETERMINE PROCESSING MODE                                    *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VKEY                                                             
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VREC                                                             
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DKEY                                                             
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DREC                                                             
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    MODINVAL                                                         
         CLI   MODE,PRINTREP       PRINT RECORDS                                
         BE    MODINVAL                                                         
         CLI   MODE,RECADD         ADD RECORD                                   
         BE    MODINVAL                                                         
         CLI   MODE,RECDEL         DEL RECORD                                   
         BE    MODINVAL                                                         
*                                                                               
         B     XIT                 IGNORE                                       
*                                                                               
MODINVAL MVI   ERROR,INVACT        INVALID MODE                                 
         LA    R2,CONACTH                                                       
         B     MODERRX                                                          
*                                                                               
MODERRX  DS    0H                                                               
         GOTO1 ERREX                                                            
*                                                                               
XIT      XIT1                                                                   
*                                                                               
         TITLE 'T43008 - INVOICE DETAILS - VKEY'                                
***********************************************************************         
*                                                                     *         
*        VKEY - VALIDATE KEY                                          *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VKEY     DS    0H                                                               
*                                                                               
         MVI   USEIO,C'Y'                                                       
*                                                                               
         XC    WRKSQN,WRKSQN       INIT ENTERED SEQUENCE NUMBER                 
         MVC   DMDSKADD,=X'0000FFFF' RESET DISK ADDRESS                         
*                                                                               
*        ALWAYS VALIDATE MEDIA TO GET VIABLE KEY FOR GENCON                     
*                                                                               
         MVI   CURSYST,C'M'        INDICATE WE ARE IN A MEDIA SYS-PRINT         
*                                                                               
         GOTO1 VALIFAS             SWITCH TO MEDIA SYSTEM                       
*                                                                               
         LA    R2,DTLMEDH          POINT TO MEDIA FIELD HEADER                  
*                                                                               
         GOTO1 VALIMED             VALIDATE MEDIA                               
*                                                                               
         MVC   MEDKEYSV,KEY        SAVE MEDIA KEY                               
*                                                                               
VKMEDX   DS    0H                                                               
*                                                                               
         OI    DTLMEDH+4,X'20'     INDICATE VALID FIELD                         
         OI    DTLMEDH+6,X'80'     TRANSMIT FIELD                               
*                                                                               
*                                                                               
*        IF NO FIELDS ARE CHANGED, JUST KEEP ON WITH DISPLAY *                  
*                                                                               
         TM    DTLDFTRH+4,X'20'    FILTERS                                      
         BZ    VK000                                                            
         TM    DTLMEDH+4,X'20'     BATCH MEDIA                                  
         BZ    VK000                                                            
         TM    DTLPUBH+4,X'20'     BATCH PUB                                    
         BZ    VK000                                                            
         TM    DTLBDTH+4,X'20'     BATCH DATE                                   
         BZ    VK000                                                            
         TM    DTLBSQH+4,X'20'     BATCH SEQ                                    
         BZ    VK000                                                            
         TM    DTLSQNH+4,X'20'     SEQUENCE NUMBER                              
         BZ    VK000                                                            
*                                                                               
         NI    GENSTAT2,X'FF'-RETEQSEL   SET NO RETURN THIS SELECTION           
*                                                                               
         B     VKXIT               NO CHANGES TO SCREEN - EXIT                  
*                                                                               
*        VALIDATE KEY FIELDS                                                    
*                                                                               
VK000    MVI   NEWDISP,C'N'        ASSUME NO RE-DISPLAY                         
*                                                                               
         MVI   NEWKEY,C'Y'         ASSUME CHANGE IN KEY                         
*                                                                               
*        VALIDATE PUB                                                           
*                                                                               
VKPUB    DS    0H                                                               
*                                                                               
         LA    R2,DTLPUBH          POINT TO PUB FIELD                           
*                                                                               
         XC    RQPUB,RQPUB         KILL FILTER PUB                              
*                                                                               
         CLI   5(R2),0             SKIP IF PUB NOT ENTERED                      
         BE    VKPUBX                                                           
*                                                                               
         GOTO1 VALIPUB             VALIDATE PUB ENTRY                           
*                                                                               
         CLC   RQPUB,QPUB          IF PUB HAS CHANGED                           
         BE    VKPUBX                                                           
*                                                                               
         MVI   NEWDISP,C'Y'           FORCE RE-DISPLAY OF SCREEN                
*                                                                               
         MVC   RQPUB,QPUB          SAVE PUB ID                                  
*                                                                               
         XC    DTLPUBN,DTLPUBN     PUT OUT NEW PUBNAME                          
         MVC   DTLPUBN(L'PUBPNM),PUBPNM                                         
         OI    DTLPUBNH+6,X'80'    FORCE FIELD TRANSMISSION                     
*                                                                               
         XC    DTLZONE,DTLZONE     PUT OUT NEW ZONE NAME                        
         MVC   DTLZONE(L'PUBPZNM),PUBPZNM                                       
         OI    DTLZONEH+6,X'80'    FORCE FIELD TRANSMISSION                     
*                                                                               
VKPUBX   OI    4(R2),X'20'         INDICATE PUB IS VALID                        
*                                                                               
         B     VK200               DO NOT VALIDATE ON DETAIL SCREEN             
*                                                                               
         LA    R2,DTLBDTH          POINT TO BATCH DATE                          
*                                                                               
         MVI   DTPLUS,C'N'         ASSUME EXACT DATE ENTERED                    
*                                                                               
         CLI   5(R2),0             IF NO DATE                                   
         BNE   VK120                                                            
*                                                                               
         MVI   NEWDISP,C'Y'        THEN NEW BATCH - FORCE RE-DISPLAY            
         XC    RQDTE,RQDTE                                                      
*                                                                               
         B     VK200                                                            
*                                                                               
VK120    ZIC   RF,5(R2)               LOOK FOR FINAL +                          
*                                                                               
         LA    RE,FHDRLEN-1(R2,RF)    POINT TO LAST CHAR                        
*                                                                               
         CLI   0(RE),C'+'          IF DATE ENDS IN '+'                          
         BNE   VK130                                                            
*                                                                               
         MVI   0(RE),C' '             ELIMINATE '+' FROM DATE                   
         BCTR  RF,0                   DECREMENT INPUT LENGTH                    
         STC   RF,5(R2)                                                         
         MVI   DTPLUS,C'Y'            LOOK FOR THIS DATE OR LATER               
*                                                                               
VK130    GOTO1 DATVAL,DMCB,(0,FHDRLEN(R2)),WORK VALIDATE LENGTH                 
*                                                                               
         OC    DMCB,DMCB           MUST HAVE A VALID DATE                       
         BZ    VKBADATE                                                         
*                                                                               
         GOTO1 DATCON,(R1),(0,WORK),(2,DUB)  CONVERT TO COMPRESSED DATE         
*                                                                               
         CLC   RQDTE,DUB           IF DATE WAS CHANGED                          
         BE    *+8                                                              
         MVI   NEWDISP,C'Y'           FORCE SCREEN RE-DISPLAY                   
*                                                                               
         MVC   RQDTE,DUB                                                        
*                                                                               
VK200    OI    4(R2),X'20'         INDICATE DATE IS VALID                       
*                                                                               
*        VALIDATE SEQUENCE NUMBER                                               
*                                                                               
         LA    R2,DTLBSQH          SEQUENCE NUMBER FIELD                        
*                                                                               
         CLI   5(R2),0             IF NO INPUT                                  
         BNE   VK210                                                            
*                                                                               
         MVI   NEWDISP,C'Y'           NEW BATCH-FORCE SCREEN RE-DISPLAY         
         XC    RQSEQ,RQSEQ            INIT SEQUENCE NUMBER                      
         XC    RQBSEQ,RQBSEQ                                                    
*                                                                               
         B     VK300                                                            
*                                                                               
VK210    MVC   WORK(8),=8C'0'      VALIDATE SQN AS NUMERIC                      
*                                                                               
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,VKMVN                                                         
         EX    R1,VKCLC                                                         
         BNE   VKNUMERR                                                         
*                                                                               
         EX    R1,VKPK             PACK SQN                                     
         OI    DUB+7,X'0F'         FORCE SIGN                                   
         CVB   R0,DUB              CVB                                          
         UNPK  FULL,DUB            FILL OUT TO 8 DIGITS                         
*                                                                               
         CLC   RQSEQ,FULL          IF SQN HAS CHANGED                           
         BE    *+8                                                              
         MVI   NEWDISP,C'Y'           FORCE RE-DISPLAY OF SCREEN                
*                                                                               
         MVC   RQSEQ,FULL          SAVE SEQUENCE NUMBER                         
*                                                                               
         CLM   R0,3,DKBSEQ         SKIP UPDATE IF SAME AS BKEY VALUE            
         BE    *+8                                                              
         STCM  R0,3,RQBSEQ                                                      
*                                                                               
         B     VK300                                                            
*                                                                               
VKMVN    MVN   WORK(0),8(R2)       EXECUTED INSTRUCTIONS TO VALIDATE            
VKCLC    CLC   WORK(0),8(R2)         NUMERIC                                    
VKPK     PACK  DUB,8(0,R2)                                                      
*                                                                               
VK300    OI    4(R2),X'20'         INDICATE SQN FIELD IS VALID                  
*                                                                               
VKXIT    DS    0H                  INIT KEY AREA                                
*                                                                               
         MVI   CURSYST,C'M'        INDICATE WE ARE IN A MEDIA SYS-PRINT         
*                                                                               
         GOTO1 VALIFAS             SWITCH TO MEDIA SYSTEM                       
*                                                                               
         MVC   KEY,MEDKEYSV        RETURN MEDIA KEY                             
*                                                                               
         B     XIT                                                              
*                                                                               
VKNUMERR MVI   ERROR,NOTNUM        NOT NUMERIC                                  
         B     VKERRX                                                           
*                                                                               
VKBADATE MVI   ERROR,INVDATE       INVALID DATE                                 
         B     VKERRX                                                           
*                                                                               
VKERRX   DS    0H                                                               
         GOTO1 ERREX                                                            
*                                                                               
         TITLE 'T43008 - INVOICE DETAILS - DREC'                                
***********************************************************************         
*                                                                     *         
*        DREC - DISPLAY RECORD                                        *         
*        DISPLAY DETAILS VIA LINUP                                    *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
DREC     DS    0H                                                               
*                                                                               
         CLI   PFAID,0             IF PFKEY HIT                                 
         BE    DREC1                                                            
*                                                                               
         GOTO1 =A(VPFKEY),RR=RELO     GO ANALYZE                                
*                                                                               
         BE    XIT                 PFKEY ACTION TAKEN                           
*                                                                               
DREC1    DS    0H                                                               
*                                                                               
         GOTO1 =A(LINSET),RR=RELO    INTERFACE WITH LINUP                       
*                                                                               
         MVI   NEWKEY,0            RESET NEWKEY SWITCH                          
*                                                                               
         XC    WRKSQN,WRKSQN       INIT ENTERED SEQUENCE NUMBER                 
*                                                                               
DRECX    DS    0H                                                               
         B     XIT                                                              
*                                                                               
         TITLE 'T43008 - INVOICE DETAILS - VREC'                                
***********************************************************************         
*                                                                     *         
*        VALIDATE DETAIL INPUT VIA LINUP                              *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VREC     DS    0H                                                               
*                                                                               
         CLI   PFAID,0             IF PFKEY HIT                                 
         BE    VREC1                                                            
*                                                                               
         GOTO1 =A(VPFKEY),RR=RELO     GO ANALIZE                                
*                                                                               
         BE    XIT                 PFKEY ACTION TAKEN                           
*                                                                               
VREC1    DS    0H                                                               
*                                                                               
         LA    R0,SVLSVTAB         SET UP FOR MVCL                              
         LA    R1,L'SVLSVTAB                                                    
         LA    RE,LSVTAB                                                        
         LR    RF,R1                                                            
         MVCL  R0,RE               SAVE SAVE COPY OF LINUP SAVE TABLE           
*                                  TO RESTORE AFTER VALIDATION ERRORS           
         GOTO1 =A(LINSET),RR=RELO  LINUP INTERFACE                              
*                                                                               
         MVI   NEWKEY,0            RESET NEWKEY SWITCH                          
*                                                                               
         XC    WRKSQN,WRKSQN       INIT ENTERED SEQUENCE NUMBER                 
*                                                                               
VRECX    DS    0H                                                               
         B     XIT                                                              
*                                                                               
         TITLE 'T43008 - INVOICE DETAILS - DKEY'                                
***********************************************************************         
*                                                                     *         
*        DKEY - DISPLAY KEY                                           *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
DKEY     DS    0H                                                               
*                                                                               
*        DISPLAY KEY                                                            
*                                                                               
         MVI   NEWKEY,C'Y'         ASSUME CHANGE IN KEY                         
*                                                                               
*        DETAIL FILTERS LINE                                                    
*                                                                               
         CLI   THISLARG,C'X'       SKIP IF NO PRODUCT FILTER WANTED             
         BNE   DKDFLTX                                                          
*                                                                               
         MVC   SVDFLTR(7),=C'PCODE=X'  SET FILTER                               
         MVI   SVDFLTRL,7                                                       
*                                                                               
         LA    R2,DTLDFTRH         POINT TO DETAIL FILTERS                      
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,1,SVDFLTRL       DETAIL FILTERS EXECUTE LENGTH                
         BCTR  R1,0                DECREMENT FOR EXECUTE                        
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   DTLDFTR(0),SVDFLTR  SHOW DETAIL FILTERS                          
*                                                                               
         MVC   5(1,R2),SVDFLTRL    SET LENGTH OF ENTERED FILTERS                
*                                                                               
         NI    4(R2),X'FF'-X'20'   INDICATE FILTERS JUST ENTERED                
         OI    6(R2),X'80'         TRANSMIT FIELD                               
*                                                                               
DKDFLTX  DS    0H                                                               
*                                                                               
         ZIC   R5,SELLISTN         GET LINE NUMBER                              
         MH    R5,=Y(INVENTL)                                                   
         LA    R5,INVLIST(R5)                                                   
         USING INVLISTD,R5         ESTABLISH ENTRY IN INVOICE TABLE             
*                                                                               
         MVC   SVLSTBAT(SVLSTBTL),INVLIST  FORCES RE-DISPLAY OF LAST            
         MVC   SVINVLST(SVLSTBTL),INVLISTD SETS PLACE IN INVOICE TABLE          
*                                     LIST SCREEN                               
         MVC   DTLMED(1),INVMEDIA                                               
         OI    DTLMEDH+6,X'80'     TRANSMIT                                     
         OI    DTLMEDH+4,X'20'     INDICATE VALIDATED                           
*                                                                               
         GOTO1 VPUBEDIT,DMCB,(C'0',INVPUB),(C'S',DTLPUB) PUB CODE               
*                                                                               
         LA    R2,DTLPUBH          POINT TO PUB FIELD                           
*                                                                               
         LA    R0,L'DTLPUB         MAX FIELD LENGTH                             
         LA    R1,8-1+L'DTLPUB(R2) LAST BYTE OF FIELD                           
*                                                                               
         CLI   0(R1),C' '          FIND END OF FIELD                            
         BH    *+10                                                             
         BCTR  R1,0                BACK UP A BYTE                               
         BCT   R0,*-10                                                          
*                                                                               
         STC   R0,5(R2)            SET FIELD LENGTH                             
*                                                                               
         MVI   CURSYST,C'M'        MEDIA (SPOT/NET)                             
         GOTO1 VALIFAS             SWITCH                                       
*                                                                               
         GOTO1 VALIPUB             VALIDATE PUB ENTRY                           
*                                                                               
         OI    DTLPUBH+4,X'20'     FLAG AS VALIDATED FIELD                      
*                                                                               
         XC    DTLPUBN,DTLPUBN     PUT OUT NEW PUBNAME                          
         MVC   DTLPUBN(L'PUBPNM),PUBPNM                                         
         OI    DTLPUBNH+6,X'80'    FORCE FIELD TRANSMISSION                     
*                                                                               
         XC    DTLZONE,DTLZONE     PUT OUT NEW ZONE NAME                        
         MVC   DTLZONE(L'PUBPZNM),PUBPZNM                                       
         OI    DTLZONEH+6,X'80'    FORCE FIELD TRANSMISSION                     
*                                                                               
         XC    WORK(L'DTLBDT),WORK          BATCH DATE                          
         GOTO1 DATCON,DMCB,(2,INVBDTE),(5,WORK)                                 
*                                                                               
         CLC   DTLBDT,WORK                                                      
         BE    *+14                                                             
         MVC   DTLBDT,WORK                                                      
         OI    DTLBDTH+6,X'80'                                                  
*                                                                               
         OI    DTLBDTH+4,X'20'     INDICATE VALIDATED                           
*                                                                               
         XC    ELEM(L'DTLBSQ),ELEM          BATCH SEQ                           
         SR    R0,R0                                                            
         ICM   R0,3,INVBSEQ                                                     
         EDIT  (R0),(5,ELEM),ALIGN=LEFT                                         
         MVC   DKBSEQ,INVBSEQ      SAVE DISPLAYED BATCH SEQ NUMBER              
*                                                                               
         CLC   DTLBSQ,ELEM                                                      
         BE    *+14                                                             
         MVC   DTLBSQ,ELEM                                                      
         OI    DTLBSQH+6,X'80'                                                  
*                                                                               
         OI    DTLBSQH+4,X'20'     INDICATE VALIDATED                           
*                                                                               
         OI    DTLSCRLH+4,X'20'    INDICATE A VALIDATED FIELD                   
         OI    DTLSCRLH+6,X'80'    FORCE TRANSMITTAL                            
*                                                                               
DKEYX    DS    0H                                                               
*                                                                               
         B     XIT                                                              
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  R5                                                               
*                                                                               
         TITLE 'T43008 - INVOICE DETAILS - VPFKEY'                              
***********************************************************************         
*                                                                     *         
*        PFKEY ANALYSIS                                               *         
*                                                                     *         
*        PFKEY8 - MBC                                                 *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
VPFKEY   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   PFAID,9             IF PFKEY 9                                   
         BE    *+8                                                              
         CLI   PFAID,21            OR PFKEY 21 HIT                              
         BNE   VPFKMBCN                                                         
*                                     TRANSFER TO MBC                           
*                                                                               
         CLC   DTLCCD,SPACES      MUST HAVE A CLIENT CODE                       
         BNH   VPFKEMBC                                                         
*                                                                               
         L     R1,ACOMFACS         GET GLOBBER'S ADDRESS                        
         MVC   VGLOBBER,CGLOBBER-COMFACSD(R1)                                   
*                                                                               
*        SET UP TRANSFER CONTROL BLOCK                                          
*                                                                               
         XC    FLD,FLD             INIT WORK AREA                               
         LA    R1,FLD                                                           
         USING GLVXFRSY,R1                                                      
*                                                                               
         MVC   GLVXFRSY,=C'PRI'    FROM THE PRINT SYSTEM                        
         MVC   GLVXFRPR,=C'EPI'    EPIC  PROGRAM                                
         MVC   GLVXTOSY,=C'PRI'    TO THE PRINT SYSTEM                          
         MVC   GLVXTOPR,=C'MBC'    MBC PROGRAM                                  
         OI    GLVXFLG1,GLV1SEPS   CALL BASE ON TRANSFER                        
*                                                                               
******** OI    GLVXFLG1,GLV1GOTO+GLV1SEPS   CALL BASE ON TRANSFER               
* PLEASE NOTICE THAT ABOVE INSTRUCTION IS NO-OPED DUE TO PROBLEMS               
* WITH GLV1GOTO EQUATE                                                          
*                                                                               
         DROP  R1                                                               
*                                                                               
*        SEND TRANSFER CONTROL FIELD                                            
*                                                                               
         GOTO1 VGLOBBER,DMCB,=C'PUTD',FLD,14,GLVXCTL                            
         CLI   DMCB+8,0                                                         
         BNE   VPFKEMBC                                                         
*                                                                               
*        SEND MEDIA                                                             
*                                                                               
         GOTO1 VGLOBBER,DMCB,=C'PUTD',DTLMED,1,GLVPRMD                          
         CLI   DMCB+8,0                                                         
         BNE   VPFKEMBC                                                         
*                                                                               
*        SEND CLIENT                                                            
*                                                                               
         GOTO1 VGLOBBER,DMCB,=C'PUTD',DTLCCD,3,GLVPRCLT                         
         CLI   DMCB+8,0                                                         
         BNE   VPFKEMBC                                                         
*                                                                               
*        SEND PRODUCT AS 'ALL'                                                  
*                                                                               
         MVC   FLD(3),=C'ALL'                                                   
         GOTO1 VGLOBBER,DMCB,=C'PUTD',FLD,3,GLVPRPRD                            
         CLI   DMCB+8,0                                                         
         BNE   VPFKEMBC                                                         
*                                                                               
*        SEND ESTIMATE AS 'ALL'                                                 
*                                                                               
         MVC   FLD(3),=CL3'ALL'                                                 
         GOTO1 VGLOBBER,DMCB,=C'PUTD',FLD,3,GLVPREST                            
         CLI   DMCB+8,0                                                         
         BNE   VPFKEMBC                                                         
*                                                                               
*        SEND PUB                                                               
*                                                                               
         MVC   FLD,SPACES          INIT WORKAREA                                
*                                                                               
         LA    R0,L'DTLPUB         MAX PUB ID LENGTH                            
         LA    RF,DTLPUB                                                        
*                                                                               
         CLI   0(RF),C' '          FIND END OF BASE PUB ID                      
         BNH   *+24                                                             
         CLI   0(RF),C','                                                       
         BE    *+16                                                             
         LA    RF,1(RF)                                                         
         BCT   R0,*-20                                                          
         B     VPFKEMBC            NO PUB ID                                    
*                                                                               
         LA    RF,L'DTLPUB                                                      
         SR    RF,R0               LENGTH OF BASE PUB ID                        
         BNP   VPFKEMBC            NO PUB ID                                    
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   FLD(0),DTLPUB       MOVE BASE PUB ID TO WORKAREA                
*                                                                               
         LA    R1,FLD+1(RF)        POINT TO END OF ID                           
         MVC   0(4,R1),=C',ALL'    FORCE TO ALL EDITIONS                        
         LA    RF,5(RF)            LENGTH OF PUBID                              
*                                                                               
         GOTO1 VGLOBBER,DMCB,=C'PUTD',FLD,(RF),GLVPRPUB                         
         CLI   DMCB+8,0                                                         
         BNE   VPFKEMBC                                                         
*                                                                               
*        SEND PERIOD                                                            
*                                                                               
         LA    R4,FLD              ESTABLISH PERVAL CONTROL BLOCK               
         USING PERVALD,R4          ESTABLISH PERVAL WORKAREA                    
*                                                                               
         L     RF,ACOMFACS         GET PERVAL ADDRESS                           
         L     RF,CPERVAL-COMFACSD(RF)                                          
*                                                                               
         LA    R2,DTLMOSH          POINT TO MONTH OF SERVICE                    
         GOTO1 (RF),DMCB,(5,8(R2)),(0,(R4)) VALIDATE PERIOD                     
         CLI   4(R1),0             CHECK FOR ERRORS                             
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 VGLOBBER,DMCB,=C'PUTD',PVALESTA,12,GLVPRPER                      
         CLI   DMCB+8,0                                                         
         BNE   VPFKEMBC                                                         
*                                                                               
         MVC   FLD(16),=CL16'PAYABLE,STATUS=L'     PUT OUT MBC DATA             
         GOTO1 VGLOBBER,DMCB,=C'PUTD',FLD,16,GLVPRDTA                           
         CLI   DMCB+8,0                                                         
         BNE   VPFKEMBC                                                         
*                                                                               
         LA    R2,CONSERVH                                                      
         OI    6(R2),X'80'         FORCE SERVICE REQUEST FIELD TRANSMIT         
*                                                                               
         L     R1,SYSPARMS         GET A(TIOB)                                  
         L     R1,0(R1)                                                         
         MVC   DUB(2),TIOBCURD-TIOBD(R1)                                        
         LR    R1,RA                                                            
         AH    R1,DUB                                                           
         OI    6(R1),X'C0'         PUT CURSOR WHERE IT CAME IN                  
*                                                                               
VPFKMBCX DS    0H                                                               
*                                                                               
         CR    RB,RB               PFKEY ACTION TAKEN                           
*                                                                               
         B     VPFKX                                                            
*                                                                               
VPFKMBCN DS    0H                                                               
*                                                                               
         LTR   RB,RB               NO PFKEY ACTION TAKEN                        
*                                                                               
VPFKX    DS    0H                                                               
         XIT1                                                                   
*                                                                               
*        TRANSFER NOT POSSIBLE ERROR                                            
*                                                                               
VPFKEMBC DS    0H                                                               
*                                                                               
         MVI   ERROR,PZENOMBC      CAN'T SWITCH TO THE MBC PROGRAM              
         LA    R2,DTLPCDH          SET CURSOR POSITION                          
         GOTO1 ERREX                                                            
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T43008 - INVOICE DETAILS - PZMOD'                               
***********************************************************************         
*                                                                     *         
*        PRPZ - FIND RECORD, CALL PZMOD                               *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
PRPZ     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         ICM   R0,15,BSPNOR                                                     
         OC    VPZMOD,VPZMOD       SKIP IF PZMOD LOADED ALREADY                 
         BNZ   PRPZ1                                                            
*                                                                               
         GOTO1 CALLOV,DMCB,(X'10',0),ATWA    GET PZMOD                          
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RF,DMCB                                                          
         ST    RF,VPZMOD                                                        
*                                                                               
PRPZ1    DS    0H                                                               
*                                                                               
         MVI   USEIO,C'Y'                                                       
*                                                                               
         XC    CURWKIXD,CURWKIXD   ESTABLISH WORKER FILE INDEX                  
         LA    R4,CURWKIXD                                                      
         USING EZWKRIXD,R4                                                      
*                                                                               
         ZIC   RF,SELLISTN         RELATIVE LINE NUMBER                         
         MH    RF,=Y(INVENTL)                                                   
         LA    R5,INVLIST(RF)                                                   
         USING INVLISTD,R5         ESTABLISH SAVED INVOICE AREA                 
*                                                                               
         CLI   PFAID,0             IF PFKEY ENTERED                             
         NOP   *+8                                                              
         LA    R5,SVINVLST            USE INVOICE ENTRY FROM LAST TIME          
*                                                                               
         MVC   EZWIUID,INVUID      SET WORKER FILE USER ID                      
*                                                                               
         MVC   SRCEPUB,INVPZPUB    PUB                                          
         MVC   SRCEMED,INVMEDIA    MEDIA                                        
*                                                                               
         MVC   EZWIPUB,INVPZPUB    WORKER FILE PUB ID                           
         MVC   EZWIMED,INVMEDIA    MEDIA                                        
         MVI   EZWIDAY,X'98'       DAY = 98                                     
*                                                                               
         MVC   EZWKRIXD+8(2),INVBSEQ    BATCH SEQUENCE NUMBER WANTED            
*                                                                               
         DROP  R5                                                               
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'INDEX',EPICWK,EZWKRIXD,AIO2,WRKFBUFA             
         TM    DMCB+8,X'80'        TEST EOF                                     
         BZ    *+6                                                              
         DC    H'0'                BATCH HAS DISAPPEARED                        
*                                                                               
*                                  I/O2 4K WORKAREA FOR WORKER FILE             
*                                                                               
         GOTO1 DATAMGR,(R1),=C'READ',EPICWK,CURWKIXD,AIO2,WRKFBUFA              
*                                    ADDRESSES STILL LEFT FROM INDEX            
         TM    DMCB+8,X'80'        TEST EOF ON FIRST READ                       
         BZ    *+6                                                              
         DC    H'0'                BATCH HAS BEEN CLOBBERED                     
*                                                                               
         L     R5,WRKFBUFA         ESTABLISH WORKER RECORD                      
         USING W_RECD,R5                                                        
*                                                                               
         MVC   SVWCMNT,W_DESC      SAVE COMMENT AREA-MAY HAVE DATA              
*                                    FOR APPLICATION                            
*                                                                               
         LA    R5,W_DESC             FOR APPLICATION                            
*                                                                               
         DROP  R5                                                               
*                                                                               
         L     R6,AIO3             ESTABLISH PZBLOCK                            
         USING EZBLOCKD,R6                                                      
*                                                                               
         LR    R0,R6               CLEAR PZBLOCK                                
         LH    R1,=Y(EZBLOCKL)                                                  
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     RE,=A(LSBTHOOK)     BUILD LINSET TABLE HOOK                      
         A     RE,RELO                                                          
         ST    RE,EZHOOK                                                        
*                                                                               
*        INITIALIZE PZBLOCK                                                     
*                                                                               
         MVC   EZAGY,AGENCY        AGENCY                                       
         MVI   EZOFF,C'N'          NOT OFF-LINE                                 
         MVI   KEEPSW,C'Y'         KEEP WORKER FILE                             
         MVC   EZWKRFIL,EPICWK     WORKER FILE ID                               
         MVC   EZWKRIND,EZWKRIXD   SET INDEX                                    
*                                                                               
         L     R1,WRKFBUFA         PASS BUFFER ADDRESS                          
         ST    R1,EZWKRBUF                                                      
*                                                                               
         MVC   EZWKRREC,AIO2       PASS WORKER FILE WORKAREA ADDRESS            
*                                                                               
         L     RE,=A(IOA4-(CONHEADH-64))                                        
         LA    RE,0(RE,RA)                                                      
         ST    RE,EZAREC           PASS RECORD ADDRESS                          
*                                                                               
         L     RE,ACOMFACS                                                      
         ST    RE,EZCOMFCS                                                      
*                                                                               
         MVI   EZTEST,C'Y'         NO WORKER FILE MARKING                       
*                                                                               
         TM    FTRFLAG,FTRTRACE    SET TRACE INDICATOR IF NEEDED                
         BZ    *+8                                                              
         OI    EZTRACE,X'F0'                                                    
*                                                                               
         MVI   EZWRITE,C'N'        NO WRITING TO WORKER FILE                    
         MVI   FOUNDSW,0           RECORD NOT FOUND YET                         
*                                                                               
         GOTO1 VPZMOD,DMCB,(R6)    HAVE PZMOD READ FILES                        
*                                                                               
         XIT1                  RET AT BATCH END-DONE ON RET FROM PZMOD          
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  R4,R6,RB                                                         
*                                                                               
         TITLE 'T43008 - INVOICE DETAILS - LINSET'                              
***********************************************************************         
*                                                                     *         
*   LINUP INTERFACE                                                   *         
*     - BUILD LUBLK, CREATE ELEMENT TABLE, CALL LINUP                 *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
LINSET   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R6,AIO3             ESTABLISH PZBLOCK                            
         USING EZBLOCKD,R6                                                      
*                                                                               
*        VALIDATE DETAIL FILTERS LINE                                           
*                                                                               
         LA    R2,DTLDFTRH         POINT TO FILTERS FIELD                       
*                                                                               
         MVC   DFLTSW,4(R2)        SAVE DETAIL FILTERS INPUT STAT               
*                                                                               
         GOTO1 =A(VDFT),RR=RELO    VALIDATE FILTERS                             
*                                                                               
         OI    4(R2),X'20'         INDICATE FILTERS ARE VALID                   
*                                                                               
*        VALIDATE ENTERED SEQUENCE NUMBER                                       
*                                                                               
         TM    DTLSQNH+4,X'20'     SKIP UNLESS ENTERED THIS TIME                
         BNZ   LSSQNX                                                           
*                                                                               
         MVI   NEWDISP,C'Y'        FORCE RE-DISPLAY OF SCREEN                   
         LA    R2,DTLSQNH          POINT TO CURRENT FIELD                       
*                                                                               
         OI    GENSTAT2,RETEQSEL   HAVE GENCON RETURN THIS SCREEN               
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,DTLSQNH+5      GET FIELD LENGTH                             
         BZ    LSSQNX              SKIP IF NOT ENTERED                          
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
*                                                                               
         EX    RF,*+16                                                          
         EX    RF,*+18                                                          
         EX    RF,*+20                                                          
         B     *+22                                                             
         MVC   DUB(0),DTLSQN       VALIDATE AS NUMERIC                          
         NC    DUB(0),=8C'0'                                                    
         CLC   DUB(0),=8C'0'                                                    
         BNE   LSNUMERR                                                         
*                                                                               
         EX    RF,*+8                                                           
         B     *+10                                                             
         PACK  DUB,DTLSQN(0)                                                    
*                                                                               
         CVB   RF,DUB              CVB                                          
*                                                                               
         STCM  RF,3,WRKSQN         SAVE RESULT                                  
*                                                                               
LSSQNX   DS    0H                                                               
*                                                                               
         OI    DTLSQNH+4,X'20'     INDICATE VALID SEQUENCE NUMBER               
         OI    DTLSQNH+6,X'80'     TRANSMIT FIELD                               
*                                                                               
         MVI   WPRDSW,0            INIT NEW PRODUCT SWITCH                      
*                                                                               
         LA    R5,LUBLK            POINT TO  LINUP CONTROL FIELD                
         USING LUBLKD,R5           ESTABLISH LINUP CONTROL FIELD                
         XC    LUBLKD(LUBLKL),LUBLKD   CLEAR LINUP CONTROL FIELD                
*                                                                               
         MVC   LUATWA,ATWA         PASS A(TWA)                                  
         L     R1,SYSPARMS         GET A(TIOB)                                  
         L     R1,0(R1)                                                         
         ST    R1,LUATIOB                                                       
*                                                                               
         MVI   LUNLINS,NLINS       SET NUMBER OF LINES ON SCREEN                
         MVI   LUNFLDS,12              FIELDS PER LINE                          
*                                                                               
*                                  BUILD LIST OF FIELD DISPLACEMENTS            
*                                                                               
         LA    R3,LINDSPS          POINT TO LIST OF DISPLACEMENTS               
         ST    R3,LUADSPS          A(LIST OF DISPLACEMENTS)                     
*                                                                               
         LA    R2,DTLPCDH          A(FIRST FIELD)                               
         ZIC   R4,LUNLINS          NUMBER OF LINES                              
*                                                                               
LS04     DS    0H                                                               
*                                                                               
         LA    RF,0(R2)            POINT TO FIRST FIELD ON NEXT LINE            
         S     RF,ATWA             GET DISPLACEMENT                             
         STH   RF,0(R3)            SET DISPLACEMENT IN LIST                     
*                                                                               
         LA    R3,2(R3)            BUMP TO NEXT SLOT IN LIST                    
*                                                                               
         ZIC   R0,LUNFLDS          GET NUMBER OF FIELDS ON LINE                 
         BAS   RE,BUMP             BUMP TO START OF NEXT LINE                   
         BCT   R0,*-4                BY BUMPING THRU ALL FLDS ON LINE           
*                                                                               
         BCT   R4,LS04             ADD NEXT LINE TO LIST                        
*                                                                               
         XC    0(2,R3),0(R3)       FORCE NULLS AT END OF LIST                   
*                                                                               
         MVI   LUSCROLL,LUPAGEQ    SCROLL FACTOR OF A PAGE IS DEFAULT           
*                                                                               
         CLI   DTLSCRL,C'P'        CHECK FOR PAGE SCROLL                        
         BE    *+8                                                              
         CLI   DTLSCRL,X'97'         LOWERCASE 'P'                              
         BNE   *+8                                                              
         MVI   LUSCROLL,LUPAGEQ                                                 
*                                                                               
         CLI   DTLSCRL,C'H'        CHECK FOR HALF PAGE SCROLL                   
         BE    *+8                                                              
         CLI   DTLSCRL,X'88'            LOWERCASE 'H'                           
         BNE   *+8                                                              
         MVI   LUSCROLL,LUHALFQ                                                 
*                                                                               
         TM    DTLSCRLH+4,X'08'    SKIP IF NOT A NUMERIC FIELD                  
         BNO   LS051                                                            
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,DTLSCRLH+5     FIELD INPUT LENGTH                           
         BZ    LS051               NO ENTRY                                     
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         PACK  DUB,DTLSCRL(0)      CONVERT SCROLL AMOUNT TO NUMBER              
*                                                                               
         CVB   RF,DUB                                                           
         STC   RF,LUSCROLL         PASS SCROLL AMOUNT                           
*                                                                               
LS051    DS    0H                                                               
*                                                                               
         CLI   PFAID,19            CHECK FOR UP KEY                             
         BE    *+8                                                              
         CLI   PFAID,7             CHECK FOR UP KEY                             
         BNE   *+8                                                              
         MVI   LUPFKEY,LUPFUPQ                                                  
*                                                                               
         CLI   PFAID,20            CHECK FOR DOWN KEY                           
         BE    *+8                                                              
         CLI   PFAID,8             CHECK FOR DOWN KEY                           
         BNE   *+8                                                              
         MVI   LUPFKEY,LUPFDNQ                                                  
*                                                                               
         CLI   LUPFKEY,0           IF PFKEY USED                                
         BE    *+8                                                              
         MVI   NEWKEY,C'N'         ASSUME NO KEY CHANGE                         
*                                                                               
         MVC   LUNEW,NEWKEY        SET NEW OR OLD RECORD INDICATOR              
*                                                                               
         CLI   MODE,VALREC         SET LINUP MODE                               
         BNE   *+8                                                              
         MVI   LUAPMODE,LUAPVALQ   VALIDATE                                     
*                                                                               
         CLI   MODE,DISPREC        SET LINUP MODE - DISPREC                     
         BE    *+8                                                              
         CLI   MODE,XRECADD        RE-DISPLAY AFTER ADD                         
         BE    *+8                                                              
         CLI   MODE,XRECPUT        RE-DISPLAY AFTER CHANGE                      
         BE    *+8                                                              
         CLI   MODE,XRECDEL        RE-DISPLAY AFTER DELETE                      
         BE    *+8                                                              
         CLI   MODE,RECDEL         DELETE RECORD                                
         BE    *+8                                                              
         CLI   MODE,XRECREST       RE-DISPLAY AFTER RESTORE                     
         BNE   *+8                                                              
         MVI   LUAPMODE,LUAPDSPQ   DISPLAY                                      
*                                                                               
         OC    WRKSQN,WRKSQN       DISPLAY IF SEQUENCE NUMBER ENTERED           
         BZ    *+8                                                              
         MVI   LUAPMODE,LUAPDSPQ   DISPLAY                                      
*                                                                               
         TM    DFLTSW,X'20'        DISPLAY IF DETAIL FILTERS  ENTERED           
         BO    *+8                                                              
         MVI   LUAPMODE,LUAPDSPQ   DISPLAY                                      
*                                                                               
         CLI   PFAID,0             DISPLAY IF PFKEY HIT                         
         BE    *+8                                                              
         MVI   LUAPMODE,LUAPDSPQ   DISPLAY                                      
*                                                                               
         MVI   LUCNTL,LUBACKQ      WINDOW SUPPORTS BACKWARD SCROLLING           
*                                                                               
         TM    IPSTAT,LUWCURSQ     CHECK IF CURSOR FOUND YET                    
         BNO   *+8                 NO - NO CURSOR SELECT                        
         OI    LUCNTL,LUCURSQ      YES - MAKE CURSOR SENSITIVE                  
*                                                                               
         LA    RF,LINHOOK          PROCESSING ROUTINE                           
         ST    RF,LUHOOK                                                        
*                                                                               
         MVC   LUSVLEN,=AL2(LSVTABL) SAVED BYTES PER LINE                       
*                                                                               
         LA    RF,LSVTAB           LINUP SAVE AREA                              
         ST    RF,LUSVTAB                                                       
*                                                                               
*              BUILD TABLE OF ELEMENTS                                          
*                                                                               
         BAS   RE,LSBLDTAB            BUILD ELEM TABLE                          
*                                                                               
         CLI   LUAPMODE,LUAPVALQ   SKIP IF NOT VALIDATING                       
         BNE   LSCNVX                                                           
*                                                                               
*        CHECK IF CHANGES CAN BE MADE                                           
*                                                                               
         CLI   CONVRTSW,C'Y'       IF INVOICE CONVERTED                         
         BNE   LSCNVX                                                           
*                                                                               
         CLI   RECONVSW,C'Y'       OKAY IF ALREADY SET FOR RE-CONVERST          
         BE    LSCNVX                                                           
*                                                                               
         B     LSECVCHG            ELSE NO CHANGES ALLOWED                      
*                                                                               
LSCNVX   DS    0H                                                               
*                                                                               
LSDISHD  DS    0H                                                               
*                                                                               
*        DISPLAY INVOICE HEADER DETAILS                                         
*                                                                               
         CLI   LUAPMODE,LUAPDSPQ   SKIP IF NOT DISPLAYING                       
         BNE   LSDISHDX                                                         
*                                                                               
*        INVOICE HEADER IN SVINVHDR                                             
*                                                                               
         MVC   DTLINO,EZIHINV-EZIHFRST+SVINVHDR INVOICE NUMBER                  
         OI    DTLINOH+6,X'80'     TRANSMIT FIELD                               
*                                                                               
         MVC   DTLMOS(3),EZIHDMOS-EZIHFRST+SVINVHDR MONTH OF SERVICE            
         MVC   DTLMOS+3(2),EZIHDMOS+4-EZIHFRST+SVINVHDR   MMMYY                 
         OI    DTLMOSH+6,X'80'     TRANSMIT FIELD                               
*                                                                               
         MVC   DTLSRCE,EZSRCE      SOURCE                                       
         OI    DTLSRCEH+6,X'80'    TRANSMIT FIELD                               
*                                                                               
         MVC   DTLCCD,EZIHCVAD-EZIHFRST+SVINVHDR DEFAULT TO CONVERT CLT         
*                                                                               
         CLC   DTLCCD,SPACES       IF CONVERTED DDS CLT CODE MISSING            
         BH    *+10                                                             
         MVC   DTLCCD,EZIHLADC-EZIHFRST+SVINVHDR USE LOOKED UP CLI CODE         
*                                                                               
         OI    DTLCCDH+6,X'80'     TRANSMIT FIELD                               
*                                                                               
         XC    DTLCNMD,DTLCNMD     INIT DDS CLIENT NAME                         
*                                                                               
         CLC   DTLCCD,SPACES       SKIP IF NO CLIENT CODE                       
         BH    *+14                                                             
         MVC   DTLCNMD,EZIHAVNM-EZIHFRST+SVINVHDR    USE PROVIDED NAME          
         B     LSDH140                                                          
*                                                                               
         OC    DTLCCD,SPACES                                                    
*                                                                               
         MVC   FILENAME,=C'PRTDIR  '                                            
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING PCLTRECD,R4                                                      
*                                                                               
         MVC   PCLTKAGY,QAGY                                                    
         MVC   PCLTKMED,QMED                                                    
         MVI   PCLTKRCD,PCLTKIDQ                                                
         MVC   PCLTKCLT,DTLCCD                                                  
         MVI   RDUPDATE,C'N'       NOT READ FOR UPDATE                          
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(25),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R4,AIO1                                                          
         ST    R4,AIO                                                           
*                                                                               
         MVI   RDUPDATE,C'N'       NOT READ FOR UPDATE                          
*                                                                               
         MVC   FILENAME,=C'PRTFIL  '                                            
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         XC    FILENAME,FILENAME                                                
*                                                                               
         MVC   DTLCNMD,PCLTNAME    CLIENT NAME                                  
*                                                                               
LSDH140  DS    0H                                                               
*                                                                               
         OI    DTLCNMDH+6,X'80'    TRANSMIT FIELD                               
*                                                                               
*        END OF INVOICE HEADER FIELDS                                           
*                                                                               
LSDISHDX DS    0H                                                               
*                                                                               
*        FIRST LINE UP CALL                                                     
*        ------------------                                                     
*                                                                               
         MVC   SVLSVENT,LSVTAB     SAVE FIRST ENTRY IN TABLE                    
*                                                                               
         GOTO1 VLINUP,DMCB,LUBLKD     LINUP                                     
*                                                                               
         OC    IPSTAT,LUWSTAT      OR IN WINDOW INPUT STATUS                    
*                                                                               
         CLI   LUAPMODE,LUAPVALQ      TEST VALIDATING                           
         BNE   LSMOR                                                            
*                                                                               
         TM    IPSTAT,LUWVERRQ     CONTINUE IF NO ERRORS                        
         BNO   LS22                                                             
*                                  ELSE                                         
         LA    R0,SVLSVTAB         SET UP FOR MVCL                              
         LA    R1,L'SVLSVTAB                                                    
         LA    RE,LSVTAB                                                        
         LR    RF,R1                                                            
         MVCL  RE,R0               RESTORE LINUP SAVE TABLE                     
*                                                                               
         OI    GENSTAT2,RETEQSEL   HAVE GENCON RETURN THIS SCREEN               
*                                                                               
         B     LSX                 AND DON'T UPDATE ELEMS                       
*                                                                               
LS22     DS    0H                                                               
*                                                                               
         TM    LUWSTAT,LUSNPVQ     UPDATE RECORD IF THERE WAS AT LEAST          
         BNO   *+8                 ONE NON-PREVIOUSLY VALIDATED FIELD           
         TM    LUWSTAT,LUSDATQ     AND DATA ENTERED IN SOME FIELD               
         BNO   *+8                                                              
         BAS   RE,LSWRTTAB         WRITES CHANGES TO RECORD                     
*                                                                               
         TM    IPSTAT,LUSNPVQ      IF ALL PREVIOUSLY VALIDATED                  
         BO    LSNCHA                                                           
         CLI   ACTNUM,ACTCHA       AND ACTION IS CHANGE                         
         BNE   LSNCHA              THEN WANT TO RE-DISPLAY IN CASE OF           
*                                    NEED TO SCROLL                             
         TM    LUSTAT,LUCLEARQ     SKIP IF CLEAR COMMAND ISSUED                 
         BO    LSNCHA                                                           
*                                    NEED TO SCROLL                             
         MVI   LUAPMODE,LUAPDSPQ   SET FOR DISPLAY                              
         MVI   MODE,DISPREC        SET FOR DISPLAY RECORD                       
         MVI   LUWSTAT,0           RESET WINDOW STAT                            
*                                                                               
         GOTO1 VLINUP,DMCB,LUBLKD SCROLL IF NEEDED                              
*                                                                               
LSNCHA   DS    0X                                                               
*                                                                               
LSMOR    DS    0X                  SET 'MORE' FIELDS                            
*                                                                               
         TM    LUSTAT,LUCLEARQ     CLEAR LOWER MORE FIELD IF SCREEN             
         BO    LSLOW                 CLEARED - LEAVE UPPER AS IS                
*                                                                               
         CLI   LUAPMODE,LUAPDSPQ   ONLY IF IN DISPLAY MODE                      
         BNE   LSMORX                                                           
*                                                                               
         MVC   FLD,SPACES          INIT WORK OUTPUT AREA                        
         LA    R1,DTLMOR1H         POINT TO FIRST MORE FIELD                    
*                                                                               
         L     R3,LUSVTAB          POINT TO FIRST SAVED ENTRY                   
         USING LSVTABD,R3          ESTABLISH ENTRY                              
*                                                                               
         L     R4,ELTFRST          POINT TO FIRST TABLE ENTRY                   
         USING ELTABD,R4           ESTABLISH ENTRY                              
*                                                                               
         CLC   LSVKEY,ELTKEY       IF SCREEN DOES NOT START AT START            
         BNH   *+10                OF TABLE THEN SET UP INDICATOR               
         MVC   FLD(2),=C'<<'                                                    
*                                                                               
         BAS   RE,DSPFLD           DISPLAY IT                                   
*                                                                               
LSLOW    DS    0H                                                               
*                                                                               
         MVC   FLD,SPACES          INIT WORK OUTPUT AREA                        
         LA    R1,DTLMORLH         POINT TO LAST MORE FIELD                     
*                                                                               
         TM    LUSTAT,LUCLEARQ     CLEAR LOWER MORE FIELD IF SCREEN             
         BO    LSLOWOUT              CLEARED                                    
*                                                                               
         ZIC   RF,LUNLINS          GET NUMBER OF LINES ON SCREEN                
         BCTR  RF,0                DECREMENT FOR INDEXING                       
         MH    RF,=Y(LSVTABL)      GET INDEX                                    
         AR    R3,RF               POINT TO LAST ELEMENT IN TABLE               
         L     R4,ELTLAST          POINT TO LAST ELEMENT IN TABLE               
*                                                                               
         OC    LSVKEY,LSVKEY       NULLS INDICATE END OF TABLE ALREADY          
         BZ    LSLOWOUT            ON SCREEN                                    
*                                                                               
         CLC   LSVKEY,ELTKEY       IF SCREEN DOES NOT END AT END                
         BNL   *+10                OF TABLE THEN SET DOWN INDICATOR             
         MVC   FLD(2),=C'>>'                                                    
*                                                                               
LSLOWOUT DS    0H                                                               
*                                                                               
         BAS   RE,DSPFLD           DISPLAY IT                                   
*                                                                               
LSMORX   DS    0X                                                               
*                                                                               
LSX      DS    0H                                                               
*                                                                               
         OC    ACURFORC,ACURFORC   SET CURSOR IF IT IS NOT SET ALREADY          
         BNZ   *+8                                                              
         LA    R2,DTLPCDH          FORCE CURSOR TO PRODUCT FIELD                
         ST    R2,ACURFORC                                                      
*                                                                               
         MVI   TWALACT,ACTSEL      FORCE LAST ACTION TO BE SELECT               
*                                                                               
         CLI   LUERR,0             RESET CC                                     
*                                                                               
         XIT1                                                                   
*                                                                               
LSECVCHG DS    0H                  CAN'T CHANGE CONVERTED INVOICE               
         MVI   ERROR,PZECVTCH                                                   
         LA    R2,CONACTH                                                       
         B     LSERRX                                                           
*                                                                               
LSNUMERR MVI   ERROR,NOTNUM        NOT NUMERIC                                  
         B     LSERRX                                                           
*                                                                               
LSERRX   DS    0H                                                               
*                                                                               
         NI    4(R2),X'FF'-X'20'   TURN OFF VALIDATED STATUS                    
         GOTO1 ERREX               HANDLE FIELD IN ERROR                        
*                                                                               
*                                  LINES IN WINDOW                              
NLINS     EQU   1                                                               
*NLINS    EQU   ((DTLSPLH-DTLSP1H)/(DTLSP2H-DTLSP1H))+1                         
         DROP  R3                                                               
         DROP  R4                                                               
         EJECT                                                                  
**********************************************************************          
*                                                                    *          
*   LINHOOK - LINUP PROCESSING HOOK                                  *          
*                                                                    *          
**********************************************************************          
         SPACE 2                                                                
LINHOOK  NTR1                                                                   
         CLI   LUMODE,LUVALQ       VALIDATE                                     
         BE    LHVAL                                                            
         CLI   LUMODE,LUDSPQ       DISPLAY                                      
         BE    LHDIS                                                            
         CLI   LUMODE,LUMOREQ      MORE TO DISPLAY                              
         BE    LHMORE                                                           
         DC    H'0'                INVALID MODE                                 
         EJECT                                                                  
**********************************************************************          
*   LINHOOK - LINUP VALIDATION HOOK ROUTINE                          *          
**********************************************************************          
         SPACE 2                                                                
LHVAL    DS    0H                                                               
*                                                                               
*              IF FIRST INPUT FIELD HAS '++' THEN USER WANTS TO CLEAR           
*              OUT WINDOW FROM THIS POINT ON                                    
*                                                                               
         L     R2,LUACLIN          TEST FOR SCREEN CLEAR REQUEST                
*                                                                               
         CLC   8(2,R2),=C'++'      CHECK FOR CLEARING INDICATOR                 
         BNE   LHV04                                                            
*                                                                               
         NI    LULSTAT,X'FF'-LUSNPVQ  TURN OFF NOT VALIDATED INDICATOR          
         OI    LUSTAT,LUCLEARQ     TELL LINUP TO CLEAR FROM HERE ON             
*                                                                               
         OC    ACURFORC,ACURFORC   SET CURSOR IF IT IS NOT SET ALREADY          
         BNZ   *+8                                                              
         ST    R2,ACURFORC         FORCE CURSOR TO THIS FIELD                   
*                                                                               
         OI    GENSTAT2,RETEQSEL   HAVE GENCON RETURN THIS SCREEN               
*                                                                               
         B     LHVALX                                                           
*                                                                               
LHV04    DS    0H                                                               
*                                                                               
         GOTO1 =A(LHVALLIN),RR=RELO YES, VALIDATE LINE AND ADD TO TABLE         
         BNE   LHVALX              IF ERROR DONT DELETE OLD AND DONT            
*                                  CLEAR SAVE TABLE ENTRY                       
         EJECT                                                                  
LHV06    DS    0H                                                               
*                                                                               
         LA    R2,SVLSVENT         NEEDED BECAUSE ONLY 1 ENTRY IN TABLE         
         USING LSVTABD,R2          ESTABLISH ENTRY                              
*                                                                               
         OC    LSVKEY,LSVKEY       WAS ANYTHING THERE BEFORE                    
         BZ    LHV10               NO                                           
*                                  YES, MARK OLD ENTRY DELETED                  
*                                  NOTE- ADD+DEL=CHANGE, THIS ENTRY             
*                                        MAY BE SAME AS ABOVE                   
         GOTO1 =V(BINSRCH),BSPPRMS,('BSPFIND',(R2)),RR=RELO                     
         CLI   BSPCNTL,BSPNF       MUST FIND ELEMENT                            
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RF,BSPAREC          POINT TO FOUND ELEMENT                       
         USING ELTABD,RF           ESTABLISH ELEMENT                            
*                                                                               
         OI    ELTCTL,ELTDELQ      SET ENTRY DELETED                            
         DROP  RF                                                               
*                                                                               
LHV10    DS    0H                  SET NEW SAVE TABLE ENTRY                     
         XC    LSVKEY,LSVKEY       INIT SAVE TABLE ENTRY                        
         ICM   RF,15,ELTENT        GET ADDRESS OF ELEMENT                       
         BZ    *+10                PUT IN TABLE IF FOUND                        
         MVC   LSVKEY,ELTKEY-ELTABD(RF)                                         
*                                                                               
LHVALX   DS    0H                                                               
         B     LHOOKX                                                           
*                                                                               
         DROP  R2                                                               
*                                                                               
         TITLE 'T43008 - LINUP DISPLAY HOOK ROUTINE - LHDIS'                    
**********************************************************************          
*   LINHOOK - LINUP DISPLAY HOOK ROUTINE                             *          
**********************************************************************          
         SPACE 2                                                                
LHDIS    DS    0H                                                               
*                                                                               
         MVI   LUSTAT,0            INIT STATUS                                  
*                                                                               
         L     R4,LUACTAB          CURRENT SAVE TABLE ENTRY                     
         USING LSVTABD,R4                                                       
*                                                                               
         XC    0(LSVTABL,R4),0(R4)  CLEAR IT                                    
*                                                                               
         BAS   RE,LHSRCH           FIND ELEM TO DISPLAY                         
         GOTO1 =A(LHDISLIN),RR=RELO   BUILD SCREEN LINE                         
*                                                                               
LHDISX   DS    0H                                                               
         B     LHOOKX                                                           
         EJECT                                                                  
*                                  DO 'MORE' MESSAGE                            
*                                  -----------------                            
LHMORE   DS    0H                                                               
         B     LHOOKX                                                           
*                                                                               
LHOOKX   DS    0H                                                               
         XIT1                                                                   
         DROP  R4                                                               
         TITLE 'PPEZF08 - BUILD TABLE OF ELEMENTS ON FILE LSBLDTAB'             
***********************************************************************         
*                                                                     *         
* ROUTINE TO BUILD ELEMENT TABLE FOR LINUP                            *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
LSBLDTAB NTR1                                                                   
*                                                                               
         XC    BSPPRMS(BSPPRML),BSPPRMS INIT BINSRCH PARAMETERS                 
*                                                                               
         XC    INSCTR,INSCTR       INIT INSERTION COUNTER                       
         XC    ELTNOR,ELTNOR       INIT NUMBER OF ACTIVE MEMBERS IN TAB         
*                                                                               
         MVC   BSPATAB,ELTABA      PASS TABLE ADDRESS                           
*                                                                               
         LA    R1,ELTABL           PASS ENTRY LENGTH                            
         ST    R1,BSPLENR                                                       
*                                                                               
         LA    R1,ELTKEYL          PASS KEY LENGTH                              
         ST    R1,BSPLENK                                                       
*                                                                               
         MVI   BSPKEYD,0           PASS KEY DISPLACEMENT                        
*                                                                               
         LA    R1,ELTMAX           PASS MAXIMUM COUNT                           
         ST    R1,BSPMAX                                                        
*                                                                               
         ICM   R0,15,BSPNOR                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 =A(PRPZ),RR=RELO    FIND RECORD, CALL PZMOD, ETC.                
*                                                                               
         MVC   TINSCTR,INSCTR      SAVE TOTAL NUMBER OF INSERTIONS              
*                                                                               
         ICM   R4,15,BSPNOR        NUMBER OF ENTRIES                            
         BZ    *+10                                                             
         BCTR  R4,R0               MINUS ONE                                    
         MH    R4,=Y(ELTABL)       TIMES ENTRY LENGTH                           
         A     R4,BSPATAB          PLUS START OF TABLE                          
*                                                                               
         ST    R4,ELTLAST          SET A(LAST)                                  
         USING ELTABD,R4           ESTABLISH TABLE ELEMENT                      
*                                                                               
         TM    ELTCTL,ELTFLTXQ     BACK UP IF IT ENDS IN FILTER FLUNKER         
         BNO   *+16                                                             
         SH    R4,=Y(ELTABL)                                                    
         C     R4,BSPATAB          STOP AT START OF TABLE                       
         BNL   *-16                                                             
*                                                                               
         ST    R4,ELTLAST          SET A(LAST)                                  
*                                                                               
         L     R4,BSPATAB          START OF TABLE                               
*                                                                               
         TM    ELTCTL,ELTFLTXQ     BYPASS FILTER FLUNKERS                       
         BNO   *+16                                                             
         LA    R4,ELTABL(R4)                                                    
         C     R4,ELTLAST                                                       
         BNH   *-16                                                             
*                                                                               
         ST    R4,ELTFRST          SET A(FIRST)                                 
*                                                                               
         XIT1                                                                   
*                                                                               
         DROP  R4                                                               
*                                                                               
         TITLE 'PPEZF08 - SEARCH TABLE FOR ELEMENT - LHSRCH'                    
***********************************************************************         
*                                                                     *         
*        ROUTINE TO SEARCH TABLE FOR ELEMENT                          *         
*        AND SET ADDRESS IN ELTENT                                    *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
LHSRCH   NTR1                                                                   
*                                                                               
         XC    ELTENT,ELTENT       INIT ELEMENT ADDRESS RETURN                  
         NI    LUSTAT,255-LUEOLQ   SET OFF END OF LIST INDICATOR                
*                                                                               
         OC    ELTNOR,ELTNOR       IF NO ENTRIES                                
         BZ    LHSRCHX             RETURN EMPTY-HANDED                          
*                                                                               
         L     R4,ELTFRST          DEFAULT TO FIRST VALID ENTRY                 
         USING ELTABD,R4                                                        
*                                                                               
         L     R3,LUAPTAB          A(PREVIOUS SAVE TABLE)                       
*                                                                               
         CLI   LUNEW,C'Y'          IF NOT FIRST TIME                            
         BE    *+8                                                              
         LA    R3,SVLSVENT            USE PREVIOUS SAVED ENTRY                  
*                                                                               
         USING LSVTABD,R3                                                       
*                                                                               
         OC    WRKSQN,WRKSQN       IF SEQUENCE NUMBER ENTERED                   
         BZ    *+14                                                             
         MVC   LSVSORT,WRKSQN         CHANGE PREVIOUS ENTRY                     
         MVI   LUDIR,C'='             FORCE RE-DISPLAY                          
*                                                                               
         OC    LSVKEY,LSVKEY       NO PREVIOUS ENTRY MEANS FIRST TIME           
         BZ    LHSRCH11            OR SCROLLING FROM A NON-FILLED               
*                                  SCREEN - USE DEFAULT                         
*                                                                               
         CLC   LSVKEY,HIVALS       IF PREVIOUS IS X'FF'S                        
         BNE   LHSRCH05                                                         
*                                                                               
         L     RE,BSPNOR              USE LAST ENTRY IN TABLE                   
         BCTR  RE,0                   DECREMENT FOR INDEXING                    
         L     RF,BSPLENR             RECORD LENGTH                             
         MR    RE,RE                  INDEX TO LAST ENTRY                       
         LA    R4,0(RF,R4)            A(LAST TABLE ENTRY)                       
         B     LHSRCH10                                                         
*                                                                               
LHSRCH05 DS    0H                                                               
*                                                                               
         GOTO1 =V(BINSRCH),BSPPRMS,('BSPRDHI',LSVKEY),RR=RELO                   
*                                                                               
         L     R4,BSPAREC          GET TABLE ENTRY ADDRESS                      
*                                                                               
         CLI   BSPCNTL,BSPNF       DEFAULT TO FIRST OF TABLE IF                 
         BNE   LHSRCH10            PREVIOUS ENTRY WAS NOT FOUND                 
*                                                                               
         L     R4,ELTFRST          NO, POINT TO FIRST-(END OF TABLE)            
*                                                                               
         B     LHSRCH11            DONE (NO MOVEMENT)                           
         EJECT                                                                  
LHSRCH10 DS    0H                                                               
         CLI   LUDIR,C'-'          CHECK FOR BACKWARD SCROLLING                 
         BE    LHSRCH16                                                         
*                                                                               
         CLI   LUDIR,C'='          SKIP BUMPING ENTRY IF RE-DISPLAYING          
         BE    *+8                                                              
         LA    R4,ELTABL(R4)       BUMP TO NEXT ENTRY IN TABLE                  
*                                                                               
LHSRCH11 DS    0H                                                               
*                                                                               
         C     R4,ELTLAST          DONE IF NOT AT END OF TABLE                  
         BL    LHSRCH30                                                         
         BE    LHSRCH12            AT END OF TABLE TELL LINUP                   
*                                  PAST END - ONLY IF SCROLLING DOWN            
*                                  AND PRIOR SCREEN ENDED WITH LAST             
*                                  ELEMENT IN TABLE - USE DEFAULT               
*                                  OR UP SCROLLING AND TABLE HAS ONLY           
*                                  ONE ELEMENT - STOP WITH NO DISPLAY           
         L     R4,ELTFRST          POINT TO FIRST ENTRY IN TABLE                
*                                                                               
         CLC   ELTNOR,=F'1'        DONE IF MORE THAN ONE ENTRY IN TABLE         
         BH    LHSRCH30                                                         
*                                  EXIT WITHOUT FINDING ELEMENT                 
         OI    LUSTAT,LUEOLQ       TELL LINUP THIS IS LAST                      
         B     LHSRCH30                                                         
*                                                                               
LHSRCH12 DS    0H                                                               
         OI    LUSTAT,LUEOLQ       TELL LINUP THIS IS LAST                      
         B     LHSRCH30                                                         
*                                                                               
         EJECT                                                                  
*                                                                               
LHSRCH16 DS    0H                  GOING BACKWARDS                              
*                                                                               
         C     R4,ELTFRST          IF AT START                                  
         BNH   LHSRCH18               DONT GO FURTHER                           
         SH    R4,=Y(ELTABL)       ELSE BACK UP AN ENTRY                        
*                                                                               
LHSRCH17 DS    0H                                                               
         C     R4,ELTFRST          IF AT START                                  
         BH    *+8                                                              
LHSRCH18 DS    0H                                                               
         OI    LUSTAT,LUEOLQ       TELL LINUP THIS IS LAST                      
*                                                                               
LHSRCH30 DS    0H                                                               
         TM    ELTCTL,ELTDELQ+ELTADDQ DISPLAY CHANGED ELEMENTS                  
         BO    LHSRCH40                                                         
         TM    ELTCTL,ELTFLTXQ     BYPASS FILTERED OUT ELEMENTS                 
         BO    *+8                                                              
         TM    ELTCTL,ELTDELQ      BYPASS DELETED ELEMENTS                      
         BNO   LHSRCH40                                                         
         TM    LUSTAT,LUEOLQ       EXIT IF AT END OF LIST                       
         BO    LHSRCHX                                                          
         CLI   LUDIR,C'='          QUIT IF RE-DISPLAY                           
         BE    LHSRCH40                                                         
         B     LHSRCH10            ELSE GO FIND NEXT ELEMENT                    
*                                                                               
LHSRCH40 DS    0H                                                               
         ST    R4,ELTENT           RETURN TABLE ENTRY ADDRESS                   
         L     R3,LUACTAB          POINT TO CURRENT SAVE TABLE ENTRY            
         MVC   LSVKEY,ELTKEY       SAVE APPROPRIATE DATA                        
*                                                                               
LHSRCHX  DS    0H                                                               
         XIT1                                                                   
         DROP  R3                                                               
         DROP  R4                                                               
HIVALS   DC    32X'FF'             HIGH VALUES                                  
*                                                                               
         TITLE 'PPEZF08 - UPDATE RECORD - LSWRTTAB'                             
***********************************************************************         
*                                                                     *         
* ROUTINE TO UPDATE RECORD BASED ON ELEMENT TABLE CHANGES             *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
LSWRTTAB NTR1                                                                   
*                                                                               
         OI    GENSTAT2,RETEQSEL   HAVE GENCON RETURN THIS SCREEN               
         MVI   ACTELOPT,C'N'       NO ACTIVITY ELEMENT                          
*                                                                               
         MVC   SAVMSGNO,ERROR      SAVE MESSAGE NUMBER                          
*                                                                               
*        ADD ELEMENTS IN TABLE TO RECORD                                        
*                                                                               
         L     R4,ELTFRST          START OF TABLE                               
         USING ELTABD,R4           ESTABLISH ENTRY IN TABLE                     
*                                                                               
         OC    ELTNOR,ELTNOR       SKIP IF NO ENTRIES                           
         BZ    LSWTLPDN                                                         
*                                                                               
LSWTLOOP DS    0H                                                               
*                                                                               
         TM    ELTCTL,ELTADDQ      ADD ELEMENTS FLAGGED FOR ADD                 
         BNO   LSWTNADD                                                         
*                                                                               
LSWTADD  DS    0H                                                               
*                                                                               
*        NOW THAT WE KNOW THERE IS AN ELEMENT TO CHANGE                         
*        WE CAN CALL PZMOD TO FIND IT. LOGIC IS EFFICIENT BECAUSE               
*        ONLY ONE ELEMENT CAN POSSIBLY BE CHANGED. IF THERE WERE                
*        MORE ON THE SCREEN SOME CHANGES WOULD NEED TO BE MADE.                 
*        NAMELY, TO FIND NEXT ELEMENT BEFORE PZMOD IS DONE WITH                 
*        INVOICE                                                                
*                                                                               
         MVI   USEIO,C'Y'          USER WILL DO IO                              
*                                                                               
         ST    R4,ELTENT           SAVE ADDRESS OF ELEMENT TO CHANGE            
*                                                                               
         DROP  R4                                                               
*                                                                               
         XC    CURWKIXD,CURWKIXD   ESTABLISH WORKER FILE INDEX                  
         LA    R4,CURWKIXD                                                      
         USING EZWKRIXD,R4                                                      
*                                                                               
         LA    R5,SVINVLST         USE INVOICE ENTRY FROM LAST TIME             
         USING INVLISTD,R5         ESTABLISH SAVED INVOICE AREA                 
*                                                                               
         MVC   EZWIUID,INVUID      SET WORKER FILE USER ID                      
         MVC   EZWIPUB,INVPZPUB    WORKER FILE PUB ID                           
         MVC   EZWIMED,INVMEDIA    MEDIA                                        
         MVI   EZWIDAY,X'98'       DAY = 98                                     
*                                                                               
         MVC   EZWKRIXD+8(2),INVBSEQ    BATCH SEQUENCE NUMBER WANTED            
*                                                                               
         DROP  R5                                                               
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'INDEX',EPICWK,EZWKRIXD,AIO2,WRKFBUFA             
         TM    DMCB+8,X'80'        TEST EOF                                     
         BZ    *+6                                                              
         DC    H'0'                BATCH HAS DISAPPEARED                        
*                                                                               
*                                  I/O2 4K WORKAREA FOR WORKER FILE             
*                                                                               
         GOTO1 DATAMGR,(R1),=C'READ',EPICWK,CURWKIXD,AIO2,WRKFBUFA              
*                                    ADDRESSES STILL LEFT FROM INDEX            
         TM    DMCB+8,X'80'        TEST EOF ON FIRST READ                       
         BZ    *+6                                                              
         DC    H'0'                BATCH HAS BEEN CLOBBERED                     
*                                                                               
         L     R5,WRKFBUFA         ESTABLISH WORKER RECORD                      
         USING W_RECD,R5                                                        
*                                                                               
         MVC   SVWCMNT,W_DESC      SAVE COMMENT AREA-MAY HAVE DATA              
*                                    FOR APPLICATION                            
*                                                                               
         LA    R5,W_DESC                                                        
*                                                                               
         DROP  R5                                                               
*                                                                               
         L     R6,AIO3             ESTABLISH PZBLOCK                            
         USING EZBLOCKD,R6                                                      
*                                                                               
         LR    R0,R6               CLEAR PZBLOCK                                
         LH    R1,=Y(EZBLOCKL)                                                  
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         XC    INSCTR,INSCTR       INIT INSERTION COUNTER                       
*                                                                               
         L     RE,=A(LSWRHOOK)     WRITE HOOK                                   
         A     RE,RELO                                                          
         ST    RE,EZHOOK                                                        
*                                                                               
*        INITIALIZE PZBLOCK                                                     
*                                                                               
         MVI   FOUNDSW,0           INIT FOUND SWITCH                            
*                                                                               
         MVC   EZAGY,AGENCY        AGENCY                                       
         MVI   EZOFF,C'N'          NOT OFF-LINE                                 
         MVI   KEEPSW,C'Y'         KEEP WORKER FILE                             
         MVC   EZWKRFIL,EPICWK     WORKER FILE ID                               
         MVC   EZWKRIND,SVWPZIND   SET INDEX                                    
*                                                                               
         L     R1,WRKFBUFA         PASS BUFFER ADDRESS                          
         ST    R1,EZWKRBUF                                                      
*                                                                               
         MVC   EZWKRREC,AIO2       PASS WORKER FILE WORKAREA ADDRESS            
*                                                                               
         L     RE,=A(IOA4-(CONHEADH-64))                                        
         LA    RE,0(RE,RA)                                                      
         ST    RE,EZAREC           PASS RECORD ADDRESS                          
*                                                                               
         L     RE,ACOMFACS                                                      
         ST    RE,EZCOMFCS                                                      
*                                                                               
         TM    FTRFLAG,FTRTRACE    SET TRACE INDICATOR IF NEEDED                
         BZ    *+8                                                              
         OI    EZTRACE,X'F0'                                                    
*                                                                               
         GOTO1 VPZMOD,DMCB,(R6)    HAVE PZMOD READ FILES                        
*                                                                               
         L     R4,ELTENT           RE-POINT TO ELEMENT IN TABLE                 
*                                                                               
LSWTADDX DS    0H                                                               
*                                                                               
         B     LSWTLPCN                                                         
*                                                                               
LSWTNADD DS    0H                                                               
*                                                                               
LSWTLPCN DS    0H                                                               
*                                                                               
         A     R4,BSPLENR          BUMP TO NEXT ENTRY                           
         C     R4,ELTLAST          LOOP IF NOT LAST ENTRY                       
         BNH   LSWTLOOP                                                         
*                                                                               
LSWTLPDN DS    0H                                                               
*                                                                               
LSWRTX   DS    0H                                                               
         MVC   ERROR,SAVMSGNO      RESTORE MESSAGE NUMBER                       
         XIT1                                                                   
         DROP  R4                                                               
         LTORG                                                                  
         TITLE 'PPEZF08 - FIND INSERTION DETAILS ON FILE - LSDRHOOK'            
***********************************************************************         
*                                                                     *         
*        ROUTINE TO FIND INSERTION DETAILS ON WORKER FILE             *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
LSDRHOOK NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R4,ELTENT           POINT TO ELEMENT TO CHANGE                   
         USING ELTABD,R4           ESTABLISH TABLE ENTRY                        
*                                                                               
         L     R6,AIO3             ESTABLISH PZBLOCK                            
         USING EZBLOCKD,R6                                                      
*                                                                               
*        DETERMINE HOOK MODE                                                    
*                                                                               
         CLI   EZMODE,EZINVP       INVOICE START                                
         BE    LSDRINVP                                                         
*                                                                               
         CLI   EZMODE,EZSPTP       INSERTION DETAIL                             
         BE    LSDRSPTP                                                         
*                                                                               
         B     LSDRHKX             UNWANTED HOOK                                
*                                                                               
*        DETERMINE IF THIS IS DESIRED INVOICE                                   
*                                                                               
LSDRINVP DS    0H                                                               
*                                                                               
         CLI   FOUNDSW,C'Y'        IF WE JUST DID THE WANTED INVOICE            
         BNE   *+8                                                              
         MVI   FOUNDSW,C'X'           SET ALL OVER FLAG                         
*                                                                               
         CLI   FOUNDSW,0           IF INVOICE ALREADY FOUND                     
         BNE   LSDRSKIP               SKIP TO NEXT INVOICE                      
*                                                                               
*        TEST IF WANTED INVOICE                                                 
*                                                                               
         LA    R5,SVINVLST         USE INVOICE ENTRY FROM LAST TIME             
         USING INVLISTD,R5         ESTABLISH SAVED INVOICE AREA                 
*                                                                               
         CLC   EZINVSEQ,INVRSEQ    SKIP INVOICE IF NOT WANTED ONE               
         BNE   LSDRSKIP                                                         
*                                                                               
         MVC   SVINVSEQ,EZINVSEQ     SAVE INVOICE HEADER SEQ                    
         MVI   FOUNDSW,C'Y'                                                     
*                                                                               
         B     LSDRHKX                                                          
*                                                                               
LSDRSKIP DS    0H                  SKIP TO NEXT INVOICE                         
*                                                                               
         MVI   EZMODE,EZINVL       SKIP TO NEXT INVOICE                         
*                                                                               
         B     LSDRHKX             RETURN TO PZMOD                              
*                                                                               
*        SAVE DETAILS OF INSERTION ON FILE                                      
*                                                                               
LSDRSPTP DS    0H                                                               
*                                  REGIONAL COMBO TEST                          
         CLC   EZSNSTA(8),=C'WSJ'  SKIP UNLESS WSJ                              
         BNE   LSDRSP06                                                         
*                                  REGIONAL COMBO TEST                          
         CLC   EZSPRCOM(3),=C'N00' ALWAYS PASS N00                              
         BE    LSDRSP06                                                         
         CLI   EZSPRCOM,C'Y'       IF A COMBO SUMMARY                           
         BNE   LSDRSP04                                                         
*                                                                               
         CLI   EZRCSOPT,C'Y'       SKIP IF PROFILE SAYS TO                      
         BNE   LSDRSPTX                                                         
         B     LSDRSP06                                                         
*                                                                               
LSDRSP04 DS    0H                  NOT A COMBO                                  
         CLI   EZRCSOPT,C'Y'       SKIP IF PROFILE SAYS TO                      
         BE    LSDRSPTX                                                         
*                                                                               
LSDRSP06 DS    0H                                                               
         ICM   RF,3,INSCTR         BUMP INSERTION COUNTER                       
         LA    RF,1(RF)                                                         
         STCM  RF,3,INSCTR                                                      
*                                                                               
         CLM   RF,3,ELTSEQ         SEQUENCE NUMBER IN TABLE ELEMENT             
         BNE   LSDROVRN            SKIP IF NOT CORRECT INSERTION                
*                                                                               
         MVC   WRKELTAB(256),EZSPFRST COPY INSERTION DETAILS                    
         MVC   WRKELTAB+256(256),EZSPFRST+256                                   
         MVC   WRKELTAB+512(EZSPDLEN-512),EZSPFRST+512                          
*                                                                               
         MVI   EZMODE,EZINVL       SKIP TO NEXT INVOICE                         
*                                                                               
LSDROVRN DS   0H                                                                
*                                                                               
LSDRSPTX DS   0H                                                                
*                                                                               
         B     LSDRHKX                                                          
*                                                                               
LSDRHKX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         DROP  R4,R5,R6                                                         
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'PPEZF08 - CHANGE INSERTION DETAILS ON FILE - LSWRHOOK'          
***********************************************************************         
*                                                                     *         
*        ROUTINE TO CHANGE INSERTION DETAILS ON WORKER FILE           *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
LSWRHOOK NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R4,ELTENT           POINT TO ELEMENT TO CHANGE                   
         USING ELTABD,R4           ESTABLISH TABLE ENTRY                        
*                                                                               
         L     R6,AIO3             ESTABLISH PZBLOCK                            
         USING EZBLOCKD,R6                                                      
*                                                                               
*        DETERMINE HOOK MODE                                                    
*                                                                               
         CLI   EZMODE,EZINVP       INVOICE START                                
         BE    LSWRINVP                                                         
*                                                                               
         CLI   EZMODE,EZSPTP       INSERTION DETAIL                             
         BE    LSWRSPTP                                                         
*                                                                               
         CLI   EZMODE,EZINVL       INVOICE LAST                                 
         BE    LSWRINVL                                                         
*                                                                               
         B     LSWRKX              UNWANTED HOOK                                
*                                                                               
*        DETERMINE IF THIS IS DESIRED INVOICE                                   
*                                                                               
LSWRINVP DS    0H                                                               
*                                                                               
         CLI   FOUNDSW,C'Y'        IF WE JUST DID THE WANTED INVOICE            
         BNE   *+8                                                              
         MVI   FOUNDSW,C'X'           SET ALL OVER FLAG                         
*                                                                               
         CLI   FOUNDSW,0           IF INVOICE ALREADY FOUND                     
         BNE   LSWRSKIP               SKIP TO NEXT INVOICE                      
*                                                                               
*        TEST IF WANTED INVOICE                                                 
*                                                                               
         ZIC   RF,SELLISTN         GET LINE NUMBER                              
         MH    RF,=Y(INVENTL)      CALCULATE INDEX INTO TABLE                   
*                                                                               
         LA    RF,INVLIST(RF)      ESTABLISH SAVED INVOICE ID                   
         USING INVLISTD,RF                                                      
*                                                                               
         CLI   PFAID,0             IF PFKEY ENTERED                             
         NOP   *+8                                                              
         LA    RF,SVINVLST            USE INVOICE ENTRY FROM LAST TIME          
*                                                                               
         CLC   EZINVSEQ,INVRSEQ    SKIP INVOICE IF NOT WANTED ONE               
         BNE   LSWRSKIP                                                         
*                                                                               
         MVC   SVINVSEQ,EZINVSEQ     SAVE INVOICE HEADER SEQ                    
         MVI   FOUNDSW,C'Y'                                                     
*                                                                               
LSWRNWP  DS    0H                                                               
*                                                                               
*        IF NEW PRODUCT IN DETAIL CHANGE MASTER PRODUCT TO '***'                
*                                                                               
         TM    WPRDSW,WPRDNEWQ     SKIP UNLESS NEW PRD FOR INV                  
         BNO   LSWRNWPX                                                         
*                                                                               
         CLC   EZIHCVPR,=C'***'    SKIP IF ALREADY FOR PRODUCT VARIOUS          
         BE    LSWRNWPX                                                         
*                                                                               
         L     RF,EZWKRREC         POINT TO WORKER RECORD                       
         LA    RF,7(RF)            BYPASS RECLEN(4),RECCODE(2),DELIM(1)         
         USING EZIHFRST,RF         ESTABLISH HEADER AREA                        
*                                                                               
         MVC   EZIHCVPR,=C'***'    REPLACE PRODUCT                              
         XC    EZIHCVES,EZIHCVES   CLEAR   ESTIMATE                             
         OI    WPRDSW,WPRDDFTQ     INDICATE DEFAULT PRODUCT CHANGED             
*                                                                               
         DROP  RF                                                               
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'WRITE',EPICWK,CURWKIXD,AIO2,WRKFBUFA             
*                                  WRITE CHANGES TO FILE                        
*                                                                               
LSWRNWPX DS    0H                                                               
*                                                                               
LSWRINHX DS    0H                                                               
*                                                                               
         B     LSWRKX                                                           
*                                                                               
LSWRSKIP DS    0H                  SKIP TO NEXT INVOICE                         
*                                                                               
         MVI   EZMODE,EZINVL       SKIP TO NEXT INVOICE                         
*                                                                               
         B     LSWRKX              RETURN TO PZMOD                              
*                                                                               
*        CHANGE INSERTION ON FILE                                               
*                                                                               
LSWRSPTP DS    0H                                                               
*                                  REGIONAL COMBO TEST                          
         CLC   EZSNSTA(8),=C'WSJ'  SKIP UNLESS WSJ                              
         BNE   LSWRSP06                                                         
*                                  REGIONAL COMBO TEST                          
         CLC   EZSPRCOM(3),=C'N00' ALWAYS PASS N00                              
         BE    LSWRSP06                                                         
         CLI   EZSPRCOM,C'Y'       IF A COMBO SUMMARY                           
         BNE   LSWRSP04                                                         
*                                                                               
         CLI   EZRCSOPT,C'Y'       SKIP IF PROFILE SAYS SO                      
         BNE   LSWRSPTX                                                         
         B     LSWRSP06                                                         
*                                                                               
LSWRSP04 DS    0H                  NOT A COMBO                                  
         CLI   EZRCSOPT,C'Y'       SKIP IF PROFILE SAYS SO                      
         BE    LSWRSPTX                                                         
*                                                                               
LSWRSP06 DS    0H                                                               
         ICM   RF,3,INSCTR         BUMP INSERTION COUNTER                       
         LA    RF,1(RF)                                                         
         STCM  RF,3,INSCTR                                                      
*                                                                               
         CLM   RF,3,ELTSEQ         SEQUENCE NUMBER IN TABLE ELEMENT             
         BNE   LSWROVRN            SKIP IF NOT CORRECT INSERTION                
*                                                                               
         L     RF,EZWKRREC         POINT TO WORKER RECORD                       
         LA    RF,7(RF)            BYPASS RECLEN(4),RECCODE(2),DELIM(1)         
         USING EZSPFRST,RF         ESTABLISH INSERT AREA                        
*                                                                               
         MVC   EZSPCVPR,ELTCVPR    REPLACE PRODUCT                              
         MVC   EZSPCVES,ELTCVES    REPLACE ESTIMATE                             
*                                                                               
         DROP  RF                                                               
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'WRITE',EPICWK,CURWKIXD,AIO2,WRKFBUFA             
*                                  WRITE CHANGES TO FILE                        
LSWROVRX DS    0H                                                               
*                                                                               
         B     LSWRSPTX                                                         
*                                                                               
LSWROVRN DS   0H                                                                
*                                                                               
         TM    WPRDSW,WPRDDFTQ     SKIP UNLESS DEFAULT PRD CHANGED              
         BNO   LSWRDFTX                                                         
*                                                                               
         CLC   EZSPCVPR,SPACES     SKIP IF ALREADY OVERRIDDEN                   
         BH    LSWRDFTX                                                         
*                                                                               
         L     RF,EZWKRREC         POINT TO WORKER RECORD                       
         LA    RF,7(RF)            BYPASS RECLEN(4),RECCODE(2),DELIM(1)         
         USING EZSPFRST,RF         ESTABLISH INSERT AREA                        
*                                                                               
         MVC   EZSPCVPR,EZIHCVPR   FORCE TO OLD DEFAULT PRODUCT                 
         MVC   EZSPCVES,EZIHCVES   AND ESTIMATE                                 
*                                                                               
         DROP  RF                                                               
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'WRITE',EPICWK,CURWKIXD,AIO2,WRKFBUFA             
*                                  WRITE CHANGES TO FILE                        
LSWRDFTX DS    0H                                                               
*                                                                               
         B     LSWRSPTX                                                         
*                                                                               
LSWRSPTX DS   0H                                                                
*                                                                               
*        HERE IS WHERE WE SHOULD GET NEXT ELEMENT IF THERE WERE MORE            
*                                                                               
         B     LSWRKX                                                           
*                                                                               
*        LAST FOR INVOICE                                                       
*        IF CORRECT ONE, SAVE INVOICE HEADER SECTION OF PZBLOCK                 
*                                                                               
LSWRINVL DS    0H                                                               
*                                                                               
         CLI   FOUNDSW,C'Y'        SKIP IF NOT WANTED INVOICE                   
         BNE   LSWRINLX                                                         
*                                                                               
LSWRINLX DS    0H                                                               
*                                                                               
         B     LSWRKX                                                           
*                                                                               
LSWRKX   DS    0H                                                               
         XIT1                                                                   
*                                                                               
         DROP  R4,R6                                                            
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'PPEZF08 - ADD ELEMENT TO INSERTION TABLE - LSBTHOOK'            
***********************************************************************         
*                                                                     *         
*        ROUTINE TO BUILD ELEMENT TABLE FOR LINUP                     *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
LSBTHOOK NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R4,WRKELTAB         POINT TO ELTAB WORK ELEMENT                  
         USING ELTABD,R4           ESTABLISH TABLE ENTRY                        
*                                                                               
         L     R6,AIO3             ESTABLISH PZBLOCK                            
         USING EZBLOCKD,R6                                                      
*                                                                               
*        DETERMINE HOOK MODE                                                    
*                                                                               
         CLI   EZMODE,EZINVP       INVOICE START                                
         BE    LSBHINVP                                                         
*                                                                               
         CLI   EZMODE,EZSPTP       INSERTION DETAIL                             
         BE    LSBHSPTP                                                         
*                                                                               
         CLI   EZMODE,EZINVL       INVOICE LAST                                 
         BE    LSBHINVL                                                         
*                                                                               
         B     LSBHKX              UNWANTED HOOK                                
*                                                                               
*        DETERMINE IF THIS IS DESIRED INVOICE                                   
*                                                                               
LSBHINVP DS    0H                                                               
*                                                                               
         CLI   FOUNDSW,C'Y'        IF WE JUST DID THE WANTED INVOICE            
         BNE   *+8                                                              
         MVI   FOUNDSW,C'X'           SET ALL OVER FLAG                         
*                                                                               
         CLI   FOUNDSW,0           IF INVOICE ALREADY FOUND                     
         BNE   LSBHSKIP               SKIP TO NEXT INVOICE                      
*                                                                               
*        TEST IF WANTED INVOICE                                                 
*                                                                               
         ZIC   RF,SELLISTN         GET LINE NUMBER                              
         MH    RF,=Y(INVENTL)      CALCULATE INDEX INTO TABLE                   
*                                                                               
         LA    RF,INVLIST(RF)      ESTABLISH SAVED INVOICE ID                   
         USING INVLISTD,RF                                                      
*                                                                               
         CLI   PFAID,0             IF PFKEY ENTERED                             
         NOP   *+8                                                              
         LA    RF,SVINVLST            USE INVOICE ENTRY FROM LAST TIME          
*                                                                               
         CLC   EZINVSEQ,INVRSEQ    SKIP INVOICE IF NOT WANTED ONE               
         BNE   LSBHSKIP                                                         
*                                                                               
         MVC   SVINVSEQ,EZINVSEQ     SAVE INVOICE HEADER SEQ                    
         MVI   FOUNDSW,C'Y'                                                     
*                                                                               
         MVI   DELETESW,C'N'       INIT DELETE SWITCH                           
         MVI   CONVRTSW,C'N'       SET NOT CONVERTED                            
         MVI   RECONVSW,C'N'                                                    
*                                                                               
         TM    EZIHCVST,EZIHCDEL   TEST DELETED                                 
         BZ    *+8                                                              
         MVI   DELETESW,C'Y'                                                    
*                                                                               
         TM    EZIHCVST,EZIHCVQ    TEST CONVERTED                               
         BZ    *+8                                                              
         MVI   CONVRTSW,C'Y'                                                    
*                                                                               
         TM    EZIHCVST,EZIHRCVQ   TEST RECONVERT                               
         BZ    *+8                                                              
         MVI   RECONVSW,C'Y'                                                    
*                                                                               
LSBHINHX DS    0H                                                               
*                                                                               
         B     LSBHKX                                                           
*                                                                               
LSBHSKIP DS    0H                  SKIP TO NEXT INVOICE                         
*                                                                               
         MVI   EZMODE,EZINVL       SKIP TO NEXT INVOICE                         
*                                                                               
         B     LSBHKX              RETURN TO PZMOD                              
*                                                                               
         DROP  RF                                                               
*                                                                               
*        ADD INSERTION TO TABLE                                                 
*                                                                               
LSBHSPTP DS    0H                                                               
*                                                                               
         CLC   EZSNSTA(8),=C'WSJ'  SKIP UNLESS WSJ                              
         BNE   LSBHSP06                                                         
*                                  REGIONAL COMBO TEST                          
         CLC   EZSPRCOM(3),=C'N00' ALWAYS PASS N00                              
         BE    LSBHSP06                                                         
         CLI   EZSPRCOM,C'Y'       IF A COMBO SUMMARY                           
         BNE   LSBHSP04                                                         
*                                                                               
         CLI   EZRCSOPT,C'Y'       SKIP IF PROFILE SAYS SO                      
         BNE   LSBHSPTX                                                         
         B     LSBHSP06                                                         
*                                                                               
LSBHSP04 DS    0H                  NOT A COMBO                                  
         CLI   EZRCSOPT,C'Y'       SKIP IF PROFILE SAYS SO                      
         BE    LSBHSPTX                                                         
*                                                                               
LSBHSP06 DS    0H                                                               
*                                                                               
         XC    ELTABD(ELTABL),ELTABD    INIT ENTRY                              
*                                                                               
*        APPLY DETAIL FILTERS IF NEEDED                                         
*                                                                               
*        PRODUCT CODE FILTER                                                    
*                                                                               
         ICM   RF,3,INSCTR         BUMP INSERTION COUNTER                       
         LA    RF,1(RF)                                                         
         STCM  RF,3,INSCTR                                                      
*                                                                               
         OC    DFTQPRD,DFTQPRD     SKIP IF NO PRODUCT CODE FILTER               
         BZ    LSBHPRDX                                                         
*                                                                               
         TM    DFTQPRD,X'40'       LOWERCASE INDICATES NEGATIVE FILTER          
         BNO   LSBHPRD5                                                         
*                                                                               
         CLC   DFTQPRD,=X'FFFFFF' IF LOOKING FOR ABSENCE OF PRD CODE            
         BNE   LSBHPRD1                                                         
*                                                                               
         CLC   EZSPCVPR,SPACES        IF DETAIL OVERRIDE                        
         BH    LSBHNEXT                  REJECT                                 
*                                                                               
         CLC   =C'***',EZIHCVPR       SKIP TEST IF VARIOUS PRODUCTS             
         BE    *+14                                                             
         CLC   EZIHCVPR,SPACES        IF HEADER OVERRIDE                        
         BH    LSBHNEXT                  REJECT                                 
*                                                                               
         CLC   EZSPLPRC,SPACES        IF LOOKED-UP PRODUCT CODE                 
         BH    LSBHNEXT                  REJECT                                 
*                                                                               
         CLC   =C'***',EZIHLPRC       SKIP TEST IF VARIOUS PRODUCTS             
         BE    *+14                                                             
         CLC   EZIHLPRC,SPACES        IF LOOKED-UP PRODUCT CODE                 
         BH    LSBHNEXT                  REJECT                                 
*                                                                               
         B     LSBHPRDX               ELSE ACCEPT                               
*                                                                               
LSBHPRD1 DS    0H                  MATCHING TO A PRODUCT CODE                   
*                                                                               
         CLC   DFTQPRD,EZSPCVPR    IF PRODUCT CODE MATCHES OVERRIDE             
         BE    LSBHPRDX               ACCEPT                                    
         CLC   EZSPCVPR,SPACES     REJECT IF CONVERTED PROD PRESENT             
         BH    LSBHNEXT                                                         
*                                                                               
         CLC   =C'***',EZIHCVPR    SKIP IF VARIOUS PRODUCTS                     
         BE    *+24                                                             
         CLC   DFTQPRD,EZIHCVPR    IF PRODUCT CODE MATCHES OVERRIDE             
         BE    LSBHPRDX                  ACCEPT                                 
         CLC   EZIHCVPR,SPACES     REJECT IF CONVERTED PROD PRESENT             
         BH    LSBHNEXT                                                         
*                                                                               
         CLC   DFTQPRD,EZSPLPRC    IF LOOKED-UP PRODUCT CODE MATCHES            
         BE    LSBHPRDX                  ACCEPT                                 
         CLC   EZIHLPRC,SPACES     REJECT IF CONVERTED PROD PRESENT             
         BH    LSBHNEXT                                                         
*                                                                               
         CLC   =C'***',EZIHLPRC    SKIP IF VARIOUS PRODUCTS                     
         BE    *+14                                                             
         CLC   DFTQPRD,EZIHLPRC    IF PRODUCT CODE MATCHES LOOKUP               
         BE    LSBHPRDX                  ACCEPT                                 
*                                                                               
         B     LSBHNEXT               ELSE REJECT                               
*                                                                               
LSBHPRD5 DS    0H                  NEGATIVE MATCHING                            
*                                                                               
         CLC   DFTQPRD,=X'BFFFFF' IF LOOKING FOR PRESENCE OF PRD CODE           
         BNE   LSBHPRD6                                                         
*                                                                               
         CLC   EZSPCVPR,SPACES        IF DETAIL OVERRIDE                        
         BH    LSBHPRDX                  ACCEPT                                 
*                                                                               
         CLC   =C'***',EZIHCVPR       SKIP TEST IF VARIOUS PRODUCTS             
         BE    *+14                                                             
         CLC   EZIHCVPR,SPACES        IF HEADER OVERRIDE                        
         BH    LSBHPRDX                  ACCEPT                                 
*                                                                               
         CLC   EZSPLPRC,SPACES        IF LOOKED-UP PRODUCT CODE                 
         BH    LSBHPRDX                  ACCEPT                                 
*                                                                               
         CLC   =C'***',EZIHLPRC       SKIP TEST IF VARIOUS PRODUCTS             
         BE    *+14                                                             
         CLC   EZIHLPRC,SPACES        IF LOOKED-UP PRODUCT CODE                 
         BH    LSBHPRDX                  ACCEPT                                 
*                                                                               
         B     LSBHNEXT               ELSE REJECT                               
*                                                                               
LSBHPRD6 DS    0H                  EXCLUDING A PRODUCT CODE                     
*                                                                               
         OI    DFTQPRD,X'40'       FORCE UPPERCASE                              
*                                                                               
         CLC   DFTQPRD,EZSPCVPR    IF PRODUCT CODE MATCHES OVERRIDE             
         BE    LSBHPRD7               REJECT                                    
*                                                                               
         CLC   EZSPCVPR,SPACES     ACCEPT IF CONVERTED PROD PRESENT             
         BH    LSBHPRD8                                                         
*                                                                               
         CLC   =C'***',EZIHCVPR    SKIP IF VARIOUS PRODUCTS                     
         BE    *+24                                                             
         CLC   DFTQPRD,EZIHCVPR    IF PRODUCT CODE MATCHES OVERRIDE             
         BE    LSBHPRD7                  REJECT                                 
         CLC   EZIHCVPR,SPACES     ACCEPT IF CONVERTED PROD PRESENT             
         BH    LSBHPRD8                                                         
*                                                                               
         CLC   DFTQPRD,EZSPLPRC    IF LOOKED-UP PRODUCT CODE MATCHES            
         BE    LSBHPRD7                  REJECT                                 
         CLC   EZIHLPRC,SPACES     ACCEPT IF CONVERTED PROD PRESENT             
         BH    LSBHPRD8                                                         
*                                                                               
         CLC   =C'***',EZIHLPRC    SKIP IF VARIOUS PRODUCTS                     
         BE    *+14                                                             
         CLC   DFTQPRD,EZIHLPRC    IF PRODUCT CODE MATCHES LOOKUP               
         BE    LSBHPRD7                  REJECT                                 
*                                                                               
         B     LSBHPRD8               ELSE ACCEPT                               
*                                                                               
LSBHPRD7 DS    0H                  EXCLUDING A PRODUCT CODE                     
*                                                                               
         NI    DFTQPRD,X'FF'-X'40'       RETURN TO LOWERCASE                    
         B     LSBHNEXT                  REJECT                                 
*                                                                               
LSBHPRD8 DS    0H                  EXCLUDING A PRODUCT CODE                     
*                                                                               
         NI    DFTQPRD,X'FF'-X'40'    RETURN TO LOWERCASE                       
         B     LSBHPRDX               ELSE ACCEPT                               
*                                                                               
LSBHPRDX DS    0H                                                               
*                                                                               
*        PRODUCT NAME FILTER                                                    
*                                                                               
         OC    DFTPRDN,DFTPRDN     SKIP IF NO PRODUCT NAME FILTER               
         BZ    LSBHPRNX                                                         
*                                                                               
         TM    DFTPRDN,X'40'       LOWERCASE INDICATES NEGATIVE FILTER          
         BNO   LSBHPRN5                                                         
*                                                                               
         CLI   DFTPRDN,X'FF'       IF LOOKING FOR ABSENCE OF PRD NAME           
         BNE   LSBHPRN1                                                         
*                                                                               
         CLC   EZSPLPRN,SPACES        IF NO OVERRIDE                            
         BH    *+10                                                             
         CLC   EZSPPRNM,SPACES        OR SOURCE NAME                            
         BNH   LSBHPRNX                  ACCEPT                                 
*                                                                               
         B     LSBHNEXT               ELSE REJECT                               
*                                                                               
LSBHPRN1 DS    0H                  MATCHING TO A PRODUCT NAME                   
*                                                                               
         SR    RF,RF                                                            
         IC    RF,DFTPRDNL         PRODUCT NAME EXECUTE LENGTH                  
*                                                                               
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   EZSPLPRN(0),DFTPRDN    MATCH TO DDS OVERRIDE NAME                
         BE    LSBHPRNX                                                         
*                                                                               
         CLC   EZSPLPRN,SPACES     REJECT IF CONVERTED PROD PRESENT             
         BH    LSBHNEXT                                                         
*                                                                               
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   EZSPPRNM(0),DFTPRDN    MATCH TO SOURCE PRODUCT NAME              
         BE    LSBHPRNX                                                         
*                                                                               
         B     LSBHNEXT            ELSE REJECT                                  
*                                                                               
LSBHPRN5 DS    0H                  NEGATIVE MATCHING                            
*                                                                               
         CLI   DFTPRDN,X'BF'       IF LOOKING FOR PRESENCE OF PRD NAME          
         BNE   LSBHPRN6                                                         
*                                                                               
         CLC   EZSPLPRN,SPACES        IF OVERRIDE                               
         BH    *+10                                                             
         CLC   EZSPPRNM,SPACES        OR SOURCE NAME PRESENT                    
         BH    LSBHPRNX                  ACCEPT                                 
*                                                                               
         B     LSBHNEXT               ELSE REJECT                               
*                                                                               
LSBHPRN6 DS    0H                  EXCLUDING A PRODUCT NAME                     
*                                                                               
         OI    DFTPRDN,X'40'       FORCE UPPERCASE                              
*                                                                               
         SR    RF,RF                                                            
         IC    RF,DFTPRDNL         PRODUCT NAME EXECUTE LENGTH                  
*                                                                               
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   EZSPLPRN(0),DFTPRDN    MATCH TO DDS OVERRIDE NAME                
         BE    LSBHPRN8               REJECT                                    
*                                                                               
         CLC   EZSPLPRN,SPACES     ACCEPT IF CONVERTED PROD PRESENT             
         BH    LSBHPRN7                                                         
*                                                                               
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   EZSPPRNM(0),DFTPRDN    MATCH TO SOURCE PRODUCT NAME              
         BE    LSBHPRN8               REJECT                                    
*                                                                               
LSBHPRN7 DS    0H                                                               
*                                                                               
         NI    DFTPRDN,X'FF'-X'40'    RETURN TO LOWERCASE                       
         B     LSBHPRNX            ELSE ACCEPT                                  
*                                                                               
LSBHPRN8 DS    0H                                                               
*                                                                               
         NI    DFTPRDN,X'FF'-X'40'       RETURN TO LOWERCASE                    
         B     LSBHNEXT                  REJECT                                 
*                                                                               
LSBHPRNX DS    0H                                                               
*                                                                               
*        INSERTION ORDER NUMBER FILTER                                          
*                                                                               
         OC    DFTIONO,DFTIONO     SKIP IF NO INS ORDER NUMBER FILTER           
         BZ    LSBHIONX                                                         
*                                                                               
         TM    DFTIONO,X'40'       LOWERCASE INDICATES NEGATIVE FILTER          
         BNO   LSBHION5                                                         
*                                                                               
         CLI   DFTIONO,X'FF'       IF LOOKING FOR ABSENCE OF I/O NUMBER         
         BNE   LSBHION1                                                         
*                                                                               
         CLC   EZSPIORD,SPACES       IF NO I/O NUMBER                           
         BNH   LSBHIONX                  ACCEPT                                 
*                                                                               
         B     LSBHNEXT               ELSE REJECT                               
*                                                                               
LSBHION1 DS    0H                  MATCHING TO AN I/O NUMBER                    
*                                                                               
         SR    RF,RF                                                            
         IC    RF,DFTIONLN         I/O NUMBER EXECUTE LENGTH                    
*                                                                               
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   EZSPIORD(0),DFTIONO   MATCH TO SOURCE I/O NUMBER                 
         BE    LSBHIONX                                                         
*                                                                               
         B     LSBHNEXT            ELSE REJECT                                  
*                                                                               
LSBHION5 DS    0H                  NEGATIVE MATCHING                            
*                                                                               
         CLI   DFTIONO,X'BF'       IF LOOKING FOR PRESENCE OF I/O NO.           
         BNE   LSBHION6                                                         
*                                                                               
         CLC   EZSPIORD,SPACES        IF I/O NUMBER PRESENT                     
         BH    LSBHIONX                  ACCEPT                                 
*                                                                               
         B     LSBHNEXT               ELSE REJECT                               
*                                                                               
LSBHION6 DS    0H                  EXCLUDING AN I/O NUMBER                      
*                                                                               
         OI    DFTIONO,X'40'       FORCE UPPERCASE                              
*                                                                               
         SR    RF,RF                                                            
         IC    RF,DFTIONLN         I/O NUMBER EXECUTE LENGTH                    
*                                                                               
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   EZSPIORD(0),DFTIONO    MATCH TO I/O NUMBER                       
         BE    LSBHION8               REJECT                                    
*                                                                               
         NI    DFTIONO,X'FF'-X'40'    RETURN TO LOWERCASE                       
         B     LSBHIONX            ELSE ACCEPT                                  
*                                                                               
LSBHION8 DS    0H                                                               
*                                                                               
         NI    DFTIONO,X'FF'-X'40'       RETURN TO LOWERCASE                    
         B     LSBHNEXT                  REJECT                                 
*                                                                               
LSBHIONX DS    0H                                                               
*                                                                               
*        INSERTION DATE FILTER                                                  
*                                                                               
         CLI   DFTIDATE,0          FILTER ON INSERTION DATE                     
         BNH   LSBHINSX            NO FILTER                                    
*                                                                               
         CLI   DFTDATES,0         FILTER ON EXACT DATE                          
         BNE   LSBH174                                                          
*                                                                               
         CLC   EZSPDATE,DFTIDATE   FILTER ON EXACT DATE                         
         BNE   LSBHNEXT                                                         
         B     LSBHINSX                                                         
*                                                                               
LSBH174  CLI   DFTDATES,C'+'      PLUS                                          
         BE    LSBH176                                                          
         CLI   DFTDATES,C'-'      MINUS                                         
         BE    LSBH178                                                          
         DC    H'0'                                                             
*                                                                               
LSBH176  CLC   EZSPDATE,DFTIDATE                                                
         BL    LSBHNEXT                                                         
         B     LSBHINSX                                                         
*                                                                               
LSBH178  CLC   EZSPDATE,DFTIDATE                                                
         BH    LSBHNEXT                                                         
*                                                                               
LSBHINSX DS    0H                                                               
*                                                                               
         B     LSBHUSE             USE ENTRY                                    
LSBHNEXT DS    0H                  INDICATE ELEMENT FLUNKED FILTERING           
*                                                                               
         OI    ELTCTL,ELTFLTXQ     FLUNKED FILTERING                            
*                                                                               
LSBHUSE  DS    0X                                                               
*                                                                               
         MVC   ELTSEQ,INSCTR       SET SEQUENCE NUMBER IN TABLE ELEMENT         
*                                                                               
         MVC   ELTSORT,ELTSEQ      SET KEY                                      
         MVC   ELTCVPR,EZSPCVPR    SAVE CONVERTED PRODUCT                       
         MVC   ELTCVES,EZSPCVES    SAVE CONVERTED ESTIMATE                      
*                                                                               
         ICM   R0,15,BSPNOR                                                     
         GOTO1 =V(BINSRCH),BSPPRMS,('BSPADD',WRKELTAB),RR=RELO                  
         OC    1(3,R1),1(R1)       TEST IF ELEMENT FIT INTO TABLE               
         BNZ   *+6                                                              
         DC    H'0'                TOO MANY LINES (SHOULD NOT HAPPEN)           
*                                                                               
         ICM   RF,15,ELTNOR        UPDATE NUMBER OF RELEVANT ENTRIES            
*                                                                               
         TM    ELTCTL,ELTFLTXQ     SKIP IF IRRELEVANT ENTRY                     
         BO    *+8                                                              
         LA    RF,1(RF)            BUMP COUNTER                                 
*                                                                               
         STCM  RF,15,ELTNOR        REST NUMBER OF ENTRIES                       
*                                                                               
LSBHSPTX DS   0H                                                                
*                                                                               
         B     LSBHKX                                                           
*                                                                               
*        LAST FOR INVOICE                                                       
*        IF CORRECT ONE, SAVE INVOICE HEADER SECTION OF PZBLOCK                 
*                                                                               
LSBHINVL DS    0H                                                               
*                                                                               
         CLI   FOUNDSW,C'Y'        SKIP IF NOT CORRECT INVOICE                  
         BNE   LSBHINLX                                                         
*                                                                               
         MVC   SVINVHDR(256),EZIHFRST  SAVE INVOICE HEADER DATA                 
         MVC   SVINVHDR+256(256),EZIHFRST+256                                   
         MVC   SVINVHDR+512(EZIHDLEN-512),EZIHFRST+512                          
*                                                                               
LSBHINLX DS    0H                                                               
*                                                                               
         B     LSBHKX                                                           
*                                                                               
LSBHKX   DS    0H                                                               
         XIT1                                                                   
*                                                                               
         DROP  R4,R6                                                            
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'PPEZF08 - BUILD WINDOW LINE - LHDISLIN'                         
***********************************************************************         
*                                                                     *         
*        ROUTINE TO BUILD WINDOW LINE                                 *         
*        FROM TABLE ENTRY IN ELTENT                                   *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
LHDISLIN NTR1  BASE=*,LABEL=*                                                   
*                                                                               
*        RE-READ INSERTION DETAILS VIA PZMOD                                    
*          WE CAN'T STORE ENOUGH DATA IN THE TABLE                              
*                                                                               
         B     LHDEZRDX            DATA SHOULD STILL BE IN CORE                 
*                                                                               
         XC    CURWKIXD,CURWKIXD   ESTABLISH WORKER FILE INDEX                  
         LA    R4,CURWKIXD                                                      
         USING EZWKRIXD,R4                                                      
*                                                                               
         LA    R5,SVINVLST         USE INVOICE ENTRY FROM LAST TIME             
         USING INVLISTD,R5         ESTABLISH SAVED INVOICE AREA                 
*                                                                               
         MVC   EZWIUID,INVUID      SET WORKER FILE USER ID                      
         MVC   EZWIPUB,INVPZPUB    WORKER FILE PUB ID                           
         MVC   EZWIMED,INVMEDIA    MEDIA                                        
         MVI   EZWIDAY,X'98'       DAY = 98                                     
*                                                                               
         MVC   EZWKRIXD+8(2),INVBSEQ    BATCH SEQUENCE NUMBER WANTED            
*                                                                               
         DROP  R5                                                               
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'INDEX',EPICWK,EZWKRIXD,AIO2,WRKFBUFA             
         TM    DMCB+8,X'80'        TEST EOF                                     
         BZ    *+6                                                              
         DC    H'0'                BATCH HAS DISAPPEARED                        
*                                                                               
*                                  I/O2 4K WORKAREA FOR WORKER FILE             
*                                                                               
         GOTO1 DATAMGR,(R1),=C'READ',EPICWK,CURWKIXD,AIO2,WRKFBUFA              
*                                    ADDRESSES STILL LEFT FROM INDEX            
         TM    DMCB+8,X'80'        TEST EOF ON FIRST READ                       
         BZ    *+6                                                              
         DC    H'0'                BATCH HAS BEEN CLOBBERED                     
*                                                                               
         L     R5,WRKFBUFA         ESTABLISH WORKER RECORD                      
         USING W_RECD,R5                                                        
*                                                                               
         MVC   SVWCMNT,W_DESC      SAVE COMMENT AREA-MAY HAVE DATA              
*                                    FOR APPLICATION                            
*                                                                               
         LA    R5,W_DESC                                                        
*                                                                               
         DROP  R5                                                               
*                                                                               
LHDEZRDX DS    0H                                                               
*                                                                               
         L     R6,AIO3             ESTABLISH PZBLOCK                            
         USING EZBLOCKD,R6                                                      
*                                                                               
         LR    R0,R6               CLEAR PZBLOCK                                
         LH    R1,=Y(EZBLOCKL)                                                  
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     RE,=A(LSDRHOOK)     DISPLAY HOOK                                 
         A     RE,RELO                                                          
         ST    RE,EZHOOK                                                        
*                                                                               
         XC    INSCTR,INSCTR       INIT INSERTION COUNTER                       
*                                                                               
*        INITIALIZE PZBLOCK                                                     
*                                                                               
         MVI   FOUNDSW,0           INIT FOUND SWITCH                            
*                                                                               
         MVC   EZAGY,AGENCY        AGENCY                                       
         MVI   EZOFF,C'N'          NOT OFF-LINE                                 
         MVI   KEEPSW,C'Y'         KEEP WORKER FILE                             
         MVC   EZWKRFIL,EPICWK     WORKER FILE ID                               
         MVC   EZWKRIND,SVWPZIND   SET INDEX                                    
*                                                                               
         L     R1,WRKFBUFA         PASS BUFFER ADDRESS                          
         ST    R1,EZWKRBUF                                                      
*                                                                               
         MVC   EZWKRREC,AIO2       PASS WORKER FILE WORKAREA ADDRESS            
*                                                                               
         L     RE,=A(IOA4-(CONHEADH-64))                                        
         LA    RE,0(RE,RA)                                                      
         ST    RE,EZAREC           PASS RECORD ADDRESS                          
*                                                                               
         L     RE,ACOMFACS                                                      
         ST    RE,EZCOMFCS                                                      
*                                                                               
         TM    FTRFLAG,FTRTRACE    SET TRACE INDICATOR IF NEEDED                
         BZ    *+8                                                              
         OI    EZTRACE,X'F0'                                                    
*                                                                               
         GOTO1 VPZMOD,DMCB,(R6)    HAVE PZMOD READ FILES                        
*                                                                               
         ICM   R4,15,ELTENT        POINT TO ELEMENT IN TABLE                    
         USING ELTABD,R4           ESTABLISH TABLE ELEMENT                      
*                                                                               
         LA    R3,WRKELTAB         ESTABLISH SPOT DETAILS                       
         USING EZSPFRST,R3                                                      
*                                                                               
*        DISPLAY SEQUENCE NUMBER                                                
*                                                                               
         XC    DTLSQN,DTLSQN       CLEAR FIELD                                  
*                                                                               
         LTR   R4,R4               SKIP IF NO ELEMENT TO DISPLAY                
         BZ    LHDSQNX                                                          
*                                                                               
         EDIT  ELTSEQ,(3,DTLSQN),ALIGN=LEFT  DISPLAY INSERT SEQ NUMBER          
*                                                                               
LHDSQNX  DS    0H                                                               
*                                                                               
         OI    DTLSQNH+4,X'20'     SET FIELD AS VALIDATED                       
*                                                                               
         OI    DTLSQNH+6,X'80'     TRANSMIT FIELD                               
*                                                                               
*        DISPLAY TOTAL NUMBER OF INSERTIONS                                     
*                                                                               
         XC    DTLTSQN,DTLTSQN     CLEAR FIELD                                  
*                                                                               
         LTR   R4,R4               SKIP IF NO ELEMENT TO DISPLAY                
         BZ    LHDTSQNX                                                         
*                                                                               
         EDIT  TINSCTR,(3,DTLTSQN),ALIGN=LEFT   DISPLAY TOTAL #                 
*                                                                               
LHDTSQNX DS    0H                                                               
*                                                                               
         OI    DTLTSQNH+6,X'80'    TRANSMIT FIELD                               
*                                                                               
*        DISPLAY INVOICE PRODUCT CODE                                           
*                                                                               
         XC    DTLPPE,DTLPPE       CLEAR FIELD                                  
*                                                                               
         LTR   R4,R4               SKIP IF NO ELEMENT TO DISPLAY                
         BZ    LHDPPEX                                                          
*                                                                               
         MVC   DTLPPE,EZSPPRCD     INVOICE DETAIL PRODUCT CODE                  
*                                                                               
         CLC   DTLPPE,SPACES       IF NOT PRESENT ON DETAIL LEVEL               
         BH    *+10                                                             
         MVC   DTLPPE,EZIHPRCD-EZIHFRST+SVINVHDR  USE INV PROD CODE             
*                                                                               
LHDPPEX DS     0H                                                               
*                                                                               
         OI    DTLPPEH+6,X'80'     TRANSMIT FIELD                               
*                                                                               
*        DISPLAY INVOICE PRODUCT NAME                                           
*                                                                               
         XC    DTLPNM,DTLPNM       CLEAR FIELD                                  
*                                                                               
         LTR   R4,R4               SKIP IF NO ELEMENT TO DISPLAY                
         BZ    LHDPNMX                                                          
*                                                                               
         MVC   DTLPNM,EZSPPRNM     INVOICE DETAIL PRODUCT NAME                  
*                                                                               
         CLC   DTLPNM,SPACES       IF NOT PRESENT ON DETAIL LEVEL               
         BH    *+10                                                             
         MVC   DTLPNM,EZIHPRNM-EZIHFRST+SVINVHDR   USE INV PROD NAME            
*                                                                               
LHDPNMX  DS    0H                                                               
*                                                                               
         OI    DTLPNMH+6,X'80'     TRANSMIT FIELD                               
*                                                                               
*        DISPLAY DDS PRODUCT CODE/NAME                                          
*                                                                               
         XC    DTLPCD,DTLPCD       CLEAR FIELDS                                 
         XC    DTLPNMD,DTLPNMD     CLEAR FIELDS                                 
         OI    DTLPCDH+1,X'08'     HIGHLIGHT FIELD                              
*                                                                               
         LTR   R4,R4               SKIP IF NO ELEMENT TO DISPLAY                
         BZ    LHDPCDX                                                          
*                                                                               
         MVC   DTLPCD(3),EZSPCVPR  DEFAULT TO PRODUCT OVERRIDE                  
*                                                                               
         CLC   =C'***',DTLPCD      IF PRODUCT VARIOUS                           
         BE    *+14                                                             
         CLC   DTLPCD,SPACES       OR IF NOTHING FOUND                          
         BH    LHDPCD1                                                          
*                                                                               
         NI    DTLPCDH+1,X'FF'-X'0C'  TURN OFF HIGHLIGHTING                     
*                                                                               
         MVC   DTLPCD(3),EZIHCVPR-EZIHFRST+SVINVHDR  USE INV HD OVR             
*                                                                               
         CLC   =C'***',DTLPCD      IF PRODUCT VARIOUS                           
         BE    *+14                                                             
         CLC   DTLPCD,SPACES       IF NOTHING FOUND                             
         BH    LHDPCD1                                                          
*                                                                               
         MVC   DTLPCD(3),EZSPLPRC     USE LOOKED UP DETAIL PRODUCT              
*                                                                               
         CLC   =C'***',DTLPCD      IF PRODUCT VARIOUS                           
         BE    *+14                                                             
         CLC   DTLPCD,SPACES       IF NOTHING FOUND                             
         BH    LHDPCD1                                                          
         MVC   DTLPCD(3),EZIHLPRC-EZIHFRST+SVINVHDR  LOOKED UP HDR PRD          
*                                                                               
         CLC   DTLPCD,SPACES       IF NOTHING FOUND                             
         BH    LHDPCD1                                                          
*                                                                               
         OI    DTLPCDH+1,X'08'        HIGHLIGHT FIELD                           
         B     LHDPCDX                ALL DONE                                  
*                                                                               
LHDPCD1  DS    0H                                                               
*                                                                               
         CLC   =C'***',DTLPCD      IF PRODUCT VARIOUS                           
         BNE   *+14                                                             
         MVC   DTLPCD,SPACES          CLEAR FIELD                               
         B     LHDPCDX                                                          
*                                                                               
         MVC   FILENAME,=C'PRTDIR  '                                            
*                                                                               
         XC    KEY,KEY                                                          
         LA    R1,KEY                                                           
         USING PPRDRECD,R1                                                      
*                                                                               
         MVC   PPRDKAGY,QAGY                                                    
         MVC   PPRDKMED,QMED                                                    
         MVI   PPRDKRCD,PPRDKIDQ                                                
         MVC   PPRDKCLT,DTLCCD                                                  
         OC    PPRDKCLT,SPACES                                                  
         MVC   PPRDKPRD,DTLPCD                                                  
         OC    PPRDKPRD,SPACES                                                  
*                                                                               
         MVI   RDUPDATE,C'N'       NOT READ FOR UPDATE                          
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(25),KEYSAVE                                                  
         BNE   LHDPRDNF                                                         
*                                                                               
         L     R1,AIO1                                                          
         ST    R1,AIO                                                           
*                                                                               
         MVI   RDUPDATE,C'N'       NOT READ FOR UPDATE                          
*                                                                               
         MVC   FILENAME,=C'PRTFIL  '                                            
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         XC    FILENAME,FILENAME                                                
*                                                                               
         L     R1,AIO1                                                          
*                                                                               
         MVC   DTLPNMD,PPRDNAME                                                 
*                                                                               
         DROP  R1                                                               
*                                                                               
LHDPCDX  DS    0H                                                               
*                                                                               
         OI    DTLPCDH+4,X'20'     SET FIELD VALIDATED                          
         OI    DTLPCDH+6,X'80'     TRANSMIT FIELD                               
         OI    DTLPNMDH+6,X'80'    TRANSMIT FIELD                               
*                                                                               
*        DISPLAY INVOICE ESTIMATE                                               
*                                                                               
         XC    DTLEST,DTLEST       CLEAR FIELD                                  
*                                                                               
         LTR   R4,R4               SKIP IF NO ELEMENT TO DISPLAY                
         BZ    LHDESTX                                                          
*                                                                               
         MVC   DTLEST,EZSPEST      DEFAULT TO DETAIL ESTIMATE                   
*                                                                               
         CLC   DTLEST,SPACES       IF NOTHING FOUND                             
         BH    LHDESTX                                                          
         MVC   DTLEST,EZIHEST-EZIHFRST+SVINVHDR      USE INV HDR EST            
*                                                                               
LHDESTX  DS    0H                                                               
*                                                                               
         OI    DTLESTH+6,X'80'     TRANSMIT FIELD                               
*                                                                               
*        DISPLAY DDS ESTIMATE CODE/NAME                                         
*                                                                               
         XC    DTLECD,DTLECD       CLEAR FIELDS                                 
         XC    DTLENMD,DTLENMD     CLEAR FIELDS                                 
         OI    DTLECDH+1,X'08'     HIGHLIGHT FIELD                              
*                                                                               
         LTR   R4,R4               SKIP IF NO ELEMENT TO DISPLAY                
         BZ    LHDECDX                                                          
*                                                                               
*                                  DEFAULT TO ESTIMATE OVERRIDE                 
         OC    EZSPCVES,EZSPCVES   SKIP IF NULLS                                
         BE    LHDECD0                                                          
*                                                                               
         EDIT  (B2,EZSPCVES),(3,DTLECD),ALIGN=LEFT                              
*                                                                               
         CLC   DTLECD,SPACES       IF NOTHING FOUND                             
         BH    LHDECD1                                                          
*                                                                               
LHDECD0  DS    0H                                                               
*                                                                               
         NI    DTLECDH+1,X'FF'-X'0C'  TURN OFF HIGHLIGHTING                     
*                                                                               
         LA    R2,EZIHCVES-EZIHFRST+SVINVHDR  USE INV HDR OVR                   
         EDIT  (B2,0(R2)),(3,DTLECD),ALIGN=LEFT                                 
*                                                                               
         CLC   DTLECD,SPACES       IF NOTHING FOUND                             
         BH    LHDECDX                                                          
*                                                                               
         OI    DTLECDH+1,X'08'        HIGHLIGHT FIELD                           
         B     LHDECDX                ALL DONE                                  
*                                                                               
LHDECD1  DS    0H                                                               
*                                                                               
         MVC   FILENAME,=C'PRTDIR  '                                            
*                                                                               
         XC    KEY,KEY                                                          
         LA    R1,KEY                                                           
         USING PESTRECD,R1                                                      
*                                                                               
         MVC   PESTKAGY,QAGY                                                    
         MVC   PESTKMED,QMED                                                    
         MVI   PESTKRCD,PESTKIDQ                                                
         MVC   PESTKCLT,DTLCCD                                                  
         MVC   PESTKPRD,DTLPCD                                                  
         OC    EZSPCVES,EZSPCVES   SKIP IF NULLS                                
         BZ    *+20                                                             
         MVC   PESTKEST,EZSPCVES   DETAIL ESTIMATE OVERRIDE                     
         OC    PESTKEST,PESTKEST   IF NO OVERRIDE AVAILABLE                     
         BNZ   *+10                                                             
         MVC   PESTKEST,EZIHCVES-EZIHFRST+SVINVHDR  USE INV OVERRIDE            
*                                                                               
         MVI   RDUPDATE,C'N'       NOT READ FOR UPDATE                          
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(25),KEYSAVE                                                  
         BE    LHDECD2                                                          
*                                                                               
         CLC   DTLPCD(3),EZSPLPRC  IF USING A DETAIL PRODUCT                    
         BE    *+10                                                             
         CLC   DTLPCD(3),EZSPCVPR                                               
         BE    LHDESTNF               ERROR                                     
*                                                                               
         MVC   DTLECD,SPACES       ELSE WIPE OUT ESTIMATE CODE                  
         B     LHDECDX                                                          
*                                                                               
LHDECD2  DS    0H                                                               
*                                                                               
         L     R1,AIO1                                                          
         ST    R1,AIO                                                           
*                                                                               
         MVI   RDUPDATE,C'N'       NOT READ FOR UPDATE                          
*                                                                               
         MVC   FILENAME,=C'PRTFIL  '                                            
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         XC    FILENAME,FILENAME                                                
*                                                                               
         L     R1,AIO1                                                          
         USING PESTRECD,R1                                                      
*                                                                               
         MVC   DTLENMD,PESTNAME                                                 
*                                                                               
         DROP  R1                                                               
*                                                                               
LHDECDX  DS    0H                                                               
*                                                                               
         OI    DTLECDH+4,X'20'     SET FIELD AS VALIDATED                       
         OI    DTLECDH+6,X'80'     TRANSMIT FIELD                               
         OI    DTLENMDH+6,X'80'    TRANSMIT FIELD                               
*                                                                               
*        DISPLAY ZONE/EDITION                                                   
*                                                                               
         XC    DTLEDN,DTLEDN       INIT EDITION FIELDS- SOURCE CODE             
         XC    DTLEDDC,DTLEDDC     DDS CODE                                     
         XC    DTLEDDN,DTLEDDN     DDS NAME                                     
*                                                                               
         MVC   DTLEDN,EZSPEDCD     EDITION CODE                                 
*                                                                               
         LTR   R4,R4               SKIP IF NO ELEMENT TO DISPLAY                
         BZ    LHDEDNX                                                          
*                                                                               
         CLI   EZSPLZON,0          SKIP UNLESS ZONE                             
         BNE   *+8                                                              
         CLI   EZSPLEDC,0          OR EDITION PRESENT                           
         BE    LHDEDNX                                                          
*                                                                               
         MVC   DUB(4),EZSNPUB      FORMAT DDS EDITION ID                        
         MVC   DUB+4(1),EZSPLZON                                                
         MVC   DUB+5(1),EZSPLEDC                                                
*                                                                               
         GOTO1 VPUBEDIT,DMCB,(C'0',DUB),(C'S',DTLEDDC) ZONE/EDN CODE            
*                                                                               
*                                                                               
         MVC   DTLEDDN,EZSPEDNM    EDITION NAME                                 
*                                                                               
LHDEDNX  DS    0H                  INDICATE EDITION IS VALID                    
*                                                                               
         OI    DTLEDNH+6,X'80'     FORCE FIELD TRANSMISSION                     
         OI    DTLEDDCH+6,X'80'    FORCE FIELD TRANSMISSION                     
         OI    DTLEDDNH+6,X'80'    FORCE FIELD TRANSMISSION                     
*                                                                               
*                                                                               
*        DISPLAY INSERT DATE                                                    
*                                                                               
         XC    DTLIDT,DTLIDT       CLEAR FIELD                                  
*                                                                               
         LTR   R4,R4               SKIP IF NO ELEMENT TO DISPLAY                
         BZ    LHDIDTX                                                          
*                                                                               
         MVC   DTLIDT,EZSPDDT      SET INSERTION DATE                           
*                                                                               
LHDIDTX  DS    0H                                                               
*                                                                               
         OI    DTLIDTH+6,X'80'     TRANSMIT FIELD                               
*                                                                               
*        DISPLAY INSERTION ORDER NUMBER                                         
*                                                                               
         XC    DTLION,DTLION       CLEAR FIELD                                  
*                                                                               
         LTR   R4,R4               SKIP IF NO ELEMENT TO DISPLAY                
         BZ    LHDIONX                                                          
*                                                                               
         MVC   DTLION,EZSPIORD     SET INSERTION ORDER NUMBER                   
*                                                                               
LHDIONX  DS    0H                                                               
*                                                                               
         OI    DTLIONH+6,X'80'     TRANSMIT FIELD                               
*                                                                               
*        DISPLAY CAPTION                                                        
*                                                                               
         XC    DTLCAP,DTLCAP       CLEAR FIELD                                  
*                                                                               
         LTR   R4,R4               SKIP IF NO ELEMENT TO DISPLAY                
         BZ    LHDCAPX                                                          
*                                                                               
         MVC   DTLCAP,EZSPCAPT     SET CAPTION                                  
*                                                                               
LHDCAPX  DS    0H                                                               
*                                                                               
         OI    DTLCAPH+6,X'80'     TRANSMIT FIELD                               
*                                                                               
*        DISPLAY SPACE                                                          
*                                                                               
         XC    DTLSPC,DTLSPC       CLEAR FIELD                                  
*                                                                               
         LTR   R4,R4               SKIP IF NO ELEMENT TO DISPLAY                
         BZ    LHDSPCX                                                          
*                                                                               
         MVC   DTLSPC,EZSPSPCE     SET SPACE DESCRIPTION                        
*                                                                               
LHDSPCX  DS    0H                                                               
*                                                                               
         OI    DTLSPCH+6,X'80'     TRANSMIT FIELD                               
*                                                                               
*        DISPLAY LINES                                                          
*                                                                               
         XC    DTLLNS,DTLLNS       CLEAR FIELD                                  
*                                                                               
         LTR   R4,R4               SKIP IF NO ELEMENT TO DISPLAY                
         BZ    LHDLNSX                                                          
*                                                                               
         EDIT  EZSPBLNS,(5,DTLLNS),ALIGN=LEFT  SET NUMBER OF LINES              
*                                                                               
LHDLNSX DS     0H                                                               
*                                                                               
         OI    DTLLNSH+6,X'80'     TRANSMIT FIELD                               
*                                                                               
*        DISPLAY COLUMNS                                                        
*                                                                               
         XC    DTLCOLS,DTLCOLS     CLEAR FIELD                                  
*                                                                               
         LTR   R4,R4               SKIP IF NO ELEMENT TO DISPLAY                
         BZ    LHDCOLSX                                                         
*                                                                               
         EDIT  EZSPBCLS,(5,DTLCOLS),ALIGN=LEFT    SET NUMBER OF COLUMNS         
*                                                                               
LHDCOLSX DS    0H                                                               
*                                                                               
         OI    DTLCOLSH+6,X'80'    TRANSMIT FIELD                               
*                                                                               
*        DISPLAY INCHES                                                         
*                                                                               
         XC    DTLINS,DTLINS       CLEAR FIELD                                  
*                                                                               
         LTR   R4,R4               SKIP IF NO ELEMENT TO DISPLAY                
         BZ    LHDINSX                                                          
*                                                                               
         EDIT  EZSPBICS,(6,DTLINS),3,ALIGN=LEFT   SET NUMBER OF INCHES          
*                                                                               
LHDINSX DS     0H                                                               
*                                                                               
         OI    DTLINSH+6,X'80'     TRANSMIT FIELD                               
*                                                                               
*        DISPLAY AD CODE                                                        
*                                                                               
         XC    DTLADC,DTLADC       CLEAR FIELD                                  
*                                                                               
         LTR   R4,R4               SKIP IF NO ELEMENT TO DISPLAY                
         BZ    LHDADCX                                                          
*                                                                               
         MVC   DTLADC,EZSPADCD     SET AD CODE                                  
*                                                                               
LHDADCX DS     0H                                                               
*                                                                               
         OI    DTLADCH+6,X'80'     TRANSMIT FIELD                               
*                                                                               
*        DISPLAY COPY CODE                                                      
*                                                                               
         XC    DTLCPY,DTLCPY       CLEAR FIELD                                  
*                                                                               
         LTR   R4,R4               SKIP IF NO ELEMENT TO DISPLAY                
         BZ    LHDCPYX                                                          
*                                                                               
         MVC   DTLCPY,EZSPCOPY     SET COPY CODE                                
*                                                                               
LHDCPYX DS     0H                                                               
*                                                                               
         OI    DTLCPYH+6,X'80'     TRANSMIT FIELD                               
*                                                                               
*        DISPLAY COLOR CODE                                                     
*                                                                               
         XC    DTLCLC,DTLCLC       CLEAR FIELD                                  
*                                                                               
         LTR   R4,R4               SKIP IF NO ELEMENT TO DISPLAY                
         BZ    LHDCLCX                                                          
*                                                                               
         MVC   DTLCLC,EZSPCLPR     SET COLOR CODE                               
*                                                                               
LHDCLCX DS     0H                                                               
*                                                                               
         OI    DTLCLCH+6,X'80'     TRANSMIT FIELD                               
*                                                                               
*        DISPLAY POSITION                                                       
*                                                                               
         XC    DTLPOS,DTLPOS       CLEAR FIELD                                  
*                                                                               
         LTR   R4,R4               SKIP IF NO ELEMENT TO DISPLAY                
         BZ    LHDPOSX                                                          
*                                                                               
*        COLLAPSE 3 POSITION FIELDS INTO 1                                      
*           SEPARATE WITH SLASHES                                               
*                                                                               
         LA    R2,L'DTLPOS-1       MAX ROOM IN FIELD                            
         LA    R1,DTLPOS           START OF POSITION ON SCREEN                  
         LA    R5,EZSPBPM1         PREMIUM FOR POSITION                         
         LA    RE,EZSPPOS1         START OF A POSITION FIELD                    
*                                                                               
LHDPOSLP DS    0H                                                               
*                                                                               
         LA    RF,L'EZSPPOS1       LENGTH OF A POSITION FIELD                   
         ST    RE,FULL             SAVE START POSITION                          
         LA    RE,L'EZSPPOS1-1(RE) LAST BYTE IN FIELD                           
*                                                                               
         CLI   0(RE),C' '          FIND LAST NON-SPACE                          
         BH    *+14                                                             
         BCTR  RE,0                                                             
         BCT   RF,*-10                                                          
         B     LHDPOS99                                                         
*                                                                               
         CR    RF,R2               MAKE SURE THERE IS ROOM FOR FIELD            
         BNH   *+6                                                              
         LR    RF,R2               DEFAULT TO REMAINING LENGTH                  
*                                                                               
         L     RE,FULL             RE-POINT TO START OF POSITION                
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
*                                                                               
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RE),=20C'0'     SKIP IF '000..0'                             
         BE    LHDPOS99                                                         
*                                                                               
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),0(RE)       MOVE POSITION TO SCREEN                      
*                                                                               
         LA    R1,1(RF,R1)         NEXT OUTPUT POSITION                         
*                                                                               
         SR    R2,RF               DECREMENT REMAINING COUNTER                  
         BNP   LHDPOSD1                                                         
*                                                                               
         MVI   0(R1),C'/'          SET SEPARATOR                                
         LA    R1,1(R1)            BUMP OUTPUT POSITION                         
         BCTR  R2,0                DECREMENT COUNTER                            
*                                                                               
LHDPOS99 DS    0H                                                               
*                                                                               
         OC    0(L'EZSPPRM1,R5),0(R5) SKIP IF NO PREMIUM                        
         BZ    LHDPRMX                                                          
*                                                                               
         EDIT  (4,0(R5)),(11,ELEMENT),2,ALIGN=LEFT,ZERO=BLANK SET PREM          
*                                                                               
*        R0 CONTAINS LENGTH OF RESULT                                           
*                                                                               
         SR    R2,R0               DECREMENT REMAINING SPACE COUNTER            
         BM    LHDPOSDN            NO ROOM                                      
*                                                                               
         LTR   RF,R0               PREMIUM LENGTH                               
         BZ    LHDPRMX             NOTHING THERE                                
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),ELEMENT     MOVE PREMIUM TO FIELD                        
*                                                                               
         LA    R1,1(RF,R1)         NEXT OUTPUT POSITION                         
*                                                                               
         BCT   R2,*+8              DECREMENT COUNTER                            
         BM    LHDPOSD1            NO ROOM                                      
*                                                                               
         MVI   0(R1),C'/'          SET SEPARATOR                                
         LA    R1,1(R1)            BUMP OUTPUT POSITION                         
*                                                                               
LHDPRMX  DS    0H                                                               
*                                                                               
LHDPOSCN DS    0H                                                               
*                                                                               
         LA    R5,L'EZSPBPM1(R5)   BUMP TO NEXT PREMIUM                         
         L     RE,FULL             POINT TO LAST POSITION FIELD                 
         LA    RE,EZSPPOS2-EZSPPOS1(RE)   BUMP TO NEXT POSITION FIELD           
         LA    RF,EZSPPOS3         POINT TO LAST POSITION FIELD                 
*                                                                               
         CR    RE,RF               CHECK IF PAST FIELDS                         
         BNH   LHDPOSLP                                                         
*                                                                               
LHDPOSDN DS    0H                                                               
*                                                                               
         BCTR  R1,0                BACK UP TO LAST BYTE                         
*                                                                               
         LA    RF,DTLPOS                                                        
         CR    R1,RF                                                            
         BNH   LHDPOSD1            NO POSITION DISPLAYED                        
*                                                                               
         CLI   0(R1),C'/'          IF IT ENDS IN A SLASH                        
         BNE   *+8                                                              
         MVI   0(R1),C' '             REPLACE WITH A SPACE                      
*                                                                               
LHDPOSD1 DS    0H                                                               
*                                                                               
LHDPOSX DS     0H                                                               
*                                                                               
         OI    DTLPOSH+6,X'80'     TRANSMIT FIELD                               
*                                                                               
*        DISPLAY UNIT RATE                                                      
*                                                                               
         XC    DTLRATE,DTLRATE     CLEAR FIELD                                  
*                                                                               
         LTR   R4,R4               SKIP IF NO ELEMENT TO DISPLAY                
         BZ    LHDRATEX                                                         
*                                                                               
         EDIT  EZSPBRAT,(13,DTLRATE),5,ALIGN=LEFT,ZERO=BLANK UNIT RATE          
*                                                                               
LHDRATEX DS    0H                                                               
*                                                                               
         OI    DTLRATEH+6,X'80'    TRANSMIT FIELD                               
*                                                                               
*        DISPLAY GROSS                                                          
*                                                                               
         XC    DTLGRSS,DTLGRSS     CLEAR FIELD                                  
*                                                                               
         LTR   R4,R4               SKIP IF NO ELEMENT TO DISPLAY                
         BZ    LHDGRSSX                                                         
*                                                                               
         EDIT  EZSPBGRS,(13,DTLGRSS),2,ALIGN=LEFT,  SET GROSS AMOUNT   C        
               MINUS=YES,ZERO=NOBLANK,COMMAS=YES                                
*                                                                               
LHDGRSSX DS    0H                                                               
*                                                                               
         OI    DTLGRSSH+6,X'80'    TRANSMIT FIELD                               
*                                                                               
*        DISPLAY NET                                                            
*                                                                               
         XC    DTLORDR,DTLORDR     CLEAR FIELD                                  
*                                                                               
         LTR   R4,R4               SKIP IF NO ELEMENT TO DISPLAY                
         BZ    LHDORDRX                                                         
*                                                                               
         EDIT  EZSPBNET,(13,DTLORDR),2,ALIGN=LEFT, SET NET DUE AMOUNT  C        
               MINUS=YES,ZERO=NOBLANK,COMMAS=YES                                
*                                                                               
LHDORDRX DS    0H                                                               
*                                                                               
         OI    DTLORDRH+6,X'80'    TRANSMIT FIELD                               
*                                                                               
         OC    ELTNOR,ELTNOR       CHECK FOR NO DATA TO DISPLAY                 
         BZ    LHDENDAT                                                         
*                                                                               
LHDLX    DS    0H                                                               
         XIT1                                                                   
*                                                                               
LHDENDAT MVI   ERROR,PZENDATA      NO INSERTIONS FOR FILTER                     
         LA    R2,DTLDFTRH         POINT TO FIELD                               
         B     LHDERRX                                                          
*                                                                               
LHDESTNF MVI   ERROR,PZEESTNF      ESTIMATE NOT ON FILE                         
         LA    R2,DTLECDH          POINT TO FIELD                               
         B     LHDERRX                                                          
*                                                                               
LHDPRDNF MVI   ERROR,PZEPRDNF      PRODUCT  NOT ON FILE                         
         LA    R2,DTLPCDH          POINT TO FIELD                               
         B     LHDERRX                                                          
*                                                                               
LHDERRX  DS    0H                                                               
*                                                                               
         NI    4(R2),X'FF'-X'20'   TURN OFF VALIDATED STATUS                    
         GOTO1 ERREX               HANDLE FIELD IN ERROR                        
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  R3                                                               
         DROP  R4                                                               
         TITLE 'PPEZF08 - VALIDATE INPUT - LHVALLIN'                            
***********************************************************************         
*                                                                     *         
*        ROUTINE TO VALIDATE INPUT FOR A LINE                         *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
LHVALLIN NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R5,LUBLK            POINT TO  LINUP CONTROL FIELD                
         USING LUBLKD,R5           ESTABLISH LINUP CONTROL FIELD                
*                                                                               
         XC    ELTENT,ELTENT       CLEAR A(ENTRY)                               
*                                                                               
         TM    LULSTAT,LUSNPVQ     SKIP IF ALL FIELDS PREVIOUSLY                
         BO    *+8                   VALIDATED                                  
         TM    LULSTAT,LUSDATQ     AND NO DATA IN FIELDS ON THIS LINE           
         BZ    LHVLX                                                            
*                                                                               
         XC    WRKELTAB(256),WRKELTAB   INIT WORK AREA                          
         XC    WRKELTAB+256(256),WRKELTAB+256                                   
         XC    WRKELTAB+512(L'WRKELTAB-512),WRKELTAB+512                        
*                                                                               
         LA    R4,WRKELTAB         SET TO BUILD TABLE ELEMENT                   
         USING ELTABD,R4           IN WORK AREA                                 
*                                                                               
         LA    R2,SVLSVENT         NEEDED BECAUSE ONLY 1 ENTRY IN TABLE         
         USING LSVTABD,R2          ESTABLISH ENTRY                              
*                                                                               
         OC    LSVKEY,LSVKEY       WAS ANYTHING THERE BEFORE                    
         BZ    LHVL10              NO                                           
*                                  YES, MARK OLD ENTRY DELETED                  
*                                  NOTE- ADD+DEL=CHANGE, THIS ENTRY             
*                                        MAY BE SAME AS ABOVE                   
         GOTO1 =V(BINSRCH),BSPPRMS,('BSPFIND',(R2)),RR=RELO                     
         CLI   BSPCNTL,BSPNF       MUST FIND ELEMENT                            
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RF,BSPAREC          POINT TO FOUND ELEMENT                       
*                                                                               
         MVC   WRKELTAB(ELTABL),0(RF)      COPY OLD ELEMENT                     
*                                                                               
LHVL10   DS    0H                                                               
*                                                                               
*        VALIDATE PRODUCT AND STORE IN WORK ELEMENT                             
*                                                                               
         LA    R2,DTLPCDH                                                       
*                                                                               
         XC    DTLPNMD,DTLPNMD     INIT PRODUCT NAME                            
         OI    DTLPNMDH+6,X'80'       FIELD                                     
*                                                                               
         CLI   5(R2),3             PRODUCT IS MAX 3 LONG                        
         BH    LHVEPRLN                                                         
*                                                                               
         XC    ELTCVPR,ELTCVPR     INIT CONVERTED PRODUCT CODE                  
*                                                                               
         CLI   5(R2),0             IF NO ENTRY                                  
         BNE   LHVPRD10                                                         
*                                                                               
         NI    DTLPCDH+1,X'FF'-X'0C'  TURNOFF HIGHLIGHTING                      
         MVC   DTLPCD(3),EZIHCVPR-EZIHFRST+SVINVHDR  USE HDR OVERRIDE           
*                                                                               
         CLC   =C'***',DTLPCD      IF PRODUCT VARIOUS                           
         BE    *+14                                                             
         CLC   DTLPCD,SPACES          IF NO RESULT                              
         BH    *+10                                                             
         MVC   DTLPCD(3),EZIHLPRC-EZIHFRST+SVINVHDR  USE HDR LOOKUP             
*                                                                               
         CLC   =C'***',DTLPCD      IF PRODUCT VARIOUS                           
         BNE   *+10                                                             
         MVC   DTLPCD,SPACES          CLEAR ENTRY                               
*                                                                               
         CLC   DTLPCD,SPACES          IF NO RESULT                              
         BNH   LHVPRDX                   SKIP VALIDATION                        
*                                                                               
         MVI   DTLPCDH+5,3         SET ENTRY LENGTH                             
*                                                                               
LHVPRD10 DS    0H                                                               
*                                                                               
         MVI   CURSYST,C'M'        MEDIA (SPOT/NET)                             
         GOTO1 VALIFAS             SWITCH                                       
*                                                                               
         MVC   QCLT,DTLCCD         PASS CLIENT CODE                             
*                                                                               
         GOTO1 VALIPRD                                                          
*                                                                               
         MVC   ELTCVPR,WORK        SAVE PRODUCT CODE                            
*                                                                               
         MVC   DTLPNMD,WORK+4      DISPLAY PRODUCT NAME                         
*                                                                               
LHVPRDX  DS    0H                                                               
*                                                                               
         NI    DTLPCDH+1,X'FF'-X'0C'   TURN OFF HIGHLIGHTING                    
*                                                                               
         CLC   =C'***',EZIHCVPR-EZIHFRST+SVINVHDR  SKIP IF VARIOUS PROD         
         BE    LHVPRDX0                                                         
         CLC   ELTCVPR,EZIHCVPR-EZIHFRST+SVINVHDR                               
         BE    LHVPRDX1                                                         
         CLC   EZIHCVPR-EZIHFRST+SVINVHDR,SPACES  SKIP IF HAVE PROD             
         BH    LHVPRDX0                                                         
         CLC   ELTCVPR,EZIHLPRC-EZIHFRST+SVINVHDR                               
         BE    LHVPRDX1                                                         
*                                                                               
LHVPRDX0 DS    0H                                                               
*                                                                               
         OI    DTLPCDH+1,X'08'     HIGHLIGHT FIELD                              
         OI    WPRDSW,WPRDNEWQ     NEW PRODUCT IN INVOICE                       
*                                                                               
         B     LHVPRDX2                                                         
*                                                                               
LHVPRDX1 DS    0H                                                               
*                                                                               
         XC    ELTCVPR,ELTCVPR     NOT AN OVERRIDE                              
*                                                                               
LHVPRDX2 DS    0H                                                               
*                                                                               
         OI    DTLPCDH+4,X'20'     SET PRODUCT VALIDATED                        
         OI    DTLPCDH+6,X'80'     TRANSMIT PRODUCT CODE                        
*                                                                               
*        VALIDATE ESTIMATE                                                      
*                                                                               
LHVEST   DS    0H                                                               
*                                                                               
         XC    DTLENMD,DTLENMD     INIT ESTIMATE NAME                           
         OI    DTLENMDH+6,X'80'    TRANSMIT FIELD                               
*                                                                               
         LA    R2,DTLECDH          SET FIELD POINTER                            
*                                                                               
         CLI   5(R2),3             ESTIMATE IS MAX 3 LONG                       
         BH    LHVESTLN                                                         
*                                                                               
         XC    ELTCVES,ELTCVES     INIT CONVERTED ESTIMATE CODE                 
*                                                                               
         CLI   5(R2),0             IF NOT ENTERED                               
         BNE   LHVEST10                                                         
*                                                                               
         NI    DTLECDH+1,X'FF'-X'0C'   TURN OFF HIGHLIGHTING                    
*                                                                               
         MVC   DUB(2),EZIHCVES-EZIHFRST+SVINVHDR  USE HDR OVERRIDE              
         EDIT  (2,DUB),(3,DTLECD),ALIGN=LEFT,ZERO=BLANK                         
*                                                                               
         STC   R0,DTLECDH+5        SET FIELD LENGTH                             
*                                                                               
         OI    DTLECDH+6,X'80'     TRANSMIT FIELD                               
*                                                                               
         CLI   5(R2),0             SKIP IF NO ESTIMATE CODE                     
         BE    LHVESTX                                                          
*                                                                               
LHVEST10 DS    0H                                                               
*                                                                               
         SR    RF,RF                                                            
         IC    RF,5(R2)            VALIDATE ESTIMATE AS NUMERIC                 
         BCTR  RF,0                                                             
         EX    RF,*+16                                                          
         EX    RF,*+18                                                          
         EX    RF,*+20                                                          
         B     *+22                                                             
         MVC   DUB(0),8(R2)                                                     
         NC    DUB(0),=8C'0'                                                    
         CLC   DUB(0),=8C'0'                                                    
         BNE   LHVNUMER                                                         
*                                                                               
         EX    RF,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)         PACK ESTIMATE NUMBER                         
*                                                                               
         CVB   R0,DUB                                                           
         STCM  R0,3,DUB            SAVE BINARY ESTIMATE                         
*                                                                               
*        VALIDATE ESTIMATE AGAINST FILE                                         
*                                                                               
         MVC   FILENAME,=C'PRTDIR  '                                            
*                                                                               
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING PESTRECD,R6         ESTABLISH ESTIMATE KEY                       
*                                                                               
         MVC   PESTKAGY,QAGY       AGENCY                                       
         MVC   PESTKMED,QMED       MEDIA                                        
         MVI   PESTKRCD,X'07'      RECORD ID                                    
         MVC   PESTKCLT,DTLCCD     CLIENT                                       
         MVC   PESTKPRD,DTLPCD     PRODUCT                                      
         MVC   PESTKEST,DUB        ESTIMATE                                     
*                                                                               
         MVI   RDUPDATE,C'N'       NOT READ FOR UPDATE                          
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(25),KEYSAVE                                                  
         BNE   LHVESTNF                                                         
*                                                                               
         L     R1,AIO1                                                          
         ST    R1,AIO                                                           
*                                                                               
         MVI   RDUPDATE,C'N'       NOT READ FOR UPDATE                          
*                                                                               
         MVC   FILENAME,=C'PRTFIL  '                                            
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         XC    FILENAME,FILENAME                                                
*                                                                               
         L     R6,AIO1             POINT TO FOUND RECORD                        
         USING PESTRECD,R6                                                      
         XC    FILENAME,FILENAME                                                
*                                                                               
         CLC   PESTST,EZIHIEDT-EZIHFRST+SVINVHDR  EST STR AFTER MOS             
         BH    LHVESTDT                                                         
         CLC   PESTEND,EZIHISDT-EZIHFRST+SVINVHDR EST END BEFORE MOS            
         BL    LHVESTDT                                                         
*                                                                               
         CLC   ELTDATE,SPACES      SKIP IF NO INSERTION DATE                    
         BNH   LHVEST99                                                         
*                                                                               
         CLC   PESTST,ELTDATE      INSERT DATE IN ESTIMATE PERIOD               
         BH    LHVESTDT                                                         
         CLC   PESTEND,ELTDATE                                                  
         BL    LHVESTDT                                                         
*                                                                               
LHVEST99 DS    0H                                                               
*                                                                               
         MVC   DTLENMD,PESTNAME    DISPLAY ESTIMATE DESCRIPTION                 
         OI    DTLENMDH+6,X'80'    TRANSMIT ESTIMATE DESCRIPTION                
*                                                                               
         MVC   ELTCVES,PESTKEST    SAVE ESTIMATE NUMBER                         
*                                                                               
LHVESTOK DS    0H                                                               
*                                                                               
LHVESTX  DS    0H                                                               
*                                                                               
         NI    DTLECDH+1,X'FF'-X'0C'   TURN OFF HIGHLIGHTING                    
*                                                                               
         CLC   ELTCVES,EZIHCVES-EZIHFRST+SVINVHDR IF NEW ESTIMATE               
         BE    LHVESTX1                                                         
*                                                                               
         OI    DTLECDH+1,X'08'        HIGHLIGHT FIELD                           
         B     LHVESTX2                                                         
*                                                                               
LHVESTX1 DS    0H                                                               
*                                                                               
         XC    ELTCVES,ELTCVES     NOT AN OVERRIDE                              
*                                                                               
LHVESTX2 DS    0H                                                               
*                                                                               
         OI    DTLECDH+4,X'20'     SET ESTIMATE VALIDATED                       
         OI    DTLECDH+6,X'80'     TRANSMIT FIELD                               
*                                                                               
         DROP  R6                                                               
*                                                                               
         TITLE 'PPEZF08 - ADD INPUT DATA TO TABLE -LHVCMP'                      
***********************************************************************         
*                                                                     *         
*        ROUTINE TO ADD INPUT TO ELEMENT TABLE                        *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
LHVCMP   DS    0H                                                               
*                                                                               
         LTR   R4,R4               DONE IF NOT BUILDING AN ELEMENT              
         BZ    LHVLX                                                            
*                                                                               
         MVC   SAVMSGNO,ERROR      SAVE CURRENT MESSAGE NUMBER                  
*                                                                               
         GOTO1 =V(BINSRCH),BSPPRMS,('BSPADD',ELTABD),RR=RELO                    
*                                                                               
         OC    1(3,R1),1(R1)       TEST ROOM                                    
         BZ    LHVCMPE1                                                         
*                                                                               
         L     R4,BSPAREC          POINT TO FOUND TABLE ELEMENT                 
*                                                                               
         STCM  R4,7,ELTENT+1                                                    
*                                                                               
         CLI   BSPCNTL,BSPNF       IF NO MATCH FOUND                            
         BNE   *+20                                                             
         ICM   RF,15,ELTNOR           BUMP ELEGIBLE ELEMENTS                    
         LA    RF,1(RF)                                                         
         STCM  RF,15,ELTNOR                                                     
         B     LHVL92                 NEW ENTRY FOR TABLE                       
*                                                                               
         LA    R2,SVLSVENT         NEEDED BECAUSE ONLY 1 ENTRY IN TABLE         
         USING LSVTABD,R2                                                       
*                                                                               
         CLC   LSVKEY,ELTKEY       ALSO OK IF ENTRY KEY NOT CHANGED             
         BNE   LHVCMPE2                                                         
*                                                                               
LHVL92   DS    0H                                                               
*                                                                               
         OI    ELTCTL,ELTADDQ      SET ADDED (NB- ADD+DEL=CHA)                  
*                                                                               
*        TRANSFER NEW DATA TO TABLE ELEMENT                                     
*                                                                               
         MVC   ELTCVPR,ELTCVPR-ELTABD+WRKELTAB    PRODUCT CODE                  
         MVC   ELTCVES,ELTCVES-ELTABD+WRKELTAB    ESTIMATE NUMBER               
*                                                                               
*        SET NEW ELTLAST                                                        
*                                                                               
         ICM   R1,15,BSPNOR        NUMBER OF ENTRIES                            
         BZ    *+10                                                             
         BCTR  R1,R0                                                            
         MH    R1,=Y(ELTABL)                                                    
         A     R1,BSPATAB          PLUS START OF TABLE                          
         ST    R1,ELTLAST          SET A(LAST)                                  
*                                                                               
         B     LHVLX                                                            
*                                                                               
         EJECT                                                                  
LHVESTDT MVI   ERROR,PZEESTDT      ESTIMATE DATES OUTSIDE MOS                   
         B     LHVERRX                                                          
*                                                                               
LHVESTNF MVI   ERROR,PZEESTNF      ESTIMATE NOT ON FILE                         
         B     LHVERRX                                                          
*                                                                               
LHVESTLN MVI   ERROR,PZEESTLN      ESTIMATE MUST BE 1 - 999                     
         B     LHVERRX                                                          
*                                                                               
LHVEPRLN MVI   ERROR,PZEPRDLN      PRODUCT 2 OR 3 LONG                          
         B     LHVERRX                                                          
*                                                                               
LHVNUMER MVI   ERROR,NOTNUM        NOT NUMERIC                                  
         B     LHVERRX                                                          
*                                                                               
LHVCMPE1 DS    0H                                                               
         MVI   ERROR,PZERECFL            TOO MANY DETAIL LINES                  
         B     LHVERRX                                                          
*                                                                               
LHVCMPE2 DS    0H                                                               
         L     R2,LUACLIN          POINT TO FIRST FIELD ON LINE =SPACE          
         MVI   ERROR,PZEDUPE       DUPLICATE                                    
         B     LHVERRX                                                          
*                                                                               
LHVERRX  DS    0H                                                               
*                                                                               
         NI    4(R2),X'FF'-X'20'   TURN OFF VALIDATED STATUS                    
         GOTO1 ERREX               HANDLE FIELD IN ERROR                        
*                                                                               
LHVLX    DS    0H                                                               
         CLI   ERROR,0             SET CC                                       
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  R4                                                               
         DROP  R2                                                               
         TITLE 'PPEZF08 - VALIDATE DETAIL FILTERS - VDFT'                       
***********************************************************************         
*                                                                     *         
*        VALIDATE DETAIL FILTERS (IF ANY)                             *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0H                                                               
VDFT     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    DFLTERS,DFLTERS     INIT FILTERS SAVEAREA                        
         XC    SVDFLTR,SVDFLTR     INIT FILTERS SAVEAREA                        
         MVI   SVDFLTRL,0          INIT LENGTH OF FILTERS SAVEAREA              
*                                                                               
         CLI   5(R2),0             ANY ENTRY                                    
         BE    VDFTX               NO                                           
*                                                                               
*        INITIALIZE DDVAL PARAMETERS                                            
*                                                                               
         MVC   DMCB+4(4),=X'D9000A40'                                           
         GOTO1 CALLOV,DMCB,0       LOAD PHASE T00A40 - DDVAL                    
*                                                                               
         CLI   DMCB+4,X'FF'        MUST FIND DDVAL                              
         BNE   *+6                                                              
         DC    H'0'                WHERE IS DDVAL                               
*                                                                               
         L     RF,DMCB             SAVE A(VDDVAL)                               
         ST    RF,VDDVAL                                                        
*                                                                               
         XC    VLBLOCK(VLBLOCKL),VLBLOCK CLEAR DDVAL CONTROL BLOCK              
*                                                                               
         MVC   VLACFACS,ACOMFACS   A(COMFACS)                                   
         MVC   VLCTRY,CTRY         SET COUNTRY CODE                             
         MVC   VLLANG,LANG         LANGUAGE CODE                                
         MVC   VLAGENCY,TWAAGY     AGENCY                                       
         MVI   VLSYSTEM,VLSYSPRQ   PRINT SYSTEM                                 
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),(2,VLTODAYC) GET TODAY'S DATE                  
*                                                                               
         ZIC   R1,5(R2)            INPUT LENGTH                                 
         BCTR  R1,0                                                             
*                                                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),=CL4'HELP'                                               
         BE    VDFTHLP                                                          
*                                                                               
         MVC   SVDFLTR,SPACES      INIT FILTERS SAVEAREA                        
*                                                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SVDFLTR(0),8(R2)    SAVE ENTERED FILTERS                         
*                                                                               
         MVC   SVDFLTRL,5(R2)      SAVE LENGTH OF ENTERED FILTERS               
*                                                                               
         LA    R0,25               NON-STANDARD LENGTH                          
         MVI   BYTE,1                                                           
         LA    R4,BLOCK            ADDRESS OF FIRST BLOCK                       
*                                                                               
         GOTO1 SCANNER,DMCB,((R0),(R2)),(5,(R4)),0                              
         CLI   4(R1),0                                                          
         BE    VDFEMISS             SCANNER DIDN'T FIND ANYTHING                
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,1,DMCB+4         GET NUMBER OF BLOCKS                         
*                                                                               
*        VALIDATE FILTERS                                                       
*                                                                               
VDFTLOOP DS    0H                                                               
*                                                                               
*        CHECK FOR HELP REQUEST                                                 
*                                                                               
         CLI   12(R4),C'+'         IF HELP REQUESTED                            
         BE    *+8                                                              
         CLI   12(R4),C'?'         IF HELP REQUESTED                            
         BE    *+8                                                              
         CLI   13(R4),C'?'         IF HELP REQUESTED                            
         BNE   VDFTHLPX                                                         
*                                                                               
         LA    R6,HELPCBLK         ESTABLISH HELP CONTROL BLOCK                 
         USING HELPCB,R6                                                        
*                                                                               
         XC    HELPCB(HELPCBL),HELPCB  INIT HELP CONTROL BLOCK                  
*                                                                               
         L     RF,=A(HELPSAVE-(CONHEADH-64))                                    
         LA    RF,0(RF,RA)                                                      
         ST    RF,HCBASAVE         PASS ADDRESS                                 
*                                                                               
         MVC   HCBMTIC,=AL2(PRQHLPMN)  USE PRINT HELP MENUS                     
         MVI   HCBSCRN,X'D2'       SCREEN TO DISPLAY MENUS ON                   
         MVI   HCBPAGE,1           SAVE IN SECOND TWA PAGE                      
         ST    RA,HCBATWA          SET SYSTEM ADDRESSES                         
         MVC   HCBACOM,ACOMFACS                                                 
*                                                                               
         L     R1,SYSPARMS         POINT TO TIOBD                               
         MVC   HCBATIOB,0(R1)                                                   
         MVC   HCBASYRD,SYSRD                                                   
         MVC   HCBCTRY,CTRY        SET COUNTRY                                  
         MVC   HCBLANG,LANG        SET LANGUAGE                                 
         MVI   HCBSYST,4           SET SYSTEN AS PRINT                          
         MVI   HCBSEGS,10          DDVAL TAB AREA IS 10*256 LONG                
         MVC   HCBATAB,WRKFBUFA    SET A(DDVAL TABLE)                           
*                                                                               
         GOTO1 VPRHELP,DMCB,=AL2(PRQHMPZD),(R2),HELPCBLK PUT OUT HELP           
*                                                                               
         NOP   VDFTHLPX            FOR TESTING                                  
         DROP  R6                                                               
*                                                                               
VDFTHLPX DS    0H                                                               
*                                                                               
         ZIC   R1,0(R4)            GET LENGTH                                   
*                                                                               
         CH    R1,=H'2'                                                         
         BL    VDFEFTLN                                                         
*                                                                               
*        SAVE ANY '+' OR '-' MODIFIERS                                          
*                                                                               
         BCTR  R1,0                                                             
         LA    R5,12(R1,R4)        POINT TO LAST CHAR FOUND                     
*                                                                               
         CLI   0(R5),C'+'          PLUS                                         
         BE    *+8                  YES, SAVE IT                                
         CLI   0(R5),C'-'          MINUS                                        
         BNE   *+12                 NO, NEITHER                                 
         MVC   HOLDSIGN,0(R5)                                                   
         BCTR  R1,0                -1 MORE FOR GREATER/LESS THAN SIGN           
*                                                                               
*        VALIDATE FIELD ENTRY                                                   
*                                                                               
         LA    RF,1(R1)            INPUT LENGTH                                 
         LH    R3,=Y(WVLTAB-SYSD)  GET A(TABLE BUILD AREA)                      
         LA    R3,SYSD(R3)                                                      
*                                                                               
         GOTO1 VDDVAL,VLPARMS,('VLPVALQ',=Y(PRQPZFDT)),                X        
               ((RF),12(R4)),(R3),0,0,0                                         
         CLI   VLPERR,0                                                         
         BNE   VDFEFTNV            INVALID FILTER                               
*                                                                               
         USING VLTABD,R3           ESTABLISH RETURNED TABLE ENTRY               
*                                                                               
*        INSERTION DATE                                                         
*                                                                               
VDFTINS  DS    0H                                                               
*                                                                               
         CLC   VLTICODE,=Y(PRQZFDIN)   INSERTION DATE                           
         BNE   VDFTINSN                                                         
*                                                                               
         LA    R5,22(,R4)                                                       
*                                                                               
         GOTO1 DATVAL,DMCB,(0,(R5)),DFTIDATE                                    
*                                                                               
         OC    DMCB(4),DMCB        WAS DATE VALID                               
         BZ    VDFEDTNV             NO                                          
*                                                                               
         MVC   DFTDATES,HOLDSIGN                                                
*                                                                               
         B     VDFTLPCN                                                         
         EJECT                                                                  
*                                                                               
VDFTINSN DS    0H                                                               
*                                                                               
*        PRODUCT CODE FILTER 'PC='                                              
*              'X' MEANS ALL INVOICES WITHOUT OVERRIDE                          
*                  OR LOOKED UP PRODUCT CODE                                    
*              ELSE REPRESENTS PRODUCT CODE                                     
*                                                                               
*              IF IT STARTS WITH '-' TREATED AS NEGATIVE FILTER                 
*                                                                               
*              C'ALL' TREATED AS '-X' INTERNALLY                                
*                                                                               
VDFTPC   DS    0H                                                               
*                                                                               
         CLC   VLTICODE,=Y(PRQZFDPC)   PRODUCT CODE (PC)                        
         BNE   VDFTPCN             NO                                           
*                                                                               
         XC    FULL,FULL           INIT WORKAREA                                
         SR    RF,RF                                                            
*                                                                               
         ICM   RF,1,1(R4)          ENTRY LENGTH                                 
         BZ    VDFEPCLN               MUST HAVE ONE                             
*                                                                               
         LA    R1,22(R4)           START OF PRODUCT CODE                        
*                                                                               
         CLI   0(R1),C'-'          IF NEGATIVE FILTER                           
         BNE   *+20                                                             
         MVI   FULL,C'-'              SET INDICATOR                             
         LA    R1,1(R1)               BUMP VALUE POINTER                        
         BCT   RF,*+8                 DECREMENT LENGTH                          
         B     VDFEPCLN               NO CODE ENTERED                           
*                                                                               
         CH    RF,=H'1'            IF ENTRY IS 1 LONG                           
         BNE   VDFTPC7                                                          
*                                                                               
         CLI   0(R1),C'X'             IT MUST BE C'X' FOR MISSING CODES         
         BNE   VDFEPCLN                                                         
*                                                                               
         MVC   DFTQPRD,=X'FFFFFF'     SET SPECIAL CODE                          
*                                                                               
         B     VDFTPC9                                                          
*                                                                               
VDFTPC7  DS    0H                                                               
*                                                                               
         CH    RF,=H'2'            PRODUCT CODE MUST BE 2 OR 3 LONG             
         BL    VDFEPCLN                                                         
         CH    RF,=H'3'                                                         
         BH    VDFEPCLN                                                         
*                                                                               
         MVC   DFTQPRD,0(R1)       SAVE PRODUCT CODE                            
*                                                                               
         CLC   DFTQPRD,=C'ALL'     IF 'ALL' PRODUCT CODES                       
         BNE   *+14                                                             
         MVC   DFTQPRD,=X'FFFFFF'     TREAT AS '-X'                             
         MVI   FULL,C'-'                                                        
*                                                                               
         B     VDFTPC9                                                          
*                                                                               
VDFTPC9 DS     0H                                                               
*                                                                               
         CLI   FULL,C'-'              IF NEGATIVE FILTER                        
         BNE   *+8                                                              
         NI    DFTQPRD,X'FF'-X'40'       MAKE FIRST LETTER LOWERCASE            
*                                                                               
VDFTPCX  DS    0H                                                               
*                                                                               
         B     VDFTLPCN                                                         
*                                                                               
VDFTPCN  DS    0H                                                               
*                                                                               
*        PRODUCT NAME FILTER 'PN='                                              
*              'X' MEANS ALL INVOICES WITHOUT OVERRIDE                          
*                  OR LOOKED UP                                                 
*                  OR SOURCE    PRODUCT NAME                                    
*              ELSE REPRESENTS PRODUCT NAME                                     
*                                                                               
*              IF IT STARTS WITH '-' TREATED AS NEGATIVE FILTER                 
*                                                                               
*        COMPARES MATCH ON LENGTH OF FILTER. AN EXACT MATCH IS NOT              
*              NECESSARY                                                        
*                                                                               
VDFTPN   DS    0H                                                               
*                                                                               
         CLC   VLTICODE,=Y(PRQZFDPN)   PRODUCT NAME (PN)                        
         BNE   VDFTPNN             NO                                           
*                                                                               
         XC    FULL,FULL           INIT WORKAREA                                
         SR    RF,RF                                                            
*                                                                               
         ICM   RF,1,1(R4)          ENTRY LENGTH                                 
         BZ    VDFEPNLN               MUST HAVE ONE                             
*                                                                               
         LA    R1,22(R4)           START OF PRODUCT NAME                        
*                                                                               
         CLI   0(R1),C'-'          IF NEGATIVE FILTER                           
         BNE   *+20                                                             
         MVI   FULL,C'-'              SET INDICATOR                             
         LA    R1,1(R1)               BUMP VALUE POINTER                        
         BCT   RF,*+8                 DECREMENT LENGTH                          
         B     VDFEPNLN               NO NAME ENTERED                           
*                                                                               
         CH    RF,=H'1'            IF ENTRY IS 1 LONG                           
         BNE   VDFTPN7                                                          
*                                                                               
         CLI   0(R1),C'X'             IT MAY BE C'X' FOR MISSING NAMES          
         BNE   VDFTPN7                                                          
*                                                                               
         MVI   DFTPRDN,X'FF'          SET SPECIAL NAME                          
         MVI   DFTPRDNL,0             SET SPECIAL NAME EXECUTE LENGTH           
*                                                                               
         B     VDFTPN9                                                          
*                                                                               
VDFTPN7  DS    0H                                                               
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   DFTPRDN(0),0(R1)    SAVE PRODUCT NAME                            
*                                                                               
         STC   RF,DFTPRDNL         SAVE PRODUCT NAME EXECUTE LENGTH             
*                                                                               
         B     VDFTPN9                                                          
*                                                                               
VDFTPN9 DS     0H                                                               
*                                                                               
         CLI   FULL,C'-'              IF NEGATIVE FILTER                        
         BNE   *+8                                                              
         NI    DFTPRDN,X'FF'-X'40'       MAKE FIRST LETTER LOWERCASE            
*                                                                               
VDFTPNX  DS    0H                                                               
*                                                                               
         B     VDFTLPCN                                                         
*                                                                               
VDFTPNN  DS    0H                                                               
*                                                                               
*        INSERTION ORDER FILTER 'ION='                                          
*                                                                               
*              IF IT STARTS WITH '-' TREATED AS NEGATIVE FILTER                 
*                                                                               
*        COMPARES MATCH ON LENGTH OF FILTER. AN EXACT MATCH IS NOT              
*              NECESSARY                                                        
*                                                                               
VDFTION  DS    0H                                                               
*                                                                               
         CLC   VLTICODE,=Y(PRQZFDIO)   INSERTION ORDER NUMBER (ION)             
         BNE   VDFTIONN            NO                                           
*                                                                               
         XC    FULL,FULL           INIT WORKAREA                                
         SR    RF,RF                                                            
*                                                                               
         ICM   RF,1,1(R4)          ENTRY LENGTH                                 
         BZ    VDFEIONL               MUST HAVE ONE                             
*                                                                               
         LA    R1,22(R4)           START OF INSERTION ORDER                     
*                                                                               
         CLI   0(R1),C'-'          IF NEGATIVE FILTER                           
         BNE   *+20                                                             
         MVI   FULL,C'-'              SET INDICATOR                             
         LA    R1,1(R1)               BUMP VALUE POINTER                        
         BCT   RF,*+8                 DECREMENT LENGTH                          
         B     VDFEIONL               NO NUMBER ENTERED                         
*                                                                               
         CH    RF,=H'17'           MAX 17 LONG                                  
         BH    VDFEIONL                                                         
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   DFTIONO(0),0(R1)    SAVE INSERTION ORDER NUMBER                  
*                                                                               
         STC   RF,DFTIONLN         SAVE INSERTION ORDER EXECUTE LENGTH          
*                                                                               
         CLI   FULL,C'-'              IF NEGATIVE FILTER                        
         BNE   *+8                                                              
         NI    DFTIONO,X'FF'-X'40'       MAKE FIRST LETTER LOWERCASE            
*                                                                               
VDFTIONX DS    0H                                                               
*                                                                               
         B     VDFTLPCN                                                         
*                                                                               
VDFTIONN DS    0H                                                               
*                                                                               
*        UNKNOWN DETAIL FILTER                                                  
*                                                                               
         B     VDFEFTNV            UNKNOWN FILTER                               
*                                                                               
*        CONTINUATION OF DETAIL FILTERS VALIDATION LOOP                         
*                                                                               
VDFTLPCN ZIC   RE,BYTE             UP FIELD NUMBER CTR                          
         LA    RE,1(RE)                                                         
         STC   RE,BYTE                                                          
         LA    R4,47(R4)           NOTE- NON-STANDARD LENGTH                    
         MVI   HOLDSIGN,0          RESET GREATER/LESS THAN SIGN                 
*                                                                               
         BCT   R0,VDFTLOOP         FOR NUMBER OF BLOCKS FOUND                   
*                                                                               
VDFTX    XIT1                                                                   
*                                                                               
VDFEREPE MVI   ERROR,PZEREPER      REPORT CAN'T HAVE FILTERS                    
         B     VDFERRX                                                          
VDFEFTLN MVI   ERROR,PZEFTRLN      FILTER CODE MUST BE AT LEAST 2 LONG          
         B     VDFERRX                                                          
VDFEPCLN MVI   ERROR,PZEPCLN       PRODUCT CODE LENGTH ERROR                    
         B     VDFERRX                                                          
VDFEPNLN MVI   ERROR,PZEPNLN       PRODUCT NAME LENGTH ERROR                    
         B     VDFERRX                                                          
VDFEIONL MVI   ERROR,PZEIONLN      INSERTION NUMBER LENGTH ERROR                
         B     VDFERRX                                                          
VDFEMISS MVI   ERROR,MISSING                                                    
         B     VDFERRX                                                          
VDFEFTNV MVI   ERROR,PZEFTRNV      INVALID FILTER CODE                          
         B     VDFERRX                                                          
VDFEDTNV MVI   ERROR,INVDATE       INVALID DATE                                 
         B     VDFERRX                                                          
*                                                                               
VDFERRX  DS    0H                                                               
         GOTO1 ERREX                                                            
*                                                                               
VDFTHLP  LA    R1,DFTHELP                                                       
VDFTERX  XC    CONHEAD,CONHEAD                                                  
         BCTR  R1,0                BACK UP TO LENGTH BYTE                       
         CLI   0(R1),L'CONHEAD-1                                                
         BNH   *+6                                                              
         DC    H'0'                                                             
         ZIC   RF,0(R1)                                                         
         EX    RF,*+8                                                           
         B     VDFTERY                                                          
         MVC   CONHEAD(0),1(R1)                                                 
*                                                                               
VDFTERY  GOTO1 ERREX2                                                           
*                                                                               
         DC    AL1(L'DFTHELP-1)                                                 
DFTHELP  DC    CL60'FILTERS=PC/PN/ION/INS *'                                    
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'PPEZF08 - COMMON SUBROUTINES -SUBROUTS'                         
***********************************************************************         
*                                                                     *         
*        COMMONLY ADRESSABLE SUBROUTINES                              *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
SUBROUTS DS    0D                                                               
***********************************************************************         
*                                                                     *         
* HANDLE FIELD IN ERROR - R1 -POINTS TO FIELD                         *         
*         HIGHLIGHT FIELD                                             *         
*         ERROR MESSAGE IS IN ERROR                                   *         
*         IF SAVMSGNO IS NOT ZERO THEN THIS IS NOT FIRST ERROR        *         
*            ROUTINE RESTORES ERROR TO SAVMSGNO                       *         
*         ELSE                                                        *         
*            ROUTINE SETS CURSOR TO THIS FIELD                        *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         USING FLDHDRD,R1          ESTABLISH HEADER                             
ERRFLD   OI    IPSTAT,LUWVERRQ     INDICATE VALIDATION ERROR                    
         OI    FLDATB,FATBHIGH     HIGHLIGHT FIELD                              
         OI    FLDOIND,FOUTTRN     TRANSMIT FIELD                               
         NI    FLDIIND,X'FF'-FINPVAL TURN OFF VALID INDICATOR                   
         CLI   SAVMSGNO,0          IF NOT FIRST ERROR                           
         BE    *+14                                                             
         MVC   ERROR,SAVMSGNO      RESTORE PRIOR MESSAGE                        
         B     ERRFLDX                                                          
*                                                                               
         ST    R1,ACURFORC         PUT CURSOR HERE                              
*                                                                               
ERRFLDX  DS    0H                                                               
         BR    RE                                                               
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
* OTHER LITTLE ROUTINES                                               *         
***********************************************************************         
         SPACE 2                                                                
BUMP     ZIC   RF,0(R2)            BUMP TO NEXT SCREEN FIELD                    
         AR    R2,RF                                                            
         CLI   0(R2),0             EOS- RETURN =                                
         BR    RE                                                               
         SPACE 2                                                                
BUMPU    ZIC   RF,0(R2)            BUMP TO NEXT UNPROTECTED FIELD               
         AR    R2,RF                                                            
         CLI   0(R2),0             EOS- RETRUN =                                
         BER   RE                                                               
         TM    1(R2),X'20'                                                      
         BNZ   BUMPU                                                            
         LTR   RE,RE               NOT EOS- RETURN NOT =                        
         BR    RE                                                               
***********************************************************************         
*        DISPLAY DATA IN  FLD IN FIELD POINTED TO BY R1               *         
***********************************************************************         
         SPACE 2                                                                
DSPFLD   NTR1                      DISPLAY SCREEN FIELD                         
*                                                                               
         USING FLDHDRD,R1          ESTABLISH SCREEN FIELD                       
*                                                                               
         SR    RF,RF                                                            
         IC    RF,FLDLEN           FIELD LENGTH                                 
         SH    RF,=H'8'            HEADER LENGTH                                
*                                                                               
         TM    FLDATB,X'02'        IF THERE IS EXTENED HEADER                   
         BNO   *+8                                                              
         SH    RF,=H'8'               TAKE OFF HEADER LENGTH                    
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R1),FLD         MOVE DATA TO OUTPUT                          
*                                                                               
         OI    FLDOIND,X'80'       TRANSMIT FIELD                               
*                                                                               
         XIT1                                                                   
*                                                                               
         EJECT                                                                  
         PRINT GEN                                                              
         GETEL R6,DATADISP,ELCODE                                               
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
CHGOKMS  DC    C'* IF CHANGES OK, HIT ENTER TO RESUME *'                        
T43008X  DS    0D                                                               
         EJECT                                                                  
* PPEZFWORKD                                                                    
       ++INCLUDE PPEZFWORKD                                                     
* PPEZFCNVWD                                                                    
       ++INCLUDE PPEZFCNVWD                                                     
*                                                                               
         TITLE 'T43008 - EPIC- DETAIL RECORD WORKAREA'                          
***********************************************************************         
*                                                                     *         
*        WORKAREA FOR DETAIL RECORD PROCESSING                        *         
*                                                                     *         
***********************************************************************         
*                                                                               
SYSD     DSECT                                                                  
         ORG   SYSAPPLW                                                         
         DS    0D                                                               
LSVTAB   DS    XL(NLINS*LSVTABL)   SAVE AREA FOR LINE DETAILS                   
         DS    0D                                                               
ELTENT   DS    A                   A(ELEM TABLE ENTRY)                          
ELTLAST  DS    A                   A(LAST ENTRY)                                
ELTFRST  DS    A                   A(FIRST ENTRY)                               
ELTABA   DS    A                   A(ELEMENT TABLE)                             
ELTMAX   EQU   125                 MAX NUMBER OF ELEMENTS IN TABLE              
*                                                                               
ELTNOR   DS    F                   NUMBER OF RELEVANT ENTRIES IN TABLE          
*                                                                               
INSCTR   DS    XL2                 INSERTION COUNTER                            
TINSCTR  DS    XL2                 TOTAL INSERTION COUNTER                      
DKBSEQ   DS    XL2                 DKEY BATCH SQN SAVEAREA                      
*                                                                               
WRKELTAB DS    XL(EZSPDLEN)        WORK TABLE ELEMENT                           
SAVMSGNO DS    XL1                 CURRENT MESSAGE NUMBER SAVEAREA              
SAVCURI  DS    XL1                 INDEX OF ERROR INTO FIELD                    
IPSTAT   DS    XL1                 CUMULATIVE INPUT STATISTICS                  
WRKSQN   DS    XL2                 WORK SEQUENCE NUMBER                         
DFLTSW   DS    X                   X'20' DETAIL FILTERS ENTERED                 
*                                                                               
         DS    0F                                                               
       ++INCLUDE DDBSRPRMD                                                      
         DS    0D                                                               
LUBLK    DS    XL(LUBLKL)          LINUP CONTROL FIELD                          
         DS    0F                                                               
LINDSPS  DS    XL(NLINS+1)                                                      
SVLSVTAB DS    XL(NLINS*LSVTABL)      HOLD COPY OF LINUP SAVE TABLE             
SVLSVENT DS    XL(LSVTABL)            HOLD STARTING 1ST ENTRY IN TABLE          
         DS    0D                                                               
CTR      DS    PL2                                                              
*                                                                               
         EJECT                                                                  
       ++INCLUDE PPEZFFFD                                                       
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
         PRINT ON                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE PPEZFD8D                                                       
*                                                                               
         ORG   CONHEADH-64+X'2000'                                              
HELPSAVE DS    XL512                                                            
IOA4     DS    XL4096              AN EXTRA IOAREA                              
*                                                                               
         EJECT                                                                  
*                                                                               
*                                                                               
LSVTABD  DSECT                     LINUP SAVE AREA DSECT                        
LSVKEY   DS    0XL(L'LSVSORT)                                                   
LSVSORT  DS    XL(L'ELTSEQ)                                                     
LSVKEYL  EQU   *-LSVTABD                                                        
LSVTABL  EQU   *-LSVTABD                                                        
         SPACE 2                                                                
ELTABD   DSECT                     DSECT FOR ELEM TABLE                         
ELTKEY   DS    0XL(L'ELTSORT)                                                   
ELTSORT  DS    XL(L'ELTSEQ)        SORT VALUE                                   
ELTKEYL  EQU   *-ELTABD            KEY LENGTH                                   
ELTCTL   DS    XL1                 CONTROL BYTE                                 
ELTDELQ  EQU   X'80'                 DELETE                                     
ELTADDQ  EQU   X'40'                 ADD                                        
ELTFLTXQ EQU   X'01'                 FAILED FILTERING                           
ELTSEQ   DS    XL2                 INSERTION SEQUENCE NUMBER                    
ELTELEM  DS    0X                  NO DETAILS SAVED                             
ELTCVPR  DS    XL(L'EZSPCVPR)      CONVERTED PRODUCT  CODE                      
ELTCVES  DS    XL(L'EZSPCVES)      CONVERTED ESTIMATE CODE                      
ELTDATE  DS    XL(L'EZSPDATE)      INSERTION DATE                               
ELTABL   EQU   *-ELTABD            ENTRY LENGTH                                 
*                                                                               
         PRINT ON                                                               
* PZBLOCK                                                                       
         PRINT OFF                                                              
       ++INCLUDE PZBLOCK                                                        
         PRINT ON                                                               
* PZGLOBEQUS                                                                    
         PRINT OFF                                                              
       ++INCLUDE PZGLOBEQUS                                                     
         PRINT ON                                                               
* PRGLOBEQUS                                                                    
         PRINT OFF                                                              
       ++INCLUDE PRGLOBEQUS                                                     
         PRINT ON                                                               
* PRVALTABD                                                                     
         PRINT OFF                                                              
       ++INCLUDE PRVALTABD                                                      
         PRINT ON                                                               
* DMWRKRD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DMWRKFD                                                        
         PRINT ON                                                               
* DMWRKRK                                                                       
         PRINT OFF                                                              
       ++INCLUDE DMWRKFK                                                        
         PRINT ON                                                               
* PRGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE PRGENFILE                                                      
         PRINT ON                                                               
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
* DDSPLWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
* DDSPOOLD                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         PRINT ON                                                               
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
* DDGLVXCTLD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDGLVXCTLD                                                     
         PRINT ON                                                               
* DDGLOBEQUS                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDGLOBEQUS                                                     
         PRINT ON                                                               
* DDPERVALD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDPERVALD                                                      
         PRINT ON                                                               
* DDFLDIND                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDFLDIND                                                       
         PRINT ON                                                               
* FATIOB                                                                        
         PRINT OFF                                                              
       ++INCLUDE FATIOB                                                         
         PRINT ON                                                               
* DDFLDHDR                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDFLDHDR                                                       
         PRINT ON                                                               
* PPSRCHPARM                                                                    
         PRINT OFF                                                              
       ++INCLUDE PPSRCHPARM                                                     
         PRINT ON                                                               
* DDLINUPD                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDLINUPD                                                       
         PRINT ON                                                               
* PRHELPCB                                                                      
         PRINT OFF                                                              
       ++INCLUDE PRHELPCB                                                       
         PRINT ON                                                               
* PPGENPZ                                                                       
         PRINT OFF                                                              
       ++INCLUDE PPGENPZ                                                        
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'075PPEZF08   05/01/02'                                      
         END                                                                    
