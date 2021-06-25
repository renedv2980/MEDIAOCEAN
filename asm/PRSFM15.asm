*          DATA SET PRSFM15    AT LEVEL 049 AS OF 04/09/03                      
*PHASE T41C15A                                                                  
*INCLUDE SRCHCALL                                                               
*INCLUDE PUBEDIT                                                                
*INCLUDE NUMED                                                                  
*INCLUDE BINSRCH2                                                               
*INCLUDE PPBROWSE                                                               
         TITLE 'T41C15 SPACE DATA RECORDS'                                      
*                                                                               
*   CHANGE LOG                                                                  
*                                                                               
* SMYE  07/02    IN VK IF REPORTING AND LIMIT ACCESS ACTIVE, REQUIRE            
*                CLIENT CODE ENTRY                                              
*                                                                               
* SMYE  10/01     INCLUDE CALL TO BROWSE FOR SPACE DESCRIPTION                  
*                                                                               
* SMYE  09/01     CONDITIONALLY VALIDATE SPACE DESCRIPTION                      
*                                                                               
* SMYE  11/02/00  ADDED CLIENT VALIDATION (SECURITY) TO DK                      
*                                                                               
* SMYE  02/25/00  FIXED BUGS IN LHVCU1, LHV45 AND LHV47 - WERE NOT              
*                 CORRECTLY HANDLING LEADING ZEROS IN REL MTH, REL DAY          
*                 AND OVG FIELDS                                                
*                                                                               
T41C15   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T41C15,RR=R3                                                   
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         ST    R3,RELO                                                          
         L     R7,=A(SUBROUTS)     SET UP ADDRESSABILITY TO SUBROUTINES         
         A     R7,RELO             RELOCATE ADDRESS                             
         USING SUBROUTS,R7         ESTABLISH ADDRESSABILITY                     
*===>                                                                           
         MVI   CONSERVH+6,X'81'    FORCE SRV REQ FIELD MODIFIED                 
*===>                                                                           
         MVI   IPSTAT,0            INIT INPUT STATISTICS                        
         MVI   SAVMSGNO,0          INIT MESSAGE NUMBER SAVEAREA                 
         MVI   ERROR,0             INIT MESSAGE NUMBER                          
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,XRECADD        RE-DISPLAY RECORD                            
         NOP   DR                                                               
         CLI   MODE,XRECPUT        RE-DISPLAY RECORD                            
         NOP   DR                                                               
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LR                                                               
         CLI   MODE,PRINTREP       PRINT REPORT                                 
         BE    PR                                                               
         CLI   MODE,RECDEL         DELETE RECORD                                
         BE    DR                                                               
         CLI   MODE,RECREST        RESTORE RECORD                               
         BE    DR                                                               
         B     XIT                                                              
*                                                                               
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
*                                                                               
*     VALIDATE KEY ROUTINE                                                      
*                                                                               
VK       DS    0H                                                               
         CLI   ACTNUM,ACTLIST      LIST                                         
         BE    VKL                                                              
*                                                                               
         CLI   ACTNUM,ACTREP       REPORT                                       
         BE    VKL                                                              
*                                                                               
         LA    R2,SPAMEDH         MEDIA                                         
         GOTO1 VALIMED                                                          
*                                                                               
*                                                                               
VK5      LA    R2,SPAPUBH                                                       
         BAS   RE,MYPUBVAL                                                      
         MVC   SPAPUBN,PUBNM       DISPLAY PUB NAME                             
         OI    SPAPUBNH+6,X'80'                                                 
         MVC   SPAPUBZ,PUBZNM      DISPLAY PUB ZONE                             
         OI    SPAPUBZH+6,X'80'                                                 
*                                                                               
         LA    R2,SPACLTH          CLIENT                                       
         MVC   CLTNM,SPACES                                                     
         MVC   QCLT,=X'FFFFFF'     SET TO DEFAULT                               
         CLI   5(R2),0                                                          
         BE    VK20                                                             
         CLC   =C'ALL',8(R2)                                                    
         BE    VK20                                                             
         GOTO1 VALICLT                                                          
*                                                                               
VK20     MVC   SPACLTN,CLTNM       DISPLAY CLT NAME                             
         OI    SPACLTNH+6,X'80'                                                 
*                                                                               
         XC    KEY,KEY             BUILD KEY                                    
         LA    R4,KEY                                                           
         USING PSPCREC,R4                                                       
         MVC   PSPCKAGY,AGENCY                                                  
         MVC   PSPCKMED,QMED                                                    
         MVI   PSPCKTYP,X'2B'                                                   
         MVC   PSPCKCLT,QCLT                                                    
         MVC   PSPCKPUB,BPUB                                                    
         MVC   ORIGKEY,KEY         SAVE THIS KEY (USE WITH LIST)                
         CLC   TWAKEYSV(L'PSPCKEY),KEY   CHECK FOR NEWKEY                       
         BE    *+8                                                              
         MVI   NEWKEY,C'Y'                                                      
         B     VKXIT                                                            
*                                                                               
*        VALIDATE LIST KEY                                                      
*                                                                               
VKL      LA    R2,SPLMEDH          MEDIA                                        
         GOTO1 VALIMED                                                          
*                                                                               
VKL5     LA    R2,SPAPUBH          PUB                                          
         XC    BPUB,BPUB                                                        
         XC    SPAPUBN,SPAPUBN                                                  
         XC    SPAPUBZ,SPAPUBZ                                                  
         OI    SPAPUBNH+6,X'80'                                                 
         OI    SPAPUBZH+6,X'80'                                                 
         CLI   5(R2),0                                                          
         BE    VKL10                                                            
         BAS   RE,MYPUBVAL                                                      
         MVC   SPAPUBN,PUBNM       DISPLAY PUB NAME                             
         OI    SPAPUBNH+6,X'80'                                                 
         MVC   SPAPUBZ,PUBZNM      DISPLAY PUB ZONE                             
         OI    SPAPUBZH+6,X'80'                                                 
*                                                                               
*                                                                               
VKL10    XC    QCLT,QCLT           FOR LISTING CLEAR QCLT                       
         XC    SPACLTN,SPACLTN                                                  
         OI    SPACLTNH+6,X'80'                                                 
         LA    R2,SPLCLTH          CLIENT                                       
         CLI   5(R2),0                                                          
         BE    VKL20                                                            
         CLC   =C'ALL',8(R2)                                                    
         BNE   VKL30                                                            
         MVC   QCLT,=X'FFFFFF'     SET FOR CLIENT 'ALL'                         
         B     VKXIT                                                            
VKL20    DS    0H                  CLIENT NOT ENTERED                           
         OC    TWAACCS(2),TWAACCS  ANY LIMIT ACCESS ?                           
         BZ    VKXIT               NO                                           
         CLI   CONWHENH+5,0        NOW, SOON, OR OV ?                           
         BE    VKXIT               NO                                           
         MVI   ERROR,NEEDCLT       SECURITY - CLIENT REQUIRED                   
         B     ERRX                                                             
*                                                                               
VKL30    GOTO1 VALICLT                                                          
         MVC   SPACLTN,CLTNM                                                    
*                                                                               
*                                                                               
VKXIT    CLI   SPAMED,C'O'                                                      
         BNE   *+12                                                             
         MVI   RCSUBPRG,1          OUTDOOR                                      
         B     *+8                                                              
         MVI   RCSUBPRG,0          ALL OTHER MEDIA                              
         B     XIT                                                              
         DROP  R4                                                               
*                                                                               
NEEDCLT  EQU   85             SPECIFIC CLIENT ENTRY REQUIRED (SECURITY)         
*                                                                               
         EJECT                                                                  
*                                                                               
*        VALIDATE PUB FIELD                                                     
*                                                                               
MYPUBVAL NTR1                                                                   
         LR    R4,R2                                                            
         XC    BPUB(6),BPUB                                                     
         MVI   ALLZE,C'N'          INITIALIZE                                   
         MVI   ERROR,MISSING                                                    
         CLI   5(R2),0                                                          
         BNE   VKPUB5                                                           
         CLI   ACTNUM,ACTREP       SEE IF REPORTING                             
         BE    VKPX                                                             
         B     ERRX                IF NOT THE MUST HAVE PUB                     
*                                                                               
VKPUB5   DS    0H                                                               
         CLI   5(R2),3                                                          
         BNE   VKPUB7                                                           
         CLC   8(3,R2),=C'ALL'                                                  
         BNE   VKPUB7                                                           
         MVC   BPUB(6),=6X'FF'                                                  
         MVC   PUBNM,=CL20'ALL PUBS'                                            
         MVC   PUBZNM,=CL20' '                                                  
         B     VKPX                                                             
*                                                                               
VKPUB7   CLI   8(R2),C'='          PUB NAME SEARCH                              
         BNE   VKPUB10                                                          
         SR    R2,RA                                                            
         LA    R3,WORK                                                          
         USING DSPARM,R3                                                        
         XC    DSPARM(DSPARML),DSPARM                                           
         MVC   DSMEDCOD,QMED                                                    
         GOTO1 =V(SRCHCALL),DMCB,(3,(R2)),(X'80',(RA)),ACOMFACS,       X        
               ('DSPARML',WORK),(1,=CL8'PUB'),0,RR=RELO                         
         B     VKPUB30                                                          
         DROP  R3                                                               
*                                                                               
VKPUB10  DS    0H                                                               
         LR    R2,R4                                                            
         MVI   ERROR,INVALID                                                    
         XC    SCANBLK,SCANBLK                                                  
         GOTO1 SCANNER,DMCB,(R2),(2,SCANBLK)                                    
         CLI   DMCB+4,0                                                         
         BE    ERRX                                                             
         LA    R1,SCANBLK                                                       
         CLC   =C'ALL',44(R1)                                                   
         BNE   VKPUB30                                                          
         MVI   ALLZE,C'Y'          WANT ONLY ACROSS ALL ZONES AND ED            
         ZIC   R3,0(R1)                                                         
         STC   R3,5(R2)                                                         
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),12(R1)                                                   
*                                                                               
VKPUB30  LR    R2,R4                                                            
         GOTO1 VALIPUB                                                          
         CLI   ALLZE,C'Y'                                                       
         BNE   VKPX                                                             
         MVC   BPUB+4(2),=X'FFFF'  ALL ZONES/EDTNS                              
         ZIC   R1,5(R2)                                                         
         AH    R1,=H'4'            PUT BACK THE ORIGINAL LEN                    
         STC   R1,5(R2)                                                         
*                                                                               
VKPX     B     XIT                                                              
         EJECT                                                                  
*                                                                               
* DISPLAY KEY                                                                   
*                                                                               
DK       L     R6,AIO                                                           
         USING PSPCREC,R6                                                       
         MVC   SPAMED,PSPCKMED     MEDIA                                        
         OI    SPAMEDH+6,X'80'     TRANSMIT                                     
*                                                                               
         MVI   NEWKEY,C'Y'         FORCE RE-DISPLAY FROM START OF PUB           
*                                                                               
         MVC   SPACLT,PSPCKCLT     CLIENT                                       
         CLC   PSPCKCLT,=X'FFFFFF'                                              
         BNE   DK5                                                              
         MVC   CLTNM,SPACES                                                     
         MVC   SPACLT,=C'ALL'                                                   
         B     DK10                                                             
*                                                                               
DK5      DS    0H                                                               
*                                                                               
         MVC   MYKEY(L'PSPCKEY),PSPCKEY                                         
*                                                                               
         LA    R2,SPAMEDH                                                       
         MVI   5(R2),1                                                          
         GOTO1 VALIMED                                                          
*                                                                               
         OI    SPACLTH+6,X'80'                                                  
         LA    R2,SPACLTH                                                       
         MVI   5(R2),3                                                          
         GOTO1 VALICLT                                                          
*                                  RESTORE KEY AND RECORD                       
         MVC   KEY(L'PSPCKEY),MYKEY                                             
         GOTO1 HIGH                                                             
         CLC   KEY(L'PSPCKEY),MYKEY         SAME RECORD ?                       
         BE    *+6                 YES - OK                                     
         DC    H'0'                SOMETHING WRONG                              
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
*NOP*    BAS   RE,MYVCLT           READS THE CLIENT                             
*                                                                               
DK10     OI    SPACLTH+6,X'80'                                                  
         MVC   SPACLTN,CLTNM       DISPLAY CLT NAME                             
         OI    SPACLTNH+6,X'80'                                                 
*                                                                               
         CLC   PSPCKPUB,=6X'FF'                                                 
         BNE   DK12                                                             
         MVC   SPAPUB(3),=C'ALL'                                                
         OI    SPAPUBH+6,X'80'                                                  
         XC    SPAPUBN,SPAPUBN                                                  
         OI    SPAPUBNH+6,X'80'                                                 
         XC    SPAPUBZ,SPAPUBZ                                                  
         OI    SPAPUBZH+6,X'80'                                                 
         B     XIT                                                              
*                                                                               
DK12     GOTO1 =V(PUBEDIT),DMCB,(C'0',PSPCKPUB),(0,SPAPUB),RR=RELO              
         OI    SPAPUBH+6,X'80'                                                  
*                                                                               
         MVC   MYPUB,PSPCKPUB                                                   
         BAS   RE,MYVPUB                                                        
         MVC   SPAPUBN,PUBNM       DISPLAY PUB NAME                             
         OI    SPAPUBNH+6,X'80'                                                 
         MVC   SPAPUBZ,PUBZNM      DISPLAY PUB ZONE                             
         OI    SPAPUBZH+6,X'80'                                                 
*                                                                               
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
* VALIDATE RECORD                                                               
*                                                                               
VR       DS    0H                                                               
***********************************************************************         
*                                                                     *         
* VALIDATE DETAIL INPUT VIA LINUP                                     *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
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
VRX      DS    0H                                                               
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
* DISPLAY RECORD                                                                
*                                                                               
DR       DS    0H                                                               
***********************************************************************         
*                                                                     *         
*  DISPLAY DETAILS VIA LINUP                                          *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         GOTO1 =A(LINSET),RR=RELO    INTERFACE WITH LINUP                       
*                                                                               
         MVI   NEWKEY,0            RESET NEWKEY SWITCH                          
*                                                                               
DRX      DS    0H                                                               
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
* LIST RECORDS                                                                  
*                                                                               
LR       DS    0H                                                               
         OI    GLSTSTAT,RETEXTRA                                                
         LA    R6,KEY                                                           
         USING PSPCREC,R6                                                       
         OC    KEY,KEY             TEST FIRST TIME                              
         BNZ   LR030               KEY IS LAST RECORD READ                      
*                                  SO GO CHECK VS. KEYSAVE                      
*                                                                               
         MVC   PSPCKAGY,AGENCY     CREATE KEY - AGENCY                          
         MVC   PSPCKMED,QMED                    MEDIA CODE                      
         MVI   PSPCKTYP,X'2B'                  TYPE                             
         CLI   SPLPUBH+5,0                                                      
         BE    *+10                                                             
         MVC   PSPCKPUB,BPUB                    PUB                             
         CLI   SPLCLTH+5,0                                                      
         BE    *+10                                                             
         MVC   PSPCKCLT,QCLT                    CLIENT                          
*                                                                               
LR010    GOTO1 HIGH                                                             
         B     LR030                                                            
*                                                                               
LR020    GOTO1 SEQ                                                              
*                                                                               
LR030    DS    0H                                                               
         CLC   KEY(4),KEYSAVE      TEST FOR ALL DONE                            
         BE    LR035                                                            
         XC    KEY,KEY                                                          
         B     LRX                                                              
*                                                                               
LR035    DS    0H                                                               
         OC    BPUB,BPUB          SEE IF PUB GIVEN                              
         BZ    LR037                                                            
         CLC   PSPCKPUB,BPUB                                                    
         BE    LR037                                                            
         XC    KEY,KEY                                                          
         B     LRX                                                              
*                                                                               
LR037    OC    QCLT,QCLT          SEE IF CLIENT GIVEN                           
         BZ    LR039                                                            
         CLC   PSPCKCLT,QCLT                                                    
         BNE   LR020               MUST GO TO SEQ                               
*                                                                               
LR039    DS    0H                                                               
         GOTO1 GETREC              GET THE COST RECORD                          
         MVC   MYDSKADD,DMDSKADD   SAVE D/A FOR LIST                            
*                                                                               
         L     R6,AIO                                                           
         USING LISTD,R5                                                         
         LA    R5,P1               USE P LINES                                  
         CLI   MODE,PRINTREP                                                    
         BE    *+8                                                              
         LA    R5,LISTAR           OR LIST AREA                                 
         MVC   LISTAR,SPACES                                                    
*                                                                               
         CLC   PSPCKPUB,=6X'FF'                                                 
         BNE   LR39D                                                            
         MVC   LPUB(3),=C'ALL'                                                  
         B     LR39X                                                            
*                                                                               
LR39D    GOTO1 =V(PUBEDIT),DMCB,(C'0',PSPCKPUB),(0,LPUB),RR=RELO                
         MVC   MYPUB,PSPCKPUB                                                   
         BAS   RE,MYVPUB                                                        
         MVC   LPUBN,PUBNM         DISPLAY PUB NAME                             
*                                                                               
LR39X    MVC   LCLT,PSPCKCLT       CLIENT                                       
         CLC   PSPCKCLT,=X'FFFFFF'                                              
         BNE   *+10                                                             
         MVC   LCLT,=C'ALL'                                                     
         CLI   MODE,PRINTREP       SEE IF PRINTING REPORT                       
         BNE   LR085               NO THEN GO TO NEXT RECORD                    
*                                                                               
         USING PSPCEL01,R6                                                      
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
         BNE   LR080                                                            
         B     LR050                                                            
*                                                                               
LR040    DS    0H                                                               
         BAS   RE,NEXTEL                                                        
         BNE   LR080                                                            
*                                                                               
LR050    MVC   LSPACE,PSPCDESC                                                  
         CLC   =PL2'0',PSPCMCM                                                  
         BE    LR053                          MONTH WAS NOT ENTERED             
         MVI   LMTH+2,C'0'                                                      
         CLC   =PL2'999',PSPCMCM                                                
         BE    LR053                          REL MONTH= ZERO                   
         EDIT  (P2,PSPCMCM),(3,LMTH),FLOAT=-  OR DISPLAY REL MONTH              
*                                                                               
LR053    DS    0H                                                               
         CLC   =PL2'0',PSPCMCD                                                  
         BE    LR055                          DAY WAS NOT ENTERED               
         MVI   LDAY+2,C'0'                                                      
         CLC   =PL2'999',PSPCMCD                                                
         BE    LR055                          REL DAY = ZERO                    
         EDIT  (P2,PSPCMCD),(3,LDAY),FLOAT=-  OR DISPLAY REL DAY                
*                                                                               
LR055    OC    PSPCORV,PSPCORV                                                  
         BZ    LR070               OVERAGE WAS NOT ENTERED                      
         MVI   LOVG+2,C'0'                                                      
         CLI   PSPCORV,X'FF'                                                    
         BE    LR070               OVG = ZERO                                   
         EDIT  PSPCORV,(3,LOVG)    OR DISPLAY OVERAGE                           
*                                                                               
LR070    DS    0H                                                               
         CLI   MODE,PRINTREP       SEE IF PRINTING REPORT                       
         BE    LR090                                                            
*                                                                               
LR080    DS    0H                  END OF RECORD                                
*                                                                               
         CLI   MODE,PRINTREP       SEE IF PRINTING REPORT                       
         BNE   LR085                                                            
         GOTO1 SPOOL,DMCB,SPOOLD   SKIP A LINE                                  
         LA    R6,KEY              MUST RESET R6 TO KEY                         
         B     LR020               GO TO SEQ READ                               
*                                                                               
LR085    MVC   DMDSKADD,MYDSKADD   RESTORE D/A                                  
         GOTO1 LISTMON             CALL LISTMON                                 
         LA    R6,KEY              MUST RESET R6 TO KEY                         
         B     LR020                                                            
*                                                                               
LR090    DS    0H                                                               
         GOTO1 SPOOL,DMCB,SPOOLD                                                
         B     LR040               GO BACK AND DO NEXT ELEM                     
*                                                                               
LRX      B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
* SET UP THE PUB NUMBER & GET NAME                                              
*                                                                               
MYVPUB   NTR1                                                                   
*                                                                               
         MVC   MYKEY,KEY           SAVE KEY                                     
         MVC   PUBNM,SPACES                                                     
         MVC   PUBZNM,SPACES                                                    
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING PUBRECD,R4                                                       
         MVC   PUBKMED,QMED                                                     
         MVC   PUBKPUB(6),MYPUB    MOVE PUB/ZONE/EDTN                           
         CLC   MYPUB+4(2),=X'FFFF'    ALL ZONES/EDTS                            
         BNE   *+10                                                             
         XC    PUBKPUB+4(2),PUBKPUB+4   READ "BASE" PUB                         
         MVC   PUBKAGY,AGENCY                                                   
         MVI   PUBKCOD,X'81'                                                    
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         MVC   FILENAME,=CL8'PUBDIR'                                            
         GOTO1 HIGH                                                             
         XC    FILENAME,FILENAME                                                
*                                                                               
         CLC   KEY(25),KEYSAVE                                                  
         BNE   VPNO                                                             
*                                                                               
         L     R6,AIO2                                                          
         ST    R6,AIO                                                           
         MVC   FILENAME,=CL8'PUBFILE'                                           
         GOTO1 GETREC                                                           
         XC    FILENAME,FILENAME                                                
*                                                                               
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING PUBNAMEL,R6                                                      
         MVC   PUBNM,PUBNAME                                                    
         MVC   PUBZNM(3),=C'ALL'                                                
         CLC   MYPUB+4(2),=X'FFFF'   ALL ZONES/EDTS                             
         BE    *+10                                                             
         MVC   PUBZNM,PUBZNAME                                                  
*                                                                               
         MVC   AIO,AIO1                                                         
         MVC   KEY,MYKEY           RESTORE KEY                                  
         GOTO1 HIGH                                                             
         B     XIT                                                              
*                                                                               
VPNO     MVC   AIO,AIO1                                                         
         MVC   KEY,MYKEY           RESTORE KEY                                  
         GOTO1 HIGH                                                             
         LTR   RB,RB                                                            
         B     XIT                                                              
         DROP  R4,R6                                                            
         EJECT                                                                  
*                                                                               
MYVCLT   NTR1                                                                   
*                                                                               
         MVC   MYKEY,KEY           SAVE KEY                                     
         MVC   CLTNM,SPACES                                                     
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING PCLTRECD,R4                                                      
         MVC   PCLTKMED,QMED                                                    
         MVC   PCLTKAGY,AGENCY                                                  
         MVI   KEY+3,X'02'                                                      
         MVC   KEY+4(3),SPACLT                                                  
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         MVC   FILENAME,=CL8'PRTDIR'                                            
         GOTO1 HIGH                                                             
         XC    FILENAME,FILENAME                                                
*                                                                               
         CLC   KEY(10),KEYSAVE                                                  
         BNE   VCNO                                                             
*                                                                               
         L     R4,AIO2                                                          
         ST    R4,AIO                                                           
         MVC   FILENAME,=CL8'PRTFILE'                                           
         GOTO1 GETREC                                                           
         XC    FILENAME,FILENAME                                                
         MVC   CLTNM,PCLTNAME                                                   
*                                                                               
         MVC   AIO,AIO1                                                         
         MVC   KEY,MYKEY           RESTORE KEY                                  
         GOTO1 HIGH                                                             
         B     XIT                                                              
*                                                                               
VCNO     MVC   AIO,AIO1                                                         
         MVC   KEY,MYKEY           RESTORE KEY                                  
         GOTO1 HIGH                                                             
         LTR   RB,RB                                                            
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
*                                                                               
* PRINT REPORT                                                                  
*                                                                               
PR       L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
*                                                                               
         CLI   SPAMED,C'O'                                                      
         BNE   *+12                                                             
         MVI   RCSUBPRG,1          OUTDOOR                                      
         B     *+8                                                              
         MVI   RCSUBPRG,0          ALL OTHER MEDIA                              
*                                                                               
         LA    R1,HEDSPECS         SET UP HEADHOOK AND SPECS                    
         ST    R1,SPECS                                                         
         LA    R1,HOOK                                                          
         ST    R1,HEADHOOK                                                      
         B     LR                  USE LIST REC LOGIC                           
*                                                                               
PRX      B     XIT                                                              
         EJECT                                                                  
HOOK     NTR1                      HEADLINE ROUTINES                            
*                                                                               
         MVC   HEAD4(5),=C'MEDIA'                                               
         MVC   HEAD4+7(1),QMED                                                  
         MVC   HEAD4+9(10),MEDNM                                                
*                                                                               
HOOKX    B     XIT                                                              
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
*   LINUP INTERFACE                                                  *          
*     - BUILD LUBLK, CREATE ELEMENT TABLE, CALL LINUP                *          
*                                                                    *          
**********************************************************************          
         SPACE 2                                                                
         DS    0D                                                               
LINSET   NMOD1 0,**LINSET                                                       
*                                                                               
         LA    RC,SPOOLEND         POINT TO WORKING STORAGE                     
*                                                                               
         LA    R5,LUBLK            POINT TO  LINUP CONTROL BLOCK                
         USING LUBLKD,R5           ESTABLISH LINUP CONTROL BLOCK                
         XC    LUBLKD(LUBLKL),LUBLKD   CLEAR LINUP CONTROL BLOCK                
*                                                                               
         MVC   LUNEW,NEWKEY        SET NEW OR OLD RECORD INDICATOR              
*                                                                               
         MVC   LUATWA,ATWA         PASS A(TWA)                                  
         L     R1,SYSPARMS         GET A(TIOB)                                  
         L     R1,0(R1)                                                         
         ST    R1,LUATIOB                                                       
*                                                                               
         MVI   LUNLINS,NLINS       SET NUMBER OF LINES ON SCREEN                
         MVI   LUNFLDS,4               FIELDS PER LINE                          
*                                                                               
*                                  BUILD LIST OF FIELD DISPLACEMENTS            
*                                                                               
         LA    R3,LINDSPS          POINT TO LIST OF DISPLACEMENTS               
         ST    R3,LUADSPS          A(LIST OF DISPLACEMENTS)                     
*                                                                               
         LA    R2,SPASP1H          A(FIRST FIELD)                               
         ZIC   R4,LUNLINS          NUMBER OF LINES                              
         SRL   R4,1                HALVE SINCE TWO SETS ON A LINE               
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
         SLL   R0,1                DOUBLE SINCE 2 SETS ON A LINE                
         BAS   RE,BUMP             BUMP TO START OF NEXT LINE                   
         BCT   R0,*-4                BY BUMPING THRU ALL FLDS ON LINE           
*                                                                               
         BCT   R4,LS04             ADD NEXT LINE TO LIST                        
*                                                                               
         LA    R2,SPASP2H          A(FIRST FIELD OF SECOND COLUMN)              
         ZIC   R4,LUNLINS          NUMBER OF LINES                              
         SRL   R4,1                HALVE SINCE TWO SETS ON A LINE               
*                                                                               
LS05     DS    0H                                                               
*                                                                               
         LA    RF,0(R2)            POINT TO FIRST FIELD ON NEXT LINE            
         S     RF,ATWA             GET DISPLACEMENT                             
         STH   RF,0(R3)            SET DISPLACEMENT IN LIST                     
*                                                                               
         LA    R3,2(R3)            BUMP TO NEXT SLOT IN LIST                    
*                                                                               
         ZIC   R0,LUNFLDS          GET NUMBER OF FIELDS ON LINE                 
         SLL   R0,1                DOUBLE SINCE 2 SETS ON A LINE                
         BAS   RE,BUMP             BUMP TO START OF NEXT LINE                   
         BCT   R0,*-4                BY BUMPING THRU ALL FLDS ON LINE           
*                                                                               
         BCT   R4,LS05             ADD NEXT LINE TO LIST                        
*                                                                               
         XC    0(2,R3),0(R3)       FORCE NULLS AT END OF LIST                   
*                                                                               
         MVI   LUSCROLL,LUPAGEQ    SCROLL FACTOR OF A PAGE IS DEFAULT           
*                                                                               
         CLI   SPASCRL,C'P'        CHECK FOR PAGE SCROLL                        
         BE    *+8                                                              
         CLI   SPASCRL,X'97'         LOWERCASE 'P'                              
         BNE   *+8                                                              
         MVI   LUSCROLL,LUPAGEQ                                                 
*                                                                               
         CLI   SPASCRL,C'H'        CHECK FOR HALF PAGE SCROLL                   
         BE    *+8                                                              
         CLI   SPASCRL,X'88'            LOWERCASE 'H'                           
         BNE   *+8                                                              
         MVI   LUSCROLL,LUHALFQ                                                 
*                                                                               
         TM    SPASCRLH+4,X'08'    SKIP IF NOT A NUMERIC FIELD                  
         BNO   LS051                                                            
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,SPASCRLH+5     FIELD INPUT LENGTH                           
         BZ    LS051               NO ENTRY                                     
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         PACK  DUB,SPASCRL(0)      CONVERT SCROLL AMOUNT TO NUMBER              
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
*        CLI   LUPFKEY,0           IF VALID PFKEY HIT                           
*        BE    *+8                                                              
*        OI    GENSTAT2,RETEQSEL   RE-DISPLAY SAME SCREEN                       
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
         CLI   MODE,RECREST        RESTORE RECORD                               
         BE    *+8                                                              
         CLI   MODE,XRECREST       RE-DISPLAY AFTER RESTORE                     
         BNE   *+8                                                              
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
*                                     FIRST LINE UP CALL                        
*                                     ------------------                        
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
         LA    R1,SPAMOR1H         POINT TO FIRST MORE FIELD                    
*                                                                               
         L     R3,LUSVTAB          POINT TO FIRST SAVED ENTRY                   
         USING LSVTABD,R3          ESTABLISH ENTRY                              
*                                                                               
         L     R4,BSPATAB          POINT TO FIRST TABLE ENTRY                   
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
         LA    R1,SPAMORLH         POINT TO LAST MORE FIELD                     
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
         CLI   LUERR,0             RESET CC                                     
*                                                                               
         XIT1                                                                   
         SPACE 2                                                                
*                                  LINES IN WINDOW                              
*NLINS    EQU   6                                                               
NLINS    EQU   ((SPASPLH-SPASP1H)/(SPASP2H-SPASP1H))+1                          
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
         BAS   RE,LHVALLIN         YES, VALIDATE LINE AND ADD TO TABLE          
         BNE   LHVALX              IF ERROR DONT DELETE OLD AND DONT            
*                                  CLEAR SAVE TABLE ENTRY                       
         EJECT                                                                  
LHV06    DS    0H                                                               
         L     R2,LUACTAB          POINT TO CURRENT SAVED TABLE ENTRY           
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
         EJECT                                                                  
**********************************************************************          
*   LINHOOK - LINUP DISPLAY HOOK ROUTINE                             *          
**********************************************************************          
         SPACE 2                                                                
LHDIS    DS    0H                                                               
         MVI   LUSTAT,0            INIT STATUS                                  
         L     R4,LUACTAB          CURRENT SAVE TABLE ENTRY                     
         USING LSVTABD,R4                                                       
         XC    0(LSVTABL,R4),0(R4)  CLEAR IT                                    
*                                                                               
         BAS   RE,LHSRCH           FIND ELEM TO DISPLAY                         
         BAS   RE,LHDISLIN         BUILD SCREEN LINE                            
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
         DROP  R2                                                               
         DROP  R4                                                               
         TITLE 'PRSFM0C - BUILD TABLE OF ELEMENTS ON FILE'                      
***********************************************************************         
*                                                                     *         
* ROUTINE TO BUILD ELEMENT TABLE FOR LINUP                            *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
LSBLDTAB NTR1                                                                   
         XC    BSPPRMS(BSPPRML),BSPPRMS INIT BINSRCH PARAMETERS                 
         L     R1,AIO2             STORE TABLE IN I/O2 & I/O3                   
         ST    R1,BSPATAB          PASS TABLE ADDRESS                           
         LA    R1,ELTABL           PASS ENTRY LENGTH                            
         ST    R1,BSPLENR                                                       
         LA    R1,ELTKEYL          PASS KEY LENGTH                              
         ST    R1,BSPLENK                                                       
         MVI   BSPKEYD,0           PASS KEY DISPLACEMENT                        
         LA    R1,ELTMAX           PASS MAXIMUM COUNT                           
         ST    R1,BSPMAX                                                        
*                                                                               
         LA    R4,WRKELTAB         POINT TO ELTAB WORK ELEMENT                  
         USING ELTABD,R4           ESTABLISH TABLE ENTRY                        
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'01'                                                     
         USING PSPCEL01,R6                                                      
         BAS   RE,GETEL                                                         
         B     LSBT20                                                           
*                                                                               
LSBT10   BAS   RE,NEXTEL                                                        
*                                                                               
LSBT20   BNE   LSBTX               NO ELEMENTS LEFT                             
*                                                                               
         XC    ELTABD(ELTABL),ELTABD INIT ENTRY                                 
*                                                                               
         MVC   ELTELEM,PSPCEL01    ADD ELEMENT TO TABLE                         
         MVC   ELTSORT,PSPCDESC    SET KEY                                      
*                                                                               
LSBT30   DS    0H                                                               
*                                                                               
         GOTO1 =V(BINSRCH),BSPPRMS,('BSPADD',WRKELTAB),RR=RELO                  
         OC    1(3,R1),1(R1)       TEST IF ELEMENT FIT INTO TABLE               
         BNZ   *+6                                                              
         DC    H'0'                TOO MANY LINES (SHOULD NOT HAPPEN)           
*                                                                               
         B     LSBT10                                                           
*                                                                               
LSBTX    DS    0H                                                               
*                                                                               
         ICM   R1,15,BSPNOR        NUMBER OF ENTRIES                            
         BZ    *+10                                                             
         BCTR  R1,R0               MINUS ONE                                    
         MH    R1,=Y(ELTABL)       TIMES ENTRY LENGTH                           
         A     R1,BSPATAB          PLUS START OF TABLE                          
         ST    R1,ELTLAST          SET A(LAST)                                  
*                                                                               
         XIT1                                                                   
         DROP  R4                                                               
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
* ROUTINE TO SEARCH TABLE FOR ELEMENT                                 *         
* AND SET ADDRESS IN ELTENT                                           *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
LHSRCH   NTR1                                                                   
         XC    ELTENT,ELTENT       INIT ELEMENT ADDRESS RETURN                  
         NI    LUSTAT,255-LUEOLQ   SET OFF END OF LIST INDICATOR                
*                                                                               
         OC    BSPNOR,BSPNOR       IF NO ENTRIES                                
         BZ    LHSRCHX             RETURN EMPTY-HANDED                          
*                                                                               
         L     R4,BSPATAB          DEFAULT TO FIRST ENTRY IN TABLE              
         USING ELTABD,R4                                                        
*                                                                               
         L     R3,LUAPTAB          A(PREVIOUS SAVE TABLE)                       
         USING LSVTABD,R3                                                       
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
         L     R4,BSPATAB          NO, POINT TO FIRST-(END OF TABLE)            
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
         L     R4,BSPATAB          POINT TO FIRST ENTRY IN TABLE                
         CLC   BSPNOR,=F'1'        DONE IF MORE THAN ONE ENTRY IN TABLE         
         BH    LHSRCH30                                                         
*                                  EXIT WITHOUT FINDING ELEMENT                 
         OI    LUSTAT,LUEOLQ       TELL LINUP THIS IS LAST                      
         B     LHSRCHX                                                          
*                                                                               
LHSRCH12 DS    0H                                                               
         OI    LUSTAT,LUEOLQ       TELL LINUP THIS IS LAST                      
         B     LHSRCH30                                                         
         EJECT                                                                  
LHSRCH16 DS    0H                  GOING BACKWARDS                              
         C     R4,BSPATAB          IF AT START                                  
         BNH   LHSRCH18            DONT GO FURTHER                              
         SH    R4,=Y(ELTABL)       BACK UP AN ENTRY                             
LHSRCH17 DS    0H                                                               
         C     R4,BSPATAB          IF AT START                                  
         BH    *+8                                                              
LHSRCH18 DS    0H                                                               
         OI    LUSTAT,LUEOLQ       TELL LINUP THIS IS LAST                      
*                                                                               
LHSRCH30 DS    0H                                                               
         TM    ELTCTL,ELTDELQ+ELTADDQ DISPLAY CHANGED ELEMENTS                  
         BO    LHSRCH40                                                         
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
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
* ROUTINE TO VALIDATE WINDOW LINE                                     *         
* BUILD ENTRY IN APWORK AND ADD TO ELEM TABLE                         *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
LHVALLIN NTR1                                                                   
*                                                                               
         XC    ELTENT,ELTENT       CLEAR A(ENTRY)                               
*                                                                               
         TM    LULSTAT,LUSNPVQ     SKIP IF ALL FIELDS PREVIOUSLY                
         BO    *+8                   VALIDATED                                  
         TM    LULSTAT,LUSDATQ     AND NO DATA IN FIELDS ON THIS LINE           
         BZ    LHVLX                                                            
*                                                                               
         XC    WRKELTAB,WRKELTAB   INIT WORK AREA                               
         LA    R4,WRKELTAB         SET TO BUILD TABLE ELEMENT                   
         USING ELTABD,R4           IN WORK AREA                                 
*                                                                               
         LA    R3,ELTELEM          INITIALIZE ELEMENT                           
         USING PSPCEL01,R3         ESTABLISH STANDARD CONTRACT UNIT ELM         
*                                                                               
         SLR   R4,R4               INIT TABLE ELEMENT POINTER                   
*                                                                               
         L     R2,LUACLIN          POINT TO FIRST FIELD ON LINE =SPACE          
*                                                                               
         CLI   5(R2),0             IF NO INPUT IN SPACE FIELD                   
         BNE   LHV30                                                            
         LR    R0,R2               SAVE R2                                      
         BAS   RE,BUMP             & SEE IF ANY INPUT ON THIS LINE              
         CLI   5(R2),0                                                          
         BNE   LHV20                                                            
         BAS   RE,BUMP             ANY INPUT ON THIS LINE                       
         CLI   5(R2),0                                                          
         BE    LHV50                                                            
LHV20    LR    R2,R0               RESET R2 FOR ERROR                           
         B     MISSERR                                                          
*                                                                               
LHV30    DS    0H                                                               
*                                                                               
         CLC   =C'DELETE',8(R2)    DELETE THIS ENTRY                            
         BE    LHVSPOK             IGNORE ANY OTHER INPUT                       
*                                                                               
         CLI   8(R2),C'='          BROWSE REQUEST ?                             
         BNE   LHV32               NO                                           
*                                                                               
         GOTO1 =V(PPBROWSE),DMCB,ACOMFACS,SYSRD,(R2),                  X        
               0,(QMED,C' STD'),0,RR=RELO                                       
         DC    H'0'                BROWSE SHOULD HAVE TAKEN IT                  
*                                                                               
LHV32    DS    0H                                                               
         LA    R4,WRKELTAB         SET TO BUILD TABLE ELEMENT                   
*                                                                               
         CLI   SPAMED,C'N'                                                      
         BNE   LHV40D                                                           
         CLI   5(R2),8             MAX LENGTH IS 8 FOR NEWSPAPRES               
         BH    INVERR                                                           
         B     LHV40H                                                           
LHV40D   DS    0H                                                               
         CLI   SPAMED,C'O'         SEE IF OUTDOOR                               
         BNE   LHV40H                                                           
         CLC   8(4,R2),=C'SRI='    CHK FOR SPECIAL SRI INPUT                    
         BNE   LHV40H                                                           
         SPACE 2                                                                
CHKOUT   DS    0H                 OUTPUT IS X'FF' FOLLOWED BY                   
*                                 3-3 BYTE PACKED FIELDS                        
         XC    WORK(10),WORK                                                    
         MVI   WORK,X'FF'                                                       
         ZAP   WORK+1(3),=P'99999'                                              
         LA    R6,15(R2)                                                        
         CLC   12(3,R2),=C'SPC'        SPECIAL                                  
         BE    CO4                                                              
         GOTO1 =V(NUMED),DMCB,12(R2),DUB,RR=RELO                                
*                                                                               
         CP    DUB,=P'100'          SHOWING CAN'T EXCEED 100                    
         BH    COERR                                                            
*                                                                               
         ZAP   WORK+1(3),DUB                                                    
         L     R6,DMCB                                                          
         CLI   0(R6),C' '                                                       
         BE    COERR                                                            
CO4      DS    0H                                                               
         GOTO1 =V(NUMED),DMCB,1(R6),DUB,RR=RELO                                 
*                                                                               
         ZAP   WORK+4(3),DUB                                                    
         L     R6,DMCB                                                          
         CLI   0(R6),C' '                                                       
         BE    COERR                                                            
         GOTO1 (RF),(R1),1(R5)                                                  
*                                                                               
         ZAP   WORK+7(3),DUB                                                    
         L     R6,DMCB                                                          
         CLI   0(R6),C' '                                                       
         BH    COERR                                                            
COX      DS    0H                                                               
         MVI   0(R3),X'01'         ELEMENT CODE                                 
         MVI   1(R3),PSPCELLN      ELEMENT LENGTH                               
         MVC   PSPCDESC(10),WORK                                                
         B     LHV40H5                                                          
*                                                                               
COERR    B     INVERR              INVALID SRI= FORMAT                          
*                                                                               
LHV40H   DS    0H                                                               
         BAS   RE,CKSPDESC         CHECK SPACE DESCRIPTION                      
         BNE   LHVCMPE3            INVALID SPACE DESCRIPTION                    
*                                                                               
         MVI   0(R3),X'01'         ELEMENT CODE                                 
         MVI   1(R3),PSPCELLN      ELEMENT LENGTH                               
         MVC   PSPCDESC,8(R2)                                                   
         OC    PSPCDESC,SPACES                                                  
LHV40H5  DS    0H                                                               
*                                                                               
         MVC   ELTSORT,PSPCDESC    SET SORT KEY TO SPACE                        
*                                                                               
LHVSPOK  DS    0H                                                               
*                                                                               
         OI    4(R2),X'20'         INDICATE FIELD VALIDATED                     
         OI    6(R2),X'80'         TRANSMIT FIELD                               
*                                                                               
LHVCU1   DS    0H                                                               
*                                                                               
         BAS   RE,BUMP             BUMP TO REL MONTH FLD                        
*                                                                               
         LTR   R4,R4               IF DELETING ENTRY                            
         BZ    LHV45                                                            
*                                                                               
         MVC   PSPCMCM(2),=PL2'0'  IF P'000' - NO DATA ENTERED                  
         ZIC   RF,5(R2)                                                         
         LTR   RF,RF                                                            
         BZ    LHV45               NO INPUT CHECK REL DAY FLD                   
*NOP*    MVC   PSPCMCM(2),=PL2'999'                                             
*NOP*    CLI   8(R2),C'0'          IF P'999' - ZERO REL MTH ENTERED             
*NOP*    BE    LHV45                                                            
         CLI   8(R2),C' '                                                       
         BE    INVERR                                                           
         GOTO1 CASHVAL,DMCB,(0,8(R2)),(RF)                                      
         CLI   0(R1),X'FF'                                                      
         BE    INVERR                                                           
         L     RF,DMCB+4                                                        
         CVD   RF,DUB                                                           
         DP    DUB,=P'100'                                                      
         CP    DUB+6(2),=P'0'      MUST GET REMAINDER 0                         
         BNE   INVERR                                                           
         MVC   PSPCMCM(2),=PL2'999'                                             
         CP    DUB+4(2),=P'0'      P'999' = ZERO REL MTH ENTERED                
         BE    LHV45                                                            
         MVC   PSPCMCM(2),DUB+4                                                 
*                                                                               
LHV45    DS    0H                                                               
*                                                                               
         OI    4(R2),X'20'         INDICATE FIELD VALIDATED                     
         OI    6(R2),X'80'         TRANSMIT FIELD                               
*                                                                               
         BAS   RE,BUMP             BUMP TO REL DAY FLD                          
*                                                                               
         LTR   R4,R4               IF DELETING ENTRY                            
         BZ    LHV47                                                            
*                                                                               
         MVC   PSPCMCD(2),=PL2'0'                                               
         ZIC   RF,5(R2)                                                         
         LTR   RF,RF                                                            
         BZ    LHV47               NO INPUT CHECK OVERAGE FLD                   
*NOP*    MVC   PSPCMCD(2),=PL2'999'                                             
*NOP*    CLI   8(R2),C'0'          IF P'999' - ZERO REL DAY ENTERED             
*NOP*    BE    LHV47               NO INPUT CHECK OVERAGE FLD                   
         CLI   8(R2),C' '                                                       
         BE    INVERR                                                           
         GOTO1 CASHVAL,DMCB,(0,8(R2)),(RF)                                      
         CLI   0(R1),X'FF'                                                      
         BE    INVERR                                                           
         L     RF,DMCB+4                                                        
         CVD   RF,DUB                                                           
         DP    DUB,=P'100'                                                      
         CP    DUB+6(2),=P'0'      MUST GET REMAINDER 0                         
         BNE   INVERR                                                           
         MVC   PSPCMCD(2),=PL2'999'                                             
         CP    DUB+4(2),=P'0'      P'999' = ZERO REL DAY ENTERED                
         BE    LHV47               DONE WITH REL DAY                            
         CP    DUB+4(2),=P'31'                                                  
         BH    INVERR                                                           
         CP    PSPCMCM(2),=P'0'                                                 
         BNL   LHV46                                                            
         CP    DUB+4(2),=P'0'                                                   
         BL    INVERR                                                           
*                                                                               
LHV46    MVC   PSPCMCD(2),DUB+4                                                 
*                                                                               
LHV47    DS    0H                                                               
*                                                                               
         OI    4(R2),X'20'         INDICATE FIELD VALIDATED                     
         OI    6(R2),X'80'         TRANSMIT FIELD                               
*                                                                               
         BAS   RE,BUMP             BUMP TO OVERAGE FLD                          
*                                                                               
         LTR   R4,R4               IF DELETING ENTRY                            
         BZ    LHVCU1OK                                                         
*                                                                               
         MVI   PSPCORV,0           X'00' IF NO DATA ENTERED                     
         CLI   5(R2),0                                                          
         BE    LHV48                                                            
*                                                                               
         CLI   SPAMED,C'O'         MUST BE OUTDOOR                              
         BNE   INVERR                                                           
*                                                                               
         TM    4(R2),X'08'         IS IT NUMERIC (NO DECIMALS)                  
         BO    *+8                                                              
         B     INVERR                                                           
*NOP*    MVI   PSPCORV,X'FF'       X'FF' IF ZERO PCT. ENTERED                   
*NOP*    CLI   8(R2),C'0'                                                       
*NOP*    BE    LHVCU1OK                                                         
*NOP*    BL    INVERR                                                           
         CLI   5(R2),3                                                          
         BL    *+14                                                             
         CLC   8(3,R2),=C'200'     ACCEPTABLE OVERAGE IS 1-200                  
         BH    INVERR                                                           
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         MVI   PSPCORV,X'FF'       X'FF' IF ZERO PCT. ENTERED                   
         CP    DUB+6(2),=P'0'                                                   
         BE    LHVCU1OK                                                         
         BL    INVERR                                                           
         CVB   RF,DUB                                                           
         STC   RF,PSPCORV                                                       
         B     LHVCU1OK                                                         
*                                  OVERAGE FIELD IS BLANK                       
*                                  SEE IF THERE IS ANY INPUT                    
LHV48    CLC   PSPCMCM(2),=PL2'0'  IN EIGTHER MONTH OR DAY FIELD                
         BNE   LHVCU1OK                                                         
         CLC   PSPCMCD(2),=PL2'0'                                               
         BNE   LHVCU1OK                                                         
*                                                                               
         L     R2,LUACLIN          POINT TO FIRST FIELD ON LINE =SPACE          
*                                                                               
         BAS   RE,BUMP             SET CURSOR AT REL MTH - "1ST" OF 3           
         B     NONEERR             ALL THREE FIELD CAN NOT BE BLANK             
*                                                                               
LHVCU1OK DS    0H                                                               
*                                                                               
         OI    4(R2),X'20'         INDICATE FIELD VALIDATED                     
         OI    6(R2),X'80'         TRANSMIT FIELD                               
*                                                                               
         LTR   R4,R4               DONE IF NOT BUILDING AN ELEMENT              
         BZ    LHVCU2OK                                                         
*                                                                               
LHV49    DS    0H                                                               
         CLC   PSPCMCM(2),=PL2'0'    SEE IF REL MONTH WAS ENTERD                
         BNE   *+14                SKIP THAT SPACE                              
         CLC   PSPCMCD(2),=PL2'0'    SEE IF REL DAY WAS ENTERED                 
         BE    LHV49X              SKIP THAT SPACE                              
         OI    ELTCTL,ELTADDQ      ADD ELEMENT                                  
         B     *+8                                                              
LHV49X   OI    ELTCTL,ELTDELQ      DELETE ELEMENT                               
*                                                                               
LHV50    DS    0H                  BUMP TO NEXT LINE                            
*                                                                               
*                                                                               
LHVCU2OK DS    0H                                                               
*                                                                               
         OI    4(R2),X'20'         INDICATE FIELD VALIDATED                     
         OI    6(R2),X'80'         TRANSMIT FIELD                               
*                                                                               
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
         CLI   BSPCNTL,BSPNF       TEST IF NO MATCH FOUND                       
         BE    LHVL92              YES - NEW ENTRY FOR TABLE                    
*                                                                               
         L     R2,LUACTAB          POINT TO CURRENT SAVED ELEMENT               
         USING LSVTABD,R2                                                       
*                                                                               
         CLC   LSVKEY,ELTKEY       ALSO OK IF ENTRY KEY NOT CHANGED             
         BNE   LHVCMPE2                                                         
*                                                                               
LHVL92   DS    0H                                                               
*                                                                               
         OI    ELTCTL,ELTADDQ      SET ADDED (NB- ADD+DEL=CHA)                  
         MVC   ELTELEM,PSPCEL01    SET NEW ELEM IN TABLE                        
*                                  SET NEW ELTLAST                              
         ICM   R1,15,BSPNOR        NUMBER OF ENTRIES                            
         BZ    *+10                                                             
         BCTR  R1,R0                                                            
         MH    R1,=Y(ELTABL)                                                    
         A     R1,BSPATAB          PLUS START OF TABLE                          
         ST    R1,ELTLAST          SET A(LAST)                                  
         B     LHVLX                                                            
*                                                                               
         EJECT                                                                  
LHVCMPE1 DS    0H                                                               
         MVI   ERROR,RECFULL             TOO MANY DETAIL LINES                  
         B     LHVCMPER                                                         
*                                                                               
LHVCMPE2 DS    0H                                                               
         L     R2,LUACLIN          POINT TO FIRST FIELD ON LINE =SPACE          
         MVI   ERROR,DUPEDATA      DUPLICATE                                    
         B     LHVCMPER                                                         
*                                                                               
LHVCMPE3 DS    0H                                                               
         MVI   ERROR,NOSPDESC            SPACE DESCRIPTION NOT FOUND            
         B     LHVCMPER                                                         
*                                                                               
LHVCMPER DS    0H                                                               
*                                                                               
         NI    4(R2),X'FF'-X'20'   TURN OFF VALIDATED STATUS                    
         GOTO1 ERREX               HANDLE FIELD IN ERROR                        
*                                                                               
LHVLX    DS    0H                                                               
         CLI   ERROR,0             SET CC                                       
         XIT1                                                                   
         DROP  R4                                                               
         DROP  R3                                                               
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
*        ROUTINE TO BUILD WINDOW LINE                                 *         
*        FROM TABLE ENTRY IN ELTENT                                   *         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
LHDISLIN NTR1                                                                   
*                                                                               
         CLI   SPAMED,C'O'         IS MEDIA = OUTDOOR                           
         BNE   *+14                                                             
         MVC   SPASPDS,OUTDOOR                                                  
         B     *+10                                                             
         MVC   SPASPDS,OTHERMD                                                  
*                                                                               
         OI    SPASPDSH+6,X'80'     SET TRANSMIT BIT                            
         L     R2,LUACLIN          A(FIRST FIELD)                               
*                                                                               
         MVC   FLD,SPACES          DEFAULT TO CLEARING                          
*                                                                               
         TWAXC (R2),(R2)           CLEAR FIELD                                  
*                                                                               
         ICM   R4,15,ELTENT        POINT TO ELEMENT IN TABLE                    
         BZ    LHDSPCX             CHECK IF NONE FOUND                          
*                                                                               
         USING ELTABD,R4           ESTABLISH TABLE ELEMENT                      
*                                                                               
         LA    R3,ELTELEM          ESTABLISH RECORD ELEMENT PART                
         USING PSPCEL01,R3                                                      
*                                                                               
*              DISPLAY SPACE                                                    
*                                                                               
         OC    PSPCDESC,PSPCDESC                                                
         BZ    LHD25                                                            
         CLI   PSPCDESC,X'FF'       CHK FOR OUTDOOR SRI=                        
         BNE   LHD24                                                            
         LA    RF,16(R2)                                                        
         MVC   8(8,R2),=C'SRI=SPC,'                                             
         CP    PSPCDESC+1(3),=P'99999'                                          
         BE    LHD20C                                                           
         LA    RF,12(R2)                                                        
         LA    R6,PSPCDESC+1                                                    
         BAS   RE,EDT                                                           
         MVI   0(RF),C','                                                       
         LA    RF,1(RF)                                                         
LHD20C   DS    0H                                                               
         LA    R6,PSPCDESC+4                                                    
         BAS   RE,EDT                                                           
         MVI   0(RF),C','                                                       
         LA    R6,PSPCDESC+7                                                    
         LA    RF,1(RF)                                                         
         BAS   RE,EDT                                                           
         B     LHD25                                                            
*                                                                               
LHD24    MVC   8(17,R2),PSPCDESC                                                
*                                                                               
         OI    4(R2),X'20'         INDICATE FIELD VALIDATED                     
*                                                                               
LHDSPCX  DS    0H                                                               
*                                                                               
LHD25    BAS   RE,BUMP                                                          
         TWAXC (R2),(R2)           CLEAR FIELD                                  
*                                                                               
         LTR   R4,R4               SKIP IF NOT ELEMENT TO DISPLAY               
         BZ    LHD27                                                            
*                                                                               
         OI    4(R2),X'20'         INDICATE FIELD VALIDATED                     
*                                                                               
         CLC   PSPCMCM(2),=PL2'0'                                               
         BE    LHD27                                                            
         MVI   8(R2),C'0'                                                       
         CLC   PSPCMCM(2),=PL2'999'                                             
         BE    LHD27                                                            
         EDIT  (P2,PSPCMCM),(3,8(R2)),ALIGN=LEFT,FLOAT=-                        
*                                                                               
LHD27    DS    0H                                                               
         BAS   RE,BUMP                                                          
         TWAXC (R2),(R2)           CLEAR FIELD                                  
*                                                                               
         LTR   R4,R4               SKIP IF NOT ELEMENT TO DISPLAY               
         BZ    LHD29                                                            
*                                                                               
         OI    4(R2),X'20'         INDICATE FIELD VALIDATED                     
*                                                                               
         CLC   PSPCMCD(2),=PL2'0'                                               
         BE    LHD29                                                            
         MVI   8(R2),C'0'                                                       
         CLC   PSPCMCD(2),=PL2'999'                                             
         BE    LHD29                                                            
         EDIT  (P2,PSPCMCD),(3,8(R2)),ALIGN=LEFT,FLOAT=-                        
*                                                                               
*                                                                               
LHD29    DS    0H                                                               
         BAS   RE,BUMP                                                          
         TWAXC (R2),(R2)           CLEAR FIELD                                  
*                                                                               
         LTR   R4,R4               SKIP IF NOT ELEMENT TO DISPLAY               
         BZ    LHD30                                                            
*                                                                               
         OI    4(R2),X'20'         INDICATE FIELD VALIDATED                     
*                                                                               
         CLI   PSPCORV,0                                                        
         BE    LHD30                                                            
         MVI   8(R2),C'0'                                                       
         CLI   PSPCORV,X'FF'                                                    
         BE    LHD30                                                            
         EDIT  PSPCORV,(3,8(R2)),ALIGN=LEFT                                     
*                                                                               
LHD30    DS    0H                                                               
*                                                                               
LHDLX    DS    0H                                                               
         XIT1                                                                   
         DROP  R3                                                               
         DROP  R4                                                               
         EJECT                                                                  
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
*        FIRST DELETE ALL CURRENT ELEMENTS                                      
*                                                                               
         L     R6,AIO1             GET RECORD ADDRESS                           
         ST    R6,DMCB             PASS TO RECUP                                
         MVI   DMCB,C'P'           INDICATE PRINT SYSTEM                        
*                                                                               
         LA    R6,PSPCELEM-PSPCREC(R6)  POINT TO FIRST ELEMENT IN REC           
*                                                                               
         XC    DMCB+8(4),DMCB+8    SET FOR DELETE                               
*                                                                               
         SR    R0,R0                                                            
         LA    R1,DMCB             POINT TO PARAMETER LIST                      
         L     RF,VRECUP           POINT TO UPDATING ROUTINE                    
*                                                                               
LSWTDELL DS    0H                                                               
*                                                                               
         USING PSPCEL01,R6         ESTABLISH SCU ELEMENT                        
*                                                                               
         CLI   PSPCEL01,0          DONE IF EOR REACHED                          
         BE    LSWTDELD                                                         
*                                                                               
         CLI   PSPCEL01,X'01'      SKIP IF NOT SPACE ELEMENT                    
         BNE   LSWTDELC                                                         
*                                                                               
         GOTO1 (RF),(R1),,(R6)     DELETE ELEMENT                               
*                                                                               
         B     LSWTDELX            BECAUSE R6 ==> NEXT ELM NOW                  
*                                                                               
LSWTDELC DS    0H                                                               
*                                                                               
         IC    R0,PSPCEL01+1       ELEMENT LENGTH                               
         AR    R6,R0               NEXT ELEMENT                                 
*                                                                               
LSWTDELX DS    0H                                                               
*                                                                               
         B     LSWTDELL                                                         
*                                                                               
LSWTDELD DS    0H                  ALL ELEMENTS DELETED                         
*                                                                               
         DROP  R6                                                               
*                                                                               
*        ADD ELEMENTS IN TABLE TO RECORD                                        
*                                                                               
         L     R4,BSPATAB          START OF TABLE                               
         USING ELTABD,R4           ESTABLISH ENTRY IN TABLE                     
*                                                                               
         OC    BSPNOR,BSPNOR       SKIP IF NO ENTRIES                           
         BZ    LSWTLPDN                                                         
*                                                                               
         L     R6,AIO1             GET RECORD ADDRESS                           
         ST    R6,DMCB             PASS TO RECUP                                
         MVI   DMCB,C'P'           INDICATE PRINT SYSTEM                        
*                                                                               
         LA    R6,PSPCELEM-PSPCREC(R6)  POINT TO FIRST ELEMENT IN REC           
*                                                                               
         SR    R0,R0                                                            
         LA    R1,DMCB             POINT TO PARAMETER LIST                      
*                                                                               
LSWTLOOP DS    0H                                                               
*                                                                               
         TM    ELTCTL,ELTADDQ      ADD ELEMENTS FLAGGED FOR ADD                 
         BO    LSWTADD                                                          
         TM    ELTCTL,ELTDELQ      AND THOSE NOT TO BE DELETED                  
         BO    LSWTNADD                                                         
*                                                                               
LSWTADD  DS    0H                                                               
*                                                                               
         XC    ELEMENT,ELEMENT     CLEAR WORKAREA                               
*                                                                               
         LA    R3,ELTELEM          POINT TO ELEMENT IN TABLE                    
         USING PSPCEL01,R3         ESTABLISH SPACE ELEMENT                      
*                                                                               
         CLI   PSPCEL01+1,0        GET ELEMENT LENGTH                           
         BZ    LSWTADDX            SKIP IF NO ENTRY                             
*                                                                               
         GOTO1 (RF),(R1),,(R3),(C'R',(R6))  ADD ELEMENT                         
*                                                                               
         CLI   DMCB+8,0            ONLY ERROR CAN BE NO ROOM IN REC             
         BE    NOMORE              NO ERROR TOLERATED                           
*                                                                               
         IC    R0,1(R6)            ELEMENT LENGTH                               
         AR    R6,R0               NEXT INSERTION POINT                         
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
         DROP  R3                                                               
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* COMMONLY ADDRESSABLE ROUTINES                                       *         
***********************************************************************         
         SPACE 1                                                                
SUBROUTS DS    0D                                                               
***********************************************************************         
*                                                                     *         
* HANDLE FIELD IN ERROR - R1 -POINTS TO FIELD                         *         
*         HIGHLIGHT FIELD                                             *         
*         ERROR MESSAGE IS IN ERROR                                   *         
*         IF SAVMSGNO IS NOT FVFOK THEN THIS IS NOT FIRST ERROR       *         
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
DSPFLD   NTR1                      BUMP TO NEXT SCREEN FIELD                    
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
         DROP  R1                                                               
         EJECT                                                                  
*                                                                               
EDT      DS    0H          USED TO DISPLAY OUTDOOR SRI SPACES                   
         MVI   0(RF),C'0'                                                       
         LA    R0,1                                                             
         CP    0(3,R6),=P'0'                                                    
         BE    EDT2                                                             
         EDIT  (P3,0(R6)),(4,0(RF)),ALIGN=LEFT                                  
*                                                                               
EDT2     DS    0H                                                               
         AR    RF,R0                                                            
         BR    RE                                                               
*                                                                               
DATERR   MVI   ERROR,INVDATE                                                    
         B     ERRX                                                             
*                                                                               
INVERR   MVI   ERROR,INVALID                                                    
         B     ERRX                                                             
*                                                                               
DUPERR   MVI   ERROR,DUPEDATA      DUPLICATE DATA                               
         B     ERRX                                                             
*                                                                               
MISSERR  MVI   ERROR,MISSING                                                    
         B     ERRX                                                             
*                                                                               
NONEERR  MVI   ERROR,NEEDDATA      AT LEAST ONE ENTRY REQUIRED                  
         B     ERRX                                                             
*                                                                               
NOMORE   MVI   ERROR,RECFULL       NO ROOM IN RECORD OR TABLE                   
         LA    R2,SPASP1H                                                       
         B     ERRX                                                             
*                                                                               
ERRX     GOTO1 ERREX                                                            
*                                                                               
NOSPDESC EQU   81                  SPACE DESCRIPTION NOT ON FILE                
NEEDDESC EQU   82                  MUST ENTER AT LEAST 1 DESCRIPTION            
NEEDDATA EQU   83                  AT LEAST ONE ENTRY REQUIRED                  
DUPEDATA EQU   179                 DUPLICATE DATA                               
RECFULL  EQU   180                 RECORD FULL                                  
*                                                                               
OUTDOOR  DC    C'Outspace 2        ^Mth ^Day  ^%     ^Outspace 2      ^X        
                 Mth ^Day  ^%'                                                  
OTHERMD  DC    C'Space             ^Mth ^Day  ^%     ^Space           ^X        
                 Mth ^Day  ^%'                                                  
*                                                                               
         EJECT                                                                  
*                                  VALIDATE SPACE DESCRIPTION                   
CKSPDESC NTR1                                                                   
         XC    BYPROF,BYPROF       CLEAR BY PROFILE                             
*                                  GET   BY PROFILE                             
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'P0BY'                                                 
         MVC   WORK+4(2),AGENCY                                                 
         MVC   WORK+6(1),QMED                                                   
         CLC   QCLT(3),=X'FFFFFF'  ALL CLIENTS ?                                
         BE    CKSPD05             YES                                          
         MVC   WORK+7(3),QCLT                                                   
         CLI   SVCPROF+30,C' '     PCLTOFF SAVED HERE IN VALICLT                
         BNH   *+14                                                             
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),SVCPROF+30                                            
CKSPD05  GOTO1 GETPROF,DMCB,WORK,BYPROF,DATAMGR                                 
*                                                                               
         CLI   BYPROF+09,C'Y'      PROFILE ALLOWS SPACE DESC LOOK UP?           
         BNE   CKSPDOK             NO NEED TO LOOK UP SPC DESC RECORD           
*                                                                               
* SEE IF SPACE DESCRIPTION ENTERED IS SAME AS IN RECORD NOW                     
*                                                                               
         MVC   WORK(17),8(R2)      R2 POINTING TO SPACE DESC                    
         OC    WORK(17),SPACES                                                  
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'01'                                                     
         USING PSPCEL01,R6                                                      
         BAS   RE,GETEL                                                         
         B     CKSPD25                                                          
*                                                                               
CKSPD20  BAS   RE,NEXTEL                                                        
*                                                                               
CKSPD25  BNE   CKSPD30             VALIDATE THE SPACE DESCRIPTION               
         CLC   PSPCDESC,WORK                                                    
         BE    CKSPDOK             NO NEED TO LOOK UP (UNCHANGED)               
         B     CKSPD20             CHECK NEXT ELEMENT                           
*                                                                               
         DROP  R6                                                               
*                                                                               
CKSPD30  DS    0H                  VALIDATE THE SPACE DESCRIPTION               
         MVC   SVKEY,KEY           SAVE CURRENT KEY                             
*                                                                               
         XC    KEY,KEY             BUILD KEY TO READ SPC DESP RECORD            
         MVC   KEY+00(02),AGENCY                                                
         MVC   KEY+02(01),QMED                                                  
         MVI   KEY+03,X'5A'        STANDARD SPACE DESCRIPTION REC CODE          
         MVC   KEY+04(17),WORK     WORK STILL CONTAINS SCREEN FIELD             
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(21),KEYSAVE     INPUT MATCHES THAT OF RECORD?                
         BNE   CKSPDNO             NO                                           
*                                                                               
         MVC   KEY,SVKEY           RESTORE CURRENT KEY                          
*                                                                               
CKSPD50  DS    0H                                                               
*                                                                               
CKSPDOK  CR    RB,RB               EQUAL (SPC DESP ENTERED MATCHES REC)         
         B     *+6                                                              
CKSPDNO  LTR   RB,RB               NOT EQUAL (SPC DESP REC UNMATCHED)           
         XIT1                                                                   
*                                                                               
BYPROF   DS    CL16                BY PROFILE                                   
*                                                                               
         EJECT                                                                  
*                                                                               
HEDSPECS DS    0H                                                               
         SPROG 0,1                                                              
         PSPEC H1,58,C'SPACE DATA REPORT'                                       
         PSPEC H2,58,C'-----------------'                                       
         PSPEC H1,95,AGYNAME                                                    
         PSPEC H2,95,AGYADD                                                     
         PSPEC H4,95,RUN                                                        
         PSPEC H5,95,REPORT                                                     
         PSPEC H1,1,REQUESTOR                                                   
         PSPEC H8,1,C'PUB CODE        PUB NAME              CLIENT'             
         PSPEC H9,1,C'--------        --------              ------'             
         PSPEC H6,46,C'                MATERIALS CLOSING'                       
*                                                                               
         SPROG 0                                                                
         PSPEC H7,46,C'                    REL  REL        OVG'                 
         PSPEC H8,46,C' SPACE              MTH  DAY         %'                  
         PSPEC H9,46,C' -----              ---  ---        ---'                 
*                                                                               
         SPROG 1                                                                
         PSPEC H7,46,C'                    REL  REL        OVG'                 
         PSPEC H8,46,C' OUTSPACE 2         MTH  DAY         %'                  
         PSPEC H9,46,C' ----------         ---  ---        ---'                 
         DC    X'00'                                                            
PUBZNM   DS    CL20                                                             
MYPUB    DS    XL6                                                              
MYDSKADD DS    XL4                                                              
         EJECT                                                                  
         PRINT GEN                                                              
         GETEL R6,DATADISP,ELCODE                                               
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
       ++INCLUDE PRSFMFFD                                                       
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
         PRINT ON                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE PRSFMC5D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE PRSFMD5D                                                       
*                                                                               
LSVTAB   DS    XL(NLINS*LSVTABL)                                                
*                                                                               
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
       ++INCLUDE PRSFMWORKD                                                     
*                                                                               
         ORG   SYSSPARE                                                         
RELO     DS    F                                                                
ORIGKEY  DS    XL(L'KEY)                                                        
MYKEY    DS    XL(L'KEY)                                                        
ALLZE    DS    CL1                                                              
SCANBLK  DS    CL70                                                             
WRKELTAB DS    XL(ELTABL)          WORK TABLE ELEMENT                           
SAVMSGNO DS    XL1                 CURRENT MESSAGE NUMBER SAVEAREA              
SAVCURI  DS    XL1                 INDEX OF ERROR INTO FIELD                    
IPSTAT   DS    XL1                 CUMULATIVE INPUT STATISTICS                  
NEWKEY   DS    XL1                 C'Y' - BASIC KEY HAS CHENGED                 
ELTENT   DS    A                   A(ELEM TABLE ENTRY)                          
ELTLAST  DS    A                   A(LAST ENTRY)                                
         DS    0F                                                               
*                                                                               
         DS    0F                                                               
       ++INCLUDE DDBSRPRMD                                                      
         DS    0D                                                               
LUBLK    DS    XL(LUBLKL)          LINUP CONTROL BLOCK                          
         DS    0F                                                               
LINDSPS  DS    XL((NLINS+1)*2)                                                  
SVLSVTAB DS    XL(NLINS*LSVTABL)      HOLD COPY OF LINUP SAVE TABLE             
ELTMAX   EQU   (3900/PSPCELLN)-1      MAX NUMBER OF ELEMENTS IN TABLE           
*                                                                               
*                                                                               
* *******************                                                           
* ON-SCREEN LIST LINE                                                           
* *******************                                                           
*                                                                               
LISTD    DSECT                                                                  
LPUB     DS    CL15                PUBLICATION NUMBER ZONE/EDT                  
         DS    CL1                                                              
LPUBN    DS    CL20                PUB NAME                                     
         DS    CL2                                                              
LCLT     DS    CL3                 CLIENT                                       
         DS    CL5                                                              
LSPACE   DS    CL17                                                             
         DS    CL2                                                              
LMTH     DS    CL3                                                              
         DS    CL2                                                              
LDAY     DS    CL3                                                              
         DS    CL8                                                              
LOVG     DS    CL3                                                              
         EJECT                                                                  
       ++INCLUDE PSPACEREC                                                      
         EJECT                                                                  
PUBRECD  DSECT                                                                  
       ++INCLUDE PUBREC                                                         
       ++INCLUDE PUBNAMEL                                                       
PCLTRECD DSECT                                                                  
       ++INCLUDE PCLTREC                                                        
         EJECT                                                                  
*DDSPOOLD                                                                       
*DDCOMFACS                                                                      
*DDFLDIND                                                                       
*DDFLDHDR                                                                       
*PPSRCHPARM                                                                     
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDFLDIND                                                       
       ++INCLUDE DDFLDHDR                                                       
       ++INCLUDE PPSRCHPARM                                                     
         EJECT                                                                  
       ++INCLUDE DDLINUPD                                                       
         EJECT                                                                  
         PRINT ON                                                               
*                                                                               
LSVTABD  DSECT                     LINUP SAVE AREA DSECT                        
LSVKEY   DS    0XL(L'LSVSORT)                                                   
LSVSORT  DS    XL(L'PSPCDESC)                                                   
LSVKEYL  EQU   *-LSVTABD                                                        
LSVTABL  EQU   *-LSVTABD                                                        
         SPACE 2                                                                
ELTABD   DSECT                     DSECT FOR ELEM TABLE                         
ELTKEY   DS    0XL(L'ELTSORT)                                                   
ELTSORT  DS    XL(L'PSPCDESC)      SORT VALUE                                   
ELTKEYL  EQU   *-ELTABD            KEY LENGTH                                   
ELTCTL   DS    XL1                 CONTROL BYTE                                 
ELTDELQ  EQU   X'80'                 DELETE                                     
ELTADDQ  EQU   X'40'                 ADD                                        
ELTELEM  DS    CL(PSPCELLN)        ELEMENT                                      
ELTABL   EQU   *-ELTABD            ENTRY LENGTH                                 
*                                                                               
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'049PRSFM15   04/09/03'                                      
         END                                                                    
