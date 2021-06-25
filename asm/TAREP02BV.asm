*          DATA SET TAREP02BV  AT LEVEL 001 AS OF 04/07/14                      
*PHASE T70302B,*                                                                
*INCLUDE DLFLD                                                                  
         TITLE 'T70302 - EXPAND TATU ELEMENTS'                                  
T70302   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T70302                                                         
         L     RC,0(R1)            RC=A(CONTROLLER W/S)                         
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(SCREEN)                                 
         USING T703FFD,RA                                                       
         L     R9,ASUBSYSD         R9=A(SYSTEM W/S)                             
         USING SUBSYSD,R9                                                       
         L     R8,ASPOOLD          R8=A(SPOOL DSECT)                            
         USING SPOOLD,R8                                                        
         LA    R7,BUFF                                                          
         LA    R7,8(R7)                                                         
         USING MYD,R7                                                           
         EJECT                                                                  
***********************************************************************         
*        MODE CONTROLLED ROUTINES                                     *         
***********************************************************************         
                                                                                
         GOTO1 INITIAL,DMCB,0      INITIALIZE                                   
                                                                                
         CLI   MODE,PRINTREP                                                    
         BNE   YES                                                              
         BRAS  RE,PREP             PRINT REPORT                                 
                                                                                
YES      XR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
                                                                                
         GETEL R4,DATADISP,ELCODE                                               
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        PRINT REPORT                                                 *         
***********************************************************************         
                                                                                
PREP     NTR1  BASE=*,LABEL=*                                                   
         USING DLCBD,R5                                                         
         LA    R5,DLCB                                                          
         BAS   RE,INITDOWN                                                      
                                                                                
         MVC   SYSFIL,=CL8'CHKFIL'                                              
         MVC   SYSDIR,=CL8'CHKDIR'                                              
                                                                                
         USING TATUD,R2                                                         
         LA    R2,ELEMENT                                                       
                                                                                
         USING TLCKD,R3                                                         
         LA    R3,KEY                                                           
         XC    KEY,KEY                                                          
         MVI   TLCKCD,TLCKCDQ                                                   
         MVC   TLCKAGY,=C'10000 '                                               
         GOTO1 HIGH                                                             
         J     PREP20                                                           
PREP10   GOTO1 SEQ                                                              
PREP20   CLI   KEY,TLCKCDQ                                                      
         JNE   XIT                                                              
         DROP  R3                                                               
                                                                                
         GOTO1 GETREC                                                           
                                                                                
         USING OTUD,R4                                                          
         L     R4,AIO                                                           
         MVI   ELCODE,TATUELQ                                                   
         BRAS  RE,GETEL                                                         
         JNE   PREP10                                                           
                                                                                
         GOTO1 PTRACE,DMCB,=C'GET CHK'                                          
                                                                                
PREP30   XC    ELEMENT,ELEMENT                                                  
         MVC   ELEMENT(OTULNQ),OTUD                                             
         MVI   TATULEN,TATULNQ                                                  
                                                                                
         MVI   OTUEL,X'FF'                                                      
                                                                                
         GOTO1 ADDELEM                                                          
         DROP  R2,R4                                                            
                                                                                
         MVI   ELCODE,X'FF'                                                     
         GOTO1 REMELEM                                                          
                                                                                
         USING OTUD,R4                                                          
         L     R4,AIO                                                           
         MVI   ELCODE,TATUELQ                                                   
         BRAS  RE,GETEL                                                         
         J     *+8                                                              
PREP40   BRAS  RE,NEXTEL                                                        
         JNE   PREP50                                                           
         CLI   OTULEN,TATULNQ                                                   
         JNE   PREP30                                                           
         J     PREP40                                                           
         DROP  R4                                                               
                                                                                
PREP50   GOTO1 PUTREC                                                           
         GOTO1 PTRACE,DMCB,=C'PUT CHK'                                          
         J     PREP10                                                           
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TRACES RECORD GETS/PUTS/ADDS                         *         
*        ON ENTRY ... P1 = A(TRACE LABEL)                             *         
***********************************************************************         
                                                                                
PTRACE   NTR1                                                                   
         L     R2,0(R1)                                                         
                                                                                
         MVI   FORCEHED,C'N'                                                    
         GOTO1 TRACE,DMCB,AIO,0,0(R2),(0,7)                                     
         MVI   FORCEHED,C'Y'                                                    
         J     XIT                                                              
                                                                                
***********************************************************************         
*        INITIALIZE DOWNLOAD SETTINGS                                 *         
*        ON ENTRY ... R5=A(DOWNLOAD BLOCK)                            *         
***********************************************************************         
                                                                                
INITDOWN NTR1                                                                   
         XC    DLCBD(DLCBXLX),DLCBD                                             
         MVI   DLCBACT,DLCBINIT    INITIALIZE FOR DOWNLOAD                      
         LA    R1,PRTDOWN          A(HOOK ROUTINE FOR PRINTING)                 
         ST    R1,DLCBAPR                                                       
         LA    R1,P                A(PRINT LINE)                                
         MVC   P,SPACES                                                         
         ST    R1,DLCBAPL                                                       
         MVC   DLCBAED,EDITOR                                                   
         MVC   DLCXMAXL,=Y(L'P)    MAXIMUM LENGTH OF PRINT LINE                 
         MVI   DLCXDELC,C' '       DELIMITER                                    
         MVI   DLCXEOTC,C'"'       TEXT DELIMITER                               
         MVI   DLCXEOTA,C''''      TEXT DELIMITER ALTERNATE                     
         MVI   DLCXEOLC,X'5E'      SEMI-COLON FOR END OF LINE                   
         MVI   DLCXEORC,C':'       END OF REPORT                                
         OI    DLCBFLG1,DLCBFXTN                                                
         GOTO1 =V(DLFLD),DLCBD                                                  
         J     XIT                                                              
                                                                                
***********************************************************************         
*        USER SUPPLIED PRINT ROUTINE                                  *         
***********************************************************************         
                                                                                
PRTDOWN  NTR1                                                                   
         MVI   LINE,1              PREVENT PAGE BREAK                           
         GOTO1 SPOOL,DMCB,(R8)                                                  
         J     XIT                                                              
*                                                                               
***********************************************************************         
*        OUTPUT DOWNLOAD ENTRY                                        *         
*        ON ENTRY ... P1, B0    = TYPE TO PASS                        *         
*                         B1-B3 = ADDRESS OF DATA                     *         
*                     P2        = LENGTH                              *         
***********************************************************************         
                                                                                
OUTPDOWN NTR1                                                                   
         MVI   DLCBACT,DLCBPUT                                                  
         MVC   DLCBTYP,0(R1)                                                    
         OI    DLCBFLG1,DLCBFXFL                                                
                                                                                
         L     RF,0(R1)            RF=A(DOWNLOAD FIELD)                         
         L     RE,4(R1)            RE=L'DOWNLOAD FIELD                          
                                                                                
         LR    R1,RF                                                            
         AR    R1,RE                                                            
OPD10    SHI   R1,1                                                             
         CLI   0(R1),C' '                                                       
         JNE   OPD20                                                            
         BCT   RE,OPD10                                                         
         LHI   RE,1                                                             
OPD20    STC   RE,DLCBLEN                                                       
                                                                                
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         J     *+10                                                             
         MVC   DLCBFLX(0),0(RF)                                                 
         GOTO1 =V(DLFLD),DLCBD                                                  
         J     XIT                                                              
                                                                                
***********************************************************************         
*        FINISH LINE OF DOWNLOAD OUPUT                                *         
***********************************************************************         
                                                                                
EOLDOWN  NTR1                                                                   
         MVI   DLCBACT,DLCBEOL      END OF LINE                                 
         GOTO1 =V(DLFLD),DLCBD      (DON'T USE RF, BASR RE,RF BELOW)            
         J     XIT                                                              
                                                                                
***********************************************************************         
*        END DOWNLOAD                                                 *         
***********************************************************************         
                                                                                
ENDDOWN  NTR1                                                                   
         MVI   DLCBACT,DLCBEOR      END OF REPORT                               
         GOTO1 =V(DLFLD),DLCBD      LAST FOR REPORT                             
         J     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
AGYHEAD  DC    C'AGENCY'                                                        
INVHEAD  DC    C'INVOICE'                                                       
PIDHEAD  DC    C'PID'                                                           
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        WORKING STORAGE                                                        
***********************************************************************         
                                                                                
MYD      DSECT                                                                  
DLCB     DS    CL(DLCBXLX)         DOWNLOAD BLOCK                               
         EJECT                                                                  
       ++INCLUDE TAREPFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TAREPEAD                                                       
         EJECT                                                                  
*DDGENTWA  (MUST FOLLOW LAST SCREEN)                                            
*DDSPOOLD                                                                       
*DDSPLWORKD                                                                     
*DDBIGBOX                                                                       
*TAGENFILE                                                                      
*DDPERVALD                                                                      
*TASYSDSECT                                                                     
*TASYSEQUS                                                                      
*DDDLCB                                                                         
*DDGETRETD                                                                      
*DDCOMFACS                                                                      
*DDTWADCONS                                                                     
*TAREPWORKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE DDDLCB                                                         
       ++INCLUDE DDGETRETD                                                      
       ++INCLUDE DDCOMFACS                                                      
TWADCOND DSECT                                                                  
       ++INCLUDE DDTWADCONS                                                     
       ++INCLUDE TAREPWORKD                                                     
OTUD     DSECT                                                                  
OTUEL    DS    XL1                 ELEMENT CODE                                 
OTUELQ   EQU   X'07'                                                            
OTULEN   DS    XL1                 ELEMENT LENGTH                               
OTUUNIT  DS    CL3                 TAX UNIT CODE                                
OTUSTAT  DS    XL1                 STATUS                                       
OTUWAGE  DS    XL4                 WAGES                                        
OTUTNWA  DS    XL4                 TAXABLE REIMBURSEMENTS                       
OTUNNWA  DS    XL4                 NON-TAXABLE REIMBURSEMENTS                   
OTUSTRE  DS    XL4                 TAXABLE REIMBS (FOR NON-INDS)                
OTULNQ   EQU   *-OTUD                                                           
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001TAREP02BV 04/07/14'                                      
         END                                                                    
