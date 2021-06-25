*          DATA SET RESUN02    AT LEVEL 004 AS OF 05/01/02                      
*PHASE T81402A,*                                                                
         TITLE 'T81402 - SONNET BOXID MAINTANCE'                                
**********************************************************************          
*                                                                    *          
*        RESUN02 (T81402) --- SONNET BOXID MAINTANCE                 *          
*                                                                    *          
* ------------------------------------------------------------------ *          
* UPDATE HISTORY:                                                    *          
*                                                                    *          
* CREATED  JUN28/96  (PXZ)                                           *          
* MODIFIED SEP02/98  (JRD)                                           *          
*                                                                    *          
**********************************************************************          
T81402   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**1402**                                                       
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         SPACE                                                                  
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VKEY                                                             
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VREC                                                             
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DKEY                                                             
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DREC                                                             
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BNE   MOD10                                                            
         CLI   1(RA),C'*'          IF DDS GIVE ACCESS TO ALL STATIONS           
         BE    LIST                                                             
         MVI   ERROR,INVACT        ELSE INVALID                                 
         LA    R2,SCBSTATH         STATION                                      
         B     TRAPERR                                                          
MOD10    DS    0H                                                               
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
*********************************************************************           
*              VALIDATE  KEY                                                    
*********************************************************************           
VKEY     DS    0H                                                               
         XC    SVKEY,SVKEY                                                      
         XC    SVSTAT,SVSTAT                                                    
                                                                                
K        USING RBOXKEY,SVKEY                                                    
         MVI   K.RBOXKTYP,RBOXKTYQ                                              
         MVC   K.RBOXKREP,AGENCY                                                
                                                                                
                                                                                
VK20     LA    R2,SCBSTATH         STATION                                      
         CLI   5(R2),0                                                          
         BE    VKX                                                              
         GOTO1 VALISTA,DMCB        RETURNS CALL LETTERS IN WORK                 
         MVC   K.RBOXKSTA,WORK                                                  
         MVC   SVSTAT,WORK                                                      
*                                                                               
VKX      MVC   KEY,SVKEY                                                        
         B     XIT                                                              
         DROP  K                                                                
         EJECT                                                                  
*******************************************************************             
* DISPLAY KEY                                                                   
*******************************************************************             
DKEY     L     R6,AIO                                                           
         USING RBOXREC,R6                                                       
*                                                                               
         LA    R2,SCBSTATH         STATION                                      
         MVC   8(5,R2),RBOXKSTA                                                 
         OI    6(R2),X'80'                                                      
*                                                                               
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
*******************************************************************             
*              VALIDATE RECORD                                                  
*******************************************************************             
VREC     EQU   *                                                                
         MVI   ERROR,MISSING                                                    
*                                                                               
* - DELETE BOX ID ELEMENTS                                                      
*                                                                               
         L     R6,AIO                                                           
         LA    R6,RBOX1STE-RBOXREC(R6)                                          
         MVI   ELCODE,RBOXIDEQ                                                  
VREC10   DS    0H                                                               
         BAS   RE,FIRSTEL                                                       
         BNE   VREC12                                                           
         GOTO1 VRECUP,DMCB,(C'R',AIO),(R6),0                                    
         B     VREC10                                                           
*                                                                               
VREC12   DS    0H                                                               
*                                                                               
* BUILD NEW BOXID ELEMENTS                                                      
*                                                                               
         LA    R2,SCBNAMEH         POINT TO INPUT LINE                          
S        USING SCBNAMEH,R2                                                      
VREC30   DS    0H                                                               
         XC    MYWORK,MYWORK                                                    
E        USING RBOXIDEL,MYWORK                                                  
         MVI   E.RBOXIDEC,RBOXIDEQ                                              
         MVI   E.RBOXIDLN,RBOXIDLQ                                              
                                                                                
         CLI   S.SCBNAMEH+5,0      ANY CODE?                                    
         BNE   VREC32              YES                                          
*                                                                               
***      CLI   S.SCBDESCH+5,0      ANY NAME?                                    
***      BNE   TRAPERR             YES - NOT ALLOWED                            
***      CLI   S.SCBRANKH+5,0      ANY RANK?                                    
***      BNE   TRAPERR             YES - NOT ALLOWED                            
         B     VREC50                                                           
*                                                                               
VREC32   DS    0H                                                               
         CLC   =C'NEW',S.SCBNAME   NEW IS A RESERVED CODE                       
         BE    TRAPERR                                                          
*                                                                               
         CLC   =C'XXX',S.SCBNAME   XXX IS A RESERVED CODE                       
         BE    TRAPERR                                                          
*                                                                               
         CLC   =C'ECD',S.SCBNAME   ECD IS A RESERVED CODE                       
**       BE    TRAPERR                                                          
*                                                                               
         CLI   S.SCBDESCH+5,0      ANY NAME?                                    
         BE    TRAPERR             NO - REQUIRED                                
*                                                                               
***      CLI   S.SCBRANKH+5,0      ANY RANK?                                    
***      BE    TRAPERR             NO - REQUIRED                                
*                                                                               
         LA    R0,SCBTAGH          NO DUPLICATES                                
C        USING SCBNAMEH,R1                                                      
         LA    R1,SCBNAM2H-SCBNAMEH(R2)                                         
VREC40   DS    0H                                                               
         CLC   C.SCBNAMEH,S.SCBNAMEH                                            
         BE    TRAPERR                                                          
         LA    R1,SCBNAM2H-SCBNAMEH(R1)                                         
         CR    R1,R0                                                            
         BL    VREC40                                                           
         DROP  C                                                                
*                                                                               
         OC    S.SCBNAME,SPACES                                                 
         MVC   E.RBOXIDCD,S.SCBNAME                                             
         OC    S.SCBDESC,SPACES                                                 
         MVC   E.RBOXIDNM,S.SCBDESC                                             
****     MVC   E.RBOXID**,S.SCBRANK                                             
*                                                                               
         L     R6,AIO              FIND ELEMENT HOME                            
         LA    R6,RBOX1STE-RBOXREC(R6)                                          
VREC42   DS    0H                                                               
         CLI   0(R6),0             EOR?                                         
         BE    VREC44                                                           
         CLI   0(R6),RBOXIDEQ      BOXID ELEMENT?                               
         BH    VREC44               NO - BUT PAST WHERE THEY BELONG             
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     VREC42                                                           
*                                                                               
VREC44   DS    0H                                                               
         GOTO1 VRECUP,DMCB,(C'R',AIO),MYWORK,(R6)                               
*                                                                               
VREC50   DS    0H                                                               
         LA    R0,SCBTAGH                                                       
         LA    R2,SCBNAM2H-SCBNAMEH(R2)                                         
         CR    R2,R0                                                            
         BL    VREC30                                                           
         DROP  E,S                                                              
*                                                                               
****     B     XIT                 ALWAYS REDISPLAY                             
         EJECT                                                                  
*******************************************************************             
* DISPLAY RECORD                                                                
*******************************************************************             
DREC     EQU   *                                                                
         LA    R2,SCBNAMEH         CLEAR SCREEN                                 
         BAS   RE,CLRSCRN                                                       
*                                                                               
         LA    R2,SCBNAMEH         POINT TO INPUT LINE                          
S        USING SCBNAMEH,R2                                                      
*                                                                               
         MVI   ELCODE,RBOXIDEQ                                                  
         USING RBOXIDEL,R6                                                      
         L     R6,AIO                                                           
         LA    R6,RBOX1STE-RBOXREC(R6)                                          
         BAS   RE,FIRSTEL                                                       
         BNE   DRECX                                                            
DREC10   DS    0H                                                               
         MVC   S.SCBNAME,RBOXIDCD                                               
         OI    S.SCBNAMEH+6,X'80'                                               
         MVC   S.SCBDESC,RBOXIDNM                                               
         OI    S.SCBDESCH+6,X'80'                                               
****     MVC   S.SCBRANK,RBOXID**                                               
****     OI    S.SCBRANKH+6,X'80'                                               
*                                                                               
         LA    R0,SCBTAGH                                                       
         LA    R2,SCBNAM2H-SCBNAMEH(R2)                                         
         CR    R2,R0                                                            
         BNL   DRECX                                                            
*                                                                               
         BAS   RE,NEXTEL                                                        
         BE    DREC10                                                           
         DROP  R6                                                               
*                                                                               
DRECX    B     XIT                                                              
         EJECT                                                                  
*******************************************************************             
*              LIST RECORDS                                                     
*******************************************************************             
LIST     EQU   *                                                                
         OC    KEY(27),KEY                                                      
         BNZ   LR10                                                             
                                                                                
         XC    KEY,KEY                                                          
K        USING RBOXKEY,KEY                                                      
         MVI   K.RBOXKTYP,RBOXKTYQ                                              
         MVC   K.RBOXKREP,AGENCY                                                
         CLI   SVSTAT,X'40'                                                     
         BNH   *+10                                                             
         MVC   K.RBOXKSTA,SVSTAT                                                
*                                                                               
         MVC   SVKEY,KEY                                                        
*                                                                               
LR10     DS    0H                                                               
         GOTO1 HIGH                                                             
                                                                                
LR20     DS    0H                                                               
         CLC   KEY((RBOXKREP-RBOXKEY)+2),SVKEY                                  
         BNE   LRX                                                              
         CLI   SVSTAT,X'40'                                                     
         BNH   *+14                                                             
         CLC   SVSTAT,K.RBOXKSTA                                                
         BH    LRSEQ                                                            
*                                                                               
         MVC   LISTAR,SPACES                                                    
         LA    R2,LISTAR                                                        
*                                                                               
         MVC   0(5,R2),K.RBOXKSTA   STATION                                     
         LA    R2,12(R2)                                                        
         DROP  K                                                                
*                                                                               
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
*                                                                               
         MVI   ELCODE,RBOXIDEQ                                                  
         USING RBOXIDEL,R6                                                      
         LA    R6,RBOX1STE-RBOXREC(R6)                                          
         BAS   RE,FIRSTEL                                                       
         BNE   LR50                                                             
*                                                                               
LR30     DS    0H                                                               
         MVC   0(L'RBOXIDCD,R2),RBOXIDCD                                        
         MVI   L'RBOXIDCD(R2),C','                                              
         LA    R2,L'RBOXIDCD+2(R2)                                              
*                                                                               
         BAS   RE,NEXTEL                                                        
         BE    LR30                                                             
         DROP  R6                                                               
*                                                                               
LR50     DS    0H                                                               
         GOTO1 LISTMON                                                          
*                                                                               
LRSEQ    GOTO1 SEQ                 GET NEXT RECORD                              
         B     LR20                                                             
                                                                                
LRX      B     XIT                                                              
         EJECT                                                                  
*******************************************************************             
*        ROUTINE TO CLEAR THE SCREEN                                            
*        FROM FIELD AT R2                                                       
*******************************************************************             
CLRSCRN  NTR1                                                                   
         SR    RE,RE                                                            
*                                                                               
CS010    IC    RE,0(R2)                                                         
         SH    RE,=H'9'                                                         
         EX    RE,CSCLC                                                         
         BE    CS020                                                            
         EX    RE,CSOC                                                          
         BZ    CS020                                                            
         EX    RE,CSXC                                                          
         OI    6(R2),X'80'                                                      
*                                                                               
CS020    LA    R2,9(RE,R2)                                                      
*                                                                               
CS021    CLI   0(R2),9                                                          
         BNH   CSX                                                              
         TM    1(R2),X'20'         PROTECTED?                                   
         BNO   CS010                                                            
         ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         B     CS021                                                            
CSX      B     XIT                                                              
*                                                                               
CSCLC    CLC   8(0,R2),SPACES                                                   
CSOC     OC    8(0,R2),8(R2)                                                    
CSXC     XC    8(0,R2),8(R2)                                                    
         EJECT                                                                  
*******************************************************************             
         GETEL R6,=Y(RBOX1STE-RBOXREC),ELCODE                                   
         SPACE                                                                  
TRAPERR  DS    0H                                                               
         GOTO1 ERREX                                                            
         SPACE 3                                                                
RELO     DS    A                                                                
         LTORG                                                                  
         EJECT                                                                  
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* RESUNFFD                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE RESUNFFD                                                       
         PRINT ON                                                               
         ORG   CONTAGH                                                          
* RESUNF3D                                                                      
       ++INCLUDE RESUNF3D                                                       
         ORG   CONTAGH                                                          
* RESUNF4D                                                                      
       ++INCLUDE RESUNF4D                                                       
         EJECT                                                                  
* REGENBOX                                                                      
BOXRECS  DSECT                                                                  
       ++INCLUDE REGENBOX                                                       
         EJECT                                                                  
* RESFMWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE RESUNWRK                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004RESUN02   05/01/02'                                      
         END                                                                    
