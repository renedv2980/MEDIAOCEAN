*          DATA SET TAREP02R   AT LEVEL 030 AS OF 06/11/13                      
*PHASE T70302R,*                                                                
*INCLUDE DLFLD                                                                  
         TITLE 'T70302 - FIND BAD FTRACKS'                                      
T70302   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORKLNQ,T70302,R6                                                
         LR    RF,RC                                                            
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
                                                                                
***********************************************************************         
                                                                                
         USING TLCAD,R3                                                         
         LA    R3,KEY                                                           
         XC    KEY,KEY                                                          
         MVI   TLCACD,TLCACDQ                                                   
         MVC   TLCACOM,=X'001B3D2D'                                             
         GOTO1 HIGH                                                             
         J     PREP20                                                           
PREP10   GOTO1 SEQ                                                              
PREP20   CLI   KEY,TLCACDQ                                                      
         JNE   XIT                                                              
         DROP  R3                                                               
                                                                                
         GOTO1 GETREC                                                           
                                                                                
         USING TACAD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TACAELQ                                                   
         BRAS  RE,GETEL                                                         
         JNE   PREP10                                                           
         CLC   TACAYEAR,=C'13 '                                                 
         JNE   PREP10                                                           
         DROP  R4                                                               
                                                                                
         USING TACRD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TACRELQ                                                   
         BRAS  RE,GETEL                                                         
         J     *+8                                                              
PREP30   BRAS  RE,NEXTEL                                                        
         JNE   PREP10                                                           
                                                                                
         CLC   TACRSTRT,=X'B30401'                                              
         JL    PREP30                                                           
         OC    TACRAPPL,TACRAPPL                                                
         JZ    PREP30                                                           
                                                                                
         L     R1,TACRAPPL                                                      
         LHI   R0,20500                                                         
         BAS   RE,MULTR0                                                        
         L     RE,TACRBAL                                                       
         CR    RE,R1                                                            
         JL    PREP10                                                           
                                                                                
         L     R1,TACRAPPL                                                      
         LHI   R0,20700                                                         
         BAS   RE,MULTR0                                                        
         L     RE,TACRBAL                                                       
         CR    RE,R1                                                            
         JH    PREP10                                                           
                                                                                
         MVC   SVCRSTRT,TACRSTRT                                                
         MVC   SVCRAPPL,TACRAPPL                                                
         MVC   SVCRBAL,TACRBAL                                                  
         MVC   SVCRINV,TACRINV                                                  
         ST    R4,ATACREL                                                       
         DROP  R4                                                               
                                                                                
         MVC   SVCAKEY,KEY                                                      
                                                                                
         USING TLCOPD,R3                                                        
         XC    KEY,KEY                                                          
         MVI   TLCOPCD,TLCOCCDQ                                                 
         MVC   TLCOCCOM,SVCAKEY+TLCACOM-TLCAD                                   
         GOTO1 HIGH                                                             
         CLC   TLCOPKEY,KEYSAVE                                                 
         JE    *+6                                                              
         DC    H'00'                                                            
         DROP  R3                                                               
                                                                                
         GOTO1 GETREC                                                           
                                                                                
         USING TLCOD,R4                                                         
         L     R4,AIO                                                           
         GOTO1 OUTPDOWN,DMCB,(C'T',TLCOAGY),L'TLCOAGY                           
         DROP  R4                                                               
                                                                                
         USING TACOD,R4                                                         
         MVI   ELCODE,TACOELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         GOTO1 OUTPDOWN,DMCB,(C'T',TACOCID),L'TACOCID                           
         DROP  R4                                                               
                                                                                
         XR    R2,R2                                                            
                                                                                
         USING TLFTD,R3                                                         
         XC    KEY,KEY                                                          
         MVI   TLFTCD,TLFTCDQ                                                   
         MVC   TLFTSSN,SVCAKEY+TLCASSN-TLCAD                                    
         MVC   TLFTCOM,SVCAKEY+TLCACOM-TLCAD                                    
         MVC   TLFTCAST,SVCAKEY+TLCASEQ-TLCAD                                   
         MVC   TLFTSTRT,SVCRSTRT                                                
         XC    TLFTSTRT,=3X'FF'                                                 
         GOTO1 HIGH                                                             
         J     PREP50                                                           
PREP40   GOTO1 SEQ                                                              
PREP50   CLC   KEY(TLFTEND-TLFTD),KEYSAVE                                       
         JE    *+6                                                              
         DC    H'00'                                                            
         AHI   R2,1                                                             
         CLC   TLFTINV,SVCRINV                                                  
         JNE   PREP40                                                           
         DROP  R3                                                               
                                                                                
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
                                                                                
         USING TAGTD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TAGTELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         MVC   SVGTPAY,TAGTPAY                                                  
                                                                                
         USING TLCAD,R3                                                         
         MVC   KEY,SVCAKEY                                                      
         GOTO1 SSNPACK,DMCB,TLCASSN,TGPID                                       
         GOTO1 OUTPDOWN,DMCB,(C'T',TGPID),L'TGPID                               
         DROP  R3                                                               
                                                                                
         EDIT  SVCRAPPL,(10,PARAS),2,FLOAT=-                                    
         GOTO1 OUTPDOWN,DMCB,(C'T',PARAS),10                                    
                                                                                
         EDIT  SVCRBAL,(10,PARAS),2,FLOAT=-                                     
         GOTO1 OUTPDOWN,DMCB,(C'T',PARAS),10                                    
                                                                                
         EDIT  SVGTPAY,(10,PARAS),2,FLOAT=-                                     
         GOTO1 OUTPDOWN,DMCB,(C'T',PARAS),10                                    
                                                                                
         L     RE,SVGTPAY                                                       
         A     RE,SVCRAPPL                                                      
         ST    RE,CORBAL                                                        
         EDIT  CORBAL,(10,PARAS),2,FLOAT=-                                      
         GOTO1 OUTPDOWN,DMCB,(C'T',PARAS),10                                    
                                                                                
         MVC   TAGTBAL,CORBAL                                                   
         DROP  R4                                                               
                                                                                
         GOTO1 PUTREC                                                           
                                                                                
         CHI   R2,1                                                             
         JE    PREP60                                                           
         GOTO1 OUTPDOWN,DMCB,(C'T',SUBACT),L'SUBACT                             
PREP60   BAS   RE,EOLDOWN                                                       
                                                                                
         GOTO1 HIGH                                                             
                                                                                
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
                                                                                
         USING TACRD,R4                                                         
         L     R4,ATACREL                                                       
         MVC   TACRBAL,CORBAL                                                   
         OI    TACRSTAT,TACRRFIX                                                
         DROP  R4                                                               
                                                                                
         GOTO1 PUTREC                                                           
         J     PREP10                                                           
                                                                                
MULTR0   MR    R0,R0               USE R0                                       
         D     R0,=F'5000'                                                      
         LTR   R1,R1                                                            
         BM    *+8                                                              
         AHI   R1,1                                                             
         SRA   R1,1                                                             
         BR    RE                  RETURN ANSWER IN R1                          
                                                                                
         EJECT                                                                  
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
                                                                                
SUBACT   DC    C'SUBSEQUENT ACTIVITY'                                           
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        WORKING STORAGE                                                        
***********************************************************************         
                                                                                
MYD      DSECT                                                                  
                                                                                
SVCAKEY  DS    XL(L'KEY)                                                        
SVCRSTRT DS    XL3                                                              
SVCRINV  DS    XL6                                                              
SVCRAPPL DS    F                                                                
SVCRBAL  DS    F                                                                
SVGTPAY  DS    F                                                                
CORBAL   DS    F                                                                
ATACREL  DS    A                                                                
                                                                                
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
TWADCOND DSECT                                                                  
       ++INCLUDE DDTWADCONS                                                     
       ++INCLUDE TAREPWORKD                                                     
         PRINT ON                                                               
WORKLNQ  EQU   (L'TLCOCOM*2000)+1                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'030TAREP02R  06/11/13'                                      
         END                                                                    
