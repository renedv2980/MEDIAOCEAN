*          DATA SET TAREP02Q   AT LEVEL 026 AS OF 06/12/13                      
*PHASE T70302Q,*                                                                
*INCLUDE DLFLD                                                                  
         TITLE 'T70302 - FIND ALL CHECKS THAT SHOULD BE CONTRACT 13'            
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
         ST    RF,ACOMTAB                                                       
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
                                                                                
         USING TLDRD,R3                                                         
         LA    R3,KEY                                                           
                                                                                
***********************************************************************         
                                                                                
         LA    R2,DATAB                                                         
                                                                                
PREP10   XC    KEY,KEY                                                          
         MVC   TLDRDA,0(R2)                                                     
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
         CLI   DMCB+8,0                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
         DROP  R3                                                               
                                                                                
         L     R4,AIO                                                           
         MVI   ELCODE,TARPELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
                                                                                
         USING TLCKD,R4                                                         
         L     R4,AIO                                                           
         GOTO1 OUTPDOWN,DMCB,(C'T',TLCKAGY),L'TLCKAGY                           
         GOTO1 TINVCON,DMCB,TLCKINV,TGINV,DATCON                                
         GOTO1 OUTPDOWN,DMCB,(C'T',TGINV),L'TLCKINV                             
         GOTO1 SSNPACK,DMCB,TLCKSSN,TGPID                                       
         GOTO1 OUTPDOWN,DMCB,(C'T',TGPID),L'TGPID                               
         BAS   RE,EOLDOWN                                                       
         DROP  R4                                                               
                                                                                
         MVI   ELCODE,TACDELQ                                                   
         GOTO1 REMELEM                                                          
                                                                                
         USING TACDD,R4                                                         
         LA    R4,ELEMENT                                                       
         XC    ELEMENT,ELEMENT                                                  
         MVI   TACDEL,TACDELQ                                                   
         MVI   TACDLEN,TACDLNQ                                                  
         GOTO1 ADDELEM                                                          
         DROP  R4                                                               
                                                                                
         GOTO1 PUTREC                                                           
                                                                                
         CLI   L'DATAB(R2),X'FF'                                                
         JE    XIT                                                              
         LA    R2,L'DATAB(R2)                                                   
         J     PREP10                                                           
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
                                                                                
DATAB    DC    0XL4                                                             
         DC    X'3E68D303'                                                      
         DC    X'3E68D302'                                                      
         DC    X'3E68D301'                                                      
         DC    X'3E68D211'                                                      
         DC    X'3E68D210'                                                      
         DC    X'3E68D20F'                                                      
         DC    X'3E68D20E'                                                      
         DC    X'3E68D20D'                                                      
         DC    X'3E68D20C'                                                      
         DC    X'3E68D20B'                                                      
         DC    X'3E68D20A'                                                      
         DC    X'3E68D209'                                                      
         DC    X'3E68D208'                                                      
         DC    X'3E68D207'                                                      
         DC    X'3E688103'                                                      
         DC    X'3E690201'                                                      
         DC    X'3E690202'                                                      
         DC    X'3E690115'                                                      
         DC    X'3E68E30E'                                                      
         DC    X'3E68C30A'                                                      
         DC    X'3E68C307'                                                      
         DC    X'FF'                                                            
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        WORKING STORAGE                                                        
***********************************************************************         
                                                                                
MYD      DSECT                                                                  
ACOMTAB  DS    A                                                                
                                                                                
SVINPKEY DS    XL(L'KEY)                                                        
SVCAKEY  DS    XL(L'KEY)                                                        
                                                                                
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
**PAN#1  DC    CL21'026TAREP02Q  06/12/13'                                      
         END                                                                    
