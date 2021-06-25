*          DATA SET TAREP62    AT LEVEL 002 AS OF 09/23/10                      
*PHASE T70362A,*                                                                
*INCLUDE DLFLD                                                                  
         TITLE 'T70362 - CLIENT GROUP UPDATE DOWNLOAD'                          
T70362   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 SVPTRLNQ+UPPTRLNQ,T70362                                         
         LR    RE,RC                                                            
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
         ST    RE,ASVPTRS          SAVE A(SAVED POINTER BLOCK)                  
         AHI   RE,SVPTRLNQ                                                      
         ST    RE,AUPPTRS          SAVE A(UPDATED POINTER BLOCK)                
         EJECT                                                                  
***********************************************************************         
*        MODE CONTROLLED ROUTINES                                     *         
***********************************************************************         
                                                                                
         GOTO1 INITIAL,DMCB,0      INITIALIZE                                   
                                                                                
         CLI   MODE,VALKEY                                                      
         BNE   *+8                                                              
         BRAS  RE,VK               VALIDATE KEY                                 
                                                                                
         CLI   MODE,PRINTREP                                                    
         BNE   *+8                                                              
         BRAS  RE,PREP             PRINT REPORT                                 
                                                                                
YES      XR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
                                                                                
         GETEL R4,DATADISP,ELCODE                                               
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        VALIDATE KEY                                                 *         
***********************************************************************         
                                                                                
VK       NTR1  BASE=*,LABEL=*                                                   
         XC    MYD(MYDLNQ),MYD     CLEAR WORKING STORAGE                        
         XC    TGCLG,TGCLG                                                      
                                                                                
         GOTO1 RECVAL,DMCB,TLAYCDQ,(8,CGUAGYH),CGUAGYNH                         
                                                                                
         GOTO1 RECVAL,DMCB,TLCLCDQ,(8,CGUCLIH),CGUCLINH                         
                                                                                
         USING TACID,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TACIELQ                                                   
         BRAS  RE,GETEL                                                         
         JNE   XIT                                                              
         MVC   TGCLG,TACICLG                                                    
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        PRINT REPORT                                                 *         
***********************************************************************         
                                                                                
PREP     NTR1  BASE=*,LABEL=*                                                   
         OC    TGCLG,TGCLG                                                      
         JZ    XIT                                                              
                                                                                
         USING DLCBD,R5                                                         
         LA    R5,DLCB                                                          
         BAS   RE,INITDOWN         INITIALIZE DOWNLOAD                          
                                                                                
         GOTO1 OUTPDOWN,DMCB,(C'T',AGYHEAD),L'AGYHEAD                           
         GOTO1 (RF),(R1),(C'T',CIDHEAD),L'CIDHEAD                               
         BAS   RE,EOLDOWN                                                       
                                                                                
         USING TLCOD,R3                                                         
         LA    R3,KEY                                                           
         XC    TLCOKEY,TLCOKEY     READ ALL OF THIS CLIENT'S                    
         MVI   TLCOCD,TLCOCDQ      COMMERCIALS                                  
         MVC   TLCOAGY,TGAGY                                                    
         MVC   TLCOCLI,TGCLI                                                    
         GOTO1 HIGH                                                             
         J     PREP20                                                           
PREP10   GOTO1 SEQ                                                              
PREP20   CLC   KEY(TLCOPRD-TLCOD),KEYSAVE                                       
         JNE   PREPX                                                            
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
                                                                                
         MVC   SVKEY,KEY                                                        
         GOTO1 ASAVPTRS,DMCB,ASVPTRS                                            
                                                                                
         USING TACOD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TACOELQ      GET COMMERCIAL DETAILS ELEMENT               
         BRAS  RE,GETEL                                                         
         JNE   PREP10                                                           
         CLC   TACOCLG,TGCLG                                                    
         JE    PREP10                                                           
                                                                                
         GOTO1 OUTPDOWN,DMCB,(C'T',TLCOAGY),L'TLCOAGY                           
         GOTO1 (RF),(R1),(C'T',TACOCID),L'TACOCID                               
         BAS   RE,EOLDOWN                                                       
                                                                                
         MVC   TACOCLG,TGCLG       UPDATE CLIENT GROUP                          
         GOTO1 PUTREC                                                           
         DROP  R4                                                               
                                                                                
         GOTO1 AADDPTRS,DMCB,(8,ASVPTRS),AUPPTRS                                
         GOTO1 DATAMGR,DMCB,=C'COMMIT'                                          
                                                                                
         MVC   KEY,SVKEY                                                        
         GOTO1 HIGH                                                             
         J     PREP10                                                           
         DROP  R3                                                               
                                                                                
PREPX    BAS   RE,ENDDOWN          CLOSE DOWNLOAD                               
         J     XIT                                                              
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
*                                                                               
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
*                                                                               
         L     RF,0(R1)            RF=A(DOWNLOAD FIELD)                         
         L     RE,4(R1)            RE=L'DOWNLOAD FIELD                          
*                                                                               
         LR    R1,RF                                                            
         AR    R1,RE                                                            
OPD10    SHI   R1,1                                                             
         CLI   0(R1),C' '                                                       
         JNE   OPD20                                                            
         BCT   RE,OPD10                                                         
         LHI   RE,1                                                             
OPD20    STC   RE,DLCBLEN                                                       
*                                                                               
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         J     *+10                                                             
         MVC   DLCBFLX(0),0(RF)                                                 
         GOTO1 =V(DLFLD),DLCBD                                                  
         J     XIT                                                              
*                                                                               
***********************************************************************         
*        FINISH LINE OF DOWNLOAD OUPUT                                *         
***********************************************************************         
                                                                                
EOLDOWN  NTR1                                                                   
         MVI   DLCBACT,DLCBEOL      END OF LINE                                 
         GOTO1 =V(DLFLD),DLCBD      (DON'T USE RF, BASR RE,RF BELOW)            
         J     XIT                                                              
*                                                                               
***********************************************************************         
*        END DOWNLOAD                                                 *         
***********************************************************************         
                                                                                
ENDDOWN  NTR1                                                                   
         MVI   DLCBACT,DLCBEOR      END OF REPORT                               
         GOTO1 =V(DLFLD),DLCBD      LAST FOR REPORT                             
         J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
AGYHEAD  DC    C'AGENCY'                                                        
CIDHEAD  DC    C'COMMERCIAL ID'                                                 
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        WORKING STORAGE                                                        
***********************************************************************         
                                                                                
MYD      DSECT                                                                  
SVKEY    DS    XL(L'KEY)           SAVED KEY                                    
MYDLNQ   EQU   *-MYD                                                            
*                                                                               
DLCB     DS    CL(DLCBXLX)         DOWNLOAD BLOCK                               
*                                                                               
ASVPTRS  DS    A                   A(SAVED PASSIVE POINTERS)                    
AUPPTRS  DS    A                   A(UPDATED PASSIVE POINTERS)                  
         EJECT                                                                  
       ++INCLUDE TAREPFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TAREPF5D                                                       
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
         EJECT                                                                  
***********************************************************************         
*        DSECT TO COVER NMOD GRABBED STORAGE                          *         
***********************************************************************         
                                                                                
SVPTRLNQ EQU   (520*L'TLDRREC)+1                                                
UPPTRLNQ EQU   (520*L'TLDRREC)+1                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002TAREP62   09/23/10'                                      
         END                                                                    
