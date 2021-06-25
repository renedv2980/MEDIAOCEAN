*          DATA SET NEMEDCA    AT LEVEL 050 AS OF 01/24/11                      
*PHASE T31ECAA,*                                                                
*INCLUDE CLUNPK                                                                 
         TITLE 'T31ECA-EDIT NETWORK CLOSEOUT REPORT'                            
T31ECA   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**NECA**,RR=R2                                                 
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING T31EFFD,RA                                                       
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
         L     R7,ANETWS4          R7-ANETWS4/WORKING STORAGE                   
         USING WORKD,R7                                                         
         ST    R2,RELO                                                          
         L     R1,ANETWS4             ANETWS4+2000=CLIST                        
         LA    R1,2000(R1)                                                      
         ST    R1,ACLISTSV                                                      
*                                                                               
         EJECT                                                                  
*HIPO******************************************************************         
*  TITLE: NEMEDCA (T31ECA) NETWORK CLOSEOUT REPORT EDIT MODULE       *          
*                                                                     *         
*  COMMENTS: NETWORK CLOSEOUT REPORT                                 *          
*                                                                     *         
*  CALLS TO: NETIO                                                              
*                                                                     *         
*  GLOBAL: R7-MYWORKD (ANETWS2+500)                                   *         
*                                                                     *         
***********************                                               *         
*  LOGIC: READS UNIT RECS. SETS UP ONE REQUEST REC(TAPED1)            *         
*                                                                     *         
*ENDHIPO***************************************************************         
         SPACE 3                                                                
         MVI   FRSTLAST,C'Y'                                                    
         CLI   MODE,VALKEY                                                      
         BE    VK                                                               
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
*************************************                                           
* VALIDATE REQUEST SCREEN DATA                                                  
*                                                                               
*                                                                               
VK       DS    0H                                                               
*                                                                               
*                                                                               
VK01     DS    0H                                                               
         MVI   FTERMFLG,0          SET REQUIRED FLAG                            
         MVI   ERROR,INVALID                                                    
         LA    R2,SPLCLIH              CLIENT                                   
         CLC   =C'***',SPLCLI      DDS 'ALL' REQUEST?                           
         BNE   VK02                                                             
         CLI   OFFLINE,C'Y'        AND OFFLINE?                                 
         BNE   TRAPERR                                                          
         XC    TWAACCS,TWAACCS     YES/CLEAR LIMIT ACCESS                       
         MVC   SPLCLI(3),=C'ALL'                                                
VK02     EQU   *                                                                
         MVI   ERROR,0             CLEAR ERROR                                  
         NETGO NVCLIALL,DMCB,SPLCLIN                                            
         OI    SPLCLINH+6,X'80'                                                 
         L     R3,NBAIO                                                         
         USING CLTHDR,R3                                                        
         L     RF,ACLISTSV                                                      
         MOVE  ((RF),880),CLIST                                                 
         MVC   OFFCDE,COFFICE                                                   
         DROP  R3                                                               
*                                                                               
         MVI   FTERMFLG,1                                                       
         LA    R2,SPLPROH               PRODUCT                                 
         NETGO NVGETFLD,DMCB         *** MUST BE POL FOR NOW ***                
         BZ    VK8                                                              
         CLC   8(3,R2),=C'POL'                                                  
         BE    VK10                                                             
         MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
         NETGO NVPRD,DMCB,SPLPRON                                               
         OI    SPLPRONH+6,X'80'                                                 
         B     VK10                                                             
VK8      MVC   8(3,R2),=C'POL'                                                  
         OI    SPLPROH+6,X'80'                                                  
*                                                                               
VK10     LA    R2,SPLESTH                ESTIMATE                               
         MVI   FTERMFLG,0                                                       
         NETGO NVESTRNG,DMCB,SPLESTN                                            
         OI    SPLESTNH+6,X'80'                                                 
*                                                                               
         MVI   FTERMFLG,1                                                       
         LA    R2,SPLNETH                                                       
         NETGO NVGETFLD,DMCB                                                    
         BZ    VK15                                                             
         NETGO NVNETALL,DMCB                                                    
*                                                                               
VK15     LA    R2,SPLENDDH                                                      
         MVI   FTERMFLG,0                                                       
         NETGO NVGETFLD,DMCB                                                    
         BZ    VK17                                                             
         GOTO1 DATVAL,DMCB,(0,SPLENDD),REQSTEND                                 
         OC    DMCB(4),DMCB                                                     
         BNZ   VK17                                                             
         MVI   ERROR,INVDATE                                                    
         B     TRAPERR                                                          
*                                                                               
VK17     DS    0H                                                               
         LA    R2,SPLKILH                                                       
         MVI   KILLSW,0                                                         
         CLI   8(R2),C'N'                                                       
         BE    VK20                                                             
         MVI   KILLSW,C'Y'                                                      
         CLI   8(R2),C'Y'                                                       
         BE    VK20                                                             
         MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
*                                                                               
VK20     LA    R2,SPLCLSH                                                       
         MVI   DELETESW,0                                                       
                                                                                
         CLI   8(R2),C'N'                                                       
         BE    VK30                                                             
                                                                                
         MVI   DELETESW,C'S'                                                    
         CLI   8(R2),C'S'                                                       
         BE    VK30                                                             
                                                                                
         CLI   OFFLINE,C'Y'                                                     
         BNE   VK29                                                             
         CLI   8(R2),C'Y'                                                       
         BNE   VK29                                                             
         MVI   DELETESW,C'Y'                                                    
         B     VK30                                                             
VK29     MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
*                                                                               
VK30     DS    0H                                                               
         MVI   FTERMFLG,1          NOT REQUIRED                                 
         MVI   PUFSW,C'Y'                                                       
         LA    R2,SPLPUFH         PU PROFILE ?                                  
         NETGO NVGETFLD,DMCB                                                    
         BZ    VK40                                                             
         CLI   8(R2),C'Y'                                                       
         BE    VK40                                                             
         MVI   PUFSW,C'N'          DON'T READ PROFILE                           
         CLI   8(R2),C'N'                                                       
         BE    VK40                                                             
         CLI   8(R2),C'O'          ONLY READ PU PROFILE CLIENTS                 
         BNE   VK35                                                             
         MVI   PUFSW,C'O'                                                       
         CLI   DELETESW,C'Y'       MUST BE DON'T DELETE                         
         BNE   VK40                                                             
         LA    R2,SPLCLSH          ELSE INVLID                                  
VK35     MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
*                                                                               
VK40     DS    0H                                                               
*                                                                               
VEXIT    CLI   OFFLINE,C'Y'        OFFLINE?                                     
         BE    EXIT                                                             
         CLI   CONOUTH+5,0         CANNOTCHANGEOUTPUTTYPE                       
         BE    EXIT                                                             
         LA    R2,CONOUTH                                                       
         MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
         SPACE 2                                                                
TRAPERR  GOTO1 ERREX                                                            
         EJECT                                                                  
WORKD    DSECT                     MYWORK AREA  ANETWS4                         
*                         THESE ARE PASSED TO PRINT MODULE                      
ESTKEYSV DS    CL13     *          SAVE CURRENT ESTIMATE KEY                    
ESTBSV   DS    CL1      *                                                       
KILLSW   DS    CL1      *                                                       
PUFSW    DS    CL1      *                                                       
DELETESW DS    CL1      *                                                       
OFFCDE   DS    CL1      *                                                       
REQSTEND DS    CL6      *                                                       
RELO     DS    F        *                                                       
ACLISTSV DS    F        *                                                       
*                                                                               
ASUMRECD DS    F                                                                
ADRSUMRC DS    F                                                                
ESTCNTR  DS    F                   COUNTER FOR ESTIMATE RECORDS                 
UNTCNTR  DS    F                   COUNTER FOR UNIT RECORDS                     
UNTCNTR2 DS    F                   COUNTER FOR UNIT RECORDS                     
BILLCNTR DS    F                   COUNTER FOR BILLING RECORDS                  
BILLCNT2 DS    F                   COUNTER FOR BILLING RECORDS                  
STABCNTR DS    F                   COUNTER FOR SPGENSTAB RECORDS                
STABCNT2 DS    F                   COUNTER FOR SPGENSTAB RECORDS                
SUMCNTR  DS    F                                                                
*                                                                               
ERRORSW  DS    CL1                                                              
FRST     DS    CL1                                                              
BOXSET   DS    CL1                                                              
PAIDSW   DS    CL1                                                              
BILLSW   DS    CL1                                                              
BPRDSV   DS    CL1                                                              
PRDSV    DS    CL3                                                              
ESTBYRS  DS    CL1                 ESTIMATE START YEAR                          
ESTBMNS  DS    CL1                 ESTIMATE START MONTH                         
ESTBYRE  DS    CL1                 ESTIMATE END YEAR                            
ESTBMNE  DS    CL1                 ESTIMATE END MONTH                           
BYEAR    DS    CL1                 REQUEST END YEAR                             
BMNTH    DS    CL1                 REQUEST END MONTH                            
*                                                                               
*                   TOTALS TAKEN FIRST TIME RECS ARE READ                       
*                     UNITS                                                     
UNTORD   DS    PL8       ORDERED UNIT TOTAL                                     
UNTBILL  DS    PL8       BILLED UNIT TOTAL                                      
UNTPAID  DS    PL8       PAID UNIT TOTAL                                        
*                     BILL RECS                                                 
BLRECAMT DS    PL8       BILL REC AMOUNT TOTAL                                  
*                     SPGENSTAB                                                 
STABAMT  DS    PL8           SPGENSTAB TOTAL                                    
*                                                                               
*                    TOTALS TAKEN AS RECS ARE DELETED/PRINTED                   
UNTORD2  DS    PL8       ORDERED UNIT TOTAL                                     
UNTBILL2 DS    PL8       BILLED UNIT TOTAL                                      
UNTPAID2 DS    PL8       PAID UNIT TOTAL                                        
*                     BILL RECS                                                 
BLRECAM2 DS    PL8       BILL REC AMOUNT TOTAL                                  
*                     SPGENSTAB                                                 
STABAMT2 DS    PL8           SPGENSTAB TOTAL                                    
*                                                                               
*                                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
BPLINED  DSECT               BILL REC DSECT FOR PRINTING                        
         DS    CL1                                                              
BPCLT    DS    CL3                                                              
         DS    CL1                                                              
BPPRD    DS    CL3                                                              
         DS    CL1                                                              
BPEST    DS    CL3                                                              
         DS    CL1                                                              
BPYRSRV  DS    CL2                                                              
         DS    CL1                                                              
BPMNSRV  DS    CL2                                                              
         DS    CL1                                                              
BPMNYR   DS    CL4                                                              
         DS    CL1                                                              
BPNUM    DS    CL6                                                              
         DS    CL1                                                              
BPINVO   DS    CL6                                                              
         DS    CL1                                                              
BPDATE   DS    CL6                                                              
         DS    CL1                                                              
BPAMT    DS    CL11                                                             
         EJECT                                                                  
*                                                                               
UPLINED  DSECT               UNIT REC DSECT FOR PRINTING                        
         DS    CL1                                                              
UPCLT    DS    CL3                                                              
         DS    CL1                                                              
UPEST    DS    CL3                                                              
         DS    CL1                                                              
UPNET    DS    CL4                                                              
         DS    CL1                                                              
UPPRD    DS    CL3                                                              
         DS    CL1                                                              
UPPAK    DS    CL3                                                              
         DS    CL1                                                              
UPPROG   DS    CL6                                                              
         DS    CL1                                                              
UPDATE   DS    CL6                                                              
         DS    CL1                                                              
UPSUBLN  DS    CL6                                                              
         DS    CL1                                                              
UPACTUAL DS    CL10                ORDERED                                      
         DS    CL1                                                              
UPBILT   DS    CL10                BILLED TIME                                  
         DS    CL1                                                              
UPBILI   DS    CL10                BILLED INTEGRATION                           
         DS    CL1                                                              
UPPAIDT  DS    CL10                PAID TIME                                    
         DS    CL1                                                              
UPPAIDI  DS    CL10                PAID INTEGRATION                             
         EJECT                                                                  
AGYTOTD  DSECT                                                                  
         DS    PL8                                                              
         DS    PL8                                                              
         DS    PL8                                                              
         DS    PL8                                                              
         DS    PL8                                                              
         DS    PL8                                                              
         DS    PL8                                                              
         DS    PL8                                                              
         DS    PL8                                                              
MULTREQ  DS    CL1                                                              
*                                                                               
ESVRECD  DSECT       *** NOT USED*** DESECT FOR SAVED ESTIMATE INFO             
ESVCLT   DS    CL3                                                              
ESVPRD   DS    CL3                                                              
ESVEST   DS    CL3                                                              
ESVDATE  DS    CL13                                                             
ESVLNE   EQU   *-ESVCLT                                                         
         EJECT                                                                  
         SPACE                                                                  
       ++INCLUDE NETINCLS                                                       
       ++INCLUDE NENETGOALD                                                     
         EJECT                                                                  
       ++INCLUDE NEMEDFFD                                                       
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE NEMEDDAD                                                       
       ++INCLUDE DDBIGBOX                                                       
         EJECT                                                                  
*                                                                               
         EJECT                                                                  
PRDHD    DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
       ++INCLUDE SPGENEST                                                       
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
BILLRECD DSECT                                                                  
       ++INCLUDE SPGENBILL                                                      
       ++INCLUDE SPGENSTAB                                                      
         EJECT                                                                  
       ++INCLUDE NEGENUNIT                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'050NEMEDCA   01/24/11'                                      
         END                                                                    
