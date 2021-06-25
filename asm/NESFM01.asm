*          DATA SET NESFM01    AT LEVEL 145 AS OF 05/12/20                      
*PHASE T31C01B,*                                                                
***********************************************************************         
*                                                                               
*  TITLE: T31C01 - MAINTENANCE/LIST OF CLIENT RECORDS                           
*                                                                               
*  COMMENTS: MAINTAINS CLIENT RECORDS                                           
*                                                                               
*  CALLED FROM: NET SFM CONTROLLER (T31C40), WHICH CALLS                        
*               DDGENCON (T00A30) WHICH CALLS THIS.                             
*                                                                               
*  CALLS TO:    DATAMGR                                                         
*                                                                               
*  INPUTS: SCREENS NESFMC1 (T31CC1) -- MAINTENANCE                              
*                  NESFMC2 (T31CC2) -- LIST                                     
*                                                                               
*  OUTPUTS: UPDATED OR NEW CLIENTS                                              
*                                                                               
*  LOCALS: REGISTER USAGE                                                       
*          R0 - WORK                                                            
*          R1 - WORK                                                            
*          R2 - POINTER TO SCREEN FLDH, ERREX WILL POSITION CURSOR              
*          R3 - WORK                                                            
*          R4 - WORK                                                            
*          R5 - WORK                                                            
*          R6 - USED FOR GETEL ELEMENT DSECT POINTER                            
*          R7 - USED FOR SECOND BASE                                            
*          R8 - SPOOLD                                                          
*          R9 - SYSD                                                            
*          RA - TWA                                                             
*          RB - FIRST BASE                                                      
*          RC - GEND                                                            
*          RD - SYSTEM                                                          
*          RE - SYSTEM                                                          
*          RF - SYSTEM                                                          
*                                                                               
*   HIST:  (PLEASE INSERT BRIEF DESCRIPTION OF UPDATES)                         
*                                                                               
*   JAN20/92   (SKU)  --- INSERT SECURITY ROUTINES FOR CLIENT AND               
*                         PRODUCT RECORDS                                       
*                                                                               
***********************************************************************         
***********************************************************************         
* USER    JIRA       DATE                  CHANGE LOG                 *         
* ---- ----------  -------- ----------------------------------------- *         
* AKAT SPEC-40080  12/04/19 SUPPORT NEW BUYING AGENCY IDENTIFIER      *         
***********************************************************************         
         TITLE 'T31C01 NETWORK CLIENT RECORD'                                   
T31C01   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,NETCLI,R7                                                      
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
*                                                                               
         CLI   T31CFFD+1,C'*'      DDS TERMINAL?                                
         BE    *+14                                                             
         CLC   AGENCY,=CL2'TH'     ZENITH CANNOT USE CLIENT REC IN SFM          
         BE    ZENERR                                                           
*                                                                               
         MVI   IOOPT,C'Y'          CONTROL MY OWN ADDREC/PUTREC                 
*                                                                               
         GOTO1 VTERMACC            CHECK FOR DISP/LIST ONLY TERMINALS           
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,RECDEL         RECORD DELETE                                
         BE    RDEL                                                             
         CLI   MODE,XRECDEL        AFTER RECORD HAS BEEN DELETED                
         BE    XD                                                               
         CLI   MODE,XRECREST       AFTER RECORD HAS BEEN RESTORED               
         BE    XR                                                               
         CLI   MODE,XRECADD        AFTER RECORD HAS BEEN ADDED                  
         BE    XA                                                               
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LR                                                               
         CLI   MODE,PRINTREP       PRINT RECORDS                                
         BE    LR                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* VALIDATE KEY                                                                  
***********************************************************************         
VK       DS    0H                                                               
         LA    R3,SVKEY                                                         
         USING CLTRECD,R3                                                       
         XC    SVKEY,SVKEY                                                      
*                                                                               
         LA    R2,CLTMEDH          MEDIA                                        
         TM    4(R2),X'20'         VALIDATED PREVIOUSLY??                       
         BNZ   VK03                                                             
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
         GOTO1 VALIMED                                                          
         OI    4(R2),X'20'         VALIDATED                                    
VK03     MVC   CKEYAM,BAGYMD                                                    
*                                                                               
         CLI   ACTNUM,ACTLIST      IF LIST                                      
         BNE   *+12                                                             
         BRAS  RE,VALFLTS                                                       
         B     VKX                                                              
*                                                                               
VK05     LA    R2,CLTCLTH          * CLIENT                                     
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
         CLI   ACTNUM,ACTADD                                                    
         BNE   VK10                                                             
*                                                                               
         CLI   8(R2),C'0'          FIRST 2 CHARACTERS CAN'T BE NUMBERS          
         BNL   INVLCLI                                                          
*!!!     CLI   9(R2),C'0'                                                       
*!!!     BNL   INVLCLI                                                          
*                                                                               
         CLC   8(3,R2),=CL3'ALL'   ALL NOT ALLOWED                              
         BE    INVLCLI                                                          
*                                                                               
         MVC   QCLT,CLTCLT         ACTION ADD ONLY                              
         OI    QCLT+2,X'40'                                                     
         GOTO1 CLPACK,DMCB,QCLT,BCLT                                            
         CLI   0(R1),0                                                          
         BNE   INVLCLI                                                          
         CLC   CTAGYSV,=X'4040'                                                 
         BNH   VK20                                                             
         BAS   RE,CHKCTF                                                        
         B     VK20                                                             
*                                                                               
VK10     GOTO1 VALICLT                                                          
*                                                                               
VK20     MVC   CKEYCLT,BCLT                                                     
*                                                                               
VKX      MVC   HOLDKEY,SVKEY                                                    
         MVC   KEY,SVKEY                                                        
         B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* DISPLAY KEY                                                                   
***********************************************************************         
DK       DS    0H                                                               
         L     R6,AIO                                                           
         USING CLTRECD,R6                                                       
         MVI   CLTMED,C'N'         * MEDIA                                      
         OI    CLTMEDH+6,X'80'     XMIT                                         
         GOTO1 CLUNPK,DMCB,(CPROF+6,CKEYCLT),CLTCLT                             
         OI    CLTCLTH+6,X'80'     XMIT                                         
         MVC   HOLDKEY,CKEY                                                     
*                                                                               
DKX      B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* DELETE RECORD                                                                 
***********************************************************************         
RDEL     DS    0H                                                               
         CLI   T31CFFD+1,C'*'      DDS ONLY CAN DELETE                          
         BNE   INVLACT                                                          
         L     R6,AIO                                                           
         USING CLTRECD,R6                                                       
         OC    CLIST(255),CLIST    MAKE SURE NO PRDS LEFT                       
         BNZ   PATTERR                                                          
         OC    CLIST+255(255),CLIST+255                                         
         BNZ   PATTERR                                                          
         OC    CLIST+510(255),CLIST+510                                         
         BNZ   PATTERR                                                          
         OC    CLIST+765(115),CLIST+765                                         
         BNZ   PATTERR                                                          
*                                                                               
         CLI   ACTNUM,ACTSEL       KEY SET FROM LIST SELECT                     
         BNE   RDEL10                                                           
         MVC   SVKEY,KEY                                                        
         B     RDEL20                                                           
*                                                                               
RDEL10   MVC   KEY,SVKEY           RESTORE KEY                                  
*                                                                               
RDEL20   GOTO1 HIGH                                                             
         CLC   KEY(L'CKEY),KEYSAVE                                              
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
         OI    KEY+13,X'80'        SET DIR DELETED                              
         BAS   RE,MYDIRWRT                                                      
         OI    CCNTRL,X'80'        SET RECORD DELETED                           
         BAS   RE,MYFILWRT            WRITE TO FILE                             
* DELETE PASSIVE POINTER                                                        
         XC    KEY,KEY                                                          
         L     R6,AIO                                                           
         USING CLTRECD,R6                                                       
         MVC   KEY(2),=X'0D80'                                                  
         MVC   KEY+2(1),CKEYAM                                                  
         MVC   KEY+9(1),COFFICE                                                 
         MVC   KEY+11(2),CKEYCLT                                                
         GOTO1 HIGH                                                             
         CLC   KEY(L'CKEY),KEYSAVE       DO WE HAVE A PASSIVE KEY               
         BNE   RDELX                                                            
         OI    KEY+13,X'80'              YES/DELETE IT                          
         BAS   RE,MYDIRWRT                                                      
*                                                                               
RDELX    B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* DELETE CLIENT GROUP PASSIVE POINTERS                                          
***********************************************************************         
XD       DS    0H                                                               
         BAS   RE,BCGRTAB          BUILD TABLE OF CLIENT GORUPS                 
*                                                                               
         L     R6,AIO                                                           
         USING CLTRECD,R6                                                       
         LA    R4,CGRPASS          POINT TO CLTGRP TABLE                        
*                                                                               
XD10     DS    0H                                                               
         OC    0(3,R4),0(R4)                                                    
         BZ    XDX                                                              
*                                                                               
         XC    KEY,KEY                                                          
         LA    R3,KEY                                                           
         USING CLGRECD,R3                                                       
         MVC   CLGCTYP,=X'0D86'                                                 
         MVC   CLGCAGMD,SVKEY+1                                                 
         MVC   CLGCID(3),0(R4)                                                  
         MVC   CLGCCLT,SVKEY+2                                                  
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(L'CLGKEY),KEYSAVE                                            
         BNE   XD20                                                             
         OI    KEY+13,X'80'        MARK FOR DELETION                            
         GOTO1 WRITE                                                            
*                                                                               
XD20     LA    R4,3(R4)                                                         
         B     XD10                                                             
*                                                                               
XDX      B     EXIT                                                             
         DROP  R3,R6                                                            
         EJECT                                                                  
***********************************************************************         
* RESTORE CLIENT GROUP PASSIVE POINTERS                                         
***********************************************************************         
XR       DS    0H                                                               
         L     R6,AIO                                                           
         USING CLTRECD,R6                                                       
* - RESTORE CLIENT KEY PASSIVE POINTER IF IT EXISTS/ ELSE ADD ONE               
         XC    KEY,KEY                                                          
         MVC   KEY(13),0(R6)                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   WORK(4),KEY+14      SAVE DISKADDRESS                             
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D80'                                                  
         MVC   KEY+2(1),CKEYAM                                                  
         MVC   KEY+9(1),COFFICE                                                 
         MVC   KEY+11(2),CKEYCLT                                                
         OI    DMINBTS,X'08'       PASS DELETES                                 
         GOTO1 HIGH                                                             
         MVI   DMINBTS,0           RESET IT                                     
         CLC   KEY(L'CKEY),KEYSAVE IS IT OUT THERE                              
         BNE   XR02                                                             
         NI    KEY+13,X'FF'-X'80'  YES/RESTORE                                  
         BAS   RE,MYDIRWRT                                                      
         B     XR05                                                             
XR02     DS    0H                  NO PASSIVE POINTER/ADD IT                    
         MVC   KEY(14),KEYSAVE     RESET PASSIVE KEY                            
         MVC   KEY+14(4),WORK      SET DISKADDRESS                              
         GOTO1 MYDIRADD                                                         
                                                                                
                                                                                
*  NOW HANDLE CLIENT GROUPS                                                     
                                                                                
XR05     DS    0H                                                               
         BAS   RE,BCGRTAB          BUILD TABLE OF CLIENT GORUPS                 
         LA    R4,CGRPASS                                                       
*                                                                               
XR10     OC    0(3,R4),0(R4)                                                    
         BZ    XRX                                                              
         XC    KEY,KEY                                                          
         LA    R3,KEY                                                           
         USING CLGRECD,R3                                                       
         MVC   CLGCTYP,=X'0D86'                                                 
         MVC   CLGCAGMD,SVKEY+1                                                 
         MVC   CLGCID(3),0(R4)                                                  
         MVC   CLGCCLT,SVKEY+2                                                  
         OI    DMINBTS,X'08'       PASS DELETES                                 
         GOTO1 HIGH                                                             
         MVI   DMINBTS,0                                                        
*                                                                               
         CLC   KEY(L'CLGKEY),KEYSAVE                                            
         BNE   XR20                                                             
         NI    KEY+13,X'FF'-X'80'  RESTORE                                      
         GOTO1 WRITE                                                            
*                                                                               
XR20     LA    R4,3(R4)                                                         
         B     XR10                                                             
*                                                                               
XRX      B     EXIT                                                             
         DROP  R3,R6                                                            
*********************************************************************           
*        BUILD TABLE OF CLIENT GROUPS                                           
*********************************************************************           
BCGRTAB  NTR1                                                                   
         L     R6,AIO                                                           
         USING CLTRECD,R6                                                       
         XC    CGRPASS,CGRPASS                                                  
*                                                                               
         LA    R4,CGRPASS                                                       
         LA    R5,CGRP1                                                         
         LA    RF,10                                                            
*                                                                               
BCGR10   DS    0H                  FILL IN ALL 10 CLIENT GROUPS                 
         OC    0(3,R5),0(R5)                                                    
         BZ    BCGRX                                                            
         MVC   0(3,R4),0(R5)                                                    
*                                                                               
         LA    RE,CGRP5                                                         
         CR    RE,R5               FINISHED FIRST 5 CLIENT GROUPS?              
         BNE   BCGR20                                                           
         LA    R5,CGRP6            YES, CHECK NEXT 5 CLIENT GROUPS              
         B     *+8                                                              
*                                                                               
BCGR20   LA    R5,3(R5)                                                         
         LA    R4,3(R4)                                                         
         BCT   RF,BCGR10                                                        
*                                                                               
BCGRX    DS    0H                                                               
         B     EXIT                                                             
         DROP  R6                                                               
*********************************************************************           
         EJECT                                                                  
* FOR CLIENTS DOING CODE COORDINATION CODE MUST BE ON CTFILE                    
* GO FIND OUT                                                                   
         SPACE 1                                                                
CHKCTF   NTR1                                                                   
         CLI   ACTNUM,ACTADD       TEST ACTION = ADD                            
         BNE   CHKX                NO - FORGET IT                               
         XC    WORK,WORK                                                        
         LA    R4,WORK                                                          
         USING ZENRECD,R4                                                       
         MVI   ZENKCODE,ZENKCODQ                                                
         MVI   ZENKTYP,ZENCLTQ                                                  
         MVC   ZENKAGY,AGENCY                                                   
         MVC   ZENKCLT,QCLT                                                     
         L     R2,AIO                                                           
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'CTFILE',WORK,(R2)                     
         CLC   ZENRKEY,0(R2)                                                    
         BE    *+12                                                             
         LA    R2,CLTCLTH                                                       
         B     CTFERR                                                           
         LR    R4,R2                                                            
         LA    R4,ZENFIRST                                                      
         USING ZENELEM,R4                                                       
* - CLIENT                                                                      
         LA    R2,CLTNAMEH                                                      
         MVI   5(R2),20            FORCE LENGTH                                 
         OI    6(R2),X'80'         FORCE XMT                                    
         MVC   8(20,R2),ZENCNAME   MOVE ZENITH NAME                             
* - ACCOFFICE                                                                   
         B     CHKX                                                             
********                                                                        
         LA    R2,CLTAONMH                                                      
         OC    ZENCOFF(4),ZENCOFF                                               
         BZ    CHKX                                                             
         MVI   5(R2),2             FORCE LENGTH                                 
         OI    6(R2),X'80'         FORCE XMT                                    
         MVC   8(2,R2),ZENCOFF                                                  
         CLI   8(R2),X'40'         IS THERE AN OFFICE CODE                      
         BH    *+10                                                             
         MVC   8(2,R2),=C'00'                                                   
         OC    ZENCAGOF,ZENCAGOF   IS THERE AGY OVERRIDE                        
         BZ    CHKX                                                             
         MVI   5(R2),4                                                          
         LA    RE,9(R2)                                                         
         CLI   9(R2),X'40'         IS IT TWO CHARACTER CODE                     
         BNH   *+12                                                             
         LA    RE,1(RE)            YES                                          
         MVI   5(R2),5                                                          
         MVI   0(RE),C'/'                                                       
         MVC   1(2,RE),ZENCAGOF                                                 
CHKX     XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* VALIDATE RECORD                                                               
***********************************************************************         
VR       DS    0H                                                               
         L     R6,AIO                                                           
         USING CLTRECD,R6                                                       
         LA    R2,CLTNAMEH                                                      
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
         MVC   CNAME,CLTNAME                                                    
         OC    CNAME,SPACES                                                     
*                                                                               
VR050    DS    0H                  EDIT CLIENT OFFICE NUMBER                    
         LA    R2,CLTONUMH                                                      
         MVI   COFFICE,0                                                        
         CLI   5(R2),0                                                          
         BNE   VR055                                                            
         CLI   USERPROF+13,C'Y'    IS OFFICE NUMBER REQUIRED                    
         BNE   VR060               NO                                           
         BE    MISSFLD             YES                                          
*                                                                               
VR055    DS    0H                                                               
         LA    R4,DUB                                                           
         USING OFFICED,R4                                                       
         XC    0(OFCLENQ,R4),0(R4)                                              
*****    MVI   OFCSYS,C'N'                                                      
         MVI   OFCSYS,C'S'                                                      
         MVC   OFCAUTH,T31CFFD+6                                                
         MVC   OFCLMT,T31CFFD+6                                                 
         MVC   OFCAGY,AGENCY                                                    
         OC    BCLT,BCLT           IS THIS AROUND?                              
         BNZ   VR056                                                            
         MVC   WORK(3),CLTCLT                                                   
         OI    WORK+2,X'40'                                                     
         GOTO1 CLPACK,DMCB,WORK,BCLT                                            
VR056    MVC   OFCCLT2,BCLT                                                     
         OI    OFCINDS,OFCI2CSC                                                 
         MVC   OFCOFC2,CLTONUM                                                  
         CLI   OFCOFC2+1,0                                                      
         BNE   *+8                                                              
         MVI   OFCOFC2+1,X'40'   **MAKE 2ND POSITITON SO                        
         MVC   OFCSECD,ASECBLK                                                  
***      LH    R0,=Y(SVSECRET-T31CFFD)                                          
***      AR    R0,RA                                                            
***      ST    R0,OFCSECD                                                       
*                                                                               
* LOAD OFFICER                                                                  
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB+4(4),=X'D9000A38'  GET OFFICER ADDRESS                      
         L     RF,CALLOV                                                        
         GOTO1 (RF),DMCB                                                        
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB             SAVE OFFICER ADDRESS                         
*                                                                               
         GOTO1 (RF),DMCB,(C'2',OFFICED),(0,ACOMFACS)                            
         CLI   0(R1),0                                                          
         BNE   INVLFLD                                                          
         MVC   COFFICE,OFCOFC      SAVE OFF 1 BYTE INTERNAL OFFICE CODE         
         TM    OFCINDS,OFCINOLA    USING 2 CHAR OFFICES?                        
         BNZ   VR058                NO                                          
         TM    OFCINDS,OFCIOINV    INVALID OFFICE?                              
         BNZ   OFCERR               YES                                         
         CLI   0(R1),0                                                          
         BNE   OFCERR                                                           
         B     VR060                                                            
         DROP  R4                                                               
*                                                                               
VR058    TM    SVAGYFL1,X'10'      OFF=HEX OPTION IN USE?                       
         BO    *+12                YES                                          
         CLI   8(R2),C'A'                                                       
         BL    INVLFLD                                                          
         CLI   CLTONUM,C'='                                                     
         BE    INVLFLD                                                          
         CLI   CLTONUM,C','                                                     
         BE    INVLFLD                                                          
         CLI   CLTONUM,C'-'                                                     
         BE    INVLFLD                                                          
         MVC   COFFICE,CLTONUM                                                  
*                                                                               
VR060    LA    R2,CLTAONMH         ACC OFFICE                                   
         BAS   RE,CHKAOFF                                                       
*                                                                               
VR065    DS    0H                                                               
         LA    R2,CLTTONMH         TRAFFIC OFFICE                               
         BRAS  RE,CHKTOFF                                                       
*                                                                               
VR070    LA    R2,CLTIFCDH         EDIT CLIENT INTERFACE CODE                   
         CLI   5(R2),0                                                          
         BE    VR080                                                            
         MVC   CCLTIFC,CLTIFCD                                                  
         B     VR100                                                            
*                                                                               
VR080    LA    R1,AGYTAB           TABLE OF AGENCIES THAT REQUIRE               
         LA    R4,AGYTABN            NUMBER OF AGYS                             
*                                                                               
VR090    CLC   AGENCY,0(R1)                                                     
         BE    INVLFLD                                                          
         LA    R1,2(R1)                                                         
         BCT   R4,VR090                                                         
*                                                                               
VR100    XC    CZENCLT,CZENCLT     EDIT CLIENT OPTION FIELD                     
         XC    CCOST2,CCOST2                                                    
         XC    CPRPRD,CPRPRD                                                    
         XC    CLEDICLT,CLEDICLT                                                
         XC    CLEDIUID,CLEDIUID                                                
         NI    COPT3,X'FF'-COP3CS2I                                             
*****    NI    COPT3,X'FF'-COP3T                                                
*****    NI    COPT3,X'FF'-COP3TI                                               
         NI    CINDS1,X'FF'-CIN1NEDI                                            
         MVC   OLDCOPT2,COPT2                                                   
         NI    COPT2,X'FF'-COP2FRZ                                              
         LA    R2,CLTOPTSH                                                      
         CLI   5(R2),0                                                          
         BE    VR125                                                            
         GOTO1 SCANNER,DMCB,(14,(R2)),BLOCK                                     
         CLI   DMCB+4,0                                                         
         BE    INVLFLD                                                          
         LA    R5,BLOCK                                                         
         USING SCAND,R5                                                         
         ZIC   R3,DMCB+4           N'ENTRIES                                    
*                                                                               
VR100A   DS    0H                                                               
         CLC   FLD1(2),=CL2'EC'    ECOST OPTION?                                
         BNE   VR101                                                            
*                                                                               
         TM    COPT3,COP3T                                                      
         BO    VR120                                                            
         TM    COPT3,COP3TI                                                     
         BO    VR120                                                            
         TM    COPT4,COP4TIS                                                    
         BO    VR120                                                            
*                                                                               
         CLI   FLD2LEN,3                                                        
         BH    INVLFLD                                                          
******   NI    COPT3,X'FF'-COP3TI                                               
******   NI    COPT3,X'FF'-COP3T                                                
         CLC   FLD2(3),=C'TIS'     TIME + INTEGRATION + SPECIAL                 
         BNE   *+12                                                             
         OI    COPT4,COP4TIS                                                    
         B     VR120                                                            
*                                                                               
         CLC   FLD2(2),=C'TI'      TIME + INTEGRATION                           
         BNE   *+12                                                             
         OI    COPT3,COP3TI                                                     
         B     VR120                                                            
*                                                                               
         CLI   FLD2,C'T'           TIME                                         
         BNE   INVLFLD                                                          
         OI    COPT3,COP3T                                                      
         B     VR120                                                            
*                                                                               
* VALIDATE ZENITH CLT ZEN=CC(C), 2-3 ALPHANUMERIC CHARACTERS                    
VR101    CLC   FLD1(3),=CL3'ZEN'                                                
         BNE   VR107                                                            
         CLI   FLD2LEN,2                                                        
         BL    INVLFLD                                                          
         CLI   FLD2LEN,3                                                        
         BH    INVLFLD                                                          
         ZIC   R1,FLD2LEN                                                       
         LA    RE,FLD2                                                          
VR102    LA    RF,ALPHANUM                                                      
VR104    CLI   0(RF),0                                                          
         BE    INVLCLI                                                          
         CLC   0(1,RE),0(RF)                                                    
         BE    VR106                                                            
         LA    RF,1(RF)                                                         
         B     VR104                                                            
VR106    LA    RE,1(RE)                                                         
         BCT   R1,VR102                                                         
         IC    R1,FLD2LEN                                                       
         BCTR  R1,0                                                             
         EXMVC R1,CZENCLT,FLD2                                                  
         B     VR120                                                            
*                                                                               
* VALIDATE ZENITH CLT ZEN=CC(C), 2-3 ALPHANUMERIC CHARACTERS                    
VR107    CLC   FLD1(3),=CL3'EDI'                                                
         BNE   VR108                                                            
         BRAS  RE,EDISET                                                        
         B     VR120                                                            
*                                                                               
VR108    CLC   FLD1(3),=CL3'FRZ'                                                
         BNE   VR109                                                            
         OI    COPT2,COP2FRZ                                                    
         B     VR120                                                            
*                                                                               
VR109    CLC   FLD1(5),=CL5'MIDAS'                                              
         BNE   *+12                                                             
         OI    COPT4,COP4MIDS                                                   
         B     VR120                                                            
*                                                                               
         CLC   FLD1(4),=C'NEDI'                                                 
         BNE   *+12                                                             
         OI    CINDS1,CIN1NEDI                                                  
         B     VR120                                                            
*                                                                               
VR110    CLC   FLD1(3),=CL3'SCJ'                                                
         BNE   VR111                                                            
         CLI   FLD2B+3,1                                                        
         BL    INVLFLD                                                          
         CLI   FLD2B+3,7                                                        
         BH    INVLFLD                                                          
         MVC   CSCJROT,FLD2B+3                                                  
         B     VR120                                                            
*                                                                               
VR111    CLC   =C'CPR',FLD1                                                     
         BNE   VR112                                                            
*                                                                               
         CLC   =C'ALL',FLD2                                                     
         BE    INVLFLD                                                          
         CLC   =C'AAA',FLD2                                                     
         BE    INVLFLD                                                          
         CLC   =C'POL',FLD2                                                     
         BE    INVLFLD                                                          
*                                                                               
         MVC   SAVEKEY,KEY                                                      
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),BCLT                                                    
         MVC   KEY+4(3),FLD2                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   INVLFLD                                                          
*                                                                               
         MVC   CPRPRD,FLD2                                                      
         OC    CPRPRD,SPACES                                                    
         MVC   KEY,SAVEKEY                                                      
*                                                                               
         CLI   ACTNUM,ACTADD                                                    
         BE    VR112                                                            
         XC    KEY,KEY                                                          
         L     RF,AIO1                                                          
         MVC   KEY(13),0(RF)                                                    
         GOTO1 HIGH                                                             
         B     VR120                                                            
*                                                                               
VR112    ZIC   R1,FLD1LEN                                                       
         BCTR  R1,0                                                             
         EX    R1,TSTCOS2                                                       
         BE    VR115                                                            
         EX    R1,TSTCOSI                                                       
         BNE   INVLFLD                                                          
         OI    COPT3,COP3CS2I      COST 2 FACTOR APPLIES ALSO TO INTEG          
VR115    BAS   RE,CHKCOS2                                                       
         BNE   INVLFLD                                                          
         B     VR120                                                            
*                                                                               
VR120    LA    R5,36(R5)           GET TO NEXT ENTRY IN BLOCK                   
         BCT   R3,VR100A                                                        
*                                                                               
*  SPECIAL MIDAS CHECK                                                          
*  IF MIDAS IS SET EC,COS2,IC0S2 CANNOT BE SET                                  
*                                                                               
         TM    COPT4,COP4MIDS       TEST IF MIDAS SET                           
         BZ    VR125                                                            
*                                                                               
         TM    COPT4,COP4TIS        TEST IF EC SET                              
         BO    INVLFLD                                                          
         TM    COPT3,COP3TI         TEST IF EC SET                              
         BO    INVLFLD                                                          
         TM    COPT3,COP3T          TEST IF EC SET                              
         BO    INVLFLD                                                          
         TM    CCOST2,X'80'         TEST IF COS2 USED                           
         BO    INVLFLD                                                          
         OC    CCOST2,CCOST2        TEST IF COS2 USED                           
         BNZ   INVLFLD                                                          
         OC    CZENCLT,CZENCLT      TEST IF "ZEN" CLIENT SET                    
         BNZ   INVLFLD                                                          
         OC    CPRPRD,CPRPRD        TEST IF "CPR" PRODUCT SET                   
         BNZ   INVLFLD                                                          
*                                                                               
*  SPECIAL WILA EDIT                                                            
*  CANNOT UNDO FREEZE OPTION UNLESS SECURITY ALLOWS                             
*                                                                               
VR125    CLC   AGENCY,=CL2'WI'                                                  
         BNE   VR129                                                            
         TM    OLDCOPT2,COP2FRZ    WAS FREEZE ALWAYS SET                        
         BZ    VR129                                                            
         TM    COPT2,COP2FRZ       IS FREEZE SET NOW                            
         BO    VR129                                                            
         TM    T31CFFD+12,X'04'    TEST AUTHORIZED TO CHANGE                    
         BO    VR129                                                            
         B     FRZERR                                                           
*                                                                               
VR129    LA    R2,CLTOP1H          EDIT CLIENT PROFILE                          
         LA    R4,PROFTAB                                                       
         MVC   WORK(15),CPROF      INITIALIZE TO PROFILES                       
         MVC   WORK+15(15),CEXTRA  INITIALIZE TO PROFILES                       
*                                                                               
VR130    CLI   0(R4),0             END OF TABLE                                 
         BE    VR190                                                            
*                                                                               
         CLI   2(R4),C'-'                                                       
         BE    VR185               NO PROFILE FOR THIS TABLE ENTRY              
         CLI   2(R4),X'FF'                                                      
         BE    VR150               GO TO SPECIAL EDIT FOR 1-9 OR A-Z            
*                                                                               
         ZIC   R5,0(R4)            LENGTH OF ENTRY                              
         BCTR  R5,0                                                             
         BCTR  R5,0                                                             
         LA    RE,2(R4)            SET RE TO FIRST ENTRY                        
*                                                                               
VR140    CLC   8(1,R2),0(RE)                                                    
         BE    VR170               VALID VALUE FOUND                            
         LA    RE,1(RE)                                                         
         BCT   R5,VR140                                                         
         B     INVLFLD                                                          
*                                                                               
VR150    CLI   8(R2),C'0'          0-9                                          
         BL    VR160                                                            
         CLI   8(R2),C'9'                                                       
         BH    INVLFLD                                                          
         B     VR170                                                            
*                                                                               
VR160    CLI   8(R2),C'A'          A-Z                                          
         BL    INVLFLD                                                          
         CLI   8(R2),C'Z'                                                       
         BH    INVLFLD                                                          
         B     VR170                                                            
*                                                                               
VR170    LA    R3,WORK                                                          
         ZIC   RE,1(R4)            DISPLACEMENT OF ENTRY                        
         AR    R3,RE                                                            
         BCTR  R3,0                                                             
         MVC   0(1,R3),8(R2)       PUT VALIDATED VALUE INTO WORK                
*                                                                               
VR175    ZIC   R0,0(R2)            FIND NEXT UNPROTECTED FIELD                  
         AR    R2,R0                                                            
         CLI   0(R2),0             END OF SCREEN??                              
         BE    INVLFLD                                                          
         TM    1(R2),X'20'                                                      
         BO    VR175                                                            
*                                                                               
VR185    ZIC   R0,0(R4)                                                         
         AR    R4,R0                                                            
         B     VR130                                                            
*                                  NOW CHECK NEW PROFILE VS OLD                 
VR190    DS    0H                  AND CROSS-CHECK NEW PROFILE                  
         MVC   CPROF,WORK          *****                                        
         MVC   CEXTRA,WORK+15      ******                                       
*                                                                               
VR250    DS    0H                                                               
         LA    R2,CLTLOCKH         CLIENT LOCK                                  
         CLI   5(R2),0             DOES NOTHING SO FAR                          
         BE    VR255                                                            
         CLI   8(R2),C'L'                                                       
         BE    VR255                                                            
         CLI   8(R2),C'U'                                                       
         BNE   INVLFLD                                                          
*                                                                               
VR255    DS    0H                                                               
         MVC   CLOCK,8(R2)                                                      
*                                                                               
VR260    DS    0H                  VALIDATE LIMITED ACCESS GROUP                
         XC    CACCESS,CACCESS                                                  
*                                                                               
         LA    R2,CLTLACCH                                                      
         CLI   5(R2),0                                                          
         BE    VR290                                                            
         CLI   5(R2),3                                                          
         BH    INVLFLD                                                          
*                                                                               
         ZIC   R3,5(R2)                                                         
         LA    RE,CLTLACC                                                       
*                                                                               
VR265    LA    RF,ALPHANUM                                                      
VR270    CLI   0(RF),0                                                          
         BE    INVLFLD                                                          
         CLC   0(1,RE),0(RF)                                                    
         BE    VR275                                                            
         LA    RF,1(RF)                                                         
         B     VR270                                                            
*                                                                               
VR275    LA    RE,1(RE)                                                         
         BCT   R3,VR265                                                         
*                                                                               
         ZIC   R3,5(R2)                                                         
         BCTR  R3,0                                                             
         EXMVC R3,CACCESS,CLTLACC                                               
*                                                                               
VR290    DS    0H                  TEST FRONTRUNNER SUB LINE LIMIT              
         MVI   CLTSLLMT,0                                                       
*                                                                               
         LA    R2,CLTSUBLH                                                      
         CLI   5(R2),0                                                          
         BE    VR300                                                            
*                                                                               
         TM    4(R2),X'08'         TEST NUMERIC                                 
         BO    *+8                                                              
         B     INVLFLD                                                          
*                                                                               
         ZIC   R1,5(R2)            GET THE YEAR                                 
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   R3,DUB                                                           
*  TEST LINE NUMBER RANGE                                                       
         C     R3,=F'1'                                                         
         BL    INVLFLD                                                          
         C     R3,=F'192'                                                       
         BH    INVLFLD                                                          
         STCM  R3,1,CLTSLLMT                                                    
*                                                                               
VR300    DS    0H                                                               
*&&DO                                                                           
         LA    R2,CLTUBCH          UCOMM BILL CONTROL                           
         NI    COPT4,X'FF'-COP4UCOM                                             
         CLI   5(R2),0             DOES NOTHING SO FAR                          
         BE    VR500                                                            
         CLI   8(R2),C'N'                                                       
         BE    VR500                                                            
         CLI   8(R2),C'Y'                                                       
         BNE   INVLFLD                                                          
         OI    COPT4,COP4UCOM                                                   
*&&                                                                             
*                                                                               
         BRAS  RE,VALBAID          VALIDATE BUYING AGENCY IDENTIFIER            
*                                                                               
VR500    DS    0H                                                               
****     MVC   CLEN,=H'1000'       SET CLIENT REC LENGTH                        
****     MVC   CLEN,=H'1280'       SET CLIENT REC LENGTH                        
         MVC   CLEN,=H'1500'       SET CLIENT REC LENGTH                        
         CLI   ACTNUM,ACTADD                                                    
         BNE   VR510                                                            
         BAS   RE,MYFILADD                                                      
         B     VRX                                                              
VR510    BAS   RE,MYFILWRT                                                      
*                                                                               
VRX      B     REQREC                                                           
TSTCOS2  CLC   FLD1(0),=C'COS2'                                                 
TSTCOSI  CLC   FLD1(0),=C'ICOS2'                                                
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
* THIS ROUTINE VLAIDATES THE DIFFERENT TYPES OF INPUT ALLOWED FOR               
* THE 'COS2' OPTION.                                                            
*                                                                               
CHKCOS2  NTR1                                                                   
         L     R6,AIO                                                           
         USING CLTRECD,R6                                                       
*                                                                               
         TM    SVAGYFL1,AGYCOS2Q   COST FACTOR REQUIRED?                        
         BZ    CKC2ERR             AGENCY NOT SET INPUT NOT ALLOWED             
*                                                                               
         OC    CCOST2,CCOST2       WAS COS2 PREVIOUSLY INPUTTED                 
         BNZ   CKC2ERR             YES ERROR                                    
*                                                                               
         ZIC   R4,FLD2LEN                                                       
         GOTO1 CASHVAL,DMCB,(6,FLD2),(R4)                                       
         CLI   DMCB,0                                                           
         BNE   CKC2ERR                                                          
         L     R3,4(R1)                                                         
         C     R3,=F'9999999'      MAX 9.999999                                 
         BH    CKC2ERR                                                          
*                                                                               
         C     R3,=F'0'            .LT. 0?                                      
         BL    CKC2ERR             YES - SO ERROR                               
*                                                                               
         MVC   CCOST2,DMCB+4       ELSE - MOVE IN THE COST FACTOR               
         OC    CCOST2,CCOST2       ZERO?                                        
         BNZ   CKC2DONE            NO - SO CONTINUE                             
*                                                                               
         OI    CCOST2,X'80'        ELSE - SET 'ZERO WAS INPUT' BIT              
*                                                                               
CKC2DONE EQU   *                                                                
*                                                                               
         SR    R0,R0               SET GOOD CC                                  
*                                                                               
CKC2EXIT EQU   *                                                                
*                                                                               
         XIT1                      RETURN                                       
*                                                                               
CKC2ERR  EQU   *                                                                
*                                                                               
         LTR   RC,RC               SET ERROR CC                                 
         B     CKC2EXIT            AND RETURN                                   
         DROP  R6                                                               
         EJECT                                                                  
*    CHECK ACCOUNTING OFFICE  MAKE SURE IT EXISTS                               
*                                                                               
CHKAOFF  NTR1                                                                   
         L     R4,AIO                                                           
         USING CLTRECD,R4                                                       
         L     RF,ACOMFACS         GET SYS NUM TO SWITCH BACK TO                
         L     RF,CGETFACT-COMFACSD(RF)                                         
*****    GOTO1 (RF),DMCB,GTFACTB                                                
         GOTO1 (RF),DMCB,0                                                      
         L     RE,0(R1)                                                         
         MVC   GTFACTB,0(RE)       FILL LOCAL GETFACT TABLE                     
         LA    R1,GTFACTB                                                       
         USING FACTSD,R1                                                        
         MVC   SYSSW,FASYS                                                      
         MVI   SWDSYS,C'N'         SET SWITCHED SYSTEM BYTE                     
         DROP  R1                                                               
*                                                                               
         XC    POWCODE,POWCODE                                                  
         XC    ACCOFF,ACCOFF                                                    
         LA    R2,CLTAONM          WHAT DID THEY ENTER                          
         LA    R1,0                                                             
         CLI   CLTAONMH+5,0                                                     
         BNE   CO05                                                             
         CLI   SVAGOF2,C'Y'       IS IT REQUIRED?                               
         BE    AOFCERR             =ERROR                                       
         B     CO10                                                             
*                                                                               
CO05     LA    R1,1                                                             
         LA    R2,1(R2)                                                         
         CLI   CLTAONMH+5,1                                                     
         BE    CO10                                                             
         LA    R1,2                                                             
         LA    R2,2(R2)                                                         
         CLI   CLTAONMH+5,2                                                     
         BE    CO10                                                             
         LA    R2,CLTAONM          WHAT DID THEY ENTER                          
         LA    R3,24                                                            
         LA    R1,0                                                             
COLOOP   CLI   0(R2),C','                                                       
         BE    CO10                                                             
         CLI   0(R2),C'/'                                                       
         BE    CO10                                                             
         CLI   0(R2),C' '                                                       
         BE    *+8                                                              
         LA    R1,1(R1)                                                         
         LA    R2,1(R2)                                                         
         BCT   R3,COLOOP                                                        
*                                                                               
CO10     STC   R1,OFFLEN           SAVE OFFICE LENGTH                           
         MVC   ACCOFF(2),CLTAONM                                                
         CLI   OFFLEN,1                                                         
         BNE   *+8                                                              
         MVI   ACCOFF+1,C' '                                                    
         CLI   1(R2),C' '          IS THERE AN OVERRIDE AGENCY                  
         BNH   *+10                                                             
         MVC   POWCODE,1(R2)       YES - SAVE POWER CODE                        
*                                                                               
         CLI   OFFLEN,2                                                         
         BH    OFCERR              NO OFFICE LENGTH > 2                         
         CLI   OFFLEN,0                                                         
         BNE   CO20                                                             
         XC    CACCOFC,CACCOFC     DEFAULT TO SPOT OFFICE                       
         XC    CACCAGY,CACCAGY                                                  
         MVC   CACCOFC,COFFICE                                                  
         MVI   CACCOFC+1,C' '                                                   
         B     COX                                                              
CO20     CLI   OFFLEN,2            IF THEY ENTERED 2 CHAR OFF                   
         BNE   CO30                                                             
         CLI   SVAGOF2,C'Y'       BUT 2 CHAR NOT REQUIRED                       
         BNE   ONEERR              =ERROR                                       
*                                                                               
CO30     DS    0H                                                               
         OC    POWCODE,POWCODE     OVERRIDE AGY?                                
         BNZ   CO40                                                             
         CLI   SVAGOF2,C'Y'       2 CHAR REQUIRED                               
         BNE   CO90                IF NOT THEN SKIP SWITCH                      
         L     RF,ACOMFACS         SWITCH TO ACC SYSTEM                         
         L     RF,CSWITCH-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,=C'ACC',0                                              
         MVI   SWDSYS,0            CLEAR SYSTEM BYTE                            
         B     CO70                                                             
*                                                                               
CO40     DS    0H                  VALIDATE AGAINST AGY HDR CODE LIST           
         LA    R3,SVACCAGY                                                      
         LA    R1,8                                                             
CO40LP   CLC   0(2,R3),POWCODE     MATCH?                                       
         BE    CO50                                                             
         CLI   0(R3),C' '                                                       
         BNH   AGYERR                                                           
         LA    R3,2(R3)                                                         
         BCT   R1,CO40LP                                                        
         B     AGYERR                                                           
*                                                                               
CO50     MVC   DATADISP,=H'28'     FIND SE NUMBER FOR SPECIFIED                 
         XC    SVKEY,SVKEY         ACC AGY CODE                                 
         LA    R6,SVKEY                                                         
         USING CT5REC,R6                                                        
         MVI   CT5KTYP,CT5KTYPQ    RECORD TYPE '5'                              
         MVC   CT5KALPH,POWCODE                                                 
         GOTO1 DATAMGR,DMCB,(0,=C'DMREAD'),=C'CTFILE',SVKEY,AIO2                
         CLI   8(R1),0             ERRORS?                                      
         BNE   AGYERR                                                           
         DROP  R6                                                               
*                                                                               
         USING CTSYSD,R6                                                        
         L     R6,AIO2                                                          
         MVI   ELCODE,X'21'        GET SE NUM FOR ACC FILE                      
         BAS   RE,GETEL                                                         
         B     *+8                                                              
CO60NX   BAS   RE,NEXTEL                                                        
         BNE   AGYERR              ERROR IF NOT FOUND                           
         CLI   CTSYSEL,X'21'       STILL X'21' EL                               
         BNE   AGYERR              ERROR IF NOT FOUND                           
         CLI   CTSYSNUM,X'06'      ACC??                                        
         BNE   CO60NX                                                           
*                                                                               
         MVC   COMPCD(1),CTSYSAGB  AGY BINARY CD                                
         XC    DMCB(8),DMCB        YES                                          
         MVC   DMCB(1),CTSYSSE     SE NUM                                       
         L     RF,ACOMFACS         SWITCH TO THAT ACC SYSTEM                    
         L     RF,CSWITCH-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB                                                        
         MVI   SWDSYS,0            CLEAR SYSTEM BYTE                            
*                                                                               
CO70     CLI   4(R1),2             SYSTEM NOT OPEN                              
         BE    SYSERR                                                           
         CLI   4(R1),1             ANY OTHER ERRORS?                            
         BE    SWERR                                                            
         CLI   COMPCD,0              TEST SAVED SF CODE BEFORE                  
         BNE   *+10                  YES - BETTER NOT DO IT AGAIN !             
         MVC   COMPCD,0(R1)          SAVE RETURNED AGENCY BINARY CODE           
*                                                                               
         MVC   SVKEY,SPACES       READ ACC COMPANY REC                          
         MVC   SVKEY(1),COMPCD                                                  
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'ACCOUNT',SVKEY,AIO2                   
         CLI   8(R1),0                                                          
         BNE   CMPERR                                                           
         L     R6,AIO2                                                          
         AH    R6,=Y(ACCORFST)     FIRST ELEM (IN OLD FILE FORMAT)              
CO80     CLI   0(R6),CPYELQ        X'10' COMPANY ELEM                           
         BE    CO85                                                             
         ZIC   R0,0(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BNE   CO80                                                             
         B     CMPERR                                                           
         USING CPYELD,R6                                                        
CO85     TM    CPYSTAT4,CPYSOFF2   2 CHAR REQ'D                                 
         BO    CO88                YES = VALIDATE OFFICE                        
         CLI   OFFLEN,1            MUST BE ONE                                  
         BNE   ONEERR              NO = NO VALIDATION                           
         B     CO90                OK                                           
         DROP  R6                                                               
*                                                                               
         USING OFFRECD,R6                                                       
CO88     CLI   OFFLEN,2            MUST BE TWO                                  
         BNE   TWOERR                                                           
         LA    R6,SVKEY            NEW OFFICE -- LOOK FOR OFFICE REC            
         MVC   SVKEY,SPACES                                                     
         MVI   OFFKTYP,OFFKTYPQ      X'01'                                      
         MVC   OFFKCPY,COMPCD                                                   
         MVC   OFFKOFF(2),ACCOFF                                                
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'ACCOUNT',SVKEY,AIO2                   
         CLI   8(R1),0                                                          
         BNE   OFCERR                                                           
         L     R6,AIO2                                                          
         TM    OFFRSTAT,OFFSLIST   OFFICE LIST?                                 
         BO    OFCERR2                                                          
*                                                                               
CO90     DS    0H                  OFFICE CODE IS GOOD                          
         MVC   CACCOFC,ACCOFF      SAVE OFFICE CODE                             
         MVC   CACCAGY,POWCODE     SAVE AGY CODE                                
*                                                                               
COX      DS    0H                                                               
         MVC   DATADISP,=H'24'     FOR SPOT                                     
         CLI   SWDSYS,C'N'         HAVE WE SWITCHED SYSTEMS                     
         BE    XIT                 NO                                           
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB(1),SYSSW       ORIGINAL SYS                                 
         L     RF,ACOMFACS         SWITCH BACK                                  
         L     RF,CSWITCH-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB                                                        
         CLI   4(R1),0             ANY ERRORS?                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         B     XIT                                                              
COXX     XIT1                                                                   
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* FIND NEXT UNPROTECTED FIELD                                                   
***********************************************************************         
FNDNXUF  ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BNE   FNDUF                                                            
         DC    H'0'           END OF SCREEN                                     
FNDUF    TM    1(R2),X'20'    FIND NEXT UNPROTECTED FIELD                       
         BO    FNDNXUF                                                          
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* DISPLAY RECORD                                                                
***********************************************************************         
DR       DS    0H                                                               
         CLI   ACTNUM,ACTSEL                                                    
         BNE   DR10                                                             
         MVC   PREVKEY,KEY                                                      
         MVI   PREVFLAG,C'Y'                                                    
*                                                                               
DR10     L     R3,AIO                                                           
         USING CLTRECD,R3                                                       
*                                                                               
         MVC   CLTNAME,CNAME       CLIENT NAME                                  
         OI    CLTNAMEH+6,X'80'    XMIT                                         
*                                                                               
* OVERFLOW PRODUCT INDICATOR                                                    
         MVC   CLTOVPR,SPACES                                                   
         CLI   T31CFFD+1,C'*'      DDS ONLY                                     
         BNE   DR15                                                             
         TM    CINDS1,CIN1OVP        CHECK OVERFLOW STATUS                      
         BZ    DR15                                                             
         MVC   CLTOVPR,=CL8'OV PRODS'                                           
DR15     OI    CLTOVPRH+6,X'80'    XMIT                                         
*                                                                               
* LOAD OFFICER                                                                  
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB+4(4),=X'D9000A38'  GET OFFICER ADDRESS                      
         L     RF,CALLOV                                                        
         GOTO1 (RF),DMCB                                                        
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB                                                          
***      MVC   CLTONUM,COFFICE     OFFICE NUMBER                                
         LA    R4,DUB                                                           
         USING OFFICED,R4                                                       
         XC    0(OFCLENQ,R4),0(R4)                                              
*****    MVI   OFCSYS,C'N'                                                      
         MVI   OFCSYS,C'S'                                                      
         MVC   OFCAUTH,T31CFFD+6                                                
         MVC   OFCLMT,T31CFFD+6                                                 
         MVC   OFCAGY,AGENCY                                                    
         MVC   OFCCLT2,CKEYCLT                                                  
         OI    OFCINDS,OFCI2CSC                                                 
         MVC   OFCOFC,COFFICE                                                   
         MVC   OFCSECD,ASECBLK                                                  
*****    LH    R0,=Y(SVSECRET-T31CFFD)                                          
*****    AR    R0,RA                                                            
*****    ST    R0,OFCSECD                                                       
*                                                                               
         GOTO1 (RF),DMCB,(C'2',OFFICED),(0,ACOMFACS)                            
         CLI   0(R1),0                                                          
         BNE   SECERR                                                           
         MVC   CLTONUM,OFCOFC2                                                  
         OI    CLTONUMH+6,X'80'    XMIT                                         
         DROP  R4                                                               
*                                                                               
**       MVC   CLTAONM,CACCOFC     OFFICE NUMBER                                
**       OI    CLTAONMH+6,X'80'    XMIT                                         
*                                                                               
         LA    R2,CLTAONM                                                       
         XC    0(5,R2),0(R2)                                                    
         MVC   0(2,R2),CACCOFC     SET OFFICE NUMBER                            
         LA    R2,1(R2)                                                         
         CLI   0(R2),X'40'                                                      
         BNH   *+8                                                              
         LA    R2,1(R2)                                                         
         OC    CACCAGY,CACCAGY     IF AGY OVERRIDE                              
         BZ    *+14                                                             
         MVI   0(R2),C'/'          SEPARATE WITH '/'                            
         MVC   1(2,R2),CACCAGY     AND PASS AGY OVERRIDE                        
         OI    CLTAONMH+6,X'80'    XMIT                                         
*                                                                               
         MVC   CLTTONM,CTRAFOFC    TRAFFIC OFFICE CODE                          
         OI    CLTTONMH+6,X'80'                                                 
*                                                                               
         MVC   CLTIFCD,CCLTIFC     INTERFACE CODE                               
         OI    CLTIFCDH+6,X'80'    XMIT                                         
*                                                                               
         XC    CLTOPTS,CLTOPTS                                                  
         LA    R2,CLTOPTS                                                       
*                                                                               
         OC    CPRPRD,CPRPRD                                                    
         BZ    DR140                                                            
         MVC   0(4,R2),=C'CPR='                                                 
         MVC   4(L'CPRPRD,R2),CPRPRD                                            
         LA    R2,6(R2)                                                         
         CLI   0(R2),X'40'                                                      
         BNH   *+8                                                              
         LA    R2,1(R2)                                                         
         MVI   0(R2),C','                                                       
         LA    R2,1(R2)                                                         
*                                                                               
DR140    CLI   CSCJROT,0                                                        
         BE    DR150                                                            
         MVC   0(4,R2),=C'SCJ='                                                 
         MVC   4(1,R2),CSCJROT                                                  
         OI    4(R2),X'F0'                                                      
         MVI   5(R2),C','                                                       
         LA    R2,6(R2)                                                         
*                                                                               
DR150    TM    COPT3,COP3T                                                      
         BZ    DR155                                                            
         MVC   0(5,R2),=C'EC=T,'                                                
         LA    R2,5(R2)                                                         
         B     DR163                                                            
*                                                                               
DR155    DS    0H                                                               
         TM    COPT3,COP3TI                                                     
         BZ    DR160                                                            
         MVC   0(6,R2),=C'EC=TI,'                                               
         LA    R2,6(R2)                                                         
*                                                                               
DR160    DS    0H                                                               
         TM    COPT4,COP4TIS                                                    
         BZ    DR163                                                            
         MVC   0(7,R2),=C'EC=TIS,'                                              
         LA    R2,7(R2)                                                         
*                                                                               
DR163    DS    0H                                                               
         OC    CLEDIUID,CLEDIUID                                                
         BZ    DR165                                                            
         BRAS  RE,EDIDISP                                                       
         L     R2,REGHOLD          SET R2 POINTER                               
*                                                                               
DR165    DS    0H                                                               
         TM    COPT2,COP2FRZ       FREEZE OPTION                                
         BZ    DR168                                                            
         MVC   0(4,R2),=CL4'FRZ,'                                               
         LA    R2,4(R2)                                                         
*                                                                               
DR168    DS    0H                                                               
         TM    COPT4,COP4MIDS      MIDAS OPTION                                 
         BZ    *+14                                                             
         MVC   0(6,R2),=CL6'MIDAS,'                                             
         LA    R2,6(R2)                                                         
*                                                                               
         TM    CINDS1,CIN1NEDI     NEDI?                                        
         BZ    *+14                                                             
         MVC   0(5,R2),=C'NEDI,'                                                
         LA    R2,5(R2)                                                         
*                                                                               
DR170    OC    CZENCLT,CZENCLT     CLIENT OPTIONS                               
         BZ    DR175                                                            
         MVC   0(4,R2),=CL4'ZEN='                                               
         MVC   4(3,R2),CZENCLT                                                  
         LA    R2,7(R2)                                                         
*                                                                               
DR175    OC    CCOST2,CCOST2       ANY COST FACTOR?                             
         BZ    DR180               NO - SO CONTINUE                             
         TM    COPT3,COP3CS2I      INTEGRATION COS2 CHECK                       
         BZ    DR176                                                            
         MVC   0(6,R2),=C'ICOS2='  MOVE OUT LITERAL                             
         LA    R2,6(R2)            GET A(NEW LINE POSITION)                     
         B     *+14                                                             
DR176    MVC   0(5,R2),=C'COS2='   MOVE OUT LITERAL                             
         LA    R2,5(R2)            GET A(NEW LINE POSITION)                     
         CLI   CCOST2,X'80'        ZERO AS INPUT DATA?                          
         BNE   DR178               NO - SO CONTINUE                             
         MVC   0(3,R2),=C'0.0'     ELSE - MOVE OUT ZERO                         
         LA    R0,3                INC A(LINE POSITION)                         
         B     DR179               AND CONTINUE                                 
*                                                                               
DR178    EQU   *                                                                
         EDIT  CCOST2,(8,0(R2)),6,ALIGN=LEFT,FILL=0,DROP=5                      
*                                                                               
DR179    EQU   *                                                                
         AR    R2,R0               INC A(LINE POSITION)                         
         MVI   0(R2),C','          MOVE OUT COMMA                               
         LA    R2,1(R2)                                                         
*                                                                               
DR180    BCTR  R2,0                                                             
         CLI   0(R2),C','                                                       
         BNE   *+8                                                              
         MVI   0(R2),X'40'                                                      
         OI    CLTOPTSH+6,X'80'    XMIT                                         
*                                                                               
DR200    MVC   CLTOP1,CPROF+5      BILL ESTIMATE CONTROL                        
         OI    CLTOP1H+6,X'80'     XMIT                                         
*                                                                               
         MVC   CLTOP2,CPROF+6      PRINT CONTROL CODE AAN                       
         OI    CLTOP2H+6,X'80'     XMIT                                         
*                                                                               
         MVC   CLTOP3,CPROF+14     CLIENT RATE CONTROL                          
         OI    CLTOP3H+6,X'80'     XMIT                                         
*                                                                               
         MVC   CLTOP4,CEXTRA+14    CLIENT RATE COVERAGE                         
         OI    CLTOP4H+6,X'80'     XMIT                                         
*                                                                               
         MVC   CLTOP5,CEXTRA+3     ESTIMATE FILTERS REQUIRED                    
         CLI   CEXTRA+3,C'0'       DISPLAY ZEROS A S C'N'                       
         BNE   *+8                                                              
         MVI   CLTOP5,C'N'                                                      
         OI    CLTOP5H+6,X'80'     XMIT                                         
*                                                                               
         MVC   CLTLOCK,CLOCK       CLIENT LOCK                                  
         OI    CLTLOCKH+6,X'80'    XMIT                                         
*                                                                               
         MVC   CLTLACC,CACCESS     LIMITED ACCESS CODE                          
         OI    CLTLACCH+6,X'80'                                                 
*                                                                               
         EDIT  CLTSLLMT,(3,CLTSUBL),ALIGN=LEFT                                  
         OI    CLTSUBLH+6,X'80'                                                 
*&&DO                                                                           
         MVI   CLTUBC,C'N'                                                      
         TM    COPT4,COP4UCOM      UCOMM BILL CONTROL                           
         BZ    *+8                                                              
         MVI   CLTUBC,C'Y'                                                      
         OI    CLTUBCH+6,X'80'                                                  
*&&                                                                             
*                                                                               
         MVC   CLTBAID,CBUYAGIN    BUYING AGENCY IDENTIFIER                     
         OI    CLTBAIDH+6,X'80'    TRANSMIT                                     
*                                                                               
DRX      DS    0H                                                               
         B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
*                                                                               
XA       DS    0H             RECORD HAS BEEN ADDED/ADD PASSIVE POINTER         
         L     R6,AIO                                                           
         USING CLTRECD,R6                                                       
         XC    KEY,KEY                                                          
         MVC   KEY(13),0(R6)                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    XA20                                                             
*                                                                               
         L     RF,ACOMFACS                 IF TEST SYSTEM ?                     
         L     RF,CGETFACT-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,0                                                      
         L     R1,0(R1)                                                         
         USING FACTSD,R1                                                        
         CLI   FASYSID,1                                                        
         BE    EXIT                SKIP/SINCE THIS BRINGS DOWN TEST             
         DC    H'0'                                                             
         DROP  R1                                                               
                                                                                
XA20     MVC   WORK(4),KEY+14      SAVE DISKADDRESS                             
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D80'                                                  
         MVC   KEY+2(1),CKEYAM                                                  
         MVC   KEY+9(1),COFFICE                                                 
         MVC   KEY+11(2),CKEYCLT                                                
         MVC   KEY+14(4),WORK      SET DISKADDRESS                              
         BAS   RE,MYDIRADD                                                      
XAX      B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
***********************************************************************         
* LIST RECORDS *                                                                
***********************************************************************         
LR       DS    0H                                                               
         LA    R5,LISTAR                                                        
         USING PLINED,R5                                                        
         CLI   MODE,PRINTREP                                                    
         BNE   LR02                                                             
         LA    R1,HEADING                                                       
         ST    R1,SPECS                                                         
         LA    R1,HDRTN                                                         
         ST    R1,HEADHOOK                                                      
*                                                                               
LR02     CLI   PREVFLAG,C'Y'                                                    
         BE    LR03                                                             
         OC    KEY,KEY                                                          
         BNZ   LR05                                                             
*                                                                               
         XC    PREVCLT,PREVCLT                                                  
*                                                                               
         MVC   KEY(2),SVKEY                                                     
         OC    CLTCLT,CLTCLT                                                    
         BZ    LR05                                                             
         CLI   CLTCLTH+5,2                                                      
         BL    LR02A                                                            
         OC    CLTCLT,SPACES                                                    
         GOTO1 CLPACK,DMCB,CLTCLT,KEY+2                                         
         B     LR05                                                             
LR02A    XC    FULL,FULL                                                        
         MVC   FULL(3),=3C'A'                                                   
         ZIC   R1,CLTCLTH+5                                                     
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   FULL(0),CLTCLT                                                   
         GOTO1 CLPACK,DMCB,FULL,KEY+2                                           
         B     LR05                                                             
*                                                                               
LR03     MVC   KEY,PREVKEY                                                      
         MVI   PREVFLAG,C'N'                                                    
*                                                                               
LR05     GOTO1 HIGH                                                             
         B     LR10                                                             
*                                                                               
LRSEQ    GOTO1 SEQ                                                              
*                                                                               
LR10     CLC   KEY(2),KEYSAVE                                                   
         BNE   LRX                                                              
         OC    KEY+4(9),KEY+4                                                   
         BNZ   LRSEQ                                                            
*                                                                               
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         USING CLTRECD,R6                                                       
*                                                                               
         BRAS  RE,TSTFILT          CLIENT LIST FILTERS                          
         JNE   LRSEQ                                                            
*                                                                               
LR20     GOTO1 CLUNPK,DMCB,(CPROF+6,CKEYCLT),PCLT     * CLIENT                  
         CLI   MODE,PRINTREP                                                    
         BE    *+8                                                              
         MVI   PCLT+L'PCLT+1,C'/'                                               
*                                                                               
         TM    FILTFLAG,FILTFRZQ   LIST FROZEN INFO?                            
         JZ    LR25                                                             
         MVC   PNME,CNAME          CLIENT NAME                                  
         MVI   POFFICES+1,C'N'                                                  
         TM    COPT2,COP2FRZ       FROZEN?                                      
         JZ    *+8                                                              
         MVI   POFFICES+1,C'Y'                                                  
         J     LR35                                                             
*                                                                               
**********************************************************                      
* LOAD OFFICER                                                                  
LR25     XC    DMCB(8),DMCB                                                     
         MVC   DMCB+4(4),=X'D9000A38'  GET OFFICER ADDRESS                      
         L     RF,CALLOV                                                        
         GOTO1 (RF),DMCB                                                        
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB                                                          
         LA    R4,DUB                                                           
         USING OFFICED,R4                                                       
         XC    0(OFCLENQ,R4),0(R4)                                              
*****    MVI   OFCSYS,C'N'                                                      
         MVI   OFCSYS,C'S'                                                      
         MVC   OFCAUTH,T31CFFD+6                                                
         MVC   OFCLMT,T31CFFD+6                                                 
         MVC   OFCAGY,AGENCY                                                    
         MVC   OFCCLT2,CKEYCLT                                                  
         OI    OFCINDS,OFCI2CSC                                                 
         MVC   OFCOFC,COFFICE                                                   
         MVC   OFCSECD,ASECBLK                                                  
***      LH    R0,=Y(SVSECRET-T31CFFD)                                          
***      AR    R0,RA                                                            
***      ST    R0,OFCSECD                                                       
*                                                                               
         GOTO1 (RF),DMCB,(C'2',OFFICED),(0,ACOMFACS)                            
         CLI   0(R1),0                                                          
         BNE   SECERR2                                                          
         MVC   POFFICES(2),OFCOFC2                                              
         DROP  R4                                                               
*                                                                               
         LA    RF,PACCOFF                                                       
         MVC   PACCOFF,CACCOFC     * ACC AGENCY                                 
         MVC   PACCAG,SPACES                                                    
*                                                                               
         LA    RF,1(RF)                                                         
         CLI   0(RF),X'40'                                                      
         BNH   *+8                                                              
         LA    RF,1(RF)                                                         
*                                                                               
         OC    CACCAGY,CACCAGY                                                  
         BZ    LR30                                                             
         MVI   0(RF),C'/'                                                       
         MVC   1(2,RF),CACCAGY                                                  
*                                                                               
LR30     MVC   PINTER,CCLTIFC      * INTERFACE CODE                             
         MVC   PTITLE,CTITLE       * TITLE                                      
         MVC   PNME,CNAME          * CLIENT NAME                                
*&&DO                                                                           
         MVI   PUBC,C' '                                                        
         TM    COPT4,COP4UCOM                                                   
         BZ    *+8                                                              
         MVI   PUBC,C'Y'                                                        
*&&                                                                             
LR35     CLI   MODE,PRINTREP                                                    
         BE    LR40                                                             
         GOTO1 LISTMON                                                          
         B     LRSEQ               GOTO READ SEQ                                
*                                                                               
LR40     DS    0H                                                               
         MVC   PBILL+3(1),CPROF+5                                               
         MVC   PCLTCDE+3(1),CPROF+6                                             
         MVC   PCLTRTE+3(1),CPROF+14                                            
         MVC   PESTFLR+3(1),CEXTRA+3  ESTIMATE FILTERS REQUIRED                 
         CLI   CEXTRA+3,C'0'       DISPLAY ZEROS AS C'N'                        
         BNE   *+8                                                              
         MVI   PESTFLR+3,C'N'                                                   
*                                                                               
         MVC   P(PLENGTH),LISTAR   KEY DATA FROM LIST TO P                      
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     LRSEQ               GET NEXT RECORD                              
*                                                                               
LRX      B     EXIT                                                             
         DROP  R5,R6                                                            
         EJECT                                                                  
***********************************************************************         
* DATAMGR INTERFACE                                                             
***********************************************************************         
MYFILADD NTR1                                                                   
         GOTO1 DATAMGR,DMCB,=C'DMADD',=C'SPTFILE ',KEY+14,AIO,MYDMWRK           
         BAS   RE,DMCHECK                                                       
         B     YES                                                              
*                                                                               
MYFILWRT NTR1                                                                   
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'SPTFILE ',KEY+14,AIO,MYDMWRK           
         BAS   RE,DMCHECK                                                       
         B     YES                                                              
*                                                                               
MYDIRWRT NTR1                                                                   
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'SPTDIR  ',KEY,KEY                      
         BAS   RE,DMCHECK                                                       
         B     YES                                                              
*                                                                               
MYDIRADD NTR1                                                                   
         GOTO1 DATAMGR,DMCB,=C'DMADD',=C'SPTDIR  ',KEY,KEY                      
         BAS   RE,DMCHECK                                                       
         B     YES                                                              
*                                                                               
DMCHECK  CLI   8(R1),0                                                          
         BER   RE                                                               
         TM    8(R1),X'90'                                                      
         BM    NO                                                               
         DC    H'0'                                                             
         SPACE 1                                                                
YES      SR    R1,R1                                                            
         B     *+8                                                              
NO       LA    R1,1                                                             
         LTR   R1,R1                                                            
         SPACE 1                                                                
XIT      XIT1  REGS=(R0,R1)                                                     
         EJECT                                                                  
***********************************************************************         
* GENERATE REQUEST RECORD                                                       
***********************************************************************         
REQREC   XC    REC(150),REC                                                     
         LA    R1,REC                                                           
         MVI   10(R1),41                                                        
         MVI   14(R1),106                                                       
         LA    R1,REC+26                                                        
         MVI   0(R1),X'40'                                                      
         MVC   1(79,R1),0(R1)                                                   
         MVC   0(2,R1),=C'41'                                                   
         MVC   2(2,R1),14(RA)                                                   
         MVC   4(1,R1),CLTMED                                                   
         MVC   5(3,R1),CLTCLT                                                   
         OC    5(3,R1),SPACES                                                   
         MVC   68(7,R1),=C'CONTROL'                                             
         MVI   61(R1),C'C'                                                      
         MVI   63(R1),C'A'                                                      
         CLI   ACTNUM,ACTADD                                                    
         BE    *+8                                                              
         MVI   63(R1),C'C'                                                      
         GOTO1 DATAMGR,DMCB,=C'DMADD',=C'REQUEST',REC,REC                       
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* HEADER AND HEAD HOOK ROUTINES                                                 
***********************************************************************         
HEADING  DS    0H                                                               
         SSPEC H1,3,REQUESTOR                                                   
         SSPEC H3,3,C'MEDIA N'                                                  
         SSPEC H1,46,C'NETWORK CLIENT RECORDS'                                  
         SSPEC H2,46,C'----------------------'                                  
         SSPEC H1,93,AGYNAME                                                    
         SSPEC H2,93,AGYADD                                                     
         SSPEC H3,93,REPORT                                                     
         SSPEC H4,93,RUN                                                        
         SSPEC H5,103,PAGE                                                      
         DC    X'00'                                                            
*                                                                               
HDRTN    NTR1                                                                   
         LA    R2,H8                                                            
         USING PLINED,R2                                                        
         MVC   PCLT(3),=C'CLT'                                                  
         MVC   PCLT+132(3),=50C'-'                                              
         MVC   POFFICES-1(3),=C'OF#'                                            
         MVC   POFFICES-1+132(3),=50C'-'                                        
         MVC   PACCOFF-1(4),=C'AOF#'                                            
         MVC   PACCOFF-1+132(4),=50C'-'                                         
         MVC   PINTER(8),=C'INT CODE'                                           
         MVC   PINTER+132(8),=50C'-'                                            
         MVC   PTITLE+1(8),=C'ID TITLE'                                         
         MVC   PTITLE+132(10),=50C'-'                                           
         MVC   PNME+4(11),=C'CLIENT NAME'                                       
         MVC   PNME+132(20),=50C'-'                                             
         MVC   PBILL(8),=C'BILL EST'                                            
         MVC   PBILL+132(8),=50C'-'                                             
         MVC   PCLTCDE(7),=C'PRT CLT'                                           
         MVC   PCLTCDE+132(7),=50C'-'                                           
         MVC   PCLTRTE(7),=C'CLT RTE'                                           
         MVC   PCLTRTE+132(7),=50C'-'                                           
         MVC   PESTFLR(8),=C'EST FTRS'                                          
         MVC   PESTFLR+132(8),=50C'-'                                           
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ERROR MESSAGES                                                                
***********************************************************************         
INVLFLD  MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
*                                                                               
MISSFLD  MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
*                                                                               
INVLCLI  MVI   ERROR,INVCLI                                                     
         B     TRAPERR                                                          
*                                                                               
INVLACT  MVI   ERROR,INVACT                                                     
         LA    R2,CONACTH                                                       
         B     TRAPERR                                                          
*                                                                               
SECERR   DS    0H                                                               
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(24),=C'*** SECURITY LOCKOUT ***'                         
         B     MYERR                                                            
*                                                                               
SECERR2  DS    0H                                                               
         L     R4,AIO                                                           
         USING CLTRECD,R4                                                       
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(21),=C'*** SECURITY LOCKOUT '                            
         MVI   CONHEAD+22,C'('                                                  
         GOTO1 CLUNPK,DMCB,(CPROF+6,CKEYCLT),CONHEAD+23                         
         MVI   CONHEAD+26,C')'                                                  
         MVC   CONHEAD+27(4),=C' ***'                                           
         B     MYERR                                                            
         DROP  R4                                                               
*                                                                               
PATTERR  DS    0H                                                               
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(40),=C'CANNOT DELETE - PRODUCT RECORDS ATTACHED'         
         B     MYERR                                                            
*                                                                               
ZENERR   DS    0H                                                               
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(40),=C'ERROR RECORD CANNOT BE MAINTAINED IN SFM'         
         B     MYERR                                                            
*                                                                               
CTFERR   DS    0H                                                               
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(40),=C'CODE NOT AUTHORIZED BY COORDINATING AGY '         
         B     MYERR                                                            
*                                                                               
*                                                                               
ONEERR   DS    0H                                                               
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(40),=C'1 CHARACTER OFFICE CODE IS REQUIRED     '         
         B     MYERR1                                                           
*                                                                               
TWOERR   DS    0H                                                               
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(40),=C'2 CHARACTER OFFICE CODE IS REQUIRED     '         
         B     MYERR1                                                           
*                                                                               
AOFCERR  DS    0H                                                               
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(40),=C'OFFICE CODE IS REQUIRED                 '         
         B     MYERR1                                                           
*                                                                               
OFCERR   DS    0H                                                               
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(40),=C'THIS OFFICE CODE IS NOT VALID           '         
         B     MYERR1                                                           
*                                                                               
OFCERR2  DS    0H                                                               
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(40),=C'OFFICE LIST NOT VALID FOR POSTING       '         
         B     MYERR1                                                           
*                                                                               
CMPERR   DS    0H                                                               
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(40),=C'NOT ABLE TO READ ACC COMPANY RECORD     '         
         B     MYERR1                                                           
*                                                                               
AGYERR   DS    0H                                                               
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(40),=C'ACC AGENCY CODE IS NOT VALID            '         
         B     MYERR1                                                           
*                                                                               
SYSERR   DS    0H                                                               
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(40),=C'ACC SYSTEM IS NOT PRESENTLY AVAILABLE   '         
         B     MYERR1                                                           
*                                                                               
SWERR    DS    0H                                                               
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(40),=C'CANNOT SEITCH TO ACC SYSTEM - CALL DDS  '         
         B     MYERR1                                                           
*                                                                               
FRZERR   DS    0H                                                               
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(40),=C'CANNOT RESET FREEZE UNLESS AUTHORIZED   '         
         B     MYERR                                                            
*                                                                               
MYERR1   LA    R2,CLTAONMH                                                      
*                                                                               
MYERR    GOTO1 ERREX2                                                           
*                                                                               
TRAPERR  GOTO1 ERREX                                                            
*                                                                               
ERRAGID1 MVC   ERRNUM,=AL2(1470)                                                
         B     SPERREX                                                          
ERRAGID2 MVC   ERRNUM,=AL2(1471)                                                
         B     SPERREX                                                          
*                                                                               
SPERREX  OI    GENSTAT2,USGETTXT                                                
         LA    RF,GETTXTCB                                                      
         USING GETTXTD,RF                                                       
         MVC   GTMSGNO,ERRNUM                                                   
         MVI   GTMTYP,GTMERR                                                    
         MVI   GTMSYS,2                                                         
VSFMERR  MVC   AIO,AIO1                                                         
         GOTO1 ERREX                                                            
         DROP  RF                                                               
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
*                         TABLE OF AGENCIES THAT REQUIRE INTERFACE CDS          
AGYTAB   DC    C'JW'                                                            
         DC    C'JT'                                                            
         DC    C'AR'                                                            
         DC    C'NH'                                                            
         DC    C'HC'                                                            
         DC    C'CE'                                                            
         DC    C'H9'                                                            
*                                                                               
AGYTABN  EQU   (*-AGYTAB)/2                                                     
         SPACE 2                                                                
ALPHANUM DC    C'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789',X'00'                    
         SPACE 2                                                                
*                TABLE OF PROFILE VALUES                                        
*                        LENGTH OF ENTRY,POSITION,VALUES  FF =1-9 OR            
*        TABLE ENTRIES WITH A POSITION GREATER THEN 15 GO INTO CEXTRA           
*                                                                               
PROFTAB  DC    AL1(04),AL1(01),C'-0'                           A-Z              
         DC    AL1(04),AL1(02),C'-0'                                            
         DC    AL1(04),AL1(03),C'-0'                                            
         DC    AL1(04),AL1(04),C'-0'                                            
         DC    AL1(04),AL1(05),C'-0'                                            
         DC    AL1(05),AL1(06),C'012'      BILL EST CONTROL                     
         DC    AL1(08),AL1(07),C'0123YN'   PRINT CLIENT CODE AS ANN             
         DC    AL1(04),AL1(08),C'-0'                                            
         DC    AL1(04),AL1(09),C'-0'                                            
         DC    AL1(04),AL1(10),C'-0'                                            
         DC    AL1(04),AL1(11),C'-0'                                            
         DC    AL1(04),AL1(12),C'-0'                                            
         DC    AL1(04),AL1(13),C'-0'                                            
         DC    AL1(04),AL1(14),C'-0'                                            
         DC    AL1(13),AL1(15),C'0123456789*'  CLIENT RATE CONTROL              
         DC    AL1(06),AL1(30),C'AIT0'         CLIENT RATE COVERAGE             
*                                                                               
         DC    AL1(04),AL1(16),C'-0'                                            
         DC    AL1(04),AL1(17),C'-0'                                            
         DC    AL1(04),AL1(18),C'-N'                                            
         DC    AL1(04),AL1(19),C'NY'          ESTIMATE FILTERS REQUIRED         
         DC    AL1(04),AL1(20),C'-0'                                            
         DC    AL1(04),AL1(21),C'-N'                                            
         DC    AL1(04),AL1(22),C'-N'                                            
         DC    AL1(04),AL1(23),C'-N'                                            
         DC    AL1(04),AL1(24),C'-N'                                            
         DC    AL1(04),AL1(25),C'-N'                                            
         DC    X'0000'                                                          
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
VALFLTS  NTR1  BASE=*,LABEL=*                                                   
         MVI   FILTOFC,0                                                        
         MVI   FILTFLAG,0                                                       
*                                                                               
         MVC   LCTHD2(29),=C'OFC AOF/Agy Int Code ID Title'                     
         MVC   LCTHD4(30),=C'--- ------- -------- ---------'                    
         OI    LCTHD2H+6,X'80'                                                  
         OI    LCTHD4H+6,X'80'                                                  
*                                                                               
         LA    R2,LCTFILH                                                       
         CLI   5(R2),0                                                          
         BE    VALFLTSX                                                         
*                                                                               
         GOTO1 SCANNER,DMCB,(R2),(10,BLOCK)                                     
         CLI   DMCB+4,0                                                         
         BE    INVLFLD                                                          
         ZIC   R0,DMCB+4             NUMER OF OPTIONS                           
         LA    R5,BLOCK                                                         
         USING SCAND,R5                                                         
*                                                                               
VFLT10   CLC   FLD1(3),=C'OFC'                                                  
         BNE   VFLT20                                                           
*                                                                               
         XC    TEMPFLDH,TEMPFLDH                                                
         MVI   TEMPFLDH,10                                                      
         MVC   TEMPFLDH+5(1),FLD2LEN                                            
         MVC   TEMPFLD,FLD2                                                     
*                                                                               
         LA    R2,TEMPFLDH                                                      
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB+4(4),=X'D9000A38'  GET OFFICER ADDRESS                      
         L     RF,CALLOV                                                        
         GOTO1 (RF),DMCB                                                        
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB             SAVE OFFICER ADDRESS                         
         LA    R4,DUB                                                           
         USING OFFICED,R4                                                       
         XC    0(OFCLENQ,R4),0(R4)                                              
         MVI   OFCSYS,C'S'                                                      
         MVC   OFCAUTH,T31CFFD+6                                                
         MVC   OFCLMT,T31CFFD+6                                                 
         MVC   OFCAGY,AGENCY                                                    
         MVC   OFCCLT2,BCLT                                                     
         OI    OFCINDS,OFCI2CSC                                                 
         MVC   OFCOFC2,8(R2)                                                    
         CLI   OFCOFC2+1,0                                                      
         BNE   *+8                                                              
         MVI   OFCOFC2+1,X'40'   **MAKE 2ND POSITITON SO                        
         MVC   OFCSECD,ASECBLK                                                  
***      LH    R0,=Y(SVSECRET-T31CFFD)                                          
***      AR    R0,RA                                                            
***      ST    R0,OFCSECD                                                       
*                                                                               
         GOTO1 (RF),DMCB,(C'2',OFFICED),(0,ACOMFACS)                            
         CLI   0(R1),0                                                          
         BE    *+12                                                             
         LA    R2,LCTFILH                                                       
         B     INVLFLD                                                          
         MVC   FILTOFC,OFCOFC      SAVE OFF 1 BYTE INTERNAL OFFICE CODE         
         J     VFLTNXT                                                          
         DROP  R4                                                               
*                                                                               
VFLT20   CLC   FLD1(3),=C'FRZ'                                                  
         BNE   INVLFLD                                                          
         OI    FILTFLAG,FILTFRZQ                                                
         MVC   LCTHD2(29),=C'Frozen                       '                     
         MVC   LCTHD4(30),=C'------                        '                    
         OI    LCTHD2H+6,X'80'                                                  
         OI    LCTHD4H+6,X'80'                                                  
*                                                                               
         CLI   FLD2,C'Y'                                                        
         JNE   *+12                                                             
         OI    FILTFLAG,FILTFRYQ                                                
         J     VFLTNXT                                                          
*                                                                               
         CLI   FLD2,C'N'                                                        
         JNE   *+12                                                             
         OI    FILTFLAG,FILTFRNQ                                                
         J     VFLTNXT                                                          
*                                                                               
VFLTNXT  AHI   R5,32                                                            
         BCT   R0,VFLT10                                                        
*                                                                               
VALFLTSX DS    0H                                                               
         J     EXIT                                                             
***********************************************************************         
* CLIENT LIST FILTER                                                            
***********************************************************************         
TSTFILT  NTR1  BASE=*,LABEL=*                                                   
         L     R6,AIO                                                           
         USING CLTRECD,R6                                                       
         CLI   FILTFLAG,0                                                       
         JE    TFILT12                                                          
*                                                                               
         TM    FILTFLAG,FILTFRYQ   FROZEN CLIENTS?                              
         JZ    *+16                                                             
         TM    COPT2,COP2FRZ       FROZEN?                                      
         JZ    NO                                                               
         J     TFILT12                                                          
*                                                                               
         TM    FILTFLAG,FILTFRNQ   NON-FROZEN CLIENTS?                          
         JZ    TFILT12                                                          
         TM    COPT2,COP2FRZ       FROZEN?                                      
         JO    NO                                                               
*                                                                               
TFILT12  CLI   FILTOFC,0                                                        
         JE    TFILT15                                                          
         CLC   COFFICE,FILTOFC                                                  
         JNE   NO                                                               
*                                                                               
TFILT15  CLC   PREVCLT,CKEYCLT                                                  
         JE    YES                                                              
         MVC   PREVCLT,CKEYCLT                                                  
*                                                                               
         GOTO1 VLMTDACC,DMCB,CKEYAM,CKEYCLT                                     
         CLI   DMCB+4,X'FF'                                                     
         JE    NO                                                               
*                                                                               
TSTFILTX J     YES                                                              
         DROP  R6                                                               
         LTORG                                                                  
***********************************************************************         
* VALIDATE BUYING AGENCY IDENTIFIER                                             
***********************************************************************         
VALBAID  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R2,CLTBAIDH         BUYING AGENCY IDENTIFIER FIELD               
         CLI   ACTNUM,ACTADD       ACTION ADD?                                  
         BE    *+12                ALWAYS VALIDATE ON ADD                       
         TM    4(R2),X'20'         HAS FIELD CHANGED                            
         JO    EXIT                NO, THEN EXIT                                
*                                                                               
         USING CLTRECD,R6          CLIENT RECORD DSECT                          
         XC    CBUYAGIN,CBUYAGIN   CLEAR BUYING AGENCY IDENTIFIER               
*                                                                               
         CLI   5(R2),0             ANY INPUT?                                   
         JE    EXIT                NO - DONE                                    
         CLI   5(R2),3             INPUT LENGTH OF 3?                           
         JNE   ERRAGID1            NO - ERROR                                   
*                                                                               
         XC    KEY1,KEY1           CLEAR CONTROL FILE KEY                       
         LA    R3,KEY1             R3 = KEY1                                    
         USING BAGRECD,R3          BUYING AGENCY RECORD DSECT                   
         MVI   BAGKMIN,BAGKMINQ    MINOR SYSTEM 'T' (FOR TRAFFIC)               
         MVI   BAGKTYP,BAGKTYPQ    BUYING AGENCY RECORD                         
         MVC   BAGKAGY,AGENCY      AGENCY ALPHA                                 
         MVC   BAGKBAGY,8(R2)      BUYING AGENCY IDENTIFIER                     
         MVC   KEY2,KEY1           KEYSAVE                                      
         DROP  R3                  DROP BUYING AGENCY RECORD USING              
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI'),=C'GENDIR',KEY1,KEY1                 
         CLI   DMCB+8,0            ANY ERRORS?                                  
         BE    *+6                 NO                                           
         DC    H'0'                YES - NO ERRORS TOLERATED                    
*                                                                               
         CLC   KEY2(32),KEY1       KEY = KEYSAVE?                               
         JNE   ERRAGID2            NO - THIS DOESN'T EXIST - ERROR              
*                                                                               
         MVC   CBUYAGIN,8(R2)      MOVE BUYING AGY ID TO CLIENT RECORD          
         J     EXIT                DONE                                         
         DROP  R6                  DROP CLIENT RECORD USING                     
         LTORG                                                                  
***********************************************************************         
* CHECK TRAFFIC OFFICE NUMBER                                                   
***********************************************************************         
CHKTOFF  NTR1  BASE=*,LABEL=*                                                   
         L     R4,AIO                                                           
         USING CLTRECD,R4                                                       
*                                                                               
         MVI   CTRAFOFC,0                                                       
         CLI   5(R2),0                                                          
         BE    CHKTOFFX                                                         
*                                                                               
         CLI   8(R2),C'='          INVALID OFFICE NUMBERS                       
         BE    INVLFLD                                                          
         CLI   8(R2),C','                                                       
         BE    INVLFLD                                                          
         CLI   8(R2),C'-'                                                       
         BE    INVLFLD                                                          
*                                                                               
         MVC   CTRAFOFC,8(R2)      VALID TRAFFIC OFFICE CODE                    
*                                                                               
CHKTOFFX XIT1                                                                   
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DISPLAY EDI INFO ONTO CLIENT SCREEN                                           
***********************************************************************         
EDIDISP  NTR1  BASE=*,LABEL=*                                                   
         L     R4,AIO1                                                          
         USING CLTRECD,R4                                                       
*                                                                               
         MVC   0(4,R2),=CL4'EDI='                                               
         LA    R2,4(R2)                                                         
         MVC   0(3,R2),CLEDICLT                                                 
         LA    R2,2(R2)                                                         
         CLI   0(R2),X'40'                                                      
         BE    EDIDET30                                                         
         LA    R2,1(R2)                                                         
*                                                                               
EDIDET30 MVI   0(R2),C'/'           CHECK IF FIELD BREAK                        
         LA    R2,1(R2)                                                         
*                                                                               
*  READ CONTROL FILE TO VALIDATE AND STORE USER ID ON CLIENT RECORD             
*                                                                               
         XC    SVKEY,SVKEY                                                      
         LA    R6,SVKEY                                                         
         USING CTIREC,R6                                                        
         MVI   CTIKTYP,CTIKTYPQ    RECORD TYPE 'I'                              
         MVC   CTIKNUM,CLEDIUID                                                 
         GOTO1 DATAMGR,DMCB,(0,=C'DMREAD'),=C'CTFILE',SVKEY,AIO2                
         CLI   8(R1),0             ERRORS?                                      
         BE    *+6                                                              
         DC    H'0'                RECORD MUST BE THERE                         
         DROP  R6                                                               
*                                                                               
         MVC   HALF,DATADISP       SAVE CURRENT ELEMENT OFFSET                  
         MVC   DATADISP,=H'28'     FIND SE NUMBER FOR SPECIFIED                 
         USING CTDSCD,R6                                                        
         L     R6,AIO2                                                          
         MVI   ELCODE,X'02'        GET ID DESCRIPTION ELEMENT                   
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         ZIC   R1,CTDSCLEN                                                      
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),CTDSC                                                    
         MVC   DATADISP,HALF       RESTORE CURRENT ELEMENT OFFSET               
*  FIND FIRST BLANK SPACE TO PUT COMMA                                          
         LA    RE,10                                                            
EDIDET50 CLI   0(R2),X'40'                                                      
         BNH   EDIDET60                                                         
         LA    R2,1(R2)                                                         
         BCT   RE,EDIDET50                                                      
*                                                                               
EDIDET60 MVI   0(R2),C','                                                       
         LA    R2,1(R2)                                                         
         ST    R2,REGHOLD          SAVE R2 LOCATION                             
*                                                                               
EDIDETEX XIT1                                                                   
         DROP  R4,R6                                                            
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* MOVE EDI INFO INTO CLIENT RECORD                                              
***********************************************************************         
EDISET   NTR1  BASE=*,LABEL=*                                                   
         L     R4,AIO1                                                          
         USING CLTRECD,R4                                                       
         USING SCAND,R5                                                         
         L     RF,ACOMFACS                                                      
         MVC   VSWITCH,CSWITCH-COMFACSD(RF)                                     
* GET CURRENT SSE NUMBER                                                        
         GOTO1 VSWITCH,DMCB,(X'FF',X'FFFFFFFF'),0                               
         L     R1,0(R1)                                                         
         MVC   CURSSN,TSYS-UTLD(R1)                                             
*                                                                               
         XC    WORK,WORK                                                        
         ZIC   RE,FLD2LEN                                                       
         LA    R6,WORK                                                          
         LA    R1,FLD2                                                          
*                                                                               
EDIST10  CLI   0(R1),C'/'           CHECK IF FIELD BREAK                        
         BNE   EDIST20                                                          
         LA    R6,WORK+5            SET UP TO USERID AREA                       
         B     EDIST30                                                          
EDIST20  MVC   0(1,R6),0(R1)                                                    
         LA    R6,1(R6)                                                         
EDIST30  LA    R1,1(R1)                                                         
         BCT   RE,EDIST10                                                       
*                                                                               
*  CONVERT THE EDI DATA                                                         
*  WORK BYTES 1-3  CONTAINS THE RECEIVING AGENCY CLIENT CODE                    
*  WORK BYTES 6-15 CONTAINS THE RECEIVING AGENCY USER ID                        
*                                                                               
         MVC   CLEDICLT,WORK        MOVE EDI CLIENT TO RECORD                   
         OI    CLEDICLT+2,X'40'                                                 
*                                                                               
*                                                                               
*  READ CONTROL FILE TO VALIDATE AND STORE USER ID ON CLIENT RECORD             
*                                                                               
         XC    SVKEY,SVKEY                                                      
         LA    R6,SVKEY                                                         
         USING CTIREC,R6                                                        
         MVI   CTIKTYP,CTIKTYPQ    RECORD TYPE 'I'                              
         MVC   CTIKID,WORK+5                                                    
         OC    CTIKID,SPACES                                                    
         PRINT GEN                                                              
         GOTO1 DATAMGR,DMCB,(0,=C'DMREAD'),=C'CTFILE',SVKEY,AIO2                
         PRINT NOGEN                                                            
         CLI   8(R1),0             ERRORS?                                      
         BNE   INVLFLD                                                          
         DROP  R6                                                               
*                                                                               
         MVC   HALF,DATADISP       SAVE CURRENT ELEMENT OFFSET                  
         MVC   DATADISP,=H'28'     FIND SE NUMBER FOR SPECIFIED                 
*                                                                               
         USING CTAGYD,R6                                                        
         L     R6,AIO2                                                          
         MVI   ELCODE,X'06'        GET ID DESCRIPTION ELEMENT                   
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
***      LIST OF VALID AGENCYS TO SEND DATA TO                                  
*                                                                               
         CLC   CTAGYID,=CL2'SJ'    SJR                                          
         BE    EDIST90                                                          
         CLC   CTAGYID,=CL2'QA'    SJRBLD                                       
         BE    EDIST90                                                          
         CLC   CTAGYID,=CL2'S5'    STW                                          
         BE    EDIST90                                                          
         CLC   CTAGYID,=CL2'Q5'    STWBLD                                       
         BE    EDIST90                                                          
         CLC   CTAGYID,=CL2'*B'    DDSB                                         
         BE    EDIST90                                                          
         CLC   CTAGYID,=CL2'OO'    OMD                                          
         BE    EDIST90                                                          
         CLC   CTAGYID,=CL2'UB'    CARNY                                        
         BE    EDIST90                                                          
         CLC   CTAGYID,=CL2'BN'    PHDNY                                        
         BNE   INVLFLD                                                          
         DROP  R6                                                               
*                                                                               
EDIST90  DS    0H                                                               
         USING CTDSCD,R6                                                        
         L     R6,AIO2                                                          
         MVI   ELCODE,X'02'        GET ID DESCRIPTION ELEMENT                   
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   CLEDIUID,CTDSC      SAVE USID # ON CLIENT RECORD                 
         DROP  R6                                                               
         MVC   DATADISP,HALF       RESTORE ELEMENT OFFSET                       
*******  B     EDISTEX                                                          
*                                                                               
*  GET SE NUMBER, AND AGENCY NUMBER FOR EDI AGENCY                              
*                                                                               
         USING CTSYSD,R6                                                        
         L     R6,AIO2                                                          
         MVI   ELCODE,X'21'                                                     
         MVC   DATADISP,=H'28'     FIND SE NUMBER FOR SPECIFIED                 
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
EDIST110 BRAS  RE,NEXTEL                                                        
         BNE   AGYERR              ERROR IF NOT FOUND                           
         CLI   CTSYSEL,X'21'       STILL X'21' EL                               
         BNE   AGYERR              ERROR IF NOT FOUND                           
         CLI   CTSYSNUM,X'03'      ACC??                                        
         BNE   EDIST110                                                         
         MVC   EDISSN,CTSYSSE                                                   
         MVC   EDIAGY,CTSYSAGB                                                  
         OI    EDIAGY,X'03'                                                     
*                                                                               
         MVC   DATADISP,HALF       RESTORE ELEMENT OFFSET                       
*                                                                               
*  VALIDATE THAT THE EDI CLIENT EXISTS ON THE RECEIVING AGENCYS FILE            
*                                                                               
         XC    DMCB(8),DMCB        YES                                          
         MVC   DMCB(1),EDISSN      SE NUM                                       
         MVC   DMCB+1(3),=XL3'FFFFFF'                                           
         GOTO1 VSWITCH,DMCB        SWITCH TO EDI AGENCY                         
         CLI   4(R1),2             SYSTEM NOT OPEN                              
         BE    SYSERR                                                           
         CLI   4(R1),1             ANY OTHER ERRORS?                            
         BE    SWERR                                                            
*                                                                               
*  READ CLIENT RECORD FOR EDI AGENCY                                            
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),EDIAGY                                                  
         GOTO1 CLPACK,DMCB,CLEDICLT,KEY+2                                       
***      GOTO1 HIGH                                                             
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,=CL8'DMRDHI',=CL8'SPTDIR',KEY,KEY,0,0               
         CLC   KEY(13),KEYSAVE                                                  
         BNE   INVLFLD                                                          
*                                                                               
*  SWITCH BACK TO LIVE AGENCY                                                   
*                                                                               
         XC    DMCB(8),DMCB        YES                                          
         MVC   DMCB(1),CURSSN      SE NUM                                       
         MVC   DMCB+1(3),=XL3'FFFFFF'                                           
         GOTO1 VSWITCH,DMCB        SWITCH TO CURRENT AGENCY                     
         CLI   4(R1),1             ANY OTHER ERRORS?                            
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
*  RESET THE POINTER                                                            
*                                                                               
         MVC   KEY,HOLDKEY         RESTORE KEY                                  
         GOTO1 DATAMGR,DMCB,=CL8'DMRDHI',=CL8'SPTDIR',KEY,KEY,0,0               
*                                                                               
EDISTEX  XIT1                                                                   
         DROP  R4,R6                                                            
         LTORG                                                                  
         EJECT                                                                  
PLINED   DSECT                                                                  
PCLT     DS    CL3                                                              
         DS    CL2                                                              
PNME     DS    CL20                                                             
         DS    CL2                                                              
POFFICES DS    CL2                                                              
         DS    CL2                                                              
PACCOFF  DS    CL2                                                              
PACCAG   DS    CL3                                                              
         DS    CL2                                                              
PINTER   DS    CL8                                                              
         DS    CL1                                                              
PTITLE   DS    CL10                                                             
         DS    CL1                                                              
PBILL    DS    CL8                                                              
         DS    CL1                                                              
PCLTCDE  DS    CL7                                                              
         DS    CL1                                                              
PCLTRTE  DS    CL7                                                              
         DS    CL1                                                              
PESTFLR  DS    CL8                                                              
PLENGTH  EQU   *-PCLT                                                           
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE FAUTL                                                          
         EJECT                                                                  
       ++INCLUDE NESFMFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE NESFMC1D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE NESFMC2D                                                       
       ++INCLUDE DDGENTWA                                                       
*                                                                               
*  SECOND SAVE STORAGE AREA                                                     
*                                                                               
         ORG   CONHEADH+BASETWA2                                                
SVAREA   DS    0H                                                               
SVSECRET DS    CL1024                                                           
SVOFFBLK DS    CL100               OFICCER BLOCK                                
         SPACE 3                                                                
*DSECT TO COVER SAVED STORAGE IN TWA0 FOR NESFM00                               
T31CFFD  DSECT                                                                  
SAVAREAL EQU   ((CONHEADH+3520)-(T31CFFD+3072))                                 
         ORG   CONHEADH+3520-SAVAREAL                                           
SAVAREA  DS    0C                                                               
CALLSP   DS    X                   CALL ROUTINE STACK POINTER                   
CALLSTK  DS    XL9                 STACK (LIST OF OVERLAYS)                     
LASTOV   DS    X                   LAST OVERLAY                                 
SVLIST   DS    XL188               LISTDIR SAVE AREA                            
AUTHCODE DS    H                                                                
*                                                                               
         EJECT                                                                  
       ++INCLUDE NESFMWORKD                                                     
         ORG   SYSSPARE                                                         
*                           *******  T31C01 WORK AREA  *******                  
WORKAREA DS    0CL1                                                             
MYDMWRK  DS    12D                                                              
PREVFLAG DS    CL1                                                              
PREVKEY  DS    CL48                                                             
REC      DS    CL150               FOR TURNAROUND REPORT                        
*                                                                               
PREVCLT  DS    XL2                                                              
*                                                                               
SYSSW    DS    CL1                                                              
SWDSYS   DS    CL1                                                              
POWCODE  DS    CL2                                                              
ACCOFF   DS    CL2                                                              
OFFLEN   DS    CL1                                                              
COMPCD   DS    CL1                                                              
OLDCOPT2 DS    CL1                                                              
GTFACTB  DS    CL88                                                             
*                                                                               
TEMPFLDH DS    XL8                                                              
TEMPFLD  DS    XL2                                                              
*                                                                               
FILTOFC  DS    XL1                                                              
FILTFLAG DS    XL1                                                              
FILTFRZQ EQU   X'01'               SHOW FROZEN COLUMN?                          
FILTFRYQ EQU   X'02'               FROZEN CLIENTS?                              
FILTFRNQ EQU   X'04'               NON-FROZEN CLIENTS?                          
*                                                                               
SAVEKEY  DS    XL13                                                             
HOLDKEY  DS    XL13                                                             
*                                                                               
CGRPASS  DS    XL50                                                             
*                                                                               
REGHOLD  DS    F                                                                
VSWITCH  DS    V                                                                
*                                                                               
CURSSN   DS   XL1                   SSN NUMBER FOR ACTIVE AGENCY                
EDISSN   DS   XL1                   SSN NUMBER FOR EDI AGENCY                   
EDIAGY   DS   XL1                                                               
ERRNUM   DS   XL2                                                               
KEY1     DS   XL48                                                              
KEY2     DS   XL48                                                              
*                                                                               
WORKEND  EQU   *                                                                
         EJECT                                                                  
       ++INCLUDE DDOFFICED                                                      
CLTRECD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE SPGENCLG                                                       
       ++INCLUDE SPGENAGY                                                       
PRDRECD  DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
       ++INCLUDE GEGENBAG                                                       
       ++INCLUDE FAGETTXTD                                                      
*                                                                               
SCAND    DSECT                                                                  
*         DSECT TO COVER SCANNER LINES                                          
FLD1LEN  DS    CL1                                                              
FLD2LEN  DS    CL1                                                              
FLD1VAL  DS    CL1                                                              
FLD2VAL  DS    CL1                                                              
FLD1B    DS    CL4                                                              
FLD2B    DS    CL4                                                              
FLD1     DS    CL10                                                             
FLD2     DS    CL14                                                             
*                                                                               
         PRINT GEN                                                              
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'145NESFM01   05/12/20'                                      
         END                                                                    
