*          DATA SET TAGEN1E    AT LEVEL 094 AS OF 09/04/15                      
*PHASE T7021EE                                                                  
         TITLE 'T7021E - AGENT MAINTENANCE'                                     
T7021E   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T7021E                                                         
         L     RC,0(R1)            RC=GENCON STORAGE AREA                       
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(TWA)                                    
         USING T702FFD,RA                                                       
         L     R9,ASYSD            R9=ROOT STORAGE AREA                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD          R8=SPOOL DSECT                               
         USING SPOOLD,R8                                                        
         SPACE 2                                                                
         LA    RE,TWAHOLE                                                       
         AHI   RE,750                                                           
         ST    RE,ASVPBLK                                                       
         AHI   RE,31*L'TLDRREC+1                                                
         ST    RE,AUPPBLK                                                       
         EJECT                                                                  
*              MODE CONTROLLED ROUTINES                                         
         SPACE 1                                                                
         GOTO1 INITIAL,DMCB,PFTABLE                                             
         MVC   SANSHED(9),=C'Pid Num  '                                         
         OI    SANSHEDH+6,X'80'                                                 
         CLI   MODE,VALKEY                                                      
         BNE   AGT10                                                            
VK10     CLI   ACTNUM,ACTADD       IF ADDING                                    
         BNE   VK20                                                             
*                                                                               
         MVC   AIO,AIO2            GET SYSTEM REC TO DISPLAY NEXT AGENT         
         GOTO1 RECVAL,DMCB,TLSYCDQ,(X'B4',TWAAGY)    NUMBER                     
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R4,AIO              GET SYSTEM ELEMENT                           
         USING TASYD,R4                                                         
         MVI   ELCODE,TASYELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XR    R1,R1               GET LAST AGENT NUMBER AND INCREMENT          
         ICM   R1,3,TASYLAGT           IT                                       
         LA    R1,1(R1)                                                         
         STCM  R1,3,SVLAGT                                                      
         MVC   AIO,AIO1                                                         
*                                                                               
         GOTO1 TRNSAGT,DMCB,(X'40',SVLAGT),SANAGT                               
         OI    SANAGTH+6,X'80'     TRANSMIT FIELD                               
         MVI   SANAGTH+5,4                                                      
         DROP  R4                                                               
*                                                                               
VK20     CLI   ACTNUM,ACTDIS        DISPLAY BY DPS OR T&R CODE                  
         BNE   VK30                                                             
         GOTO1 RECVAL,DMCB,TLANCCDQ,(X'40',SANAGTH)                             
         B     AGTX                                                             
*                                                                               
VK30     GOTO1 RECVAL,DMCB,TLANCDQ,(X'40',SANAGTH)                              
         B     AGTX                                                             
*                                                                               
AGT10    CLI   THISLSEL,C'D'       IF DELETING FROM LIST DON'T                  
         BE    AGT15                  DISPLAY REC FIRST                         
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    AGT30                                                            
*                                                                               
AGT15    CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         CLI   MODE,RECDEL         DELETE RECORD                                
         BE    AGT40                                                            
         CLI   MODE,RECREST        RESTORE RECORD                               
         BE    AGT40                                                            
         CLI   MODE,XRECDEL        RECORD DELETED                               
         BE    AGT20                                                            
         CLI   MODE,XRECREST       RECORD RESTORE                               
         BE    AGT20                                                            
         CLI   MODE,XRECADD        OR NEW RECORD ADDED                          
         BE    AGT20                                                            
         CLI   MODE,XRECPUT        OR RECORD CHANGED                            
         BNE   AGT50                                                            
         EJECT                                                                  
*                                                                               
AGT20    GOTO1 ADDPTRS,DMCB,(8,ASVPBLK),AUPPBLK                                 
*                                                                               
AGT30    BAS   RE,DISPLAY          (RE-)DISPLAY THE RECORD                      
         B     AGTX                                                             
*                                                                               
AGT40    GOTO1 SAVPTRS,DMCB,ASVPBLK HANDLE PASSIVE POINTERS                     
         CLI   MODE,RECDEL          DELETE RECORD                               
         BNE   AGTX                                                             
         BAS   RE,CHKDEL                                                        
         B     AGTX                                                             
*                                                                               
AGT50    CLI   MODE,VALREC         VALIDATE THE RECORD                          
         BNE   AGTX                                                             
         BAS   RE,BLDREC                                                        
         MVI   SAMESCR,C'N'        SET FLAG NEW RECORD CAN BE ADDED             
*                                                                               
AGTX     B     XIT                                                              
*                                                                               
         SPACE 2                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
*                                                                               
*              DISPLAY THE KEY                                                  
*                                                                               
         SPACE 2                                                                
DK       L     R4,AIO              ADDRESS OF RECORD                            
         USING TLAND,R4                                                         
         MVC   SANAGT,TLANAGT      AGENT NUMBER                                 
         OI    SANAGTH+6,X'80'                                                  
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
*              DISPLAY THE RECORD                                               
*                                                                               
DISPLAY  NTR1                                                                   
         TWAXC SANAGTNH                                                         
         XC    SANSSNN,SANSSNN                                                  
         OI    SANSSNNH+6,X'80'                                                 
         CLI   MODE,XRECDEL        IF RECORD IS DELETED GET AGENT               
         BNE   DR5                                                              
         USING TLANPD,R4           CODE FROM KEY                                
         LA    R4,KEY                                                           
         MVC   SANAGT,TLANBAGT                                                  
         B     DR7                                                              
         DROP  R4                                                               
*                                                                               
         USING TLAND,R4                                                         
DR5      L     R4,AIO                                                           
         MVC   SANAGT,TLANAGT      TP AGENT NUMBER                              
         DROP  R4                                                               
*                                                                               
DR7      OI    SANAGTH+6,X'80'                                                  
*                                                                               
         GOTO1 CHAROUT,DMCB,TANAELQ,SANAGTNH       AGENT NAME                   
         GOTO1 (RF),(R1),TASNELQ,SANNAMEH          SHORT NAME                   
         GOTO1 (RF),(R1),TAADELQ,(4,SANADDRH)      AGENT ADDRESS                
         GOTO1 (RF),(R1),TAFNELQ,SANANAMH,TAFNTATT ATTENTION NAME               
         GOTO1 (RF),(R1),TANUELQ,0,TANUTFAX        FAX NUMBER                   
         BNE   DR8                                                              
         MVC   SANFAXA,TGNAME      MOVE TO SCREEN                               
         MVC   SANFAX1,TGNAME+3                                                 
         MVC   SANFAX2,TGNAME+6                                                 
*                                                                               
DR8      GOTO1 CHAROUT,DMCB,TACMELQ,SANEADDH,TACMTYPI                           
*                                                                               
         MVI   ELCODE,TAANELQ      AGENT ELEMENT                                
         L     R4,AIO                                                           
         USING TAAND,R4                                                         
         BAS   RE,GETEL                                                         
         BNE   DR90                                                             
         OC    TAANSSN,TAANSSN     IF THERE'S A SS NUMBER                       
         BZ    DR80                                                             
         MVC   SANSSN,TAANSSN      DISPLAY IT AND GET NAME                      
         MVC   AIO,AIO2            DON'T CREAM RECORD                           
         GOTO1 RECVAL,DMCB,TLW4CDQ,(X'88',TAANSSN),SANSSNNH                     
         MVC   AIO,AIO1                                                         
         GOTO1 SSNPACK,DMCB,TGSSN,TGPID                                         
         MVC   SANSSN,SPACES                                                    
         MVC   SANSSN(L'TGPID),TGPID                                            
         MVI   SANSSNH+5,6                                                      
         OI    SANSSNH+6,X'80'                                                  
*                                                                               
DR80     MVC   SANTNR,TAANTNR      OLD T & R CODE                               
         OI    SANTNRH+4,X'20'     SET VALIDATED                                
         MVC   SANAREA,TAANTEL     AREA CODE                                    
         MVC   SANTELE,TAANTEL+3   TELEPHONE NUMBER                             
         MVC   SANTEL2,TAANTEL+6                                                
*                                                                               
         TM    TAANSTAT,TAANSNCK   IGNORE ON SAG CHS                            
         BNO   DR90                                                             
         MVI   SANSAG,C'Y'                                                      
*                                                                               
DR90     TM    TAANSTAT,TAANSOVR   ALLOWABLE OVERRIDE                           
         BNO   DR100                                                            
         MVI   SANALOV,C'Y'                                                     
*                                                                               
DR100    GOTO1 ACTVOUT,DMCB,SANLCHGH               LAST CHANGED                 
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
*              BUILD THE RECORD                                                 
*                                                                               
BLDREC   NTR1                                                                   
         GOTO1 SAVPTRS,DMCB,ASVPBLK HANDLE PASSIVE POINTERS                     
         MVI   ELCODE,TAANELQ                                                   
         GOTO1 REMELEM                                                          
         GOTO1 NAMIN,DMCB,TANAELQ,SANAGTNH         AGENT NAME                   
         GOTO1 NAMIN,DMCB,TASNELQ,(X'80',SANNAMEH) SHORT NAME                   
         GOTO1 ADDRIN,DMCB,(X'80',SANADDRH)        AGENT ADDRESS                
         GOTO1 NAMIN,DMCB,TACMELQ,(X'80',SANEADDH),TACMTYPI                     
         GOTO1 NAMIN,DMCB,TAFNELQ,(X'80',SANANAMH),TAFNTATT                     
*                                                                               
         XC    DUB,DUB             BUILD DUMMY FAX NUMBER FIELD                 
         MVI   DUB,18                                                           
         MVI   DUB+5,10                                                         
         MVC   DUB+8(3),SANFAXA                                                 
         MVC   DUB+11(3),SANFAX1                                                
         MVC   DUB+14(4),SANFAX2                                                
         OC    DUB+8(10),DUB+8     IF NO INPUT                                  
         BNZ   BLD0A                                                            
         MVI   DUB+5,0             SET INPUT LENGTH TO 0 TO DELETE OLD          
         B     BLD0B                                                            
BLD0A    GOTO1 VALPHONE,DMCB,(L'SANFAXA,SANFAXAH)                               
         GOTO1 VALPHONE,DMCB,(L'SANFAX1,SANFAX1H)                               
         GOTO1 VALPHONE,DMCB,(L'SANFAX2,SANFAX2H)                               
BLD0B    GOTO1 NAMIN,DMCB,TANUELQ,(X'80',DUB),TANUTFAX                          
*                                                                               
         XC    ELEMENT,ELEMENT                                                  
         USING TAAND,R4                                                         
         LA    R4,ELEMENT                                                       
         MVI   TAANEL,TAANELQ      ELEM CODE                                    
         MVI   TAANLEN,TAANLNQ     ELEM LENGTH                                  
*                                                                               
*                                                                               
BLD10    CLI   SANTNRH+5,0                                                      
         BE    BLD25                                                            
         LA    R2,SANTNRH                                                       
         CLI   ACTNUM,ACTADD       IF ADDING                                    
         BE    BLD15               ALWAYS VALIDATE                              
*                                                                               
         TM    4(R2),X'20'         TEST NOT PREV VALIDATED                      
         BO    BLD20                                                            
BLD15    CLI   5(R2),4                                                          
         BNE   INVERR                                                           
         CLC   SANTNR,=C'0000'      MUST BE 4 CHAR NUMERIC                      
         BNH   INVERR                                                           
         MVC   AIO,AIO2            DON'T CREAM RECORD                           
         GOTO1 RECVAL,DMCB,TLANCCDQ,(X'06',0(R2))  IF ALREADY HAVE              
         MVC   AIO,AIO1                                                         
         BE    RECFND                                                           
*                                                                               
BLD20    MVC   TAANTNR,SANTNR                                                   
*                                                                               
BLD25    CLI   SANSSNH+5,0         TEST FOR SSN INPUT                           
         BE    BLD30                                                            
         MVC   AIO,AIO2            DON'T CREAM RECORD                           
         CLI   SANSSNH+5,6                                                      
         BH    BLD26                                                            
         MVC   TGPID,SANSSN                                                     
         GOTO1 SSNUNPK,DMCB,TGPID,TGSSN                                         
         BNE   BLD26                                                            
         MVC   SANSSN,TGSSN                                                     
         MVI   SANSSNH+5,9                                                      
BLD26    GOTO1 RECVAL,DMCB,TLW4CDQ,(X'08',SANSSNH),SANSSNNH                     
*                                                                               
         USING TAW4D,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TAW4ELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   *+12                                                             
         CLI   TAW4TYPE,TAW4TYCO   MUST BE CORPORATION                          
         BNE   MUSTBEC                                                          
*                                                                               
         USING TAAND,R4                                                         
         LA    R4,ELEMENT                                                       
         MVC   AIO,AIO1                                                         
         MVC   TAANSSN,SANSSN      SAVE IN ELEMENT                              
         GOTO1 SSNPACK,DMCB,TGSSN,TGPID                                         
         MVC   SANSSN,SPACES                                                    
         MVC   SANSSN(L'TGPID),TGPID                                            
         MVI   SANSSNH+5,6                                                      
         OI    SANSSNH+6,X'80'                                                  
*                                                                               
BLD30    CLI   SANAREAH+5,0        AREA CODE                                    
         BE    BLD40                                                            
         GOTO1 VALPHONE,DMCB,(L'SANAREA,SANAREAH)                               
         MVC   TAANTEL(3),SANAREA                                               
                                                                                
BLD40    CLI   SANTELEH+5,0        ANY INPUT                                    
         BE    BLD50                                                            
         GOTO1 VALPHONE,DMCB,(L'SANTELE,SANTELEH)                               
         GOTO1 VALPHONE,DMCB,(L'SANTEL2,SANTEL2H)                               
         MVC   TAANTEL+3(3),SANTELE                                             
         MVC   TAANTEL+6(4),SANTEL2                                             
*                                                                               
BLD50    LA    R2,SANSAGH          IGNORE ON SAG CHS                            
         CLI   5(R2),0             ANY INPUT                                    
         BE    BLD60                                                            
         CLI   8(R2),C'Y'                                                       
         BNE   BLD60                                                            
         OI    TAANSTAT,TAANSNCK   STATUS                                       
*                                                                               
BLD60    LA    R2,SANALOVH         ALLOWABLE OVERRIDE                           
         CLI   5(R2),0             ANY INPUT                                    
         BE    BLD70                                                            
         CLI   8(R2),C'Y'                                                       
         BNE   BLD70                                                            
         OI    TAANSTAT,TAANSOVR   STATUS                                       
*                                                                               
BLD70    GOTO1 ADDELEM                                                          
         GOTO1 ACTVIN,DMCB,SANLCHGH                LAST CHANGED                 
*                                                                               
         CLI   ACTNUM,ACTADD       IF ADDING                                    
         BE    BLD80               GO UPDATE SYSTEM REC                         
*                                                                               
         MVC   AIO,AIO2            ELSE DON'T CREAM NEW REOCRD                  
         GOTO1 RECVAL,DMCB,TLANCDQ,(X'30',SANAGTH)  READ OLD FOR UPDATE         
         MVC   AIO,AIO1                                                         
         B     BLDX                                                             
*                                                                               
BLD80    MVC   AIO,AIO2            GET SYSTEM REC TO UPDATE LAST AGENT          
         GOTO1 RECVAL,DMCB,TLSYCDQ,(X'B4',TWAAGY)    NUMBER                     
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R4,AIO              GET SYSTEM ELEMENT                           
         USING TASYD,R4                                                         
         MVI   ELCODE,TASYELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XR    R1,R1               GET LAST AGENT NUMBER AND INCREMENT          
         ICM   R1,3,TASYLAGT           IT                                       
         LA    R1,1(R1)                                                         
         STCM  R1,3,TASYLAGT                                                    
         STCM  R1,3,SVLAGT                                                      
         GOTO1 PUTREC              PUT BACK SYSTEM RECORD                       
         MVC   AIO,AIO1                                                         
         GOTO1 TRNSAGT,DMCB,(X'40',SVLAGT),SANAGT                               
         OI    SANAGTH+6,X'80'     RE-TRANSMIT FIELD                            
         MVI   SANAGTH+5,4                                                      
         DROP  R4                                                               
         GOTO1 RECVAL,DMCB,TLANCDQ,(X'40',SANAGTH)   REBUILD KEY                
         L     R4,AIO                                                           
         MVC   0(L'TLDRKEY,R4),KEY  RESET KEY IN AIO                            
*                                                                               
BLDX     B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO VALIDATE PHONE NUMBER FIELD                       *         
*        ON ENTRY ... P1 BYTE 0 = L'PHONE NUMBER FIELD                *         
*                     P1        = A(PHONE NUMBER FIELD)               *         
***********************************************************************         
                                                                                
VALPHONE NTR1                                                                   
         J     XIT                                                              
                                                                                
         ZICM  R2,1(R1),3          R2=A(PHONE NUMBER HEADER FIELD)              
         ZIC   R0,0(R1)            R0=L'PHONE NUMBER FIELD                      
                                                                                
         LA    R1,8(R2)            R1=A(PHONE NUMBER FIELD)                     
         LR    R3,R0                                                            
         SHI   R3,1                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         OC    0(0,R1),SPACES      PAD PHONE NUMBER FIELD WITH SPACES           
                                                                                
VP10     CLI   0(R1),C' '          ENSURE FIELD ONLY CONTAINS SPACES            
         JE    VP20                                                             
         CLI   0(R1),C'0'          NUMBERS BETWEEN 0                            
         JL    INVERR                                                           
         CLI   0(R1),C'9'          AND 9                                        
         JH    INVERR                                                           
VP20     LA    R1,1(R1)                                                         
         BCT   R0,VP10                                                          
         J     XIT                                                              
                                                                                
*        ROUTINE CHECKS THAT THERE ARE NO CAST RECORDS                          
*                USING THIS AGENT BEFORE DELETING                               
*                                                                               
         USING TLAND,R3                                                         
         USING TLCAPD,R4                                                        
CHKDEL   DS    0H                                                               
         MVC   SVKEY,KEY           GENCON USES KEY TO DELETE ACTIVE PTR         
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         L     R3,AIO                                                           
         MVI   TLCAPCD,TLCAACDQ    BUILD PASSIVE KEY FOR CAST                   
         MVC   TLCAAAGT,TLANAGT    USING SAME AGENT AS RECORD                   
         NI    DMINBTS,X'F7'       DON'T READ DELETED                           
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(TLCAAAGT+L'TLCAAAGT-TLCAPKEY),KEYSAVE                        
         BE    CHKD15              IF AGENT HAS CAST CANNOT DELETE              
         DROP  R4                                                               
         MVC   KEY(L'SVKEY),SVKEY  RESTORE KEY                                  
         B     XIT                                                              
*                                                                               
CHKD15   LA    R2,SANAGTH          IF AGENT CANNOT BE DELETED                   
         BAS   RE,DISPLAY          DISPLAY RECORD                               
         B     NODELETE            ERROR - CAN'T DELETE                         
         EJECT                                                                  
*                                                                               
NODELETE MVI   ERROR,ERINVDEL      CANNOT DELETE RECORD                         
         B     THEEND                                                           
*                                                                               
INVERR   MVI   ERROR,INVALID                                                    
         B     THEEND                                                           
*                                                                               
RECFND   MVI   ERROR,RECEXIST                                                   
         B     THEEND                                                           
*                                                                               
MUSTBEC  MVC   MYMSGNO,=Y(EMUSTBEC)                                             
         MVI   BLOCK,0                                                          
         MVI   MYMTYP,C'E'                                                      
         OI    GENSTAT2,USGETTXT                                                
         LA    R2,SANSSNH                                                       
         B     THEEND                                                           
*                                                                               
INFOEND  OI    GENSTAT2,USGETTXT                                                
         LA    R2,CONRECH                                                       
THEEND   GOTO1 EXIT,DMCB,0                                                      
*                                                                               
         SPACE 2                                                                
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
*              CONSTANTS, ETC.                                                  
         SPACE 2                                                                
PFTABLE  DS    0C                  PF KEYS TABLE                                
         DC    AL1(PF13X-*,13,0,0,0)                                            
         DC    CL3' ',CL8'AGENT   ',CL8'LIST    '                               
PF13X    EQU   *                                                                
*                                                                               
         DC    AL1(PF14X-*,14,0,0,0)                                            
         DC    CL3' ',CL8'CAGENT  ',CL8'LIST    '                               
PF14X    EQU   *                                                                
*                                                                               
         DC    X'FF'                                                            
         SPACE 5                                                                
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR1ED                                                       
         EJECT                                                                  
         ORG   SANWORK                                                          
*                                                                               
SVLAGT   DS    XL2                 LAST AGENT                                   
SVKEY    DS    CL38                SAVED KEY                                    
SVTNR    DS    CL4                 SAVED OLD T & R CODE                         
SAMESCR  DS    CL1                 FLAG-HAS RECORD BEEN ADDED - UPDATE          
*                                       SYSTEM RECORD OR NOT                    
ASVPBLK  DS    A                   A(SAVED POINTER BLOCK)                       
AUPPBLK  DS    A                   A(UPDATED POINTER BLOCK)                     
         EJECT                                                                  
*                                                                               
* DDGENTWA                                                                      
* TAGENWORKD                                                                    
* TAGENFILE                                                                     
* TASYSEQUS                                                                     
* TASYSDSECT                                                                    
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'094TAGEN1E   09/04/15'                                      
         END                                                                    
