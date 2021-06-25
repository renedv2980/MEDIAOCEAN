*          DATA SET NESFM4C    AT LEVEL 126 AS OF 10/31/05                      
*PHASE T31C4CA                                                                  
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE:        T31C4C  -- BUYER RECORD MAINT/LIST                   *         
*                                                                     *         
*  COMMENTS:     MAINTAINS SUPERVISOR RECORDS ON SPFILE               *         
*                                                                     *         
*  CALLED FROM:  SFM CONTROLLER (T31C00), WHICH CALLS                 *         
*                GEGENCON (T00A30), WHICH CALLS THIS.                 *         
*                                                                     *         
*  INPUTS:       SCREENS SPSFMF8, AND SPSFME8                         *         
*                                                                     *         
*  OUTPUTS:      UPDATED BUYER RECORDS                                *         
*                                                                     *         
*  REGISTERS:    R0 -- WORK                                           *         
*                R1 -- WORK                                           *         
*                R2 -- SCREEN FIELD HEADER                            *         
*                R3 -- WORK                                           *         
*                R4 -- WORK                                           *         
*                R5 -- WORK                                           *         
*                R6 -- GETEL REGISTER                                 *         
*                R7 -- SECOND BASE                                    *         
*                R8 -- SPOOL                                          *         
*                R9 -- SYSD                                           *         
*                RA -- TWA                                            *         
*                RB -- FIRST BASE                                     *         
*                RC -- GEND                                           *         
*                RD -- SYSTEM                                         *         
*                RE -- SYSTEM                                         *         
*                RF -- SYSTEM                                         *         
*                                                                     *         
***********************************************************************         
         TITLE 'T31C4C - BUYER RECORD MAINTENANCE'                              
T31C4C   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**1C4C**,R7,RR=R3                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R3,RELO                                                          
*                                                                               
         BAS   RE,SETUP                                                         
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPKEY        DISPLAY RECORD                               
         BE    DK                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,LISTRECS                                                    
         BE    LR                  LIST RECORDS                                 
*                                                                               
         CLI   MODE,XRECADD                                                     
         BE    XR                  PASSIVE KEYS ON ADD                          
         CLI   MODE,XRECPUT                                                     
         BE    XR                  PASSIVE KEYS ON CHANGE                       
         CLI   MODE,XRECDEL                                                     
         BE    XR                  PASSIVE KEYS ON DELETE                       
         CLI   MODE,XRECREST                                                    
         BE    XR                  PASSIVE KEYS ON RESTORE                      
*                                                                               
XYES     SR    RC,RC                                                            
XNO      LTR   RC,RC                                                            
XIT      XIT1                                                                   
         EJECT                                                                  
* VALIDATE KEY                                                                  
*                                                                               
VK       DS    0H                                                               
         MVC   AIO,AIO1                                                         
         MVC   OFFICE,SPACES                                                    
         MVC   BUYR,SPACES                                                      
         XC    SAVESEL,SAVESEL                                                  
*                                                                               
         NI    MYFLAG,X'FF'-HASSGRP                                             
         NI    MYFLAG,X'FF'-HASSTA                                              
         NI    MYFLAG,X'FF'-FROMVR                                              
*                                                                               
         GOTO1 GETFACT,DMCB,0      GET SECURITY ALPHA                           
         USING FACTSD,R3                                                        
         L     R3,DMCB                                                          
         MVC   SECALPHA,FATAGYSC   SAVE SECURITY AGENCY                         
         DROP  R3                                                               
*                                                                               
         CLI   ACTNUM,ACTLIST                                                   
         BNE   VK50                                                             
*                                                                               
         LA    R2,BYLOFFH                                                       
         CLI   5(R2),0             DATA IN OFFICE FIELD?                        
         BE    VK10                NO SKIP                                      
         MVC   OFFICE,BYLOFF                                                    
         OC    OFFICE,SPACES                                                    
*                                                                               
VK10     LA    R2,BYLBYCH                                                       
         CLI   5(R2),0                                                          
         BE    VK100                                                            
         MVC   BUYR,BYLBYC                                                      
         OC    BUYR,SPACES                                                      
         B     VK100                                                            
*                                                                               
VK50     DS    0H             VALIDATE FOR DISPLAY/ADD/CHANGE                   
         LA    R2,BYMOFFH               VALIDATE OFFICE CODE                    
         CLI   5(R2),0                                                          
         BE    ERRMIS                                                           
         CLI   8(R2),C' '                                                       
         BNH   ERRINV                                                           
         MVC   OFFICE,BYMOFF                                                    
         OC    OFFICE,SPACES                                                    
         BAS   RE,VALOFF                                                        
*                                                                               
         LA    R2,BYMBYCH               VALIDATE BUYER CODE                     
         CLI   5(R2),0                                                          
         BE    ERRMIS                                                           
         CLI   8(R2),C' '                                                       
         BNH   ERRINV                                                           
*                                                                               
         CLI   ACTNUM,ACTADD                                                    
         BNE   VK59                                                             
         LA    R1,BYMBYC           VALID ALPHA NUMERIC                          
         LA    R0,L'BYMBYC                                                      
VK54     CLI   0(R1),C' '                                                       
         BNH   VK56                                                             
         CLI   0(R1),C'A'                                                       
         BL    ERRINV                                                           
         CLI   0(R1),C'9'                                                       
         BH    ERRINV                                                           
VK56     LA    R1,1(R1)                                                         
         BCT   R0,VK54                                                          
*                                                                               
VK59     MVC   BUYR,BYMBYC                                                      
         OC    BUYR,SPACES                                                      
*                                                                               
         CLI   5(R2),2                  BUYER CODE LEN. AT LEAST 2              
         BL    ERRINV                                                           
         B     VK100                                                            
*                                                                               
VK100    XC    TEMPFLD,TEMPFLD                                                  
         MVC   TEMPFLD,=XL9'0900000000010000D5'                                 
         LA    R2,TEMPFLD                                                       
         GOTO1 VALIMED                                                          
         MVC   AGYX,BAGYMD                                                      
         NI    AGYX,X'F0'                                                       
*                                                                               
         LA    R4,KEY              BUILD BUYER KEY                              
         USING BYRRECD,R4                                                       
         XC    KEY,KEY                                                          
         MVC   BYRKTYP,=X'0D62'                                                 
         MVC   BYRKAGY,AGYX                                                     
         MVC   BYRKOFC,OFFICE                                                   
         MVC   BYRKBYR,BUYR                                                     
*                                                                               
         CLC   SAVEKEY,KEY                                                      
         BE    *+8                                                              
         OI    STATUS,KEYCHG                                                    
*                                                                               
         MVC   SAVEKEY,KEY                                                      
*                                                                               
VKX      MVC   AIO,AIO1                 MUST RESTORE PROPER IO AREA             
*                                                                               
         CLI   ACTNUM,ACTLIST                                                   
         BNE   XIT                                                              
*                                                                               
         MVI   FLTFLAG,0                                                        
         XC    BGFILTER,BGFILTER                                                
         LA    R2,BYLFILH                                                       
         CLI   5(R2),0                                                          
         BE    XIT                                                              
         GOTO1 SCANNER,DMCB,(R2),SCANAREA                                       
         CLI   DMCB+4,1                 NUMBER OF FILTER,CURRENT MAX=1          
         BH    ERRINV                                                           
*                                                                               
         LA    R6,SCANAREA              FOR NOW, ONLY BG'S ACCEPTED             
         CLI   0(R6),2                                                          
         BNE   ERRINV                                                           
         CLC   =C'BG',12(R6)                                                    
         BNE   ERRINV                                                           
         CLI   1(R6),2                  ALL BUYGRP'S ARE 2 LETTERS              
         BNE   ERRINV                                                           
*                                                                               
         MVC   SVKEY1,KEY               CHECK TO SEE IF BUYGRP VALID            
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D60'                                                  
         MVC   KEY+2(1),BAGYMD                                                  
         NI    KEY+2,X'F0'                                                      
         MVC   KEY+3(2),22(R6)                                                  
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(5),KEY                                                   
         BNE   ERRINV                                                           
         MVC   BGFILTER,22(R6)                                                  
         OI    FLTFLAG,X'80'                                                    
         MVC   KEY,SVKEY1                                                       
*                                                                               
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
* VALIDATE RECORD                                                               
*                                                                               
VR       DS    0H                                                               
         OI    MYFLAG,FROMVR                                                    
**********************************************************************          
*        VALIDATE X'01' GENERAL ELEMNT                               *          
*                                                                    *          
         CLI   ACTNUM,ACTADD                                                    
         BE    VR10                                                             
         MVI   ELCODE,X'01'        BUYER NAME ELEMENT                           
*                                                                               
         L     R6,AIO                                                           
         BAS   RE,GETEL                                                         
         USING BYRNAMED,R6                                                      
         MVC   OLDSPV,BYRSPV                                                    
         DROP  R6                                                               
*                                                                               
         L     R6,AIO                                                           
         GOTO1 REMELEM                                                          
*                                                                               
VR10     LA    R5,ELEM                                                          
         USING BYRNAMED,R5                                                      
         XC    ELEM,ELEM                                                        
         MVI   BYRNAMEL,X'01'      ELEMENT CODE                                 
         MVI   BYRNAMLN,BYRNAMLQ   ELEMENT LENGTH                               
*                                                                               
         LA    R2,BYMFNMH          CHECK BUYER NAME FIELD                       
         CLI   5(R2),0                                                          
         BE    ERRMIS                                                           
         MVC   BYRFNAME,BYMFNM                                                  
         OC    BYRFNAME,SPACES                                                  
*                                                                               
         LA    R2,BYMLNMH          CHECK BUYER NAME FIELD                       
         CLI   5(R2),0                                                          
         BE    ERRMIS                                                           
         MVC   BYRLNAME,BYMLNM                                                  
         OC    BYRLNAME,SPACES                                                  
*                                                                               
         LA    R2,BYMSUPH               CHECK SUPV CODE                         
         CLI   5(R2),0                                                          
         BE    ERRMIS                                                           
         BAS   RE,VALSUPV                                                       
*                                                                               
         MVC   BYRSPV,BYMSUP                                                    
         OC    BYRSPV,SPACES                                                    
         MVC   SUPV,BYRSPV                                                      
*                                                                               
*        LA    R2,BYMNWSTH               CHECK TV NWS CODE                      
         MVC   BAGYMDS,AGYX             BINARY AGENCY/TV MEDIA CODE             
         OI    BAGYMDS,X'1'             TURN ON TV BIT                          
         CLI   5(R2),0                                                          
         BE    VR12                     NOT REQUIRED                            
*        MVC   SAVENWS,BYMNWST                                                  
*        OC    SAVENWS,SPACES                                                   
*        BAS   RE,VALNWS                                                        
*        MVC   BYRNWST,SAVENWS                                                  
*                                                                               
VR12     DS    0H                                                               
*        LA    R2,BYMDARTH               CHECK TV DARE CODE                     
         CLI   5(R2),0                                                          
         BE    VR14                                                             
*        MVC   SAVEDAR,BYMDART                                                  
*        OC    SAVEDAR,SPACES                                                   
*        BAS   RE,VALDARE                                                       
*        MVC   BYRDARET,SAVEDAR                                                 
*                                                                               
VR14     DS    0H                                                               
*        LA    R2,BYMNWSRH               CHECK RADIO NWS CODE                   
         MVC   BAGYMDS,AGYX             BINARY AGENCY/RADIO MED CODE            
         OI    BAGYMDS,X'2'             TURN ON RADIO BIT                       
         CLI   5(R2),0                                                          
         BE    VR16                                                             
*        MVC   SAVENWS,BYMNWSR                                                  
*        OC    SAVENWS,SPACES                                                   
*        BAS   RE,VALNWS                                                        
*        MVC   BYRNWSR,SAVENWS                                                  
*                                                                               
VR16     DS    0H                                                               
*        LA    R2,BYMDARRH               CHECK RADIO DARE CODE                  
         CLI   5(R2),0                                                          
         BE    VR17                                                             
*        MVC   SAVEDAR,BYMDARR                                                  
*        OC    SAVEDAR,SPACES                                                   
*        BAS   RE,VALDARE                                                       
*        MVC   BYRDARER,SAVEDAR                                                 
*                                                                               
VR17     LA    R2,BYMOFOFH                                                      
         MVC   BYROFF,SPACES                                                    
         CLI   5(R2),0                                                          
         BE    VR18                                                             
         MVC   BYROFF,BYMOFOF                                                   
*                                                                               
VR18     LA    R2,BYMFILTH                                                      
         CLI   5(R2),0                                                          
         BE    VR19                                                             
         CLI   BYMFILT,C'A'                                                     
         BL    ERRINV                                                           
         CLI   BYMFILT,C'Z'                                                     
         BH    ERRINV                                                           
         MVC   BYRFILT,BYMFILT                                                  
*                                                                               
**********************************************************************          
*        VALIDATE OPTIONS.  11/16/99                                 *          
*                                                                    *          
VR19     LA    R2,BYMOPTH                                                       
         CLI   5(R2),0                                                          
         BE    VOPTX                                                            
         XC    SCANAREA,SCANAREA   NOTE ROOM FOR 8 SCAN LINES                   
         GOTO1 SCANNER,DMCB,(R2),SCANAREA                                       
         ZIC   R4,DMCB+4           NUMBER OF ENTRIES                            
*                                                                               
         LA    R6,SCANAREA                                                      
VOPT05   CLC   =C'CABLE',12(R6)                                                 
         BNE   VOPT10                                                           
         CLI   22(R6),C'Y'                                                      
         BNE   *+12                                                             
         OI    BYROPT1,BYROPT1_CBL                                              
         B     VOPT99                                                           
         CLI   22(R6),C'N'                                                      
         BNE   ERRINV                                                           
         NI    BYROPT1,X'FF'-BYROPT1_CBL                                        
         B     VOPT99                                                           
*                                                                               
VOPT10   CLC   =C'UNWIRED',12(R6)                                               
         BNE   VOPT20                                                           
         CLI   22(R6),C'Y'                                                      
         BNE   *+12                                                             
         OI    BYROPT1,BYROPT1_UNW                                              
         B     VOPT99                                                           
         CLI   22(R6),C'N'                                                      
         BNE   ERRINV                                                           
         NI    BYROPT1,X'FF'-BYROPT1_UNW                                        
         B     VOPT99                                                           
*                                                                               
VOPT20   CLC   =C'ASST',12(R6)                                                  
         BNE   ERRINV                                                           
         CLI   22(R6),C'Y'                                                      
         BNE   *+12                                                             
         OI    BYROPT1,BYROPT1_ASS                                              
         B     VOPT99                                                           
         CLI   22(R6),C'N'                                                      
         BNE   ERRINV                                                           
         NI    BYROPT1,X'FF'-BYROPT1_ASS                                        
         B     VOPT99                                                           
*                                                                               
VOPT99   LA    R6,32(R6)                                                        
         BCT   R4,VOPT05                                                        
*                                                                               
VOPTX    GOTO1 ADDELEM                                                          
         DROP  R5                                                               
**********************************************************************          
*        VALIDATE PERSONAL ID                                        *          
*                                                                    *          
         LA    R2,BYMPIDH                                                       
         CLI   5(R2),0             REQ'D                                        
         BE    ERRMIS                                                           
         MVC   PIDNAME,BYMPID                                                   
         OC    PIDNAME,SPACES                                                   
         BAS   RE,VALPID                                                        
         BNE   ERRINV                                                           
*                                                                               
         MVI   ELCODE,BPIDELQ      X'22'                                        
         GOTO1 REMELEM                                                          
*                                                                               
         XC    ELEM,ELEM                                                        
         USING BPIDELD,R5                                                       
         LA    R5,ELEM                                                          
         MVI   BPIDEL,BPIDELQ      X'22'                                        
         MVI   BPIDLN,BPIDLNQ                                                   
         MVC   BPIDNO,PIDNUM                                                    
         GOTO1 ADDELEM                                                          
         DROP  R5                                                               
*                                                                               
**********************************************************************          
         LA    R2,BYMMEDH               FINDING INPUT MEDIA AND ELCODE          
         CLI   5(R2),0                                                          
         BE    VR40                                                             
*                                                                               
         LA    R4,MEDTAB                                                        
*                                                                               
VR20     CLC   BYMMED,0(R4)                                                     
         BE    VR30                                                             
         LA    R4,MTABLQ(R4)                                                    
         CLI   0(R4),X'FF'                                                      
         BNE   VR20                                                             
         B     ERRINV                                                           
*                                                                               
VR30     MVC   MEDELCD,1(R4)            ELCODE FOUND                            
         MVC   SVMEDTYP,1(R4)           SAVE MEDIA TYPE                         
         MVC   SVGRPID,2(R4)            SAVE MARKET/STATION GROUP ID            
*        MVI   USEIONUM,2               DON'T USE IO AREA OF RECORD             
         MVC   AIO,AIO2                                                         
         MVI   BYTE,C'A'                                                        
         GOTO1 VALIMED                  MAKES A DM CALL                         
         MVC   AIO,AIO1                 MUST RESTORE PROPER IO AREA             
         B     VR40                                                             
*                                                                               
VR40     DS    0H                                                               
         LA    R2,BYMSTAH          ANY STATIONS?                                
         LA    R5,7                                                             
         CLI   5(R2),0                                                          
         BE    VRX                                                              
*                                                                               
VR50     DS    0H                                                               
         CLI   5(R2),0             ANY MORE INPUT                               
         BE    VR100               NO - PROCESS TABLE                           
*                                                                               
         CLC   8(3,R2),=C'ALL'                                                  
         BE    ERRINV                                                           
*                                                                               
         CLC   8(4,R2),=C'-ALL'                                                 
         BE    ERRINV                                                           
*                                                                               
         CLI   8(R2),C'-'          DELETE THIS STA/GROUP?                       
         BNE   VR60                                                             
         CLI   ACTNUM,ACTADD                                                    
         BE    ERRINV                                                           
         BAS   RE,DELSTA           DELETE STATION/GROUP                         
         B     VR90                BUMP TO NEXT INPUT FIELD                     
*                                                                               
VR60     DS    0H                                                               
         CLI   9(R2),C'0'          IS THIS A STATION GROUP?                     
         BL    VR70                                                             
         CLI   9(R2),C'9'                                                       
         BH    VR70                                                             
*                                                                               
         TM    MYFLAG,HASSTA       CANNOT COMBINE GRPS & SOLO STATIONS          
         BO    ERRGRPST                                                         
         BAS   RE,RADDSTG          ADD STATION GROUP ELEMENT                    
         B     VR90                                                             
*                                                                               
VR70     DS    0H                                                               
         TM    MYFLAG,HASSGRP      CANNOT COMBINE GRPS & SOLO STATIONS          
         BO    ERRGRPST                                                         
         BAS   RE,RADDSTA          ADD STATION TO REC                           
*                                                                               
VR90     ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         BCT   R5,VR50                                                          
*                                                                               
VR100    DS    0H                                                               
*                                                                               
VRX      OI    GENSTAT2,RETEQSEL   STAY HERE FOR ONE TRANSACTION                
         B     DR                                                               
         EJECT                                                                  
********************************************************************            
*        DELETE STATION OR STATION GROUP ELEMENT                                
********************************************************************            
DELSTA   NTR1                                                                   
         L     R6,AIO                                                           
*                                                                               
         CLC   =C'ALL',9(R2)       DELETE ALL STATIONS/GROUPS                   
         BNE   DSTA10                                                           
         GOTO1 HELLO,DMCB,(C'D',=CL8'SPTFILE '),(SVGRPID,AIO1),0                
         GOTO1 HELLO,DMCB,(C'D',=CL8'SPTFILE '),(SVMEDTYP,AIO1),0               
         B     DELSTAX                                                          
*                                                                               
DSTA10   DS    0H                                                               
         CLI   10(R2),C'0'         DELETE STATION GROUP?                        
         BL    DSTA100                                                          
         CLI   10(R2),C'9'                                                      
         BH    DSTA100                                                          
*                                                                               
         MVC   TMPSTGID,9(R2)      GROUP ID                                     
*                                                                               
         MVC   TMPASTG,10(R2)       GROUP #                                     
         OC    TMPASTG,=C'0000'                                                 
*                                                                               
         PACK  DUB,TMPASTG                                                      
         L     R0,DUB+4                                                         
         SRA   R0,4                                                             
         BZ    ERRINV                                                           
         STCM  R0,3,TMPSTG        DELETE ALL STATIONS FOR THIS GROUP            
*                                                                               
         L     R6,AIO                                                           
         MVC   ELCODE,SVGRPID      FIND STATION GROUP ELEMENT                   
         BAS   RE,GETEL                                                         
         B     *+8                                                              
*                                                                               
DSTA20   BAS   RE,NEXTEL                                                        
         BNE   DELSTAX                                                          
         USING BYRMKGD,R6                                                       
*                                                                               
         CLC   BYRMKGID,TMPSTGID                                                
         BNE   DSTA20                                                           
         CLC   BYRMKGRP,TMPSTG                                                  
         BNE   DSTA20                                                           
*                                                                               
         MVC   DUB(3),2(R6)                                                     
         GOTO1 HELLO,DMCB,(C'D',=CL8'SPTFILE '),(SVGRPID,AIO1),(3,DUB)          
         B     DELSTAX                                                          
         DROP  R6                                                               
*                                                                               
DSTA100  DS    0H                  DELETE STATION                               
         MVC   TMPSTA,9(R2)                                                     
         OC    TMPSTA,SPACES                                                    
*                                                                               
         L     R6,AIO                                                           
         MVC   ELCODE,SVMEDTYP     FIND STATION ELEMENT                         
         BAS   RE,GETEL                                                         
         B     *+8                                                              
*                                                                               
DSTA120  BAS   RE,NEXTEL                                                        
         BNE   DELSTAX                                                          
         USING BYRSTAD,R6                                                       
*                                                                               
         CLC   BYRSTA,TMPSTA                                                    
         BNE   DSTA120                                                          
*                                                                               
         MVC   DUB(4),2(R6)                                                     
         GOTO1 HELLO,DMCB,(C'D',=CL8'SPTFILE '),(SVMEDTYP,AIO1),(4,DUB)         
         B     DELSTAX                                                          
         DROP  R6                                                               
*                                                                               
DELSTAX  DS    0H                                                               
         B     XIT                                                              
*                                                                               
********************************************************************            
*        ADD STATION GROUP ELEMENT                                              
********************************************************************            
RADDSTG  NTR1                                                                   
         L     R6,AIO                                                           
         XC    TMPSTG,TMPSTG                                                    
*                                                                               
         CLI   5(R2),1                                                          
         BNH   ERRINV                                                           
*                                                                               
         MVC   TMPSTGID,8(R2)      GROUP ID                                     
*                                                                               
         MVC   TMPASTG,9(R2)       GROUP #                                      
         OC    TMPASTG,=C'0000'                                                 
*                                                                               
         PACK  DUB,TMPASTG                                                      
         L     R0,DUB+4                                                         
         SRA   R0,4                                                             
         BZ    ERRINV                                                           
         STCM  R0,3,TMPSTG        DELETE ALL STATIONS FOR THIS GROUP            
*                                                                               
         LA    R4,KEY                                                           
         USING GRPRECD,R4                                                       
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D05'       STATION GROUP RECORD                       
         MVC   KEY+2(1),BAGYMD                                                  
         MVC   GRPKID,TMPSTGID                                                  
         MVC   GRPKCODE,TMPSTG     GROUP CODE                                   
         GOTO1 HIGH                                                             
         CLC   KEY(6),KEYSAVE                                                   
         BNE   ERRINV              NO SUCH GROUP ID                             
         DROP  R4                                                               
*                                                                               
         OI    MYFLAG,HASSGRP                                                   
*                                                                               
         XC    KEY,KEY                                                          
         L     RF,AIO1                                                          
         MVC   KEY(13),0(RF)                                                    
*                                                                               
         CLI   ACTNUM,ACTADD                                                    
         BE    RADSTG50                                                         
         GOTO1 HIGH                                                             
*                                                                               
RADSTG50 DS    0H                                                               
         L     R6,AIO                                                           
         XC    ELEM,ELEM                                                        
         LA    R4,ELEM                                                          
         USING BYRMKGD,R4                                                       
*                                                                               
         MVC   BYRMKGEL,SVGRPID    GROUP ID                                     
         MVI   BYRMKGLN,BYRMKGLQ   LENGTH                                       
         MVC   BYRMKGID,TMPSTGID   GROUP ID                                     
         MVC   BYRMKGRP,TMPSTG     STATION GROUP                                
         DROP  R4                                                               
*                                                                               
         MVC   DUB(3),2(R4)        SEE IF STATION GROUP ALREADY EXISTS          
         GOTO1 HELLO,DMCB,(C'G',=CL8'SPTFILE '),(SVGRPID,AIO1),(3,DUB)          
         CLI   12(R1),0                                                         
         BE    RADDSTGX                                                         
*                                                                               
         GOTO1 ADDELEM                                                          
*                                                                               
RADDSTGX DS    0H                                                               
         B     XIT                                                              
********************************************************************            
*        ADD STATION ELEMENT                                                    
********************************************************************            
RADDSTA  NTR1                                                                   
         L     R6,AIO                                                           
         OI    MYFLAG,HASSTA                                                    
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R4,ELEM                                                          
         USING BYRSTAD,R4                                                       
*                                                                               
         MVC   BYRSTAEL,SVMEDTYP                                                
         MVI   BYRSTALN,BYRSTALQ   ELEM LENGTH                                  
         MVC   BYRSTA,8(R2)        STATION                                      
         OC    BYRSTA,SPACES                                                    
         DROP  R4                                                               
*                                                                               
         MVC   DUB(4),2(R4)        SEE IF STATION GROUP ALREADY EXISTS          
         OC    DUB(4),SPACES                                                    
         GOTO1 HELLO,DMCB,(C'G',=CL8'SPTFILE '),(SVMEDTYP,AIO1),(4,DUB)         
         CLI   12(R1),0                                                         
         BE    RADDSTAX                                                         
*                                                                               
         GOTO1 ADDELEM                                                          
*                                                                               
RADDSTAX DS    0H                                                               
         B     XIT                                                              
*                                                                               
* DISPLAY RECORD                                                                
*                                                                               
DR       DS    0H                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'01'        DISPLAY HEADER INFO                          
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         USING BYRNAMED,R6                                                      
*                                                                               
         MVC   BYMFNM,BYRFNAME     BUYER FIRST NAME                             
         OI    BYMFNMH+6,X'80'                                                  
         MVC   BYMLNM,BYRLNAME     BUYER LAST NAME                              
         OI    BYMLNMH+6,X'80'                                                  
*                                                                               
         MVC   BYMSUP,BYRSPV       DISPLAY SUPERVISOR CODE                      
         OI    BYMSUPH+6,X'80'                                                  
         MVC   SUPV,BYRSPV                                                      
         OC    SUPV,SPACES                                                      
*                                                                               
         BAS   RE,GETSPN           GET AND DISPLAY SUPERVISOR NAME              
         MVC   BYMSPN,BLOCK                                                     
         OI    BYMSPNH+6,X'80'                                                  
*                                                                               
         MVC   BYMOFOF,BYROFF      BUYER OFFICE                                 
         OI    BYMOFOFH+6,X'80'                                                 
         MVC   BYMFILT,BYRFILT     FILTER                                       
         OI    BYMFILTH+6,X'80'                                                 
***                                                                             
*        DISPLAY OPTIONS: CABLE=Y/N UNWIRED=Y/N ASST=Y/N                        
***                                                                             
         LA    R2,BYMOPT                                                        
         MVC   BYMOPT,SPACES                                                    
         OI    BYMOPTH+6,X'80'           CLEAR!                                 
*                                                                               
         TM    BYROPT1,BYROPT1_CBL                                              
         BNO   DR10                                                             
         MVC   0(8,R2),=C'CABLE=Y,'                                             
         LA    R2,8(R2)                                                         
*                                                                               
DR10     TM    BYROPT1,BYROPT1_UNW                                              
         BNO   DR12                                                             
         MVC   0(10,R2),=C'UNWIRED=Y,'                                          
         LA    R2,10(R2)                                                        
*                                                                               
DR12     TM    BYROPT1,BYROPT1_ASS                                              
         BNO   DR15                                                             
         MVC   0(7,R2),=C'ASST=Y,'                                              
*                                                                               
DR15     AHI   R2,-1               BACK UP 1                                    
         CLI   0(R2),C','          AND CHECK FOR COMMA                          
         BNE   *+8                                                              
         MVI   0(R2),C' '          SPACE OUT LAST COMMA                         
         DROP  R6                                                               
*                                                                               
DR20     DS    0H                  DISPLAY PID NAME                             
         MVC   BYMPID,SPACES                                                    
         OI    BYMPIDH+6,X'80'                                                  
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'22'                                                     
         BAS   RE,GETEL                                                         
         BNE   DR30                                                             
         USING BPIDELD,R6                                                       
*                                                                               
         MVC   PIDNUM,BPIDNO       PID NUMBER                                   
         BAS   RE,GETPIDNM         GET PID NAME                                 
         MVC   BYMPID,PIDNAME                                                   
*                                                                               
DR30     DS    0H                                                               
         MVI   BYMMED,C'N'         ALWAYS MEDIA N                               
         OI    BYMMEDH+6,X'80'                                                  
*                                                                               
         BAS   RE,CLRSCR                                                        
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'15'        CHECK IF ANY STATION GROUPS                  
         BAS   RE,GETEL                                                         
         BNE   DR40                NO, JUST DIPLAY INDIVIDUAL STATIONS          
         OI    MYFLAG,HASSGRP                                                   
         BAS   RE,BLDSGTBL         BUILD TABLE OF STATION GROUPS                
         BAS   RE,DISPSTG          DISPLAY STATION GROUPS                       
*                                                                               
DR40     DS    0H                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'05'        GET STATION ELEMENTS                         
         BAS   RE,GETEL                                                         
         BNE   DR50                                                             
         OI    MYFLAG,HASSTA                                                    
         BAS   RE,BLDSTTBL         BUILD TABLE OF STATIONS TO DISPLAY           
         BAS   RE,DISPSTA          DISPLAY STATIONS                             
*                                                                               
DR50     DS    0H                                                               
         TM    MYFLAG,FROMVR       CAME FROM VREC                               
         BZ    DR60                                                             
*                                                                               
         CLI   ACTNUM,ACTSEL                                                    
         BE    *+12                                                             
DR60     CLI   ACTNUM,ACTCHA                                                    
         BNE   DRX                                                              
*                                                                               
         XC    KEY,KEY                                                          
         L     RF,AIO1                                                          
         MVC   KEY(13),0(RF)                                                    
*                                                                               
         MVC   AIO,AIO3            RESTORE DISK ADDRESS AND SEQUENCE            
         GOTO1 HIGH                                                             
         GOTO1 GETREC                                                           
*                                                                               
DRX      DS    0H                                                               
         MVC   AIO,AIO1                                                         
         MVI   RDUPDATE,C'Y'                                                    
         NI    STATUS,X'FF'-KEYCHG                                              
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
******************************************************************              
*        DISPLAY STATION GROUPS FROM TABLE                                      
******************************************************************              
DISPSTG  NTR1                                                                   
         L     R5,AIO3             TABLE WILL BE IN AIO3                        
         LA    R2,BYMCODEH                                                      
*                                                                               
         CLI   PFAID,8                                                          
         BE    DSPSTG10                                                         
*                                                                               
         XC    FRSTSTG,FRSTSTG                                                  
*                                                                               
DSPSTG10 DS    0H                                                               
         CLI   0(R5),X'FF'         ANY MORE TO DISPLAY                          
         BE    DISPSTGX            NO                                           
*                                                                               
         LA    RF,BYMENDH                                                       
         CR    R2,RF                                                            
         BNH   *+14                                                             
         MVC   FRSTSTG,0(R5)                                                    
         BH    DISPSTGX                                                         
*                                                                               
         OC    FRSTSTG,FRSTSTG                                                  
         BZ    DSPSTG30                                                         
*                                                                               
         CLC   FRSTSTG,0(R5)                                                    
         BNE   DSPSTG60                                                         
         XC    FRSTSTG,FRSTSTG                                                  
*                                                                               
DSPSTG30 DS    0H                                                               
         MVC   MGNAME,SPACES                                                    
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY              GET STATION GROUP ID RECORD AND              
         USING GRPRECD,R4          GET LENGTH OF GROUP                          
         MVI   GRPKTYP,X'0D'                                                    
         MVI   GRPKSTYP,X'05'                                                   
         MVC   GRPKAGMD,BAGYMD                                                  
         MVC   GRPKID,0(R5)        STATION GROUP ID                             
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   ERRINV                                                           
*                                                                               
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO2                                                          
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         USING GRPBRKD,R6                                                       
*                                                                               
         ZIC   RF,GRPBK1LN         GET LENGTH                                   
         ZIC   RE,GRPBK2LN                                                      
         AR    RF,RE                                                            
         STC   RF,MGRPLEN                                                       
         DROP  R6                                                               
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY              GET STATION GROUP RECORD                     
         USING GRPRECD,R4                                                       
*                                                                               
         MVI   GRPKTYP,X'0D'                                                    
         MVI   GRPKSTYP,X'05'                                                   
         MVC   GRPKAGMD,BAGYMD                                                  
         MVC   GRPKID,0(R5)        STATION GROUP ID                             
         MVC   GRPKCODE,1(R5)      STATION GROUP                                
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   ERRINV                                                           
*                                                                               
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO2                                                          
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         USING GRPGRPD,R6                                                       
         MVC   MGNAME,GRPGNAM1     1ST GROUP NAME                               
         OC    GRPGNAM2,GRPGNAM2   ANY 2ND GROUP?                               
         BZ    *+10                                                             
         MVC   MGNAME,GRPGNAM2                                                  
         DROP  R6                                                               
*                                                                               
DSPSTG50 XC    BLOCK(100),BLOCK    SET UP FOR SQUASHER                          
         MVC   BLOCK(1),0(R5)      MKT GRP ID                                   
*                                                                               
         UNPK  DUB,1(3,R5)                                                      
         ZIC   R1,MGRPLEN                                                       
         AHI   R1,-1                                                            
         EX    R1,*+4                                                           
         MVC   BLOCK+1(0),DUB+3    MKT GROUP NUMBER                             
*                                                                               
         MVC   BLOCK+8(L'MGNAME),MGNAME    MOST SPECIFIC NAME                   
*                                                                               
         GOTO1 SQUASHER,DMCB,BLOCK,50                                           
         MVC   8(L'BYMCODE,R2),BLOCK                                            
         OI    6(R2),X'80'                                                      
*                                                                               
         ZIC   RF,0(R2)            BUMP TO NEXT FLD                             
         AR    R2,RF                                                            
*                                                                               
DSPSTG60 DS    0H                                                               
         LA    R5,4(R5)                                                         
         B     DSPSTG10                                                         
*                                                                               
DISPSTGX DS    0H                                                               
         MVC   AIO,AIO1                                                         
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
******************************************************************              
*        BUILD TABLE OF STATION GROUPS TO DISPLAY                               
******************************************************************              
BLDSGTBL NTR1                                                                   
         L     RE,AIO3                                                          
         LA    RF,2000                                                          
         XCEF                                                                   
*                                                                               
         L     R4,AIO3             TABLE WILL BE IN AIO3                        
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'15'        GET STATIONS ELEMENTS                        
         BAS   RE,GETEL                                                         
         B     *+8                                                              
*                                                                               
BLDSG10  BAS   RE,NEXTEL                                                        
         BNE   BLDSG50                                                          
         USING BYRMKGD,R6                                                       
*                                                                               
         MVC   0(1,R4),BYRMKGID    STATION GROUP ID                             
         MVC   1(2,R4),BYRMKGRP    STATION GROUP                                
*                                                                               
         LA    R4,4(R4)                                                         
         B     BLDSG10                                                          
*                                                                               
BLDSG50  DS    0H                                                               
         MVI   0(R4),X'FF'         DENOTE END OF TABLE                          
*                                                                               
BLDSGTBX DS    0H                                                               
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
******************************************************************              
*        DISPLAY STATIONS FROM TABLE                                            
******************************************************************              
DISPSTA  NTR1                                                                   
         L     R4,AIO3             TABLE WILL BE IN AIO3                        
         LA    R2,BYMCODEH                                                      
*                                                                               
         CLI   PFAID,8                                                          
         BE    DSPSTA10                                                         
*                                                                               
         XC    FRSTSTA,FRSTSTA                                                  
*                                                                               
DSPSTA10 DS    0H                                                               
         CLI   0(R4),X'FF'         ANY MORE TO DISPLAY                          
         BE    DISPSTAX            NO                                           
*                                                                               
         LA    RF,BYMENDH                                                       
         CR    R2,RF                                                            
         BNH   *+14                                                             
         MVC   FRSTSTA,0(R4)                                                    
         B     DISPSTAX                                                         
*                                                                               
         OC    FRSTSTA,FRSTSTA                                                  
         BZ    DSPSTA30                                                         
*                                                                               
         CLC   FRSTSTA,0(R4)                                                    
         BNE   DSPSTA50                                                         
         XC    FRSTSTA,FRSTSTA                                                  
*                                                                               
DSPSTA30 DS    0H                                                               
         MVC   8(4,R2),0(R4)                                                    
         OI    6(R2),X'80'                                                      
*                                                                               
         ZIC   RF,0(R2)            BUMP TO NEXT FLD                             
         AR    R2,RF                                                            
*                                                                               
DSPSTA50 DS    0H                                                               
         LA    R4,5(R4)                                                         
         B     DSPSTA10                                                         
*                                                                               
DISPSTAX DS    0H                                                               
         B     XIT                                                              
         EJECT                                                                  
******************************************************************              
*        BUILD TABLE OF STATIONS TO DISPLAY                                     
******************************************************************              
BLDSTTBL NTR1                                                                   
         L     RE,AIO3                                                          
         LA    RF,2000                                                          
         XCEF                                                                   
*                                                                               
         L     R4,AIO3             TABLE WILL BE IN AIO3                        
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'05'        GET STATIONS ELEMENTS                        
         BAS   RE,GETEL                                                         
         B     *+8                                                              
*                                                                               
BLDST10  BAS   RE,NEXTEL                                                        
         BNE   BLDST50                                                          
         USING BYRSTAD,R6                                                       
*                                                                               
         MVC   0(4,R4),BYRSTA      MOVE IN STATION TO TABLE                     
         LA    R4,5(R4)                                                         
         B     BLDST10                                                          
*                                                                               
BLDST50  DS    0H                                                               
         MVI   0(R4),X'FF'         DENOTE END OF TABLE                          
*                                                                               
BLDSTTBX DS    0H                                                               
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
*                                                                               
* DISPLAY KEY                                                                   
*                                                                               
DK       L     R6,AIO                                                           
         USING BYRRECD,R6                                                       
*                                                                               
         MVC   OFFICE,BYRKOFC                                                   
         OC    OFFICE,SPACES                                                    
         MVC   BYMOFF,OFFICE                                                    
         OI    BYMOFFH+6,X'80'                                                  
*                                                                               
         MVC   BUYR,BYRKBYR                                                     
         OC    BUYR,SPACES                                                      
         MVC   BYMBYC,BUYR                                                      
         OI    BYMBYCH+6,X'80'                                                  
         MVC   SAVEKEY,0(R6)                                                    
*                                                                               
DKX      CLI   ACTNUM,ACTSEL                                                    
         BNE   *+10                                                             
         MVC   SAVESEL,THISLSEL                                                 
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
*ONLINE LIST                                                                    
*                                                                               
LR       DS    0H                                                               
         LA    R6,KEY                                                           
         USING BYRRECD,R6                                                       
         XC    SVLISTAR,SVLISTAR   LISTAR'S COMPARED WITH PREV LINE             
*                                  TO MAKE SURE NO MULTI INSTANCES              
         OC    KEY,KEY             IS THIS FIRST TIME AT LIST SCREEN?           
         BNZ   LR10                NO, DO NOT BUILD KEY                         
*                                                                               
         MVC   KEY,SAVEKEY                                                      
*                                                                               
LR10     GOTO1 HIGH               FIND FIRST DIRECTORY REC                      
         B     LR30                                                             
*                                                                               
LR20     GOTO1 SEQ                FIND SUBSEQUENT DIRECTORY RECS                
*                                                                               
LR30     DS    0H                                                               
         CLC   KEY(3),SAVEKEY     IF PAST BUYER RECORDS...                      
         BNE   LRX                STOP READING RECORDS                          
*                                                                               
         GOTO1 GETREC             GET BUYER DATA                                
         MVI   ELCODE,X'02'       LOOKING FOR TV MARKETS 1ST                    
         MVC   SVELCD,ELCODE                                                    
*                                                                               
         MVC   LISTAR,SPACES      CLEAR PRINT LINE OF LIST ENTRIES              
*                                                                               
LR32     CLI   LISTNUM,0          TOP OF THE LIST                               
         BNE   LR35                                                             
         XC    MEDIATBL,MEDIATBL                                                
         LA    R1,MEDIATBL        TABLE LOOKS LIKE  |T|R|X|...                  
         ST    R1,SAVEPTR                                                       
*                                                                               
LR35     L     R6,AIO                                                           
         BAS   RE,GETEL           DO WE HAVE THIS ELEMENT?                      
         BE    LR40               YES? PROCESS                                  
         ZIC   R1,SVELCD          NO?  LOOK FOR THE NEXT ONE                    
         LA    R1,1(R1)                                                         
         STCM  R1,1,ELCODE                                                      
         MVC   SVELCD,ELCODE      SAVE THE ELCODE AGAIN                         
         CLI   ELCODE,X'04'       ARE WE DONE WITH THE 'X' ELEMENT?             
         BH    LR60               YES? LIST THIS REC THEN SEQ                   
         B     LR35               NO?  GO ON TO THE NEXT ELEMENT                
*                                                                               
LR40     CLI   SVELCD,X'02'       T                                             
         BNE   *+8                                                              
         MVI   LSBYRMED,C'T'                                                    
         CLI   SVELCD,X'03'       R                                             
         BNE   *+8                                                              
         MVI   LSBYRMED,C'R'                                                    
         CLI   SVELCD,X'04'       X                                             
         BNE   *+8                                                              
         MVI   LSBYRMED,C'X'                                                    
*                                                                               
LR60     L     R6,AIO                                                           
         USING BYRRECD,R6                                                       
         TM    FLTFLAG,BGFLT      IS THERE AN OFFICE FILTER?                    
         BNO   *+14                                                             
         CLC   BYRKOFC,BGFILTER                                                 
         BNE   LR20                                                             
         MVC   LSOFFCD,BYRKOFC    PRINT BUYER CODE                              
         MVC   OFFICE,BYRKOFC     SAVE OFFICE CODE FOR SUPV REC                 
         MVC   LSBYRCD,BYRKBYR    PRINT BUYER CODE                              
*                                                                               
         MVI   ELCODE,X'01'       FIND BUYER ELEMENT                            
         BAS   RE,GETEL                                                         
*                                                                               
         USING BYRNAMED,R6                                                      
         XC    BLOCK(50),BLOCK                                                  
         MVC   BLOCK(L'BYRLNAME),BYRLNAME                                       
         MVC   BLOCK+15(L'BYRFNAME),BYRFNAME                                    
         GOTO1 SQUASHER,DMCB,BLOCK,(C',',L'LSBYRNM)                             
         MVC   LSBYRNM,BLOCK                                                    
*                                                                               
         MVC   LSBYRFIL,BYRFILT                                                 
         MVC   LSBYRSPV,BYRSPV                                                  
         MVC   SUPV,BYRSPV                                                      
         OC    SUPV,SPACES                                                      
*        MVC   OFFICE,BYROFF                                                    
         MVC   SVKEY1,KEY                                                       
         BAS   RE,GETSPN                                                        
         MVC   LSBYRSNM,BLOCK                                                   
         MVC   KEY,SVKEY1                                                       
         MVC   AIO,AIO1                                                         
         GOTO1 HIGH                                                             
         GOTO1 GETREC                                                           
*                                                                               
         CLC   LISTAR,SVLISTAR      DON'T LIST IDENTICAL ITEM                   
         BE    LR70                                                             
         MVC   SVLISTAR,LISTAR      SAVE LISTAR FOR NEXT COMPARE                
         L     R1,SAVEPTR           BUMP THE TABLE                              
         MVC   0(1,R1),LSBYRMED     STORE THE MEDIA IN TABLE: T,R,X,' '         
         LA    R1,1(R1)                                                         
         ST    R1,SAVEPTR           SAVE THE POINTER TO NEXT ENTRY              
         GOTO1 LISTMON                                                          
LR70     CLI   SVELCD,X'04'         ARE WE DONE WITH MED/MKT ELEMENT?           
         BNL   LR20                 YES? THEN NEXT BUYER RECORD                 
         ZIC   R1,SVELCD                                                        
         LA    R1,1(R1)             BUMP UP THE ELCODE                          
         STCM  R1,1,ELCODE                                                      
         MVC   SVELCD,ELCODE                                                    
         B     LR32                                                             
*                                                                               
LRX      DS    0H                                                               
         MVC   AIO,AIO1                                                         
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
* ADD AND DELETE PASSIVE POINTERS                                               
*                                                                               
XR       DS    0H                                                               
         L     RF,AIO1                                                          
         MVC   SAVEKEY,0(RF)                                                    
*                                                                               
         CLI   ACTNUM,ACTREST           ACTION RESTORE?                         
         BE    XR6                      YES, ADD BACK P-KEYS                    
         CLI   SAVESEL,C'D'                                                     
         BE    XR0                                                              
         CLI   ACTNUM,ACTDEL            ACTION DELETE?                          
         BNE   XR70                     NO, GOTO ROUTINE FOR ADD/CHANGE         
*                                                                               
* FOR XRECDELETE                                                                
*                                                                               
         USING BYRRECD,R4               DELETE SUPV P-KEYS                      
XR0      XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         MVC   BYRPTYP2,=X'0DE3'        SEARCHING FOR POINTERS -                
         MVC   BYRPAGY2,AGYX            - TO THE BUYER RECORD VIA -             
         MVC   BYRPOFC2,OFFICE          - SUPERVISOR                            
*                                                                               
         MVI   RDUPDATE,C'Y'                                                    
         NI    DMINBTS,X'FF'-X'08'      DON'T READ DELETED RECORDS              
         GOTO1 HIGH                                                             
         B     XR2                                                              
*                                                                               
XR1      GOTO1 SEQ                                                              
XR2      CLC   KEY(5),KEYSAVE        PASSIVE POINTER FOUND?                     
         BNE   XRX                   NO                                         
*                                                                               
         CLC   BYRPBYR2,BUYR            YES, CORRECT BUYER?                     
         BNE   XR1                      NO, CHECK NEXT                          
*                                                                               
         CLC   BYRPSPV2,SUPV       SAME SUPV                                    
         BNE   XR1                 THEN DONE                                    
*                                                                               
         OI    KEY+13,X'80'             TURN ON DELETE BIT                      
         GOTO1 WRITE                    WRITE BACK KEY FOR DELETION             
         B     XR1                      CHECK NEXT RECORD                       
*                                                                               
XR3      DS    0H                                                               
*                                                                               
* FOR XRECRESTORE                                                               
XR6      XC    KEY,KEY                  ADD SUPV P-KEYS                         
         LA    R4,KEY                                                           
         MVC   BYRPTYP2,=X'0DE3'        SEARCHING FOR POINTERS -                
         MVC   BYRPAGY2,AGYX            - TO THE BUYER RECORD VIA -             
         MVC   BYRPOFC2,OFFICE          - SUPERVISOR                            
*                                                                               
         MVI   RDUPDATE,C'Y'                                                    
         OI    DMINBTS,X'08'            READ DELETED RECORDS                    
         GOTO1 HIGH                                                             
         B     XR7                                                              
*                                                                               
XR6A     GOTO1 SEQ                                                              
XR7      CLC   KEY(5),KEYSAVE        PASSIVE POINTER FOUND?                     
         BNE   XRX                   NO                                         
*                                                                               
         CLC   BYRPBYR2,BUYR            YES, RIGHT BUYER?                       
         BNE   XR6A                     NO, CHECK NEXT                          
*                                                                               
         CLC   BYRPSPV2,SUPV       SAME SUPV                                    
         BNE   XR6A                THEN DONE                                    
*                                                                               
         NI    KEY+13,X'FF'-X'80'       TURN OFF DELETE BIT                     
         GOTO1 WRITE                    RESTORE P-KEY                           
         B     XR6A                     CHECK NEXT RECORD                       
*                                                                               
XR7A     DS    0H                                                               
*                                                                               
* FOR XRECADD AND XRECPUT                                                       
XR70     DS    0H                       LOGIC FOR SUPV TO BUYER P-KEYS          
         MVC   KEY(13),SAVEKEY                                                  
         GOTO1 HIGH                                                             
         MVC   DISKADD,KEY+14                                                   
*                                                                               
         USING BYRRECD,R4                                                       
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         MVC   BYRPTYP2,=X'0DE3'        SEARCHING FOR POINTERS -                
         MVC   BYRPAGY2,AGYX            - TO THE BUYER RECORD VIA -             
         MVC   BYRPOFC2,OFFICE          - SUPERVISOR                            
*                                                                               
         MVI   RDUPDATE,C'Y'                                                    
         NI    DMINBTS,X'FF'-X'08'      DON'T READ DELETED RECORDS              
         GOTO1 HIGH                                                             
         B     XR78                                                             
*                                                                               
XR74     GOTO1 SEQ                                                              
XR78     CLC   KEY(5),KEYSAVE        PREVIOUS PASSIVE POINTER FOUND?            
         BNE   XR80                  NO, ADD PASS PTR BY SUPV                   
*                                                                               
         CLC   BYRPBYR2,BUYR         IS THIS THE RIGHT BUYER                    
         BNE   XR74                                                             
*                                                                               
         CLC   BYRPSPV2,SUPV       SAME SUPV                                    
         BE    XRX                 THEN DONE                                    
*                                                                               
         OI    KEY+13,X'80'             TURN ON DELETE BIT                      
         GOTO1 WRITE                    WRITE BACK KEY FOR DELETION             
         B     XR74                     CHECK NEXT RECORD                       
*                                                                               
XR80     XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         MVC   BYRPTYP2,=X'0DE3'        SEARCHING FOR POINTERS                  
         MVC   BYRPAGY2,AGYX            TO THIS BUYER RECORD WITH               
         MVC   BYRPOFC2,OFFICE          THE SAME SUPERVISOR                     
         MVC   BYRPSPV2,SUPV                                                    
         MVC   BYRPBYR2,BUYR                                                    
         MVC   KEY+14(4),DISKADD       DISK ADDRESS OF RECORD IN VR             
*                                                                               
         MVI   RDUPDATE,C'Y'                                                    
         OI    DMINBTS,X'08'           READ FOR DELETED RECORDS                 
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(L'BYRPASS),KEYSAVE                                           
         BE    XR90                                                             
         MVC   KEY(18),KEYSAVE          13 FOR KEY, 1 STATUS, 4 D/A             
         GOTO1 ADD                      NO RECORD FOUND, SO ADD P PTR           
         B     XRX                                                              
*                                                                               
XR90     NI    KEY+13,X'FF'-X'80'       UNDELETE OLD PASSIVE POINTER            
         GOTO1 WRITE                                                            
*                                                                               
XRX      MVC   KEY(13),SAVEKEY                                                  
         MVC   AIO,AIO1                 RESTORE DISKADD FOR DISPLAY             
         MVI   RDUPDATE,C'N'            DON'T READ DELETED REC'S -              
         NI    DMINBTS,X'FF'-X'08'      - NEXT CALL TO DISPLAY                  
         CLI   ACTNUM,ACTDEL                                                    
         BNE   DR                       DISPLAY AFTER CHANGE PASS KEYS          
         B     XIT                      JUST EXIT IF ACTION DELETE              
         DROP  R4                                                               
*                                                                               
         EJECT                                                                  
ERRMISPV MVC   ERRNUM,=AL2(MISSUPV)                                             
         B     SPERREX                                                          
ERRELEM  MVC   ERRNUM,=AL2(MISSEL)                                              
         B     SPERREX                                                          
ERRNWS   MVC   ERRNUM,=AL2(BDNWS)                                               
         B     SPERREX                                                          
ERRDAR   MVC   ERRNUM,=AL2(BDDAR)                                               
         B     SPERREX                                                          
ERRDUP   MVC   ERRNUM,=AL2(DUPMK)                                               
         B     SPERREX                                                          
ERRNOSPV MVC   ERRNUM,=AL2(NOSPV)                                               
         B     SPERREX                                                          
ERRNOOFF MVC   ERRNUM,=AL2(NOOFF)                                               
         B     SPERREX                                                          
ERRMKTEX MVC   ERRNUM,=AL2(MKTEX)                                               
         B     SPERREX                                                          
ERRNOMKT MVC   ERRNUM,=AL2(NOMK)                                                
         B     SPERREX                                                          
ERRXALL  MVC   ERRNUM,=AL2(XALL)                                                
         B     SPERREX                                                          
ERRXIND  MVC   ERRNUM,=AL2(XIND)                                                
         B     SPERREX                                                          
ERRNOMIX MVC   ERRNUM,=AL2(NOMIX)                                               
         B     SPERREX                                                          
ERRGRPST MVC   ERRNUM,=AL2(NOGRPSTA)                                            
         B     SPERREX                                                          
ERRINV   MVI   ERROR,INVALID                                                    
         B     VSFMERR                                                          
ERRMIS   MVI   ERROR,MISSING                                                    
         B     VSFMERR                                                          
SPERREX  OI    GENSTAT2,USGETTXT                                                
         LA    RF,GETTXTCB                                                      
         USING GETTXTD,RF                                                       
         MVC   GTMSGNO,ERRNUM                                                   
         MVC   GTMTYP,GTMERR                                                    
         MVI   GTMSYS,2                                                         
VSFMERR  MVC   AIO,AIO1                                                         
         GOTO1 ERREX                                                            
         DROP  RF                                                               
         SPACE 2                                                                
*                                                                               
NOOFF    EQU   432                      OFFICE DOESN'T EXIST                    
NOSPV    EQU   435                      SUPERVISOR DOESN'T EXIST                
NOMK     EQU   434                      MARKET DOESN'T EXIST TO DELETE          
MKTEX    EQU   433                      CAN'T ADD EXISTING MARKET               
DUPMK    EQU   439                      CAN'T ADD + SUBTRACT IN ONE EX          
BDNWS    EQU   441                      BAD NWS CODE                            
BDDAR    EQU   442                      BAD DARE CODE                           
MISSEL   EQU   471                                                              
XALL     EQU   790                      MUST DEL INDIV MKTS BEFORE ALL          
XIND     EQU   791                      MUST DEL "ALL" MKTS BEFORE IND          
NOMIX    EQU   805                      CANNOT MIX MKTS AND MKT GRPS            
MISSUPV  EQU   811                      MISSING SUPV RECORD                     
NOGRPSTA EQU   824                 CANNOT COMBINE GROUP AND STATION             
*                                                                               
         SPACE 2                                                                
CTDISP   DC    Y(L'SAPEKEY+L'SAPELEN+L'SAPESTAT)                                
         GETEL2 R6,CTDISP,ELCODE                                                
         GETEL R6,DATADISP,ELCODE                                               
         EJECT                                                                  
**** ROUTINE TO BUMP TO NEXT SCREEN FIELD ****                                  
NXTSCRF  DS    0H                                                               
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
*---------------------------------------------------------------------          
*   ROUTINE TO VALIDATE OFFICE CODE AGAINST EXISTING OFFICE RECORDS **          
*---------------------------------------------------------------------          
VALOFF   NTR1                                                                   
         CLI   ACTNUM,ACTADD                                                    
         BE    *+12                                                             
         CLI   ACTEQU,13                ACTION TRANSFER?                        
         BNE   VLOFX                    NOT ADD OR TRANSFER, EXIT               
*                                                                               
         XC    TEMPFLD,TEMPFLD                                                  
         MVC   TEMPFLD,=XL9'0900000000010000D5'                                 
         LA    R2,TEMPFLD                                                       
         GOTO1 VALIMED                                                          
         MVC   AGYX,BAGYMD                                                      
         NI    AGYX,X'F0'                                                       
*                                                                               
         LA    R4,KEY              BUILD OFFICE KEY                             
         USING OFCRECD,R4                                                       
         XC    KEY,KEY                                                          
         MVC   OFCKTYP,=X'0D60'                                                 
         MVC   OFCKAGY,AGYX                                                     
         MVC   OFCKOFC,OFFICE      OFFICE CODE                                  
         DROP  R4                                                               
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(L'OFCKEY),KEYSAVE                                            
         BE    VLOFX                                                            
*                                                                               
         LA    R2,BYMOFFH                                                       
         B     ERRNOOFF                                                         
VLOFX    B     XIT                                                              
         EJECT                                                                  
*---------------------------------------------------------------------          
*   ROUTINE TO VALIDATE SUPV CODE AGAINST EXISTING SUPV RECORDS **              
*---------------------------------------------------------------------          
VALSUPV  NTR1                                                                   
         XC    TEMPFLD,TEMPFLD                                                  
         MVC   TEMPFLD,=XL9'0900000000010000D5'                                 
         LA    R2,TEMPFLD                                                       
*        MVI   USEIONUM,2                                                       
         MVC   AIO,AIO2                                                         
         MVI   BYTE,C'A'                                                        
         GOTO1 VALIMED                                                          
         MVC   AIO,AIO1                                                         
         MVC   AGYX,BAGYMD                                                      
         NI    AGYX,X'F0'                                                       
*                                                                               
         LA    R4,KEY              BUILD SUPV KEY                               
         USING SPVRECD,R4                                                       
         XC    KEY,KEY                                                          
         MVC   SPVKTYP,=X'0D61'                                                 
         MVC   SPVKAGY,AGYX                                                     
         MVC   SPVKOFC,OFFICE      OFFICE CODE                                  
*                                                                               
         MVC   SPVKSPV,BYMSUP                                                   
         OC    SPVKSPV,SPACES                                                   
         DROP  R4                                                               
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(L'SPVKEY),KEYSAVE                                            
         BE    VLSPX                                                            
*                                                                               
         LA    R2,BYMSUPH                                                       
         B     ERRNOSPV                                                         
VLSPX    B     XIT                                                              
         EJECT                                                                  
*---------------------------------------------------------------------          
*   ROUTINE TO VALIDATE BYR CODE FOR ACTION TRANSFER **                         
*---------------------------------------------------------------------          
VALBYR   NTR1                                                                   
         XC    TEMPFLD,TEMPFLD                                                  
         MVC   TEMPFLD,=XL9'0900000000010000D5'                                 
         LA    R2,TEMPFLD                                                       
*        MVI   USEIONUM,2                                                       
         MVC   AIO,AIO2                                                         
         MVI   BYTE,C'A'                                                        
         GOTO1 VALIMED                                                          
         MVC   AIO,AIO1                                                         
         MVC   AGYX,BAGYMD                                                      
         NI    AGYX,X'F0'                                                       
*                                                                               
         LA    R4,KEY              BUILD SUPV KEY                               
         USING BYRRECD,R4                                                       
         XC    KEY,KEY                                                          
         MVC   BYRKTYP,=X'0D62'                                                 
         MVC   BYRKAGY,AGYX                                                     
         MVC   BYRKOFC,OFFICE      OFFICE CODE                                  
*                                                                               
         MVC   BYRKBYR,BUYRVAL                                                  
         OC    BYRKBYR,SPACES                                                   
         DROP  R4                                                               
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(L'BYRKEY),KEYSAVE                                            
         BE    VLSPX                                                            
*                                                                               
         LR    R2,R3                    POINT R2 TO APPROP BUYER FIELD          
         B     ERRINV                  NEW ERROR CODE?                          
VLBYX    B     XIT                                                              
         EJECT                                                                  
*---------------------------------------------------------------------          
*   ROUTINE TO GET SUPERVISOR NAME **                                           
*---------------------------------------------------------------------          
GETSPN   NTR1                                                                   
         XC    BLOCK(50),BLOCK                                                  
         XC    TEMPFLD,TEMPFLD                                                  
         MVC   TEMPFLD,=XL9'0900000000010000D5'                                 
         LA    R2,TEMPFLD                                                       
*        MVI   USEIONUM,2                                                       
         MVC   AIO,AIO2                                                         
         MVI   BYTE,C'A'                                                        
         GOTO1 VALIMED                                                          
         MVC   AIO,AIO1                                                         
         MVC   AGYX,BAGYMD                                                      
         NI    AGYX,X'F0'                                                       
*                                                                               
         LA    R4,KEY              BUILD SUPV KEY                               
         USING SPVRECD,R4                                                       
         XC    KEY,KEY                                                          
         MVC   SPVKTYP,=X'0D61'                                                 
         MVC   SPVKAGY,AGYX                                                     
         MVC   SPVKOFC,OFFICE      OFFICE CODE                                  
         MVC   SPVKSPV,SUPV                                                     
         DROP  R4                                                               
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(L'SPVKEY),KEYSAVE                                            
         BE    GTSP10                                                           
         CLI   ACTNUM,ACTLIST      ACTION LIST?                                 
         BE    GTSPX                                                            
         LA    R2,BYMSUPH                                                       
         B     ERRMISPV            MISSING SUPV REC                             
*                                                                               
GTSP10   MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'01'             SUPERVISOR NAME ELEMENT                 
         BAS   RE,GETEL                                                         
*                                                                               
         USING SPVNAMED,R6                                                      
         XC    BLOCK(50),BLOCK                                                  
         MVC   BLOCK(L'SPVLNAME),SPVLNAME                                       
         MVC   BLOCK+15(L'SPVFNAME),SPVFNAME                                    
         GOTO1 SQUASHER,DMCB,BLOCK,(C',',L'BYMSPN)                              
*                                                                               
GTSPX    DS    0H                                                               
         MVC   AIO,AIO1                                                         
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
*---------------------------------------------------------------------          
*    SUBROUTINE TO ADD CLIENT A SINGLE CLIENT ELEMENT ****                      
*---------------------------------------------------------------------          
ADDMKT   NTR1                                                                   
         CLI   5(R2),4                                                          
         BH    ERRINV                                                           
*                                                                               
         L     R6,AIO                                                           
*        MVI   USEIONUM,2                                                       
         MVC   AIO,AIO2                                                         
         GOTO1 VALIMKT                  ALSO HAS A DM CALL                      
         MVC   AIO,AIO1                                                         
*                                                                               
         USING BYRMKTD,R6                                                       
         L     R6,AIO                                                           
         MVC   ELCODE,MEDELCD                                                   
         BAS   RE,GETEL                                                         
         B     *+8                                                              
*                                                                               
AMK10    BAS   RE,NEXTEL                                                        
         BNE   AMK20                                                            
         CLC   BYRMKT,BMKT                                                      
         BE    ERRMKTEX            MARKET ALREADY EXISTS                        
         CLC   BYRMKT,=X'FFFF'     ALL MKT ALREADY ADDED                        
         BE    ERRXIND                                                          
         B     AMK10                                                            
         DROP  R6                                                               
*                                                                               
AMK20    XC    ELEM,ELEM                                                        
         LA    R5,ELEM                                                          
         USING BYRMKTD,R5                                                       
*                                                                               
         MVC   BYRMKTEL,MEDELCD    ELEMENT CODE                                 
         MVI   BYRMKTLN,4          ELEMENT LENGTH                               
         MVC   BYRMKT,BMKT                                                      
*                                                                               
         GOTO1 ADDELEM                                                          
*                                                                               
ACX      B     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
*---------------------------------------------------------------------          
*    SUBROUTINE TO DELETE CLIENT ****                                           
*---------------------------------------------------------------------          
DELMKT   NTR1                                                                   
         USING BYRMKTD,R6                                                       
         L     R6,AIO                                                           
         MVC   ELCODE,MEDELCD                                                   
         BAS   RE,GETEL                                                         
         B     *+8                                                              
*                                                                               
DEL10    BAS   RE,NEXTEL                                                        
         BNE   ERRNOMKT                                                         
         CLC   BYRMKT,BMKT2                                                     
         BNE   DEL10                                                            
*                                                                               
         MVI   0(R6),X'FF'                                                      
         MVI   ELCODE,X'FF'                                                     
         GOTO1 REMELEM                                                          
         DROP  R6                                                               
DELX     B     XIT                                                              
         EJECT                                                                  
*---------------------------------------------------------------------          
*    SUBROUTINE TO ADD MKT GROUP ****                                           
*---------------------------------------------------------------------          
ADDMG    NTR1                                                                   
         USING BYRMKGD,R6                                                       
*                                                                               
         L     R6,AIO                                                           
         MVC   ELCODE,MEDELCD                                                   
         OI    ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         B     *+8                                                              
*                                                                               
AMG10    BAS   RE,NEXTEL                                                        
         BNE   AMG20                                                            
         CLC   BYRMKGID,MGRPID                                                  
         BNE   AMG12                                                            
         CLC   BYRMKGRP,MGRPNO                                                  
         BE    ERRDUP              MARKET GROUP ALREADY EXISTS ON REC           
AMG12    B     AMG10                                                            
*                                                                               
AMG20    XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
*                                                                               
         MVC   BYRMKGEL,MEDELCD    ELEMENT CODE                                 
         OI    BYRMKGEL,X'10'                                                   
         MVI   BYRMKGLN,5          ELEMENT LENGTH                               
         MVC   BYRMKGID,MGRPID                                                  
         MVC   BYRMKGRP,MGRPNO                                                  
*                                                                               
         GOTO1 ADDELEM                                                          
*                                                                               
AMGX     B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
*---------------------------------------------------------------------          
*    SUBROUTINE TO DELETE MKT GROUP ****                                        
*---------------------------------------------------------------------          
DELMG    NTR1                                                                   
         USING BYRMKGD,R6                                                       
         L     R6,AIO                                                           
         MVC   ELCODE,MEDELCD                                                   
         OI    ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         B     *+8                                                              
*                                                                               
DELMG10  BAS   RE,NEXTEL                                                        
         BNE   ERRNOMKT            <====                                        
         CLC   BYRMKGID,MGRPID                                                  
         BNE   DELMG10                                                          
         CLC   BYRMKGRP,MGRPNO                                                  
         BNE   DELMG10                                                          
*                                                                               
         MVI   0(R6),X'FF'                                                      
         MVI   ELCODE,X'FF'                                                     
         GOTO1 REMELEM                                                          
         DROP  R6                                                               
DELMGX   B     XIT                                                              
         EJECT                                                                  
*---------------------------------------------------------------------          
*        VALIDATE NWS CODE                                                      
*---------------------------------------------------------------------          
VALNWS   NTR1                                                                   
         XC    KEY,KEY                                                          
         USING NBYRRECD,R4                                                      
         LA    R4,KEY                                                           
         MVI   NBYRKTYP,X'0D'                                                   
         MVI   NBYRKSUB,X'65'                                                   
         MVC   NBYRKAGMD,BAGYMDS                                                
         MVC   NBYRKBYR,SAVENWS                                                 
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(6),KEY                                                   
         BNE   ERRNWS                                                           
         DROP  R4                                                               
VNX      B     XIT                                                              
         EJECT                                                                  
*---------------------------------------------------------------------          
*        VALIDATE DARE CODE                                                     
*---------------------------------------------------------------------          
VALDARE  NTR1                                                                   
         XC    KEY,KEY                                                          
         USING DBYRRECD,R4                                                      
         LA    R4,KEY                                                           
         MVI   DBYRKTYP,X'0D'                                                   
         MVI   DBYRKSUB,X'31'           WHAT ABOUT 'C1' IN SPADBUYER?           
         MVC   DBYRKAM,BAGYMDS                                                  
         MVC   DBYRKBYR,SAVEDAR                                                 
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(6),KEY                                                   
         BNE   ERRDAR                                                           
         DROP  R4                                                               
VDX      B     XIT                                                              
         EJECT                                                                  
*---------------------------------------------------------------------          
*        GETS PID NAME USING PIDNUM                                             
*---------------------------------------------------------------------          
GETPIDNM NTR1                                                                   
         USING SA0REC,R6                                                        
         XC    KEY2,KEY2                                                        
         LA    R6,KEY2                                                          
         MVI   SA0KTYP,SA0KTYPQ    C'0' - PERSONAL AUTH. RECORD                 
         MVC   SA0KAGY,SECALPHA    SECURITY ALPHA ID                            
         MVC   SA0KNUM,PIDNUM                                                   
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'CTFILE ',KEY2,AIO2                    
         L     R6,AIO2                                                          
         CLC   KEY2(L'SA0KEY),0(R6)                                             
         BNE   GPIDX                                                            
         TM    SA0STAT,X'20'       LOCKED                                       
         BO    GPIDX                                                            
*                                                                               
         USING SAPALD,R6                                                        
         MVI   ELCODE,SAPALELQ     X'C3' - PERSONAL ID ELEM                     
         BAS   RE,GETEL2                                                        
         BNE   GPIDX                                                            
         MVC   PIDNAME,SAPALPID                                                 
GPIDX    XIT1                                                                   
         DROP  R6                                                               
         EJECT                                                                  
*---------------------------------------------------------------------          
*        VALIDATES PID NAME AND GETS PID NUMBER                                 
*---------------------------------------------------------------------          
VALPID   NTR1                                                                   
         USING SAPEREC,R6                                                       
         XC    KEY2,KEY2                                                        
         LA    R6,KEY2                                                          
         MVI   SAPETYP,SAPETYPQ    C'F' - SECURITY PERSON REC                   
         MVI   SAPESUB,SAPESUBQ    X'04'                                        
         MVC   SAPEAGY,SECALPHA    SECURITY ALPHA ID                            
         MVC   SAPEPID,PIDNAME                                                  
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'CTFILE ',KEY2,AIO2                    
         L     R6,AIO2                                                          
         CLC   KEY2(SAPEDEF-SAPEKEY),0(R6)                                      
         BNE   VPIDXNO                                                          
*                                                                               
         USING SAPWDD,R6                                                        
         MVI   ELCODE,SAPWDELQ     X'C4' - PERSON PASSWORD ELEM                 
         BAS   RE,GETEL2                                                        
         BNE   VPIDXNO                                                          
         MVC   PIDNUM,SAPWDNUM                                                  
*                                                                               
VPIDXYES B     XYES                                                             
VPIDXNO  B     XNO                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        CLEARS BOTTOM OF SCREEN                                                
***********************************************************************         
*                                                                               
CLRSCR   NTR1                     CLEARS SCREEN FROM ADD MKT DOWN               
         LA    R2,BYMSTAH                                                       
         CLI   ACTEQU,13                ACTION TRANSFER?                        
         BNE   *+8                                                              
         LA    R2,BYTOKH                "ENTER OK" ON TRANSFER SCREEN           
*                                                                               
         LA    R3,BYMPFKYH              SAME ADD. ON TRANSF & DISP              
         CLI   ACTEQU,13                ACTION TRANSFER?                        
         BNE   *+8                                                              
         LA    R3,BYTPFKYH              "ENTER OK" ON TRANSFER SCREEN           
*                                                                               
CLRSCR10 ZIC   R1,0(R2)            FIELD LENGTH                                 
         SH    R1,=H'9'            8 FOR HEADER, 1 FOR EX                       
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),SPACES                                                   
         MVI   5(R2),0                                                          
         OI    6(R2),X'80'         TRANSMIT                                     
         ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         CR    R2,R3               ARE WE DONE                                  
         BL    CLRSCR10            NO                                           
         B     XIT                                                              
         EJECT                                                                  
*---------------------------------------------------------------------          
* ROUTINE TO LIST MARKETS BY MEDIA                                              
*---------------------------------------------------------------------          
MKLS     NTR1                                                                   
* PFKEY STUFF                                                                   
         CLI   PFAID,8                  DOWN?                                   
         BNE   ML10                     NO CHECK FOR PF6= TOP                   
         LA    R2,BYMENDH                                                       
         CLI   ACTEQU,13                                                        
         BNE   *+8                                                              
         LA    R2,BYTENDH                                                       
         CLI   7(R2),0              LAST FIELD BLANK?                           
         BNE   *+10                     NO, PAGE DOWN NORMALLY                  
         MVC   LASTSTA,FRSTSTA          YES,DON'T PAGE DOWN                     
         MVC   STARTSTA,LASTSTA                                                 
         B     ML20                                                             
*                                                                               
ML10     CLI   PFAID,6                  TOP?                                    
         BNE   *+10                                                             
         XC    FRSTSTA,FRSTSTA          START AT FIRST CLIENT KEY               
*                                                                               
         MVC   STARTSTA,FRSTSTA         IF ANY OTHER PFKEY,START                
*                                                                               
ML20     XC    FRSTSTA,FRSTSTA                                                  
         XC    LASTSTA,LASTSTA                                                  
*                                                                               
         TM    STATUS,KEYCHG            DID KEY CHANGE?                         
         BNO   *+10                     NO, DISPLAY FROM STARTCLI               
         XC    STARTSTA,STARTSTA        YES, DISPLAY FROM FIRST CLIENT          
*                                                                               
         XC    KEY,KEY                                                          
         USING BYRRECD,R4                                                       
         LA    R4,KEY                                                           
         MVC   BYRPTYP,=X'0DE2'                                                 
         MVC   BYRPAM,BAGYMD                                                    
         MVC   BYRPOFC,OFFICE                                                   
         MVC   BYRPMKT,STARTSTA                                                 
         MVC   SAVEKEY2,KEY                                                     
*                                                                               
         USING DMKTEL,R3                                                        
         LA    R3,BYMCODEH                                                      
         GOTO1 HIGH                                                             
         B     ML35                                                             
ML25     MVC   KEY(L'SAVEKEY2),SAVEKEY2                                         
         GOTO1 HIGH                                                             
ML30     GOTO1 SEQ                                                              
ML35     CLC   KEY(5),SAVEKEY2                                                  
         BNE   MLX                                                              
*                                                                               
         MVC   SAVEKEY2,KEY                                                     
         CLC   BYRPBYR,BUYR                                                     
         BNE   ML30                                                             
*                                                                               
         CLC   BYRPMKT,=X'FFFF'                                                 
         BNE   ML40                                                             
         MVC   DMKTCD(3),=C'ALL'                                                
         MVC   DMKTNM(11),=C'ALL MARKETS'                                       
         OI    6(R3),X'80'                                                      
         B     MLX                                                              
*                                                                               
ML40     ZICM  R0,BYRPMKT,2                                                     
         CVD   R0,DUB                                                           
         UNPK  DMKTCD,DUB                                                       
         OI    DMKTCD+3,X'F0'                                                   
*                                                                               
         LA    R1,BYMCODEH                                                      
         CR    R3,R1                                                            
         BNE   *+10                     NO                                      
         MVC   FRSTSTA,BYRPMKT          YES, STORE FIRST MARKET                 
*                                                                               
         MVC   LASTSTA,BYRPMKT          STORE LAST MARKET EVERY TIME            
*                                                                               
         XC    TEMPFLD,TEMPFLD                                                  
         MVC   TEMPFLD(8),=X'0000000008040000'                                  
         MVC   TEMPFLD+8(4),DMKTCD                                              
         LA    R2,TEMPFLD                                                       
*        MVI   USEIONUM,3                                                       
         MVC   AIO,AIO3                                                         
         GOTO1 VALIMKT                                                          
         MVC   AIO,AIO2                                                         
         DROP  R4                                                               
*                                                                               
         MVC   DMKTNM,MKTNM                                                     
         OI    6(R3),X'80'                                                      
*                                                                               
         LA    R3,MKDLQ(R3)                                                     
         LA    R1,BYMENDH               SAME ADD. ON TRANS AND DIS              
         CR    R3,R1                                                            
         BNH   ML25                                                             
         DROP  R3                                                               
MLX      B     XIT                                                              
         EJECT                                                                  
*---------------------------------------------------------------------          
* ROUTINE TO LIST STATION GROUPS BY MEDIA                                       
*---------------------------------------------------------------------          
STGLS    NTR1                                                                   
* PFKEY STUFF                                                                   
         CLI   PFAID,8                  DOWN?                                   
         BNE   STGL10                   NO CHECK FOR PF6= TOP                   
         LA    R2,BYMENDH                                                       
         CLI   7(R2),0              LAST FIELD BLANK?                           
         BNE   *+10                     NO, PAGE DOWN NORMALLY                  
         MVC   LASTSTG,FRSTSTG         YES,DON'T PAGE DOWN                      
         MVC   STARTSTG,LASTSTG                                                 
         B     STGL20                                                           
*                                                                               
STGL10   CLI   PFAID,6                  TOP?                                    
         BNE   *+10                                                             
         XC    FRSTSTG,FRSTSTG          START AT FIRST CLIENT KEY               
         MVC   STARTSTG,FRSTSTG         IF ANY OTHER PFKEY,START                
*                                                                               
STGL20   XC    FRSTSTG,FRSTSTG                                                  
         XC    LASTSTG,LASTSTG                                                  
*                                                                               
         TM    STATUS,KEYCHG            DID KEY CHANGE?                         
         BNO   *+10                     NO, DISPLAY FROM STARTCLI               
         XC    STARTSTG,STARTSTG        YES, DISPLAY FROM FIRST CLIENT          
*                                                                               
         LA    R2,BYMCODEH         FIRST DISPLAY FIELD                          
*                                                                               
         USING BYRMKGD,R6                                                       
         L     R6,AIO1                                                          
         MVC   ELCODE,MEDELCD                                                   
         OI    ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         B     *+8                                                              
STGL30   BAS   RE,NEXTEL                                                        
         BNE   STGLX               NO MORE STA GROUPS                           
         CLC   STARTSTG,BYRMKGID   ARE WE AT START                              
         BH    STGL30              NO, GET NEXT                                 
*                                                                               
         LA    R1,BYMCODEH                                                      
         CR    R2,R1                                                            
         BNE   *+10                     NO                                      
         MVC   FRSTSTG,BYRMKGID         YES, STORE FIRST MARKET                 
         MVC   LASTSTG,BYRMKGID         STORE LAST MARKET EVERY TIME            
*                                                                               
         MVC   MGNAME,SPACES                                                    
         MVI   MGRPLEN,4           DEFAULT TO 4 - IN CASE REC IS GONE           
         USING MKGRECD,R4                                                       
         LA    R4,KEY                   GET LEN FROM ID REC                     
         XC    KEY,KEY                                                          
         MVC   MKGKTYP,=X'0D02'                                                 
         MVC   MKGKAGMD,BAGYMD                                                  
         XC    MKGKCLT,MKGKCLT          BINARY CLIENT CODE                      
         XC    MKGKPID,MKGKPID                                                  
         XC    MKGKPGRP,MKGKPGRP                                                
         MVC   MKGKMID,BYRMKGID                                                 
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE          IF MARKET DEFIN DOESN'T EXIST           
         BNE   ERRINV                   MGROUP NOT VALID                        
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
         L     R4,AIO2                                                          
         LA    R4,24(R4)                                                        
         CLI   0(R4),X'01'         HAS TO BE FIRST                              
         BE    *+6                                                              
         DC    H'0'                                                             
         USING MKGEL01,R4                                                       
         ZIC   R1,MKGBK1LN                                                      
         ZIC   R0,MKGBK2LN                                                      
         AR    R1,R0                                                            
         ZIC   R0,MKGBK3LN                                                      
         AR    R1,R0                                                            
         STC   R1,MGRPLEN                                                       
*                                                                               
         USING MKGRECD,R4                                                       
         LA    R4,KEY                   GET LEN FROM ID REC                     
         XC    KEY,KEY                                                          
         MVC   MKGKTYP,=X'0D02'                                                 
         MVC   MKGKAGMD,BAGYMD                                                  
         XC    MKGKCLT,MKGKCLT          BINARY CLIENT CODE                      
         XC    MKGKPID,MKGKPID                                                  
         XC    MKGKPGRP,MKGKPGRP                                                
         MVC   MKGKMID,BYRMKGID                                                 
         MVC   MKGKMGRP,BYRMKGRP                                                
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE          IF MARKET GROUP DOESN'T EXIST           
         BNE   ERRINV                   MGROUP NOT VALID                        
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
         L     R4,AIO2                                                          
         LA    R4,24(R4)                                                        
STGL40   CLI   0(R4),0                                                          
         BE    STGL50                                                           
         CLI   0(R4),X'10'         NAMES ELEM                                   
         BE    STGL42                                                           
         ZIC   R1,1(R4)                                                         
         AR    R4,R1                                                            
         B     STGL40                                                           
*                                                                               
         USING MKGEL10,R4                                                       
STGL42   MVC   MGNAME,MKGNAM1      PICK OUT MOST SPECIFIC NAME                  
         CLC   MKGNAM2,SPACES                                                   
         BNH   *+10                                                             
         MVC   MGNAME,MKGNAM2                                                   
         CLC   MKGNAM3,SPACES                                                   
         BNH   *+10                                                             
         MVC   MGNAME,MKGNAM3                                                   
*                                                                               
STGL50   XC    BLOCK(100),BLOCK    SET UP FOR SQUASHER                          
         MVC   BLOCK(1),BYRMKGID   MKT GRP ID                                   
*                                                                               
         UNPK  DUB,BYRMKGRP(3)                                                  
         ZIC   R1,MGRPLEN                                                       
         AHI   R1,-1                                                            
         EX    R1,*+4                                                           
         MVC   BLOCK+1(0),DUB+3    MKT GROUP NUMBER                             
*                                                                               
         MVC   BLOCK+8(L'MGNAME),MGNAME    MOST SPECIFIC NAME                   
*                                                                               
         GOTO1 SQUASHER,DMCB,BLOCK,50                                           
         MVC   8(L'BYMCODE,R2),BLOCK                                            
         OI    6(R2),X'80'                                                      
*                                                                               
         LA    R2,MKDLQ(R2)                                                     
         LA    R1,BYMENDH                                                       
         CR    R2,R1                                                            
         BNH   STGL30                                                           
*                                                                               
STGLX    MVC   AIO,AIO1                                                         
         B     XIT                                                              
         DROP  R6,R4                                                            
         EJECT                                                                  
***********************************************************************         
*        SETUP                                                                  
***********************************************************************         
*                                                                               
SETUP    NTR1                                                                   
         OI    GENSTAT1,USKYMRG+NOSETEFH                                        
         OI    CONSERVH+1,X'01'    MODIFY SERVICE REQUEST                       
         OI    CONSERVH+6,X'80'    TRANSMIT TO GET CONTROL                      
*                                                                               
SET10    CLI   ACTNUM,ACTLIST      ACTION LIST?                                 
         BE    SETUPX                                                           
*                                                                               
         MVC   MPF02ACT,=C'        '                                            
         CLC   =C'SELECT',CONACT   ACTION SELECT?                               
         BNE   *+10                                                             
         MVC   MPF02ACT,=CL8'DISPLAY'                                           
*                                                                               
         MVC   MPF03ACT,=C'        '                                            
         CLC   =C'SELECT',CONACT   ACTION SELECT?                               
         BNE   *+10                                                             
         MVC   MPF03ACT,=CL8'DISPLAY'                                           
*                                                                               
         MVC   MPF06ACT,=C'        '                                            
         CLC   =C'SELECT',CONACT   ACTION SELECT?                               
         BNE   *+10                                                             
         MVC   MPF06ACT,=CL8'DISPLAY'                                           
*                                                                               
         MVC   MPF08ACT,=C'        '                                            
         CLC   =C'SELECT',CONACT   ACTION SELECT?                               
         BNE   *+10                                                             
         MVC   MPF08ACT,=CL8'DISPLAY'                                           
*                                                                               
*        CLI   CALLSP,0             ANYTHING WAITING TO RETURN TO?              
         LR    RE,RA                                                            
         AH    RE,=Y(TWAENDLQ-2)                                                
         CLI   1(RE),0                                                          
         BE    *+8                  NO                                          
         NI    BYMREH+1,X'FF'-X'04' LIGHT UP PF12=RETURN FIELD                  
         OI    BYMREH+6,X'80'       TRANSMIT THE RESULT                         
*                                                                               
SETUP99  GOTO1 VINITPF,DMCB,MPFTABLE                                            
SETUPX   B     XIT                                                              
         EJECT                                                                  
*                                                                               
MPFTABLE DS    0X                                                               
* PF02 = OFFICE                                                                 
         DC    AL1(MPF02X-*,02,PFTCPROG,(MPF02X-MPF02)/KEYLNQ,0)                
         DC    CL3'   '                                                         
         DC    CL8'BUYGRP '        RECORD: OFFICE                               
MPF02ACT DC    CL8'       '        ACTION:                                      
MPF02    DC    AL1(KEYTYTWA,L'BYMOFF-1),AL2(BYMOFF-T31CFFD)                     
MPF02X   EQU   *                                                                
*                                                                               
* PF03 = SUPERVISOR                                                             
         DC    AL1(MPF03X-*,03,PFTCPROG,(MPF03X-MPF03)/KEYLNQ,0)                
         DC    CL3'   '                                                         
         DC    CL8'SUPV   '        RECORD: SUPERVISOR                           
MPF03ACT DC    CL8'       '        ACTION:                                      
MPF03    DC    AL1(KEYTYTWA,L'BYMOFF-1),AL2(BYMOFF-T31CFFD)                     
         DC    AL1(KEYTYTWA,L'BYMBYC-1),AL2(BYMSUP-T31CFFD)                     
MPF03X   EQU   *                                                                
*                                                                               
* PF06 = TOP                                                                    
         DC    AL1(MPF06X-*,06,0,0,PFTRETRN)                                    
         DC    CL3' ',CL8' '                                                    
MPF06ACT DC    CL8' '                                                           
MPF06X   EQU   *                                                                
*                                                                               
* PF08 = DOWN                                                                   
         DC    AL1(MPF08X-*,08,0,0,PFTRETRN)                                    
         DC    CL3' ',CL8' '                                                    
MPF08ACT DC    CL8' '                                                           
MPF08X   EQU   *                                                                
*                                                                               
* PF12 = RETURN TO CALLER                                                       
         DC    AL1(RETCALL-*,12,PFTRPROG,0,0)                                   
         DC    CL3' ',CL8' ',CL8' '                                             
RETCALL  EQU   *                                                                
         DC    X'FF'                                                            
*                                                                               
         EJECT                                                                  
***********************************************************************         
*        LTORG                                                                  
***********************************************************************         
         EJECT                                                                  
         LTORG                                                                  
STAHEAD  DC    CL79'STATION LIST'                                               
*                                                                               
**** TABLE FOR MEDIA AND ELEMENT CODES ****                                     
MEDTAB   DS    0H                                                               
         DC    CL1'T',XL1'02',XL1'12'                                           
MTABLQ   EQU   *-MEDTAB                                                         
         DC    CL1'R',XL1'03',XL1'13'                                           
         DC    CL1'X',XL1'04',XL1'14'                                           
         DC    CL1'N',XL1'05',XL1'15'                                           
         DC    CL1'C',XL1'06',XL1'16'                                           
         DC    CL1'S',XL1'07',XL1'17'                                           
         DC    CL1'D',XL1'08',XL1'18'                                           
         DC    CL1'H',XL1'09',XL1'19'                                           
         DC    CL1'0',XL1'0A',XL1'20'                                           
         DC    X'FF'                                                            
*                                                                               
         EJECT                                                                  
*                                                                               
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
         PRINT ON                                                               
       ++INCLUDE NESFMFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NESFMA4D          MAINTENANCE SCREEN                           
         ORG   CONTAGH                                                          
       ++INCLUDE NESFMA5D          BUYER ACTION TRANSFER SCREEN                 
         ORG   CONTAGH                                                          
       ++INCLUDE NESFMA6D          LIST SCREEN                                  
         EJECT                                                                  
       ++INCLUDE DDGENTWA          LIST SCREEN                                  
         EJECT                                                                  
       ++INCLUDE SPGENBYR                                                       
         EJECT                                                                  
       ++INCLUDE SPGENGRP                                                       
         EJECT                                                                  
       ++INCLUDE SPGENMKG                                                       
         EJECT                                                                  
       ++INCLUDE SPGENOFC                                                       
         EJECT                                                                  
       ++INCLUDE SPGENSPV                                                       
         EJECT                                                                  
       ++INCLUDE SEACSFILE                                                      
       ++INCLUDE FASECRETD                                                      
         EJECT                                                                  
*PREFIX=N                                                                       
       ++INCLUDE SPNWSBYR                                                       
         EJECT                                                                  
*PREFIX=D                                                                       
       ++INCLUDE SPADBUYER                                                      
         EJECT                                                                  
*PREFIX=                                                                        
       ++INCLUDE NESFMWORKD                                                     
         EJECT                                                                  
* START OF SAVED STORAGE (6144)                                                 
*                                                                               
         ORG   SYSSPARE                                                         
*                                                                               
RELO     DS    A                   RELOCATION FACTOR                            
OFFICE   DS    CL2                                                              
BUYR     DS    CL4                      SAVED BUYER CODE                        
BUYR2    DS    CL4                      SAVED NEW BUYER CODE                    
BUYRVAL  DS    CL4                      SAVED NEW BUYER CODE                    
SUPV     DS    CL4                      STORED SUPV CODE                        
OLDSPV   DS    CL4                                                              
SAVENWS  DS    CL3                                                              
SAVEDAR  DS    CL3                                                              
STARTSTA DS    XL4                                                              
LASTSTA  DS    XL4                                                              
FRSTSTA  DS    XL4                                                              
STARTSTG DS    XL3                                                              
LASTSTG  DS    XL3                                                              
FRSTSTG  DS    XL3                                                              
SAVEKEY  DS    CL13                                                             
SAVEKEY2 DS    CL13                                                             
MEDELCD  DS    XL1                                                              
SVGRPID  DS    XL1                                                              
SVMEDTYP DS    XL1                                                              
TEMPFLD  DS    XL12                     8 BYTE HEADER + 4 MKT CODE              
ERRNUM   DS    XL2                                                              
BMKT2    DS    XL2                                                              
         DS    0H                       FORCE ALIGNMENT FOR MKT STOR            
*                                                                               
TMPASTG  DS    F                   TEMP ALPHA FIELD FOR GROUP #                 
*                                                                               
TMPSTGID DS    XL1                 GROUP ID                                     
TMPSTG   DS    XL2                 GROUP                                        
TMPSTA   DS    XL4                 STATION                                      
SVBYRKEY DS    XL13                                                             
*                                                                               
MYFLAG   DS    XL1                 FLAGS                                        
HASSGRP  EQU   X'01'               HAS STATION GROUP                            
HASSTA   EQU   X'02'               HAS STATION                                  
FROMVR   EQU   X'04'               FROM VREC                                    
*                                                                               
AGYX     DS    XL1                      AGENCY HEX, MEDIA=0                     
BAGYMDS  DS    XL1                      AGENCY/MEDIA CODE FOR SUBROUT'S         
STATUS   DS    XL1                                                              
KEYCHG   EQU   X'80'                                                            
DISKADD  DS    XL4                                                              
SAVESEL  DS    CL1                                                              
SVKEY1   DS    CL48                                                             
MEDIATBL DS    CL20                     LIST OF MEDIA IN ORDER OF LIST          
SAVEPTR  DS    F                        POINTER TO THE MEDIATABLE               
SVELCD   DS    X                        SAVE THE ELCODE                         
SVLISTAR DS    CL80                     BACK UP OF PREVIOUS LISTLINE            
SCANAREA DS    CL256                    AREA USED FOR SCANNER                   
FLTFLAG  DS    XL1                                                              
BGFLT    EQU   X'80'                    BUYGRP FILTER                           
BGFILTER DS    CL2                                                              
PIDNAME  DS    CL8                                                              
PIDNUM   DS    XL2                                                              
SECALPHA DS    CL2                                                              
KEY2     DS    CL50                                                             
ADMGPTR  DS    F                                                                
DELMGPTR DS    F                                                                
MGNAME   DS    CL24                                                             
*                                                                               
       ++INCLUDE FAGETTXTD                                                      
       ++INCLUDE FAFACTS                                                        
         EJECT                                                                  
**** ONLINE LIST LINE                                                           
*                                                                               
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
LSOFFCD  DS    CL2                 STAFF MEMBER NAME (IN E-MAIL FORM)           
         DS    CL5                                                              
LSBYRCD  DS    CL4                 STAFF MEMBER NAME (IN E-MAIL FORM)           
         DS    CL3                                                              
LSBYRNM  DS    CL21                                                             
         DS    CL2                                                              
LSBYRMED DS    CL1                 MEDIA WITH MARKETS                           
         DS    CL4                                                              
LSBYRFIL DS    CL1                 FILTER                                       
         DS    CL3                                                              
LSBYRSPV DS    CL4                 SUPERVISOR NAME (IN E-MAIL FORM)             
         DS    CL2                                                              
LSBYRSNM DS    CL19                SUPERVISOR NAME                              
         EJECT                                                                  
*                                                                               
DMKTEL   DSECT                            FOR DISPLAYING A MARKET LIST          
DMKTHD   DS    CL8                      OUTPUT FIELD HEADER                     
DMKTCD   DS    CL4                      MARKET CODE                             
         DS    CL2                                                              
DMKTNM   DS    CL17                     MARKET NAME                             
MKDLQ    EQU   *-DMKTHD                 LENGTH                                  
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'126NESFM4C   10/31/05'                                      
         END                                                                    
