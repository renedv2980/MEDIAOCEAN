*          DATA SET CTLFM00    AT LEVEL 022 AS OF 02/25/19                      
*PHASE TA0200A                                                                  
*INCLUDE SCUNKEY                                                                
         TITLE 'CTLFM00 - CONTROL FILE MAINT - ROOT'                            
CTLFM00  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 LFMTEMPX-LFMTEMP,**LFM0**,RA,RR=R7                               
         LR    R9,RC                                                            
         USING LFMTEMP,R9          R9=A(TEMP W/S)                               
         ST    R9,ATEMP                                                         
         ST    R1,APARM                                                         
         ST    RB,ABASE                                                         
         MVC   PARM,0(R1)                                                       
         MVC   ACOMFACS,PARM+12                                                 
         MVC   ATIOB,PARM+28                                                    
         L     R3,ATWA                                                          
         USING LFMSAVE,R3          R3=A(SAVE W/S)                               
         EJECT                                                                  
*                                  INITIALISE WORKING STORAGE VALUES            
         LA    R1,ACTNTBL                                                       
         ST    R1,AACTNTBL                                                      
         LA    R1,TYPETBL                                                       
         ST    R1,ATYPETBL                                                      
         LA    R1,READ                                                          
         ST    R1,AREAD                                                         
         LA    R1,READHI                                                        
         ST    R1,AREADHI                                                       
         LA    R1,RSEQ                                                          
         ST    R1,ARSEQ                                                         
         LA    R1,WRITE                                                         
         ST    R1,AWRITE                                                        
         LA    R1,ADD                                                           
         ST    R1,AADD                                                          
         LA    R1,FVAL                                                          
         ST    R1,AFVAL                                                         
         LA    R1,BLDREC                                                        
         ST    R1,ABLDREC                                                       
         LA    R1,DELEL                                                         
         ST    R1,ADELEL                                                        
         LA    R1,PUTEL                                                         
         ST    R1,APUTEL                                                        
         LA    R1,CPYEL                                                         
         ST    R1,ACPYEL                                                        
         LA    R1,DELMUL                                                        
         ST    R1,ADELMUL                                                       
         LA    R1,BLDACT                                                        
         ST    R1,ABLDACT                                                       
         LA    R1,CLEAR                                                         
         ST    R1,ACLEAR                                                        
         LA    R1,BLDSHP                                                        
         ST    R1,ABLDSHP                                                       
         LA    R1,DISPSHP                                                       
         ST    R1,ADISPSHP                                                      
         LA    R1,IOAREA           SET AREC TO IOAREA                           
         ST    R1,AREC                                                          
         SPACE 1                                                                
         L     R1,APARM                                                         
         L     RF,12(R1)           GET SOME FACILITIES FROM COMFACS             
         USING COMFACSD,RF                                                      
         MVC   VSCANNER,CSCANNER                                                
         MVC   VGETMSG,CGETMSG                                                  
         MVC   VHELLO,CHELLO                                                    
         MVC   VHEXIN,CHEXIN                                                    
         MVC   VHEXOUT,CHEXOUT                                                  
         MVC   VDATCON,CDATCON                                                  
         MVC   VDATVAL,CDATVAL                                                  
         MVC   VDATAMR,CDATAMGR                                                 
         MVC   VCASHVAL,CCASHVAL                                                
*        MVC   VSCROUT,CSCROUT                                                  
         MVC   VUNSCAN,CUNSCAN                                                  
         L     RF,CGETFACT                                                      
         DROP  RF                                                               
*                                  GET A(SYSTEM LIST) FROM GETFACT              
         GOTO1 (RF),DMCB,0                                                      
         L     R1,0(R1)                                                         
         MVC   WORK(1),FASYSID-FACTSD(R1)                                       
         MVC   VCTRY,FAACTRY-FACTSD(R1)                                         
         MVC   VLANG,FAALANG-FACTSD(R1)                                         
         L     R1,FASYSLST-FACTSD(R1)                                           
         LA    R1,6(R1)                                                         
         LA    R1,SYSLLEN(R1)      BUMP PAST SERVICE SYSTEM ENTRY               
         ST    R1,ASYSTBL                                                       
         L     R1,=V(SCUNKEY)                                                   
         AR    R1,R7                                                            
         ST    R1,VSCUNKEY                                                      
         MVI   UPDATE,C'N'                                                      
         SPACE 1                                                                
         XC    TRMNUM,TRMNUM       SET TERM NUM AND DDS INDICATOR               
         MVI   DDS,0                                                            
         L     RE,AUTL                                                          
         USING UTLD,RE                                                          
         MVC   TRMNUM,TNUM                                                      
         TM    TSTAT,TSTATDDS                                                   
         BZ    VALTA                                                            
*&&UK                                                                           
         OI    DDS,X'F0'           UK - SET DDS & UPDATIVE                      
         B     VALTA                                                            
*&&                                                                             
*&&US                                                                           
         OI    DDS,X'80'           US - SET DDS                                 
         CLI   WORK,1              TEST IF THE TEST SYSTEM                      
         BNE   *+12                                                             
         OI    DDS,X'70'           NO - SET UPDATIVE                            
         B     VALTA                                                            
         TM    TFLAG,TFLAGSEC      TEST LOGON WITH PASSWORD                     
         BZ    VALTA                                                            
         CLC   TAGY,=C'**'         TEST AGENCY ALPHA                            
         BNE   *+8                                                              
         OI    DDS,X'40'           YES - SET UPDATIVE                           
         CLC   TAGY,=C'++'         TEST AGENCY ALPHA                            
         BNE   *+8                                                              
         OI    DDS,X'20'           YES - SET UPDATIVE                           
*&&                                                                             
         DROP  RE                                                               
         EJECT                                                                  
VALTA    LR    R2,R3                                                            
         USING CTLFMFFD,R2         R2=A(TWA BASE SCREEN)                        
         GOTO1 AFVAL,BASTYPEH      VALIDATE RECORD TYPE                         
         BZ    ERROR               MISSING TYPE                                 
         ZIC   R1,FLDH+5                                                        
         CH    R1,=H'4'                                                         
         BNH   *+8                                                              
         LA    R1,4                                                             
         BCTR  R1,0                                                             
         LA    RE,TYPETBL                                                       
VTA1     CLI   0(RE),0             SEARCH TYPE TABLE                            
         BE    EIRT                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   FLD(0),0(RE)                                                     
         BE    *+12                                                             
         LA    RE,L'TYPETBL(RE)                                                 
         B     VTA1                                                             
         MVC   TYPE,8(RE)          SET TYPE,PHASE, AND SCREEN FROM TBL          
         MVC   PHASE,9(RE)                                                      
         MVC   SCREEN,10(RE)                                                    
         MVC   WORK(1),11(RE)      SAVE ACTION BITS                             
         TM    TYPE,X'01'          TEST NO LONGER SUPPORTED                     
         BO    ERNS                                                             
         TM    DDS,X'80'           TEST IF A DDS TERMINAL                       
         BNZ   VTA2                YES - ALL RECORD TYPES ARE VALID             
         TM    TYPE,X'80'          TEST IF A USER RECORD TYPE                   
         BNZ   VTA3                                                             
         B     EIRT                NO - ERROR                                   
VTA2     TM    TYPE,X'C0'          DDS CHANGING SYSTEMS/USER REC TYPE           
         BNZ   VTA3                YES - ALL ACTIONS VALID                      
         TM    TYPE,X'20'          TEST SPECIAL DDS RECORD TYPE                 
         BZ    VTA2A                                                            
         TM    DDS,X'20'           TEST SPECIAL DDS RECORD TYPE                 
         BNZ   *+8                                                              
         NI    WORK,X'80'          NO - SET DISPLAY ONLY VALID                  
         B     VTA3                                                             
VTA2A    TM    DDS,X'40'           TEST UPDATIVE ACTIONS ALLOWED                
         BNZ   VTA3                YES                                          
         NI    WORK,X'80'          NO - SET DISPLAY ONLY VALID                  
         SPACE 1                                                                
VTA3     CLC   BASTYPE(8),0(RE)    DISPLAY FULL TYPE                            
         BE    *+14                                                             
         MVC   BASTYPE,0(RE)                                                    
         OI    BASTYPEH+6,X'80'                                                 
         GOTO1 AFVAL,BASACTNH      VALIDATE ACTION                              
         BZ    ERROR               MISSING ACTION                               
         ZIC   R1,FLDH+5                                                        
         CH    R1,=H'3'                                                         
         BNH   *+8                                                              
         LA    R1,3                ONLY COMPARE ON 3 BYTES                      
         BCTR  R1,0                                                             
         LA    RE,ACTNTBL                                                       
VTA4     CLI   0(RE),0             SEARCH ACTION TABLE                          
         BE    EIAC                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   FLD(0),0(RE)                                                     
         BE    *+12                                                             
         LA    RE,L'ACTNTBL(RE)                                                 
         B     VTA4                                                             
         MVC   ACTN,8(RE)          SET ACTN FROM TBL                            
         MVC   ACTN2,ACTN                                                       
         MVC   ACTINDS,10(RE)                                                   
         MVC   BASACTN,0(RE)                                                    
         OI    BASACTNH+6,X'80'                                                 
         SPACE 1                                                                
         IC    R1,9(RE)            TEST IF ACTION VALID FOR TYPE                
         EX    R1,*+8                                                           
         B     *+8                                                              
         TM    WORK,0                                                           
         BZ    EFNA                                                             
         SPACE 1                                                                
VTA5     CLC   PHASE,LTYPE         LOAD NEW SCREEN IF NEW TYPE                  
         BE    VTAA                                                             
         L     RF,AFACLIST                                                      
         L     RF,VCALLOV-SYSFACD(RF)                                           
         LA    R1,DMCB                                                          
         LA    RE,BASTABH                                                       
         ST    RE,0(R1)                                                         
         MVC   4(4,R1),=X'D90A02FF'                                             
         MVC   7(1,R1),SCREEN                                                   
         BASR  RE,RF                                                            
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                DIE IF CANT LOAD SCREEN                      
         SPACE 1                                                                
         LA    RF,BASTABH          RETRANSMIT TOP OF SCREEN                     
         SR    R1,R1                                                            
         LA    RE,BASHDRH                                                       
VTA6     CR    RE,RF                                                            
         BNL   VTA7                                                             
         OI    6(RE),X'80'                                                      
         IC    R1,0(RE)                                                         
         AR    RE,R1                                                            
         B     VTA6                                                             
         SPACE 1                                                                
VTA7     CLI   ACTN,1              NEW TYPE CAN ONLY BE FOR                     
         BE    VTA8                DISP/ADD/CHA                                 
         CLI   ACTN,2                                                           
         BNE   *+12                                                             
         MVI   ACTN,3                                                           
         B     VTA8                                                             
         CLI   ACTN,3                                                           
         BE    VTA8                                                             
         B     EIAS                                                             
VTA8     XC    MSG,MSG                                                          
         MVC   MSG(16),=C'ENTER RECORD KEY'                                     
         CLI   ACTN,1                                                           
         BNE   *+10                                                             
         MVC   MSG+16(9),=C' AND DATA'                                          
         SPACE 1                                                                
         XC    LKEY,LKEY           SET SAVE DATA                                
         MVC   LTYPE,PHASE                                                      
         MVC   LACTN,ACTN                                                       
         MVI   LNEXT,0                                                          
         CLI   BASKEYH+5,0         ANYTHING IN VIRGIN KEY FIELD                 
         BNE   LOAD                YES - GO TO OVERLAY                          
         SPACE 1                                                                
         LA    RE,BASTABH          POSN CURSOR TO FIRST UNPROT FIELD            
         SR    R1,R1                                                            
VTA9     TM    1(RE),X'20'                                                      
         BZ    *+14                                                             
         IC    R1,0(RE)                                                         
         AR    RE,R1                                                            
         B     VTA9                                                             
         ST    RE,FADR                                                          
         B     OMSG                                                             
         SPACE 1                                                                
VTAA     CLC   ACTN,LACTN          SAME TYPE AS LAST INPUT                      
         BNE   VTAB                                                             
         CLI   ACTN,1              SUCCESSIVE ADDS OK                           
         BE    LOAD                                                             
         CLI   ACTN,2              SUCCESSIVE CHANGES OK                        
         BE    LOAD                                                             
         CLI   ACTN,3              SUCCESSIVE DISPLAYS OK                       
         BE    LOAD                                                             
         CLI   ACTN,8              SUCCESSIVE AMENDS OK                         
         BE    LOAD                                                             
         CLI   ACTN,7              SUCCESSIVE COPIES OK                         
         BNE   *+16                                                             
         TM    LNEXT,X'08'                                                      
         BO    LOAD                                                             
         B     EIAS                                                             
         SPACE 1                                                                
VTAB     CLI   ACTN,1              NEW ACTION INPUT - CHECK SEQUENCE            
         BE    LOAD                OK TO ADD                                    
         CLI   ACTN,2                                                           
         BE    LOAD                OK TO CHANGE                                 
         CLI   ACTN,3                                                           
         BE    LOAD                OK TO DISPLAY                                
         CLI   ACTN,4                                                           
         BNE   *+16                                                             
         TM    LNEXT,X'02'                                                      
         BO    DELETE              OK TO DELETE                                 
         B     ECDR                                                             
         CLI   ACTN,5                                                           
         BNE   *+16                                                             
         TM    LNEXT,X'04'                                                      
         BO    RESTORE             OK TO RESTORE                                
         B     ECRR                                                             
         CLI   ACTN,6                                                           
         BNE   *+16                                                             
         TM    LNEXT,X'08'                                                      
         BO    LOAD                OK TO COPY                                   
         B     ECCR                                                             
         CLI   ACTN,7                                                           
         BNE   *+16                                                             
         TM    LNEXT,X'08'                                                      
         BO    LOAD                OK TO COPY (LOGICAL)                         
         B     ECCR                                                             
         DC    H'0'                                                             
         EJECT                                                                  
EIIO     MVI   FERN,0                                                           
         B     ERROR                                                            
EFNA     MVI   FERN,6                                                           
         B     ERROR                                                            
EFTS     MVI   FERN,7                                                           
         B     ERROR                                                            
EIRT     MVI   FERN,10                                                          
         B     ERROR                                                            
EIAC     MVI   FERN,11                                                          
         B     ERROR                                                            
EIAS     MVI   FERN,12                                                          
         B     ERROR                                                            
ECDR     MVI   FERN,15                                                          
         B     ERROR                                                            
ECRR     MVI   FERN,16                                                          
         B     ERROR                                                            
ECCR     MVI   FERN,27                                                          
         B     ERROR                                                            
EIIF     MVI   FERN,2                                                           
         B     ERROR                                                            
ERNS     MVI   FERN,81                                                          
         B     ERROR                                                            
EXIT     XIT1                                                                   
         EJECT                                                                  
DELETE   MVC   KEY,LKEY            DELETE LAST DISPLAYED RECORD                 
         MVC   KEYNEXT,KEY                                                      
         LA    RE,IOAREA                                                        
         ST    RE,AREC                                                          
         MVI   UPDATE,C'Y'                                                      
         GOTO1 AREAD                                                            
         CLI   DMCB+8,0                                                         
         BNE   EIIO                MUST BE OK                                   
         BAS   RE,ACTIVITY                                                      
         CLI   KEY,C'0'            AUTH REC HAS PASSIVE COPIES                  
         BE    LOCK                                                             
         OI    IOAREA+27,X'80'                                                  
         GOTO1 AWRITE                                                           
         CLI   DMCB+8,0                                                         
         BNE   EIIO                                                             
         CLI   KEY,C'I'            ID REC HAS PASSIVE COPY                      
         BE    DEL2                                                             
         CLI   KEY,C'T'            TERMINAL REC HAS PASSIVE COPY(S)             
         BE    DEL4                                                             
         B     DELX                                                             
*                                                                               
DEL2     MVI   TEMP,X'02'          DELETE PASSIVE ID                            
         BAS   RE,GETEL                                                         
         BNE   DELX                                                             
         LA    RE,IOAREAX          SET IOAREA FOR PASSIVE COPY                  
         ST    RE,AREC                                                          
         XC    KEY,KEY                                                          
         MVI   KEY,C'I'                                                         
         MVC   KEY+23(2),2(R5)                                                  
         MVI   UPDATE,C'Y'                                                      
         GOTO1 AREAD                                                            
         CLI   DMCB+8,0                                                         
         BNE   EIIO                                                             
         BAS   RE,ACTIVITY                                                      
         OI    IOAREAX+27,X'80'                                                 
         GOTO1 AWRITE                                                           
         CLI   DMCB+8,0                                                         
         BNE   EIIO                                                             
         LA    RE,IOAREA                                                        
         ST    RE,AREC                                                          
         B     DELX                                                             
*                                                                               
DEL4     MVI   TEMP,X'03'          DELETE PASSIVE TERMINAL                      
         BAS   RE,GETEL                                                         
         BNE   DEL6                                                             
         LA    RE,IOAREAX          SET IOAREA FOR PASSIVE COPY                  
         ST    RE,AREC                                                          
         XC    KEY,KEY                                                          
         MVI   KEY,C'T'                                                         
         MVC   KEY+23(2),2(R5)                                                  
         MVI   UPDATE,C'Y'                                                      
         GOTO1 AREAD                                                            
         CLI   DMCB+8,0                                                         
         BNE   EIIO                                                             
         BAS   RE,ACTIVITY                                                      
         OI    IOAREAX+27,X'80'                                                 
         GOTO1 AWRITE                                                           
         CLI   DMCB+8,0                                                         
         BNE   EIIO                                                             
         LA    RE,IOAREA                                                        
         ST    RE,AREC                                                          
*                                                                               
DEL6     TM    IOAREA+27,X'04'     DELETE PRINTER PASSIVE                       
         BZ    DELX                                                             
         LA    RE,IOAREAX          SET IOAREA FOR PASSIVE COPY                  
         ST    RE,AREC                                                          
         MVC   KEY,LKEY                                                         
         MVI   KEY+6,C'P'                                                       
         MVI   UPDATE,C'Y'                                                      
         GOTO1 AREAD                                                            
         CLI   DMCB+8,0                                                         
         BNE   EIIO                                                             
         BAS   RE,ACTIVITY                                                      
         OI    IOAREAX+27,X'80'                                                 
         GOTO1 AWRITE                                                           
         CLI   DMCB+8,0                                                         
         BNE   EIIO                                                             
         LA    RE,IOAREA                                                        
         ST    RE,AREC                                                          
         B     DELX                                                             
*                                                                               
DELX     LA    R1,BASACTNH                                                      
         ST    R1,FADR                                                          
         MVI   NACTN,X'04'         OK TO RESTORE                                
         B     DONE                                                             
         EJECT                                                                  
LOCK     OI    IOAREA+27,X'20'     LOCKED RECORD HAS X'20' BIT ON               
         GOTO1 AWRITE                                                           
         CLI   DMCB+8,0                                                         
         BNE   EIIO                                                             
         LA    R1,IOAREA+28        USE 030C EL TO GET CODE KEY                  
         SR    RE,RE                                                            
LOCK2    CLI   0(R1),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   0(2,R1),=X'030C'                                                 
         BE    *+14                                                             
         IC    RE,1(R1)                                                         
         AR    R1,RE                                                            
         B     LOCK2                                                            
         XC    KEY+3(22),KEY+3     CLEAR NAME FROM KEY                          
         MVC   KEY+15(10),2(R1)    MAKE FULL KEY FROM 03                        
         MVI   UPDATE,C'Y'                                                      
         GOTO1 AREAD                                                            
         BZ    EIIO                                                             
         TM    DMCB+8,X'10'        TEST IF REC FOUND                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         OI    IOAREA+27,X'20'     SET ON LOCKED BIT                            
         GOTO1 AWRITE                                                           
         CLI   DMCB+8,0                                                         
         BNE   EIIO                                                             
*                                                                               
         LA    R1,IOAREA+28        USE 0304 EL TO GET AUTH KEY                  
         SR    RE,RE                                                            
LOCK4    CLI   0(R1),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   0(2,R1),=X'0304'                                                 
         BE    *+14                                                             
         IC    RE,1(R1)                                                         
         AR    R1,RE                                                            
         B     LOCK4                                                            
         XC    KEY+3(22),KEY+3     CLEAR NAME FROM KEY                          
         MVC   KEY+23(02),2(R1)    MAKE FULL KEY FROM 03                        
         MVI   UPDATE,C'Y'                                                      
         GOTO1 AREAD                                                            
         BZ    EIIO                                                             
         TM    DMCB+8,X'10'        TEST IF REC FOUND                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         OI    IOAREA+27,X'20'     SET ON LOCKED BIT                            
         GOTO1 AWRITE                                                           
         CLI   DMCB+8,0                                                         
         BNE   EIIO                                                             
         B     DELX                                                             
         EJECT                                                                  
RESTORE  MVC   KEY,LKEY            RESTORE LAST DELETED RECORD DISPLAYD         
         MVC   KEYNEXT,KEY                                                      
         LA    RE,IOAREA                                                        
         ST    RE,AREC                                                          
         MVI   UPDATE,C'Y'                                                      
         GOTO1 AREAD                                                            
         BZ    EIIO                                                             
         CLI   KEY,C'0'            AUTH REC HAS PASSIVE COPIES                  
         BE    UNLOCK                                                           
         TM    DMCB+8,X'02'                                                     
         BZ    EIIO                MUST BE DELETED                              
         BAS   RE,ACTIVITY                                                      
         NI    IOAREA+27,X'7F'                                                  
         GOTO1 AWRITE                                                           
         CLI   DMCB+8,0                                                         
         BNE   EIIO                                                             
         CLI   KEY,C'I'            ID REC HAS PASSIVE COPY                      
         BE    RES2                                                             
         CLI   KEY,C'T'            TERMINAL REC HAS PASSIVE COPY(S)             
         BE    RES4                                                             
         B     RESX                                                             
*                                                                               
RES2     MVI   TEMP,X'02'          RESTORE PASSIVE ID                           
         BAS   RE,GETEL                                                         
         BNE   RESX                                                             
         LA    RE,IOAREAX          SET IOAREA FOR PASSIVE COPY                  
         ST    RE,AREC                                                          
         XC    KEY,KEY                                                          
         MVI   KEY,C'I'                                                         
         MVC   KEY+23(2),2(R5)                                                  
         MVI   UPDATE,C'Y'                                                      
         GOTO1 AREAD                                                            
         BZ    EIIO                                                             
         TM    DMCB+8,X'02'                                                     
         BZ    EIIO                                                             
         BAS   RE,ACTIVITY                                                      
         NI    IOAREAX+27,X'7F'                                                 
         GOTO1 AWRITE                                                           
         CLI   DMCB+8,0                                                         
         BNE   EIIO                                                             
         LA    RE,IOAREA                                                        
         ST    RE,AREC                                                          
         B     RESX                                                             
*                                                                               
RES4     MVI   TEMP,X'03'          RESTORE PASSIVE TERMINAL                     
         BAS   RE,GETEL                                                         
         BNE   RES6                                                             
         LA    RE,IOAREAX          SET IOAREA FOR PASSIVE COPY                  
         ST    RE,AREC                                                          
         XC    KEY,KEY                                                          
         MVI   KEY,C'T'                                                         
         MVC   KEY+23(2),2(R5)                                                  
         MVI   UPDATE,C'Y'                                                      
         GOTO1 AREAD                                                            
         BZ    EIIO                                                             
         TM    DMCB+8,X'02'                                                     
         BZ    EIIO                                                             
         BAS   RE,ACTIVITY                                                      
         NI    IOAREAX+27,X'7F'                                                 
         GOTO1 AWRITE                                                           
         CLI   DMCB+8,0                                                         
         BNE   EIIO                                                             
         LA    RE,IOAREA                                                        
         ST    RE,AREC                                                          
*                                                                               
RES6     TM    IOAREA+27,X'04'     RESTORE PRINTER PASSIVE                      
         BZ    RESX                                                             
         LA    RE,IOAREAX          SET IOAREA FOR PASSIVE COPY                  
         ST    RE,AREC                                                          
         MVC   KEY,LKEY                                                         
         MVI   KEY+6,C'P'                                                       
         MVI   UPDATE,C'Y'                                                      
         GOTO1 AREAD                                                            
         BZ    EIIO                                                             
         TM    DMCB+8,X'02'                                                     
         BZ    EIIO                                                             
         BAS   RE,ACTIVITY                                                      
         NI    IOAREAX+27,X'7F'                                                 
         GOTO1 AWRITE                                                           
         CLI   DMCB+8,0                                                         
         BNE   EIIO                                                             
         LA    RE,IOAREA                                                        
         ST    RE,AREC                                                          
         B     RESX                                                             
*                                                                               
RESX     LA    R1,BASACTNH                                                      
         ST    R1,FADR                                                          
         MVI   NACTN,X'0B'         OK TO CHANGE/DELETE/COPY                     
         B     DONE                                                             
         EJECT                                                                  
UNLOCK   TM    IOAREA+27,X'20'     ERROR IF NOT LOCKED RECORD                   
         BZ    ECRR                                                             
         BAS   RE,ACTIVITY                                                      
         NI    IOAREA+27,X'DF'                                                  
         GOTO1 AWRITE                                                           
         CLI   DMCB+8,0                                                         
         BNE   EIIO                                                             
         LA    R1,IOAREA+28        USE 030C EL TO GET CODE KEY                  
         SR    RE,RE                                                            
ULOCK2   CLI   0(R1),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   0(2,R1),=X'030C'                                                 
         BE    *+14                                                             
         IC    RE,1(R1)                                                         
         AR    R1,RE                                                            
         B     ULOCK2                                                           
         XC    KEY+3(22),KEY+3     CLEAR NAME FROM KEY                          
         MVC   KEY+15(10),2(R1)    MAKE FULL KEY FROM 03                        
         MVI   UPDATE,C'Y'                                                      
         GOTO1 AREAD                                                            
         BZ    EIIO                                                             
         TM    DMCB+8,X'10'        TEST IF REC FOUND                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         NI    IOAREA+27,255-X'20' TURN OFF LOCKED BIT                          
         GOTO1 AWRITE                                                           
         CLI   DMCB+8,0                                                         
         BNE   EIIO                                                             
*                                                                               
         LA    R1,IOAREA+28        USE 0304 EL TO GET AUTH KEY                  
         SR    RE,RE                                                            
ULOCK4   CLI   0(R1),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   0(2,R1),=X'0304'                                                 
         BE    *+14                                                             
         IC    RE,1(R1)                                                         
         AR    R1,RE                                                            
         B     ULOCK4                                                           
         XC    KEY+3(22),KEY+3     CLEAR NAME FROM KEY                          
         MVC   KEY+23(02),2(R1)    MAKE FULL KEY FROM 03                        
         MVI   UPDATE,C'Y'                                                      
         GOTO1 AREAD                                                            
         BZ    EIIO                                                             
         TM    DMCB+8,X'10'        TEST IF REC FOUND                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         NI    IOAREA+27,255-X'20' TURN OFF LOCK BIT                            
         GOTO1 AWRITE                                                           
         CLI   DMCB+8,0                                                         
         BNE   EIIO                                                             
         B     RESX                                                             
         EJECT                                                                  
*                                                                               
PUTMQ    NTR1                                                                   
         CLI   ACTN,1                                                           
         BE    *+8                 ADD                                          
         CLI   ACTN,2                                                           
         BE    *+12                CHANGE                                       
         CLI   ACTN,4                                                           
         BNE   PUTMQX              DELETE                                       
*                                                                               
         L     R5,AREC                                                          
         USING CTSREC,R5                                                        
         CLI   CTSKTYP,CTSKTEQU    DSTATION?                                    
         JNE   PUTMQX                                                           
         CLI   CTSKSRC,C'C'        COMSCORE?                                    
         JNE   PUTMQX                                                           
*                                                                               
         LA    RE,IOAREAX                                                       
         LA    RF,DMQDLNQ                                                       
         XCEF                                                                   
*                                                                               
         LA    R7,IOAREAX                                                       
         USING DMQD,R7                                                          
*                                                                               
         MVC   DMQ1CR,=X'0D25'     CARRIAGE RETURN                              
         MVC   DMQ2CR,DMQ1CR                                                    
         MVC   DMQ3CR,DMQ1CR                                                    
         MVC   DMQ4CR,DMQ1CR                                                    
         MVC   DMQ5CR,DMQ1CR                                                    
         MVC   DMQ6CR,DMQ1CR                                                    
*                                                                               
         MVC   DMQTITLE,=C'DSTATION********'                                    
         MVC   DMQSRC,=C'SOURCE=COM'                                            
         MVC   DMQSTYLE,=C'STYLE=DSTATION'                                      
*                                                                               
         MVC   DMQBOOK(5),=C'BOOK='                                             
         MVC   DUB(2),CTSKBOOK                                                  
         XC    DUB(2),=X'FFFF'                                                  
         MVI   DUB+2,X'01'         DEFAULT TO FIRST OF THE MONTH                
         GOTO1 VDATCON,DMCB,(3,DUB),(23,DMQBOOK+5)                              
*                                                                               
         MVC   DMQACT,=C'ACTION=ADD    '                                        
         CLI   ACTN,1              ADD?                                         
         BE    PMQ10                                                            
         MVC   DMQACT,=C'ACTION=CHANGE '                                        
         CLI   ACTN,2              CHANGE?                                      
         BE    PMQ10                                                            
         MVC   DMQACT,=C'ACTION=DELETE'                                         
*                                                                               
PMQ10    LA    R6,DMQLINKS         =A(OUTPUT BLOCK)                             
         MVC   0(6,R6),=C'LINKS='                                               
         AHI   R6,6                                                             
         LA    R5,CTSDATA          R5=A(FIRST ELEMENT)                          
*                                                                               
PMQ12    CLI   0(R5),0             TEST E-O-R                                   
         BE    PMQ18                                                            
         CLI   0(R5),X'02'         DESCRIPTION ELEMENT                          
         BE    PMQ16                                                            
PMQ14    ZIC   RF,1(R5)                                                         
         AR    R5,RF                                                            
         B     PMQ12                                                            
*                                  ADD ENTRY TO OUTPUT BLOCK                    
PMQ16    MVI   0(R6),C' '                                                       
         MVC   1(11,R6),0(R6)                                                   
         MVC   0(5,R6),2(R5)       FORMAT IS AAAAA-BBBBB                        
         LA    RF,4(R6)                                                         
         CLI   0(RF),C' '                                                       
         BNE   *+8                                                              
         BCT   RF,*-8                                                           
         MVI   1(RF),C'-'                                                       
         MVC   2(5,RF),7(R5)                                                    
         LA    R6,12(R6)                                                        
         MVI   0(R6),C','                                                       
         LA    R6,1(R6)                                                         
         B     PMQ14                                                            
*                                                                               
PMQ18    L     RF,ACOMFACS                                                      
         USING COMFACSD,RF                                                      
         L     RF,CMQIO                                                         
         DROP  RF                                                               
*                                                                               
         LA    R6,DMQDLNQ                                                       
         AHI   R6,16               MQIO HEADER                                  
         GOTO1 (RF),DMCB,=CL8'PUT',IOAREAX,(R6),0,0,DUB                         
*                                                                               
PUTMQX   B     EXIT                                                             
*                                                                               
ACTIVITY NTR1                                                                   
         L     R5,AREC                                                          
         CLI   28(R5),X'01'                                                     
         BNE   EXIT                                                             
         GOTO1 ABLDACT                                                          
         MVC   28(5,R5),TEMP       MOVE IN NEW ACTIVITY ELEMENT                 
         B     EXIT                                                             
         SPACE 1                                                                
GETEL    LA    R5,IOAREA+28        LOCATE POINTER ELEMENT                       
         SR    R4,R4                                                            
GETEL2   CLI   0(R5),0                                                          
         BNE   *+8                                                              
         CR    R4,R5               RETURN WITH CC=NEQ IF NOT FOUND              
         BR    RE                                                               
         CLC   0(1,R5),TEMP        RETURN WHEN FOUND WITH CC=EQL                
         BER   RE                                                               
         IC    R4,1(R5)                                                         
         AR    R5,R4                                                            
         B     GETEL2                                                           
         EJECT                                                                  
* CHECK FOR INPUT IN MULTIPLE KEY FIELD & UNSCAN INTO OVERLAY SCREEN            
* KEY FIELDS THEN-                                                              
* PASS CONTROL TO OVERLAY PHASE WITH R1=A(ROOT WORKING STORAGE) AND             
* FERN=X'FF'.                                                                   
* RETURN WITH FERN=X'FF' IF NO ERRORS WITH FADR SET TO CURSOR POSITION          
* FIELDS AND NACTN SET TO DEFINE NEXT ACTION AS FOLLOWS :-                      
* X'01' OK TO CHANGE RECORD DISPLAYED                                           
* X'02' OK TO DELETE RECORD DISPLAYED                                           
* X'04' OK TO RESTORE RECORD DISPLAYED                                          
* X'08' OK TO COPY RECORD DISPLAYED                                             
* RETURN WITH FERN SET TO ERROR NUMBER OF FIRST ERROR FIELD AND FADR            
* SET TO ADDRESS OF ERROR FIELD HEADER. IF MULTIPLE DATA FIELD SET FNDX         
* TO FIELD NUMBER IN ERROR (FNDX=1 FOR FIRST FIELD).                            
*                                                                               
LOAD     CLI   BASKEYH+5,0         ANYTHING IN MULTI-KEY FIELD                  
         BE    LOAD2                                                            
         GOTO1 VSCUNKEY,DMCB,BASKEYH,BASTABH                                    
         CLI   0(R1),0                                                          
         BE    *+16                                                             
         LA    R1,BASKEYH                                                       
         ST    R1,FADR                                                          
         B     EIIF                                                             
         XC    BASKEY,BASKEY       CLEAR MULTI-KEY FIELD                        
         OI    BASKEYH+6,X'80'                                                  
*                                                                               
LOAD2    L     RF,AFACLIST         LOAD OVERLAY PHASE                           
         L     RF,VCALLOV-SYSFACD(RF)                                           
         LA    R1,DMCB                                                          
         XC    0(4,R1),0(R1)                                                    
         MVC   0(1,R1),PHASE                                                    
         ST    R3,4(R1)                                                         
         BASR  RE,RF                                                            
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                DIE IF CANT LOAD                             
         L     RF,0(R1)                                                         
         LR    R1,R9                                                            
         MVI   FERN,X'FF'                                                       
         MVI   FNDX,0                                                           
         BASR  RE,RF               GO TO PHASE PASSING A(GLOBAL W/S)            
         CLI   FERN,X'FF'                                                       
         BNE   ERROR               ERROR MSG RETURN                             
         B     DONE                ACTION COMPLETED RETURN                      
*                                                                               
DONE     MVC   LTYPE,PHASE         SAVE COMPLETED TRANSACTION DATA              
         MVC   LACTN,ACTN                                                       
         MVC   LNEXT,NACTN                                                      
         MVC   LKEY,KEYNEXT                                                     
         XC    MSG,MSG                                                          
         EJECT                                                                  
DONE1    CLI   ACTN,1              DEDUCE HEADER MESSAGE FROM ACTION            
         BNE   DONE2                                                            
         MVC   MSG(12),=C'RECORD ADDED'                                         
         B     DONEX                                                            
DONE2    CLI   ACTN,2                                                           
         BNE   DONE3                                                            
         MVC   MSG(14),=C'RECORD CHANGED'                                       
         B     DONEX                                                            
DONE3    CLI   ACTN,3                                                           
         BNE   DONE4                                                            
         MVC   MSG(16),=C'RECORD DISPLAYED'                                     
         TM    NACTN,X'04'                                                      
         BZ    DONE3A                                                           
         LA    R1,BASACTNH                                                      
         ST    R1,FADR                                                          
         MVC   MSG+16(11),=C' IS DELETED'                                       
         B     DONEX                                                            
DONE3A   TM    NACTN,X'01'                                                      
         BZ    DONEX                                                            
         CLI   ACTN2,2                                                          
         BNE   *+14                                                             
         MVC   MSG+16(18),=C' NOW ENTER CHANGES'                                
         B     DONEX                                                            
         LA    R1,BASACTNH                                                      
         ST    R1,FADR                                                          
         MVC   MSG+16(15),=C' CAN BE CHANGED'                                   
         B     DONEX                                                            
DONE4    CLI   ACTN,4                                                           
         BNE   DONE5                                                            
         MVC   MSG(14),=C'RECORD DELETED'                                       
         B     DONEX                                                            
DONE5    CLI   ACTN,5                                                           
         BNE   DONE6                                                            
         MVC   MSG(15),=C'RECORD RESTORED'                                      
         B     DONEX                                                            
DONE6    CLI   ACTN,6                                                           
         BNE   DONE7                                                            
         MVC   MSG(14),=C'RECORD RENAMED'                                       
         B     DONEX                                                            
DONE7    CLI   ACTN,7                                                           
         BNE   DONE8                                                            
         MVC   MSG(16),=C'RECORD(S) COPIED'                                     
         B     DONEX                                                            
DONE8    DC    H'0'                                                             
DONEX    BAS   RE,PUTMQ                                                         
         CLI   ACTN,3              RECORD JUST DISPLAYED                        
         BNE   OMSG                                                             
         L     R6,AREC             CHECK FOR ACTIVITY ELEMENT                   
         CLI   28(R6),X'01'                                                     
         BNE   OMSG                                                             
         LA    R5,MSG+59           TRY AND DISPLAY LAST ACTIVE DATE             
         CLI   0(R5),X'40'                                                      
         BH    *+8                                                              
         BCT   R5,*-8                                                           
         LA    R1,MSG+35           SPACE TO PUT MESSAGE                         
         CR    R5,R1                                                            
         BH    OMSG                                                             
         MVC   1(18,R5),=C' - LAST ACTIVE ON '                                  
         GOTO1 VDATCON,DMCB,(3,30(R6)),(8,19(R5))                               
         B     OMSG                                                             
         EJECT                                                                  
ERROR    OI    LNEXT,X'80'         SAVE ERROR OCCURRED                          
         CLI   FERN,X'FE'          OVERLAY HAS SUPPLIED MESSAGE                 
         BE    OMSGX                                                            
         L     RF,AFACLIST                                                      
         L     R0,VDATAMGR-SYSFACD(RF)                                          
         GOTO1 VGETMSG,TEMP,(FERN,MSG),(X'0A',DMCB),(TRMNUM,(R0))               
         CLI   FNDX,0              TEST IF MULTI FIELD ERROR                    
         BE    ERRORX                                                           
         SR    R5,R5               GET ERROR MESSAGE LENGTH                     
         IC    R5,0(R1)                                                         
         LA    R6,MSG(R5)          POINT TO END OF MESSAGE                      
         LA    R5,11(R5)                                                        
         CH    R5,=H'60'           TEST IF SPACE TO EXTEND MESSAGE              
         BH    ERRORX                                                           
         MVC   0(11,R6),=C' - FIELD#NN'                                         
         IC    R5,FNDX                                                          
         CVD   R5,DUB                                                           
         UNPK  9(2,R6),DUB                                                      
         OI    10(R6),X'F0'                                                     
ERRORX   B     OMSG                                                             
         SPACE 1                                                                
OMSG     MVC   BASHDR,MSG          OUTPUT HEADER MESSAGE                        
         OI    BASHDRH+6,X'80'                                                  
OMSGX    L     R1,FADR                                                          
         OI    6(R1),X'40'         POSITION CURSOR                              
         B     EXIT                                                             
         EJECT                                                                  
* CONTROL FILE I/O - RECORD KEY AT KEY - RECORD AT AREC                         
* EXIT WITH CC=EQUAL IF FUNNY WITH FERN=0                                       
* EXIT WITH CC=NEQ IF OK OR NORMAL ERRORS WITH FERN=X'FF'                       
* UPDATE SET TO C'Y' IF READ-FOR-UPDATE REQUIRED (RESET TO 'N' ATER IO)         
*                                                                               
READHI   NTR1  BASE=ABASE                                                       
         LA    RA,2048(RB)                                                      
         LA    RA,2048(RA)                                                      
         LA    R0,=C'DMRDHI  '                                                  
         B     IO                                                               
READ     NTR1  BASE=ABASE                                                       
         LA    RA,2048(RB)                                                      
         LA    RA,2048(RA)                                                      
         LA    R0,=C'DMREAD  '                                                  
         B     IO                                                               
RSEQ     NTR1  BASE=ABASE                                                       
         LA    RA,2048(RB)                                                      
         LA    RA,2048(RA)                                                      
         LA    R0,=C'DMRSEQ  '                                                  
         B     IO                                                               
WRITE    NTR1  BASE=ABASE                                                       
         LA    RA,2048(RB)                                                      
         LA    RA,2048(RA)                                                      
         LA    R0,=C'DMWRT   '                                                  
         B     IO                                                               
ADD      NTR1  BASE=ABASE                                                       
         LA    RA,2048(RB)                                                      
         LA    RA,2048(RA)                                                      
         LA    R0,=C'DMADD   '                                                  
         B     IO                                                               
         SPACE 1                                                                
IO       L     R5,AREC                                                          
         L     RF,AFACLIST                                                      
         L     RF,VDATAMGR-SYSFACD(RF)                                          
         MVI   FERN,X'FF'                                                       
         SPACE 1                                                                
         ST    R0,DMCB                                                          
         MVI   DMCB,X'08'          PASS BACK DELETES                            
         CLI   UPDATE,C'Y'                                                      
         BNE   *+8                                                              
         OI    DMCB,X'80'          SET READ-FOR-UPDATE                          
         GOTO1 (RF),DMCB,,=C'CTFILE  ',KEY,(R5),(TRMNUM,0)                      
         MVI   UPDATE,C'N'         RESET RFU INDICATOR                          
         SPACE 1                                                                
         TM    8(R1),X'ED'                                                      
         BZ    *+8                                                              
         MVI   FERN,0              EOF/ERROR/DUP/LOCKS                          
IOX      CLI   FERN,0              SET CC=EQUAL ON FUNNY                        
         B     EXIT                                                             
         EJECT                                                                  
* CLEAR ALL UNPROTECTED FIELDS IN A TWA AND SET TRANSMIT BITS ON.               
* R1=A(FIRST FIELD TO BE CLEARED).                                              
*                                                                               
CLEAR    NTR1  BASE=ABASE                                                       
         LA    RA,2048(RB)                                                      
         LA    RA,2048(RA)                                                      
         SR    R2,R2                                                            
CLEAR1   CLI   0(R1),0                                                          
         BE    CLEARX                                                           
         TM    1(R1),X'20'                                                      
         BO    CLEAR2                                                           
         IC    R2,0(R1)                                                         
         SH    R2,=H'9'                                                         
         EX    R2,*+8                                                           
         B     *+10                                                             
         XC    8(0,R1),8(R1)                                                    
         MVI   4(R1),X'20'                                                      
         MVI   5(R1),0                                                          
         OI    6(R1),X'80'                                                      
CLEAR2   IC    R2,0(R1)                                                         
         AR    R1,R2                                                            
         B     CLEAR1                                                           
CLEARX   B     EXIT                                                             
         EJECT                                                                  
* MOVE INPUT FIELD AT R1 TO FLD AND PAD WITH TRAILING BLANKS.                   
* EXIT WITH CC=EQUAL IF FIELD NOT INPUT AND FERN=01.                            
* EXIT WITH CC=NEQ IF FIELD INPUT AND FERN=X'FF'.                               
* SET FADR TO TWA FLD HDR AND SET FLDH(4)=BINARY VALUE OF NUMERIC FLD.          
*                                                                               
FVAL     NTR1  BASE=ABASE                                                       
         LA    RA,2048(RB)                                                      
         LA    RA,2048(RA)                                                      
         MVI   FERN,X'FF'                                                       
         MVI   FNDX,0                                                           
         ST    R1,FADR                                                          
         XC    FLDH,FLDH                                                        
         MVC   FLDH+4(2),4(R1)     SET INPUT VALUES                             
         MVI   FLD,C' '                                                         
         MVC   FLD+1(L'FLD-1),FLD                                               
         SR    R2,R2                                                            
         ICM   R2,1,FLDH+5                                                      
         BNZ   FVAL2                                                            
FVAL1    MVI   FERN,1              SET MISSING FLD ERROR NUMBER                 
         B     FVALX                                                            
FVAL2    BCTR  R2,0                                                             
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   FLD(0),8(R1)                                                     
         EX    R2,*+8                                                           
         B     *+10                                                             
         OC    FLD(0),FLD          CHECK FIELD ERASED                           
         BZ    FVAL1                                                            
         CLI   FLD,C' '                                                         
         BNE   FVAL3                                                            
         CLC   FLD+1(L'FLD-1),FLD                                               
         BE    FVAL1               TREAT BLANKS AS NOT INPUT                    
FVAL3    TM    FLDH+4,X'08'                                                     
         BZ    FVAL4                                                            
         EX    R2,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R1)                                                      
         CVB   R0,DUB                                                           
         ST    R0,FLDH             SET BINARY VALUE OF NUMERIC FIELD            
FVAL4    EQU   *                                                                
FVALX    CLI   FERN,1              SET CC=ZERO IF MISSING FIELD                 
         B     EXIT                                                             
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
* CONSTRUCT CTFILE RECORD AT AREC - TEMP CONTAINS ELEMENT                       
*                                                                               
BLDREC   NTR1  BASE=ABASE                                                       
         LA    RA,2048(RB)                                                      
         LA    RA,2048(RA)                                                      
         L     R5,AREC             R5=A(RECORD)                                 
         SPACE 1                                                                
         CLI   TEMP,0              TEMP=00 MEANS BUILD KEY                      
         BNE   BLDR1                                                            
         MVC   0(25,R5),KEY                                                     
         MVC   25(2,R5),=H'29'                                                  
         XC    27(2,R5),27(R5)                                                  
         B     BLDRX                                                            
         SPACE 1                                                                
BLDR1    CLI   TEMP,1              TEMP=01 MEANS BUILD ACTIVITY ELEMENT         
         BNE   BLDRA                                                            
         GOTO1 ABLDACT                                                          
         B     BLDRA                                                            
         SPACE 1                                                                
BLDRA    MVC   DUB(2),25(R5)       TEMP CONTAINS THE ELEMENT                    
         LH    R6,DUB                                                           
         AR    R6,R5               R6=A(END OF RECORD)                          
         BCTR  R6,0                                                             
         SR    R7,R7                                                            
         IC    R7,TEMP+1                                                        
         BCTR  R7,0                                                             
         EX    R7,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R6),TEMP        MOVE ELEMENT AT TEMP TO END                  
         LA    R6,1(R6,R7)                                                      
         MVI   0(R6),0                                                          
         SR    R6,R5                                                            
         LA    R6,1(R6)                                                         
         STH   R6,DUB                                                           
         MVC   25(2,R5),DUB        UPDATE RECORD LENGTH                         
         CH    R6,=H'1000'                                                      
         BNH   BLDRX                                                            
         DC    H'0'                DIE IF TOO LONG                              
BLDRX    B     EXIT                                                             
         EJECT                                                                  
* BUILD AN ACTIVITY ELEMENT AT TEMP                                             
*                                                                               
BLDACT   NTR1  BASE=ABASE                                                       
         LA    RA,2048(RB)                                                      
         LA    RA,2048(RA)                                                      
         MVC   TEMP(2),=X'0105'                                                 
         GOTO1 VDATCON,DMCB,(5,0),(3,TEMP+2)                                    
         B     EXIT                                                             
         EJECT                                                                  
* BUILD SHIPPING INFORMATION THAT STARTS WITH NUMERIC FROM SCREEN FIELD         
*                                                                               
*    P1 = R2 = ADDRESS OF SCREEN FIELD DATA                                     
*    P2 = R3 = ADDRESS OF ELEMENT DATA                                          
*    P3 = R4 = LENGTH OF SCREEN FIELD DATA                                      
*         BYTE 0 = RETURNED LENGTH OF ELEMENT DATA                              
*                                                                               
BLDSHP   NTR1  BASE=ABASE                                                       
         LA    RA,2048(RB)                                                      
         LA    RA,2048(RA)                                                      
*&&US                                                                           
         LM    R2,R4,0(R1)                                                      
         LR    R5,R2               SCAN UNTIL NON NUMERIC                       
BLDSHP1  CLI   0(R5),C'0'                                                       
         BL    BLDSHP2                                                          
         CLI   0(R5),C'9'                                                       
         BH    BLDSHP2                                                          
         LA    R5,1(R5)                                                         
         B     BLDSHP1                                                          
BLDSHP2  SR    R5,R2               R5 = LENGTH OF NUMERIC                       
         LR    R6,R4                                                            
         SR    R6,R5               R6 = LENGTH OF REST OF INFO                  
         LA    R4,2(R6)            R4 = FINAL LENGTH OF ELEMENT DATA            
         LA    R7,0(R2,R5)         ELEMENT DATA                                 
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         PACK  DUB,0(0,R2)                                                      
         CVB   R0,DUB              R0 = VALUE OF NUMERIC                        
         MVI   8(R1),0                                                          
         C     R0,=F'9999'                                                      
         BH    EXIT                ERROR - FIELD TO BIG                         
         STC   R4,8(R1)            OTHERWISE RETURN FINAL DATA LENGTH           
         STCM  R0,3,0(R3)                                                       
         C     R6,=F'0'                                                         
         BE    EXIT                                                             
         BCTR  R6,0                                                             
         EX    R6,*+8              MOVE IN REST OF INFO                         
         B     *+10                                                             
         MVC   2(0,R3),0(R7)                                                    
*&&                                                                             
         B     EXIT                                                             
         EJECT                                                                  
* DISPLAY SHIPPING INFORMATION THAT STARTS WITH NUMERIC TO SCREEN FIELD         
*                                                                               
*    P1 = R2 = ADDRESS OF SCREEN FIELD DATA                                     
*    P2 = R3 = ADDRESS OF ELEMENT DATA                                          
*    P3 = R4 = LENGTH OF ELEMENT DATA                                           
*         BYTE 0 = RETURNED LENGTH OF SCREEN FIELD DATA                         
*                                                                               
DISPSHP  NTR1  BASE=ABASE                                                       
         LA    RA,2048(RB)                                                      
         LA    RA,2048(RA)                                                      
*&&US                                                                           
         LM    R2,R4,0(R1)                                                      
         EDIT  (2,(R3)),(4,(R2)),ALIGN=LEFT,ZERO=NOBLANK                        
         LA    R7,2                                                             
         AR    R2,R0               BUMP R2 PAST DISPLAYED NUMERIC               
         LR    R5,R4                                                            
         SR    R5,R7                                                            
         AR    R5,R0               R5 = FINAL LENGTH OF SCREEN DATA             
         SR    R4,R7                                                            
         C     R4,=F'0'                                                         
         BE    DISPSHP1                                                         
         BCTR  R4,0                                                             
         EX    R4,*+8              MOVE IN REST OF INFO                         
         B     *+10                                                             
         MVC   0(0,R2),2(R3)                                                    
DISPSHP1 STC   R5,8(R1)            RETURN FINAL LENGTH                          
*&&                                                                             
         B     EXIT                                                             
         EJECT                                                                  
* DELETE ELEMENT(S) FROM CTFILE RECORD AT AREC - TEMP CONTAINS                  
* ELEMENT ID TO BE DELETED                                                      
*                                                                               
DELEL    NTR1  BASE=ABASE                                                       
         LA    RA,2048(RB)                                                      
         LA    RA,2048(RA)                                                      
         L     R5,AREC             R5=A(RECORD)                                 
         GOTO1 VHELLO,DMCB,(C'D',=C'CTFILE  '),(TEMP,(R5)),0                    
DELELX   B     EXIT                                                             
         SPACE 2                                                                
* DELETE MULTIPLE ELEMENT(S) FROM CTFILE RECORD AT AREC - TEMP CONTAINS         
* A LIST OF ELEMENTS TO BE DELETED TERMINATED BY X'00'                          
*                                                                               
DELMUL   NTR1  BASE=ABASE                                                       
         LA    RA,2048(RB)                                                      
         LA    RA,2048(RA)                                                      
         L     R5,AREC             R5=A(RECORD)                                 
         LA    R6,TEMP             R6=A(LIST OF ELEMENT CODES)                  
DELMUL1  SR    R0,R0                                                            
         ICM   R0,1,0(R6)                                                       
         BZ    DELMULX                                                          
         GOTO1 VHELLO,DMCB,(C'D',=C'CTFILE  '),((R0),(R5)),0                    
         LA    R6,1(R6)                                                         
         B     DELMUL1             BACK FOR NEXT ELEMENT CODE                   
DELMULX  B     EXIT                                                             
         SPACE 2                                                                
* ADD AN ELEMENT TO A CTFILE RECORD AT AREC - TEMP CONTAINS ELEMENT TO          
* BE ADDED                                                                      
* EXIT WITH CC=EQUAL IF RECORD TOO BIG AND FERN=X'44'.                          
* EXIT WITH CC=NEQ IF ELEMENT ADD OK AND FERN=X'FF'                             
*                                                                               
PUTEL    NTR1  BASE=ABASE,LABEL=N                                               
         LA    RA,2048(RB)                                                      
         LA    RA,2048(RA)                                                      
         L     R5,AREC             R5=A(RECORD)                                 
         MVI   FERN,X'FF'                                                       
         GOTO1 VHELLO,DMCB,(C'P',=C'CTFILE  '),(R5),TEMP                        
         CLI   DMCB+12,0                                                        
         BE    *+8                                                              
         MVI   FERN,68                                                          
PUTELX   CLI   FERN,68                                                          
         B     EXIT                                                             
         EJECT                                                                  
* COPY ELEMENT(S) FROM CTFILE REC AT IOAREAX INTO CTFILE RECORD AT AREC         
* TEMP CONTAINS LIST OF EL CODES TO BE COPIED                                   
*                                                                               
CPYEL    NTR1  BASE=ABASE                                                       
         LA    RA,2048(RB)                                                      
         LA    RA,2048(RA)                                                      
         L     R5,AREC             R5=A(TARGET RECORD)                          
         LA    R6,IOAREAX          R6=A(OBJECT RECORD IN IOAREAX)               
         LA    R6,28(R6)                                                        
         MVI   FERN,X'FF'          SET OK RESULT                                
*                                                                               
CPYEL1   CLI   0(R6),0             END OF OBJECT RECORD                         
         BE    CPYELX                                                           
         LA    R7,TEMP                                                          
*                                                                               
CPYEL2   CLI   0(R7),0             END OF ELEMENT COPY LIST                     
         BE    CPYEL3                                                           
         CLC   0(1,R6),0(R7)       R6=A(NEXT EL IN OBJECT REC)                  
         BE    CPYEL4                                                           
         LA    R7,1(R7)                                                         
         B     CPYEL2                                                           
*                                                                               
CPYEL3   SR    R0,R0               BUMP TO NEXT ELEMENT                         
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     CPYEL1                                                           
*                                                                               
CPYEL4   GOTO1 VHELLO,DMCB,(C'P',=C'CTFILE  '),(R5),(R6),=C'ADD=CODE'           
         CLI   DMCB+12,0                                                        
         BE    CPYEL3                                                           
         MVI   FERN,68             SET RECORD TOO LONG ERROR CODE               
CPYELX   CLI   FERN,68             EXIT WITH CC=EQL IF REC TOO BIG              
         B     EXIT                                                             
         EJECT                                                                  
* LITERALS ETC.                                                                 
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
ACTNTBL  DS    0XL11                                                            
         DC    CL8'DISPLAY ',AL1(3),X'80',X'00'                                 
         DC    CL8'ENQUIRE ',AL1(3),X'80',X'00'                                 
         DC    CL8'INQUIRE ',AL1(3),X'80',X'00'                                 
         DC    CL8'CHANGE  ',AL1(2),X'40',X'00'                                 
         DC    CL8'MOVE    ',AL1(2),X'40',X'80'                                 
         DC    CL8'AMEND   ',AL1(2),X'40',X'00'                                 
         DC    CL8'ALTER   ',AL1(2),X'40',X'00'                                 
         DC    CL8'ADD     ',AL1(1),X'20',X'00'                                 
         DC    CL8'NEW     ',AL1(1),X'20',X'00'                                 
         DC    CL8'DELETE  ',AL1(4),X'10',X'00'                                 
         DC    CL8'RESTORE ',AL1(5),X'08',X'00'                                 
         DC    CL8'RENAME  ',AL1(6),X'04',X'00'                                 
         DC    CL8'LCOPY   ',AL1(7),X'02',X'00'                                 
         DC    CL8'COPY    ',AL1(7),X'02',X'00'                                 
ACTNTBLX DC    X'00'                                                            
         EJECT                                                                  
TYPETBL  DS    0XL12                                                            
         DC    CL8'ID      ',X'00',X'01',X'FE',X'FE'                            
         DC    CL8'TERMINAL',X'00',X'02',X'FD',X'FA'                            
         DC    CL8'ERROR   ',X'40',X'03',X'FC',X'F8'                            
*********DC    CL8'BOOK    ',X'00',X'04',X'FB',X'FA' USE NFI: DEIS              
         DC    CL8'IDINFO  ',X'01',X'05',X'FA',X'C0'                            
         DC    CL8'PROFILE ',X'01',X'06',X'F9',X'FA'                            
         DC    CL8'OUTPUT  ',X'01',X'07',X'F8',X'F8'                            
*&&US*&& DC    CL8'DSTATION',X'20',X'08',X'F7',X'F8'                            
*&&US*&& DC    CL8'INTAPE  ',X'40',X'09',X'F6',X'F8'                            
         DC    CL8'IDATTN  ',X'01',X'0A',X'F5',X'C0'                            
*&&US*&& DC    CL8'DBOOK   ',X'20',X'0B',X'F4',X'F8'                            
         DC    CL8'FIELD   ',X'00',X'0C',X'F3',X'F8'                            
         DC    CL8'TEINFO  ',X'00',X'0D',X'F2',X'C0'                            
*&&US*&& DC    CL8'DNAME   ',X'20',X'0E',X'F1',X'F8'                            
*&&US*&& DC    CL8'DFORMULA',X'20',X'0F',X'F0',X'F8'                            
*&&US*&& DC    CL8'EXTRACT ',X'80',X'10',X'EF',X'F8'                            
*&&US*&& DC    CL8'FORMULA ',X'40',X'11',X'EE',X'F8'                            
*&&US*&& DC    CL8'HUT     ',X'40',X'12',X'ED',X'F8'                            
*&&US*&& DC    CL8'FILTER  ',X'80',X'13',X'EC',X'F8'                            
*&&UK*&& DC    CL8'FILTER  ',X'40',X'13',X'EC',X'F8'                            
         DC    CL8'OPTION  ',X'40',X'13',X'EC',X'F8'                            
         DC    CL8'VALUE   ',X'40',X'13',X'EC',X'F8'                            
         DC    CL8'KWXLIST ',X'80',X'14',X'EB',X'F8'                            
         DC    CL8'SYSLIST ',X'01',X'15',X'EA',X'F8'                            
         DC    CL8'PRINTERQ',X'41',X'16',X'E9',X'FA'                            
*&&US*&& DC    CL8'MARKET  ',X'40',X'17',X'E8',X'F8'                            
         DC    CL8'MAP     ',X'40',X'18',X'E7',X'F8'                            
*&&US*&& DC    CL8'DADJUST ',X'20',X'19',X'E6',X'F8'                            
*&&US*&& DC    CL8'DCONTROL',X'20',X'1A',X'E5',X'F8'                            
*&&US*&& DC    CL8'DCODE   ',X'20',X'1B',X'E4',X'F8'                            
         DC    CL8'AUTH    ',X'80',X'1C',X'E3',X'F8'                            
*&&UK*&& DC    CL8'COUNTRY ',X'40',X'1D',X'E2',X'F8'                            
         DC    CL8'BCMSG   ',X'40',X'1E',X'E1',X'F8'                            
*&&US*&& DC    CL8'TWX     ',X'40',X'1F',X'E0',X'F8'                            
         DC    CL8'ACCESS  ',X'01',X'20',X'DF',X'F8'                            
         DC    CL8'DPROGRAM',X'80',X'21',X'DE',X'F8'                            
*&&UK*&& DC    CL8'MEDBUY  ',X'80',X'22',X'DD',X'F8'                            
TYPETBLX DC    X'00'                                                            
         EJECT                                                                  
       ++INCLUDE CTLFMSAVE                                                      
         EJECT                                                                  
       ++INCLUDE CTLFMTEMP                                                      
         EJECT                                                                  
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE FADSECTS                                                       
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE FASYSLSTD                                                      
       ++INCLUDE CTGENFILE                                                      
CTLFMFFD DSECT                                                                  
         DS    CL64                                                             
* CTLFMFFD                                                                      
       ++INCLUDE CTLFMFFD                                                       
*                                                                               
* DSTATION MQ MESSAGE DSECT                                                     
*                                                                               
DMQD     DSECT                                                                  
DMQTITLE DS    CL16                                                             
DMQ1CR   DS    CL2                                                              
DMQBOOK  DS    CL15                                                             
DMQ2CR   DS    CL2                                                              
DMQSRC   DS    CL10                                                             
DMQ3CR   DS    CL2                                                              
DMQACT   DS    CL13                                                             
DMQ4CR   DS    CL2                                                              
DMQLINKS DS    CL1155                                                           
DMQ5CR   DS    CL2                                                              
DMQSTYLE DS    CL14                                                             
DMQ6CR   DS    CL2                                                              
DMQDLNQ  EQU  *-DMQTITLE                                                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'022CTLFM00   02/25/19'                                      
         END                                                                    
