*          DATA SET TAREP0A    AT LEVEL 051 AS OF 03/18/15                      
*PHASE T7030AB,*                                                                
         TITLE 'T7030A - HOLDING FEE NOTIFICATION - VALIDATE SCREEN'            
T7030A   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T7030A                                                         
         L     RC,0(R1)                                                         
         USING GEND,RC             RC=A(CONTROLLER W/S)                         
         L     RA,ATWA                                                          
         USING T703FFD,RA          RA=A(SCREEN)                                 
         L     R9,ASUBSYSD                                                      
         USING SUBSYSD,R9          R9=A(SYSTEM W/S)                             
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8           R8=A(SPOOL DSECT)                            
         LA    R7,BUFF                                                          
         LA    R7,8(R7)                                                         
         USING THD,R7              R7=A(LOCAL W/S)                              
         EJECT                                                                  
*              ROUTINE TO VALIDATE SCREEN                                       
         SPACE 1                                                                
         GOTO1 INITIAL,DMCB,0      INITIALIZE                                   
         SPACE 1                                                                
         CLI   MODE,VALKEY         RECOGNIZE ONLY VALKEY MODE                   
         BNE   XIT                                                              
         BAS   RE,INIT             INITIALIZE                                   
         SPACE 1                                                                
         CLC   =C'CERNOTEST',SHFOPT CERNO TEST MESSAGE?                         
         BNE   VK1                                                              
         OI    THOPTS,THCNOTST                                                  
         B     XIT                                                              
         SPACE 1                                                                
VK1      MVI   BYTE,0              OFFICE FILTER FLAG                           
         LA    R2,SHFOFFH          OFFICE FILTER                                
         BAS   RE,MYANY            CHECK FOR INPUT                              
         BE    VK2                                                              
         TM    WHEN,X'20'          INPUT NOT ALLOWED FOR SOON                   
         BO    NOINPUT                                                          
         MVI   BYTE,1                                                           
         BAS   RE,SPECFILT         POSTIVE OR NEGATIVE FLIST?                   
         BE    VK2                                                              
         GOTO1 RECVAL,DMCB,TLOFCDQ,(X'08',(R2)),SHFOFFNH                        
         MVC   TIFOFF,TGOFF                                                     
         SPACE 1                                                                
VK2      LA    R2,SHFAGYH                                                       
         CLI   BYTE,1              OFFICE FILTER?                               
         BE    VK3                                                              
         GOTO1 ANY                 NO, AGENCY REQUIRED                          
         B     VK4                                                              
VK3      GOTO1 MYANY               YES, AGENCY NOT REQUIRED                     
         BE    VK10                                                             
         CLC   8(3,R2),=C'ALL'     AND INPUT CANNOT BE ALL                      
         BE    FLDINV                                                           
VK4      BAS   RE,SPECFILT         POSTIVE OR NEGATIVE FLIST?                   
         BE    VK10                                                             
*                                                                               
         CLI   RECNUM,HD           ALWAYS VALIDATE FOR HFDISK                   
         BE    VK5                                                              
         CLI   5(R2),3                                                          
         BNE   VK5                                                              
         CLC   8(3,R2),=C'ALL'     IF INPUT IS ALL                              
         BNE   VK5                                                              
         TM    WHEN,X'20'          AND IF NOT FOR SOON                          
         BZ    VK10                THEN DON'T VALIDATE                          
         SPACE 1                                                                
VK5      GOTO1 RECVAL,DMCB,TLAYCDQ,(X'08',(R2)),SHFAGYNH                        
         MVC   TIFAGY,TGAGY                                                     
         SPACE 1                                                                
VK10     LA    R2,SHFCLIH          CLIENT FILTER                                
         BAS   RE,MYANY                                                         
         BE    VK20                                                             
         BAS   RE,SPECFILT         POSTIVE OR NEGATIVE FLIST?                   
         BE    VK20                                                             
         GOTO1 RECVAL,DMCB,TLCLCDQ,(X'08',(R2)),SHFCLINH                        
         MVC   TIFCLI,TGCLI                                                     
         SPACE 1                                                                
VK20     LA    R2,SHFPRDH          PRODUCT FILTER                               
         CLI   5(R2),0                                                          
         BE    VK30                                                             
         CLI   SHFCLIH+5,0         REQUIRE CLIENT                               
         BE    CLIMISS                                                          
         BAS   RE,SPECFILT         POSTIVE OR NEGATIVE FLIST?                   
         BE    VK30                                                             
         GOTO1 RECVAL,DMCB,TLPRCDQ,(X'08',(R2)),SHFPRDNH                        
         MVC   TIFPRD,TGPRD                                                     
         SPACE 1                                                                
VK30     LA    R2,SHFCOMH          COMMERCIAL FILTER                            
         BAS   RE,MYANY                                                         
         BE    VK35                                                             
***      CLI   SHFAGYH+5,0         REQUIRE AGENCY                               
***      BE    AGYMISS                                                          
         CLI   RECNUM,HC           CANNOT BE SET FOR CHANGES RUN                
         BE    FLDINV                                                           
         BAS   RE,SPECFILT         POSTIVE OR NEGATIVE FLIST?                   
         BE    VK35                                                             
VK31     GOTO1 RECVAL,DMCB,TLCOICDQ,(X'0C',(R2)),SHFCOMNH                       
         BE    VK33                                                             
         CLI   ERROR,NOTFOUND                                                   
         BE    *+6                                                              
         DC    H'00'               DIE IF ERROR IS NOT RECORD NOT FOUND         
         OI    DMINBTS,X'08'       PASS DELETES                                 
         GOTO1 RECVAL,DMCB,TLCOICDQ,(X'08',(R2)),SHFCOMNH                       
VK33     L     R3,AIO                                                           
         USING TLCOD,R3                                                         
         MVC   TGAGY,TLCOAGY                                                    
         MVC   TGCLI,TLCOCLI                                                    
         MVC   TIFCOM,TLCOCOM                                                   
         SPACE 1                                                                
VK35     LA    R2,SHFCLGH          CLIENT GROUP FILTER                          
         CLI   5(R2),0                                                          
         BE    VK40                                                             
         CLI   RECNUM,HD           INPUT NOT ALLOWED FOR HFDISK                 
         BE    NOINPUT                                                          
         GOTO1 RECVAL,DMCB,TLCGCDQ,(X'08',(R2)),SHFCLGNH                        
         MVC   TIFCLG,TGCLG                                                     
         SPACE 1                                                                
VK40     BAS   RE,VALPD            VALIDATE PERIOD                              
         TM    WHEN,X'20'          SOON?                                        
         BZ    *+10                NO, LEAVE 2 CHAR REPORT ALONE                
         MVC   QCRDCODE,=C'HL'     SET DRAFT SOON JCL                           
         BAS   RE,VALOPT           VALIDATE OPTIONS                             
         B     XIT                                                              
         EJECT                                                                  
*              INITIALIZATION ROUTINES                                          
         SPACE 1                                                                
INIT     NTR1                                                                   
         LH    RF,=AL2(THLNQ)      CLEAR LOCAL W/S                              
         XCEF  THD,(RF)                                                         
         SPACE 1                                                                
         XC    TIFAGY,TIFAGY       CLEAR AGENCY FILTER                          
         XC    TIFCLI,TIFCLI             CLIENT FILTER                          
         XC    TIFPRD,TIFPRD             PRODUCT FILTER                         
         XC    TIFCOM,TIFCOM             COMMERCIAL FILTER                      
         XC    TIFCLG,TIFCLG             CLIENT GRP FILTER                      
         XC    TIFOFF,TIFOFF             OFFICE FILTER                          
         SPACE 1                                                                
         MVC   SHFAGYN,SPACES            AGENCY NAME                            
         OI    SHFAGYNH+6,X'80'    TRANSMIT                                     
         MVC   SHFCLIN,SPACES            CLIENT NAME                            
         OI    SHFCLINH+6,X'80'    TRANSMIT                                     
         MVC   SHFPRDN,SPACES            PRODUCT NAME                           
         OI    SHFPRDNH+6,X'80'    TRANSMIT                                     
         MVC   SHFCOMN,SPACES            COMMERCIAL NAME                        
         OI    SHFCOMNH+6,X'80'    TRANSMIT                                     
         MVC   SHFCLGN,SPACES            CLIENT GRP NAME                        
         OI    SHFCLGNH+6,X'80'    TRANSMIT                                     
         MVC   SHFOFFN,SPACES            OFFICE NAME                            
         OI    SHFOFFNH+6,X'80'    TRANSMIT                                     
         SPACE 1                                                                
         CLI   RECNUM,HS           IF THIS IS HOLDING FEE SUMMARY               
         BE    INIT5                                                            
         CLI   RECNUM,SS           OR SECOND NOTICE SUMMARY                     
         BE    INIT5                                                            
         CLI   RECNUM,HD           OR HLD FEE DISK                              
         BNE   *+8                                                              
INIT5    OI    THSTAT,THSUMM       SET SUMMARY STATUS BIT                       
         SPACE 1                                                                
         CLI   RECNUM,SN           IF THIS IS SECOND NOTICES                    
         BE    *+12                                                             
         CLI   RECNUM,SS           OR SECOND NOTICE SUMMARY                     
         BNE   *+8                                                              
         OI    THSTAT,THSECOND     SET SECOND NOTICE STATUS BIT                 
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE DOES INPUT VALIDATION                                    
         SPACE 1                                                                
*                                  R2=A(FIELD HEADER)                           
MYANY    NTR1                                                                   
         CLI   5(R2),0                                                          
         BNE   NO                  RETURN CC NE IF THERE'S INPUT                
***      TM    WHEN,X'20'                                                       
***      BO    FLDMISS             IF SOON THEN INPUT REQUIRED                  
         B     YES                 ELSE RETURN CC EQ - NO INPUT                 
         EJECT                                                                  
*              ROUTINE TO VALIDATE REQUEST PERIOD                               
         SPACE 1                                                                
VALPD    NTR1                                                                   
         LA    R2,SHFPDH           R2=A(PERIOD FIELD)                           
         SPACE 1                                                                
         CLI   5(R2),0             PERIOD CANNOT BE ENTERED                     
         BE    VALPD10                                                          
         CLI   RECNUM,HC           FOR CHANGES RUN                              
         BE    FLDINV                                                           
         SPACE 1                                                                
VALPD10  LA    R3,BLOCK            R3=A(OUTPUT BLOCK FROM PERVAL)               
         USING PERVALD,R3                                                       
         SPACE 1                                                                
         GOTO1 PDVAL,DMCB,(X'80',(R3))  SYSTEM PERIOD VALIDATION RTN.           
         SPACE 1                                                                
         MVC   THPERIOD,PVALCPER   SAVE DISPLAYABLE PERIOD IN W/S               
         MVC   THPSTRT,PVALPSTA    SAVE PWOS PERIOD AS WELL                     
         MVC   THPEND,PVALPEND                                                  
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO VALIDATE OPTIONS                                      
         SPACE 1                                                                
VALOPT   NTR1                                                                   
         LA    R2,SHFOPTH          VALIDATE OPTIONS                             
         CLI   5(R2),0                                                          
         BE    VOPTX                                                            
         SPACE 1                                                                
         LA    R3,BLOCK            R3=A(SCAN BLOCK)                             
         USING SCAND,R3                                                         
         GOTO1 SCANNER,DMCB,(R2),(X'80',(R3))                                   
         CLI   4(R1),0                                                          
         BE    FLDINV                                                           
         ZIC   R0,4(R1)            R0=N'SCAN BLOCK ENTRIES                      
         SPACE 1                                                                
VOPT10   DS    0H                                                               
**N/D    MVC   ERRDISP,SCDISP1     SET DISP. INTO FLD OF LHS FOR ERRORS         
         SPACE 1                                                                
         CLC   =C'TRACE',SCDATA1  TRACE                                         
         BNE   VOPT30                                                           
         CLI   SCDATA2,C'Y'                                                     
         BNE   FLDINV                                                           
         OI    THOPTS,THTRACE      SET TRACE ON                                 
         B     VOPT70                                                           
         SPACE 1                                                                
*&&UK                                                                           
VOPT20   CLC   =C'DISK',SCDATA1    BURNETT DISK                                 
         BNE   VOPT30                                                           
         CLI   SCDATA2,C'Y'                                                     
         BNE   FLDINV                                                           
         CLI   RECNUM,HF           TEST REGULAR HOLDING FEE NOTICES             
         BNE   FLDINV                                                           
         OC    TIFAGY,TIFAGY                                                    
         BZ    AGYMISS                                                          
         OI    THOPTS,THDISK       SET CREATING DISK                            
         OI    THSTAT,THSUMM       SET STATUS BIT FOR SUMMARY REPORT            
         B     VOPT70                                                           
*&&                                                                             
VOPT30   CLC   =C'BDE',SCDATA1     SEND HF TO BDE                               
         BNE   VOPT40                                                           
         CLI   SCDATA2,C'Y'                                                     
         BNE   FLDINV                                                           
         OI    THOPTS,THBDE        SET BDE ON                                   
         B     VOPT70                                                           
*                                                                               
VOPT40   CLC   =C'WEBIT',SCDATA1   SEND SOON RESULTS TO WEB                     
         BNE   FLDINV                                                           
         CLI   SCDATA2,C'Y'                                                     
         BNE   FLDINV                                                           
         TM    WHEN,X'20'          ONLY AVAILABLE FOR SOON REQUESTS             
         BZ    FLDINV                                                           
         CLI   RECNUM,HF           MUST BE REGULAR HOLDING FEE RUN              
         BNE   FLDINV                                                           
         OC    TIFCOM,TIFCOM       COMMERCIAL FILTER MUST BE DEFINED            
         BZ    COMMISS                                                          
*                                                                               
         USING TACID,R4                                                         
         GOTO1 RECVAL,DMCB,TLCLCDQ,(X'24',0)                                    
         BE    *+6                                                              
         DC    H'00'                                                            
         L     R4,AIO                                                           
         MVI   ELCODE,TACIELQ      WEBIT IS NOT A VALID OPTION                  
         BAS   RE,GETEL            FOR PAPER ONLY CLIENTS                       
         BNE   VOPT44              WEBIT IS VALID OPTION FOR ANY                
         TM    TACISTAT,TACIHSPO   ELECTRONIC CLIENTS                           
         BO    FLDINV                                                           
         TM    TACISTAT,TACIHSNC+TACIHSEL+TACIHSPE                              
         BNZ   VOPT45                                                           
         DROP  R4                                                               
*                                                                               
         USING TAAYD,R4                                                         
VOPT44   GOTO1 RECVAL,DMCB,TLAYCDQ,(X'24',0)                                    
         BE    *+6                                                              
         DC    H'00'                                                            
         L     R4,AIO                                                           
         MVI   ELCODE,TAAYELQ      WEBIT IS NOT A VALID OPTION                  
         BAS   RE,GETEL            FOR PAPER ONLY AGENCIES                      
         BE    *+6                                                              
         DC    H'00'                                                            
         TM    TAAYHLDS,TAAYHSPO                                                
         BO    FLDINV                                                           
         DROP  R4                                                               
*                                                                               
VOPT45   OI    THOPTS,THWEBIT      SET TO SEND TO WEB AND MARK FILE             
         MVC   QCRDCODE,=C'HF'     SET UPDATIVE SOON JCL                        
         MVI   TWAWHEN,TWW$UPSO    FORCE UPDATIVE SOON                          
         B     VOPT70                                                           
*                                                                               
VOPT60   DS    0H                                                               
VOPT70   LA    R3,SCANNEXT         BUMP TO NEXT                                 
         BCT   R0,VOPT10           AND CONTINUE                                 
         SPACE 1                                                                
VOPTX    B     XIT                                                              
         EJECT                                                                  
*              VALIDATE A FILTER EXPRESSION                                     
         SPACE 3                                                                
*              INPUT               R2=A(HEADER)                                 
         SPACE 1                                                                
SPECFILT NTR1                                                                   
         CLI   5(R2),0             ANY DATA                                     
         BE    NO                                                               
         OI    6(R2),X'80'                                                      
         GOTO1 ANY                 PUT INTO WORK                                
         CLC   WORK(2),=C'-@'      CHECK FOR NEGATIVE LIST                      
         BE    NEGLIST                                                          
         CLC   WORK(2),=C'@-'                                                   
         BE    NEGLIST                                                          
         CLI   WORK,C'@'           CHECK FOR POSITIVE LIST                      
         BNE   NO                                                               
         SPACE 1                                                                
         LA    R5,WORK+1           POSITIVE LIST                                
         B     VALLIST                                                          
         SPACE 1                                                                
NEGLIST  LA    R5,WORK+2           NEGATIVE LIST                                
         SPACE 1                                                                
VALLIST  XC    KEY,KEY             CHECK LIST EXISTS                            
         LA    R3,KEY                                                           
         USING TLGLD,R3                                                         
         MVI   TLGLCD,TLGLCDQ                                                   
         MVI   TLGLTYPE,TLGLTYPF                                                
         MVC   TLGLLST,0(R5)                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(32),KEYSAVE                                                  
         BNE   FLDINV                                                           
         B     YES                                                              
         DROP  R3                                                               
         SPACE 1                                                                
*              ERRORS, EXITS                                                    
         SPACE 2                                                                
AGYMISS  LA    R2,SHFAGYH          AGENCY REQUIRED                              
         B     FLDMISS                                                          
CLIMISS  LA    R2,SHFCLIH          CLIENT REQUIRED                              
         B     FLDMISS                                                          
COMMISS  LA    R2,SHFCOMH          COMMERICAL REQUIRED                          
         B     FLDMISS                                                          
FLDMISS  MVI   ERROR,MISSING       MISSING INPUT FIELD                          
         B     THEEND                                                           
         SPACE 1                                                                
FLDINV   MVI   ERROR,INVALID       INVALID INPUT FIELD                          
         B     THEEND                                                           
         SPACE 1                                                                
STRINV   MVI   ERROR,ERINVSDT      INVALID START DATE                           
         B     THEEND                                                           
         SPACE 1                                                                
ENDINV   MVI   ERROR,ERINVEDT      INVALID END DATE                             
         B     THEEND                                                           
         SPACE 1                                                                
NOINPUT  MVI   ERROR,ERNOINP       INPUT NOT ALLOWED                            
         B     THEEND                                                           
         SPACE 1                                                                
THEEND   GOTO1 ERREX                                                            
         SPACE 1                                                                
YES      XR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         SPACE 3                                                                
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE TAHFND                                                         
         EJECT                                                                  
       ++INCLUDE TAREPFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TAREPFAD                                                       
         EJECT                                                                  
         PRINT OFF                                                              
* DDGENTWA   (MUST FOLLOW LAST SCREEN)                                          
* DDPERVAL                                                                      
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* TAGENFILE                                                                     
* TASYSDSECT                                                                    
* TASYSEQUS                                                                     
* TAREPWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE TAREPWORKD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'051TAREP0A   03/18/15'                                      
         END                                                                    
