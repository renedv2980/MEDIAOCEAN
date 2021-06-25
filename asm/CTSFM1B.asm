*          DATA SET CTSFM1B    AT LEVEL 152 AS OF 08/26/19                      
*PHASE TA0A1BA                                                                  
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE:        CTSFM1B -- EDICT RECORD MAINTENANCE/LIST             *         
*                                                                     *         
*  COMMENTS:     MAINTAINS EDICT RECORDS ON CTFILE                    *         
*                                                                     *         
*  CALLED FROM:  SFM CONTROLLER (TA0A00), WHICH CALLS                 *         
*                GEGENCON (T00A30), WHICH CALLS THIS.                 *         
*                                                                     *         
*  INPUTS:       SCREENS CTSFMD4 (MAINTENANCE)                        *         
*                        CTSFMD5 (LIST)                               *         
*                        CTSFMD7 (REPORT)                             *         
*                                                                     *         
*  OUTPUTS:      UPDATED EDICT RECORDS, OR LIST.                      *         
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
         TITLE 'TA0A1B - EDICT RECORD MAINTENANCE/LIST/REPORT'                  
TA0A1B   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**0A1B**,R7,RR=R3                                              
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
         OI    GENSTAT4,CONFDEL+NODELLST                                        
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LR                                                               
         CLI   MODE,PRINTREP       PRINT REPORT                                 
         BE    PR                                                               
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
*********************************************************************           
* VALIDATE KEY                                                                  
*********************************************************************           
VK       CLI   ACTNUM,ACTREP       NO KEY VALIDATION FOR LIST / REPORT          
         BE    VKX                                                              
         CLI   ACTNUM,ACTLIST                                                   
         BE    VKX                                                              
*                                                                               
         MVI   DDSUSER,C'N'        ASSUME NOT A DDS USER                        
         MVC   ESLUNUM,=C'*****'                                                
         OI    ESLUNUMH+6,X'80'                                                 
         XC    ESLDEST,ESLDEST                                                  
         OI    ESLDESTH+6,X'80'                                                 
*                                                                               
         LA    R2,ESLNAMEH                                                      
         CLI   ESLNAMEH+5,0        ANY DATA?                                    
         BE    VSFMISS             NO, MISSING                                  
*                                                                               
         XC    KEY,KEY             BUILD ID RECORD KEY                          
         USING CTIREC,R4                                                        
         LA    R4,KEY                                                           
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKID(L'ESLNAME),ESLNAME                                        
         OC    CTIKID,SPACES                                                    
         DROP  R4                                                               
*                                                                               
         GOTO1 HIGH                LOOK FOR ID RECORD                           
         L     R6,AIO                                                           
         CLC   KEYSAVE(25),0(R6)                                                
         BNE   VK10                NOT A DDS USERID                             
*                                                                               
         MVI   DDSUSER,C'Y'        THIS IS A DDS USERID                         
         MVI   ELCODE,X'02'        FIND PASSIVE POINTER ELEMENT                 
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         EDIT  (B2,2(R6)),(5,ESLUNUM),ALIGN=LEFT   DISPLAY ID NUMBER            
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'30'        FIND DESTINATION DETAIL ELEMENT              
         BAS   RE,GETEL                                                         
         BNE   *+10                                                             
         MVC   ESLDEST,2(R6)       DESTINATION NAME                             
*                                                                               
VK10     LA    R4,KEY              BUILD EDICT KEY                              
         USING EDIKEYD,R4                                                       
         XC    KEY,KEY                                                          
         MVI   EDIKSYS,EDIKSYSQ    SYSTEM                                       
         MVI   EDITYPE,EDITYPEQ    RECORD TYPE                                  
         MVC   EDINAME,ESLNAME     USERID                                       
         OC    EDINAME,SPACES                                                   
         DROP  R4                                                               
*                                                                               
         CLI   ACTNUM,ACTDEL                                                    
         BNE   VK20                                                             
         GOTO1 HIGH                LOOK FOR EDICT RECORD                        
         B     VK30                                                             
*                                                                               
VK20     CLI   ACTNUM,ACTREST                                                   
         BNE   VKX                                                              
         OI    DMINBTS,X'08'       READ BACK DELETED RECORDS ALSO               
         GOTO1 HIGH                LOOK FOR EDICT RECORD                        
         NI    DMINBTS,X'F7'                                                    
         TM    DMCB+8,X'02'        IS RECORD MARKED DELETED?                    
         BNO   VK40                NO - DON'T CHECK PASSWORD                    
*                                                                               
VK30     L     R6,AIO                                                           
         CLC   KEYSAVE(25),0(R6)                                                
         BE    *+14                                                             
VK40     MVC   KEY,KEYSAVE         RESTORE KEY AND EXIT                         
         B     VKX                                                              
*                                                                               
         CLI   ESLNAME,C'A'        SPECIAL RECORD?                              
         BL    VK60                YES - CHECK PWD                              
*                                                                               
         MVI   ELCODE,EDILNKEQ     EDICT ELEMENT                                
         BAS   RE,GETEL                                                         
         JNE   *+2                 THIS ELEMENT IS REQUIRED                     
*                                                                               
         USING EDILNKD,R6                                                       
         CLI   EDIMETHS,EDIEASYQ   METHOD OF SENDING = EASYLINK?                
         BE    VK60                                                             
         CLI   EDIMETHR,EDIBDFQ    METHOD OF RECEIVING = BDF?                   
         BE    VK60                                                             
         CLI   EDIMETHS,EDIEDIQ    METHOD OF SENDING = EZ-EDI?                  
         BE    VK60                                                             
         CLI   EDIMETHS,EDIBXFOQ   METHOD OF SENDING = BXF?                     
         BE    VK60                                                             
         CLI   EDIMETHS,EDIPDFQ    METHOD OF SENDING = PDF?                     
         BE    VK60                                                             
         CLI   EDIMETHS,EDIEXRRQ   METHOD OF SENDING = ER?                      
         BE    VK60                                                             
         CLI   EDIMETHS,EDIECNQ    METHOD OF SENDING = ECN?                     
         BNE   VKX                                                              
         DROP  R6                                                               
*                                                                               
VK60     BAS   RE,CHKPWD           CHECK FOR PASSWORD FOR DELETE                
*                                                                               
VKX      B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* VALIDATE RECORD                                                               
***********************************************************************         
VR       L     R6,AIO                                                           
         MVI   ELCODE,EDILNKEQ     EDICT ELEMENT                                
         XC    OLDMETHS(OLDLNQ),OLDMETHS                                        
         BAS   RE,GETEL                                                         
         BNE   VR2                                                              
         USING EDILNKD,R6                                                       
         MVC   OLDMETHS,EDIMETHS   SAVE THE SENDING METHOD                      
         MVC   OLDADVN,EDIADVNO    SAVE THE ADV EASYLINK#                       
         MVC   OLDREPN,EDIREPNO    SAVE THE REP EASYLINK#                       
         MVC   OLDACCN,EDIEACC     SAVE THE EASYLINK ACCOUNT#                   
         CLI   OLDMETHS,EDIECNQ                                                 
         BNE   *+10                                                             
         MVC   OLDECNN,EDIECNN     SAVE THE ECN ACCOUNT#                        
         CLI   OLDMETHS,EDIEXRRQ                                                
         BNE   *+10                                                             
         MVC   OLDXRRN,EDIXRRN     SAVE THE ER  ACCOUNT#                        
         DROP  R6                                                               
*                                                                               
VR2      GOTO1 REMELEM                                                          
*                                                                               
         LA    R5,ELEM                                                          
         USING EDILNKD,R5                                                       
         XC    ELEM,ELEM                                                        
         MVI   EDILNKEL,EDILNKEQ   ELEMENT CODE                                 
         MVI   EDILNKLN,EDILNKLQ   ELEMENT LENGTH                               
*                                                                               
         LA    R2,ESLUIDOH         ALTERNATE ID FIELD                           
         CLI   ESLUIDOH+5,0                                                     
         BE    VR5                 NOT PRESENT                                  
         XC    KEY,KEY             BUILD ID RECORD KEY                          
         USING CTIREC,R1                                                        
         LA    R1,KEY                                                           
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKID,ESLUIDO                                                   
         OC    CTIKID,SPACES                                                    
         MVC   AIO,AIO2                                                         
         GOTO1 HIGH                LOOK FOR ID RECORD                           
         L     R1,AIO                                                           
         MVC   AIO,AIO1                                                         
         CLC   KEYSAVE(25),0(R1)                                                
         BNE   VSFNOTF             NOT FOUND                                    
         MVC   EDIOVUID,CTIKID     SAVE ALPHA USERID                            
         DROP  R1                                                               
*********************************************************************           
* METHOD OF RECEIVING                                                           
*********************************************************************           
VR5      LA    R2,ESLMETRH                                                      
         CLI   ESLMETRH+5,0                                                     
         BE    VSFMISS                                                          
*                                                                               
*AH3     CLI   ESLMETR,EDIEASYQ    EASYLINK?                                    
*AH3     BE    VR10                                                             
         CLI   ESLMETR,EDIFTPQ     FTP?                                         
         BE    VR10                                                             
*        CLI   ESLMETR,EDINJEQ     NJE?                                         
*        BE    VR10                                                             
         CLI   ESLMETR,EDIDAREQ    DARE?                                        
         BE    VR10                                                             
*        CLI   ESLMETR,EDIADVNQ    ADVANTIS?                                    
*        BE    VR10                                                             
         CLI   ESLMETR,EDICOLQ     COLUMBINE?                                   
         BE    VR10                                                             
*        CLI   ESLMETR,EDIBIASQ    BIAS?                                        
*        BE    VR10                                                             
         CLI   ESLMETR,EDIPDFQ     PDF?                                         
         BE    VR10                                                             
         CLI   ESLMETR,EDIBDEQ     BDE-EMAIL?                                   
         BE    VR10                                                             
         CLI   ESLMETR,EDIBDFQ     BDE-FTP?                                     
         BE    VR10                                                             
         CLI   ESLMETR,EDIENCOQ    ENCODA EC?                                   
         BE    VR10                                                             
         CLI   ESLMETR,EDINONEQ    NONE?                                        
         BE    VR10                                                             
*        CLI   ESLMETR,EDIQ2QQ     Q2Q?                                         
*        BNE   *+12                                                             
*        CLI   DDSUSER,C'Y'                                                     
*        BE    VR10                Q2Q ONLY VALID FOR DDS USERS                 
         B     VSFIVAL                                                          
*                                                                               
VR10     MVC   EDIMETHR,ESLMETR    SAVE METHOD OF RECEIVING                     
                                                                                
***********************************************************************         
* METHOD OF SENDING                                                             
***********************************************************************         
         LA    R2,ESLMETSH         METHOD OF SENDING                            
         CLI   ESLMETSH+5,0                                                     
         BE    VSFMISS                                                          
*                                                                               
VR12     CLI   ESLMETS,EDINONEQ    NONE?                                        
         BE    VR15                                                             
         CLI   ESLMETS,EDIPDFQ     PDF? OPTICA                                  
         BE    VR15                                                             
         CLI   DDSUSER,C'Y'                                                     
         BNE   VSFIVAL             ONLY DDS USERS CAN SEND THINGS               
*                                                                               
VR13     CLI   ESLMETS,EDIECNQ     ECN?                                         
         BE    VR15                                                             
         CLI   ESLMETS,EDIEXRRQ    ER?                                          
         BE    VR15                                                             
         CLI   ESLMETS,EDIBXFOQ    BXF?                                         
         BE    VR15                                                             
*AH3     CLI   ESLMETS,EDIEASYQ    EASYLINK?                                    
*AH3     BE    VR15                                                             
         CLI   ESLMETS,EDIEDIQ     EASYLINK EDI?                                
         BE    VR15                                                             
         CLI   ESLMETS,EDIFAXGQ    FAXGATE?                                     
         BE    VR15                                                             
         CLI   ESLMETS,EDIMQMQ     E-MAIL?                                      
         BNE   VSFIVAL                                                          
*                                                                               
VR15     MVC   EDIMETHS,ESLMETS    SAVE METHOD OF SENDING TRANSMISSIONS         
                                                                                
***********************************************************************         
* CHECK if they want to send a copy to PM360                                    
***********************************************************************         
         LA    R2,ESLP360H                                                      
         NI    EDIOPTS,X'FF'-EDIPM360                                           
         CLI   ESLP360,C'Y'                                                     
         BNE   VR15A                                                            
         OI    EDIOPTS,EDIPM360                                                 
         CLI   EDIMETHS,EDIECNQ                                                 
         BE    VR16                   Okay to have PM360                        
         CLI   EDIMETHS,EDIEXRRQ                                                
         BE    VR16                   Okay to have PM360                        
         CLI   EDIMETHS,EDIBXFOQ                                                
         BNE   VSFIVAL                Okay to have PM360                        
         B     VR16                                                             
*                                                                               
VR15A    CLI   EDIMETHS,EDIBXFOQ                                                
         BE    VSFIVAL                PM360 has to be set to Y                  
                                                                                
***********************************************************************         
* EASYLINK ADV MAILBOX NUMBER                                                   
***********************************************************************         
VR16     LA    R2,ESLADVNH         EASYLINK ADV MAILBOX NUMBER                  
         CLI   ESLADVNH+5,0                                                     
         BE    VR18                                                             
         CLI   DDSUSER,C'Y'                                                     
         BNE   VSFIVAL             ONLY DDS USERS CAN HAVE MAILBOXES            
*                                                                               
         TM    ESLADVNH+4,X'08'    NUMERIC?                                     
         BZ    VSFNOT#                                                          
*                                                                               
         CLI   ESLADVNH+5,8        MUST BE EXACTLY 8 DIGITS                     
         BNE   VSFIVAL                                                          
*                                                                               
         CLI   ESLMETS,EDIEDIQ     EASYLINK EDI?                                
         BE    VR17                                                             
         CLC   =C'62',ESLADVN      MUST START WITH '62'                         
         BNE   VSFIVAL                                                          
*                                                                               
VR17     MVC   EDIADVNO,ESLADVN                                                 
         B     VR20                                                             
*                                                                               
VR18     CLI   ESLMETS,EDIEASYQ    EASYLINK?                                    
         BE    VR19                                                             
         CLI   ESLMETS,EDIEDIQ     EASYLINK EDI?                                
         BNE   VR20                                                             
*                                                                               
VR19     CLI   ESLREPNH+5,0                                                     
         BNE   VR20                                                             
         LA    R2,ESLADVNH                                                      
         B     VSFMISS             BOTH ADV & REP MAILBOX ARE MISSING           
                                                                                
**********************************************************************          
* EASYLINK REP MAILBOX NUMBER                                                   
**********************************************************************          
VR20     LA    R2,ESLREPNH         EASYLINK REP MAILBOX NUMBER                  
         CLI   ESLREPNH+5,0                                                     
         BE    VR25                                                             
         CLI   DDSUSER,C'Y'                                                     
         BNE   VSFIVAL             ONLY DDS USERS CAN HAVE MAILBOXES            
         TM    ESLREPNH+4,X'08'    NUMERIC?                                     
         BZ    VSFNOT#                                                          
         CLI   ESLREPNH+5,8        MUST BE EXACTLY 8 DIGITS                     
         BNE   VSFIVAL                                                          
         CLI   ESLMETS,EDIEDIQ     EASYLINK EDI?                                
         BE    VR22                                                             
         CLC   =C'62',ESLREPN      MUST START WITH '62'                         
         BNE   VSFIVAL                                                          
VR22     MVC   EDIREPNO,ESLREPN                                                 
                                                                                
***********************************************************************         
* EASYLINK EDI ACCOUNT NUMBER                                                   
***********************************************************************         
VR25     CLI   ESLMETS,EDIEDIQ     EASYLINK EDI?                                
         BNE   VR27                                                             
*                                                                               
         LA    R2,ESLACCNH                                                      
         CLI   ESLACCNH+5,0                                                     
         BE    VSFMISS             EZ-EDI ACCOUNT# IS REQUIRED                  
***********************************************************************         
* NEED TO DO SOME VALIDATION FOR THE EZ-EDI ACCOUNT#                            
***********************************************************************         
         MVC   EDIEACC,ESLACCN                                                  
         B     VR40                                                             
*                                  NEW EASYLINK ACCOUNT FORMAT                  
VR27     CLC   =C'4',ESLACCN       MUST BEGIN WITH C'4'                         
         BNE   VR31                CHECK FOR THE OLD FORMAT                     
*                                                                               
         LA    R2,ESLACCNH         EASYLINK ACCOUNT #                           
         CLI   ESLACCNH+5,9         MUST BE 9 CHARATERS                         
         BNE   VSFIVAL                                                          
*                                                                               
         LA    R1,6                                                             
         LA    RE,ESLACCN+3                                                     
VR28     CLI   0(RE),C'0'                                                       
         BL    VSFIVAL                                                          
         CLI   0(RE),C'9'                                                       
         BH    VSFIVAL                                                          
         LA    RE,1(RE)                                                         
         BCT   R1,VR28                                                          
*                                                                               
         MVC   EDIEACC,ESLACCN                                                  
         B     VR40                                                             
*                                                                               
*THIS IS NO LONGER USED, NOW ACC# 400------, 8/26/02                            
*                                                                               
VR31     LA    R2,ESLACCNH         EASYLINK ACCOUNT #                           
         CLI   ESLACCNH+5,0                                                     
         BE    VR40                                                             
         CLI   ESLACCNH+5,9         MUST BE 9 OR 10 CHARATERS                   
         BL    VSFIVAL                LESS THAN 9                               
         CLI   ESLACCNH+5,10                                                    
         BH    VSFIVAL                MORE THAN 10                              
*                                                                               
         LA    R1,3                FIRST 3 ARE ALPHA                            
         LA    RE,ESLACCN                                                       
VR32     CLI   0(RE),C'A'                                                       
         BL    VSFIVAL                                                          
         CLI   0(RE),C'Z'                                                       
         BH    VSFIVAL                                                          
         LA    RE,1(RE)                                                         
         BCT   R1,VR32                                                          
*                                                                               
         ZIC   R1,ESLACCNH+5       AND THE REST ARE NUMERIC                     
         SHI   R1,3                                                             
VR34     CLI   0(RE),C'0'                                                       
         BL    VSFIVAL                                                          
         CLI   0(RE),C'9'                                                       
         BH    VSFIVAL                                                          
         LA    RE,1(RE)                                                         
         BCT   R1,VR34                                                          
*                                                                               
         MVC   EDIEACC,ESLACCN                                                  
                                                                                
***********************************************************************         
* ECN ID NUMBER                                                                 
***********************************************************************         
VR40     LA    R2,ESLAECNH                                                      
         CLI   ESLMETS,EDIECNQ     ECN?                                         
         BE    VR41                YES                                          
         CLI   ESLAECNH+5,0        NO INPUT REQUIRED IF NOT ECN                 
         BNE   VSFIVAL             NOT VALID TO HAVE INPUT                      
         B     VR45                                                             
*                                                                               
VR41     CLI   ESLAECNH+5,0                                                     
         BE    VSFMISS                                                          
*                                                                               
         CLC   ESLAECN(3),=C'ECN'                                               
         BNE   VSFIVAL             NOT VALID WITHOUT ECN                        
         LA    RE,ESLAECN                                                       
         AHI   RE,3                                                             
         LHI   R1,L'ESLAECN-3                                                   
VR44     CLI   0(RE),C'0'                                                       
         BL    VSFIVAL                                                          
         CLI   0(RE),C'9'                                                       
         BH    VSFIVAL                                                          
         LA    RE,1(RE)                                                         
         BCT   R1,VR44                                                          
         MVC   EDIECNN,ESLAECN                                                  
                                                                                
***********************************************************************         
* ER ID REQUIRED IF SET TO SEND VIA EXTREME REACH                               
***********************************************************************         
VR45     LA    R2,ESLAERNH                                                      
         CLI   ESLMETS,EDIEXRRQ    ER                                           
         BE    VR46                YES                                          
         CLI   ESLAERNH+5,0        NO INPUT REQUIRED IF NOT ECN                 
         BNE   VSFIVAL             NOT VALID TO HAVE INPUT                      
         B     VR48                                                             
*                                                                               
VR46     CLI   ESLAERNH+5,0                                                     
         BE    VSFMISS                                                          
*                                                                               
         CLC   ESLAERN(2),=C'XR'                                                
         BNE   VSFIVAL             NOT VALID WITHOUT XR 'eXtreme Reach'         
         LA    RE,ESLAERN                                                       
         AHI   RE,2                                                             
         LHI   R1,L'ESLAERN-3                                                   
VR47     CLI   0(RE),C'0'                                                       
         BL    VSFIVAL                                                          
         CLI   0(RE),C'9'                                                       
         BH    VSFIVAL                                                          
         LA    RE,1(RE)                                                         
         BCT   R1,VR47                                                          
         MVC   EDIXRRN,ESLAERN                                                  
                                                                                
***********************************************************************         
* NJE CLASS                                                                     
***********************************************************************         
VR48     LA    R2,ESLNJECH         NJE CLASS                                    
         CLI   ESLNJECH+5,0                                                     
         BNE   VR49                                                             
         CLI   ESLMETR,EDINJEQ     REQUIRED FOR NJE                             
         BE    VSFMISS                                                          
         B     VR50                                                             
*                                                                               
VR49     MVC   EDINJEC,ESLNJEC                                                  
         LA    R2,ESLNJENH         NJE NODE                                     
         CLI   ESLNJENH+5,0                                                     
         BE    VSFMISS                                                          
*                                                                               
         MVC   EDINJEN,ESLNJEN                                                  
         OC    EDINJEN,SPACES                                                   
         LA    R2,ESLNJEUH         NJE USERID                                   
         CLI   ESLNJEUH+5,0                                                     
         BE    VSFMISS                                                          
         MVC   EDINJEU,ESLNJEU                                                  
         OC    EDINJEU,SPACES                                                   
**********************************************************************          
* FTP                                                                           
**********************************************************************          
VR50     LA    R2,ESLFTPOH         FTP OPERATING SYSTEM                         
         CLI   ESLFTPOH+5,0                                                     
         BNE   VR51                                                             
         CLI   ESLMETR,EDIFTPQ     REQUIRED FOR FTP                             
         BNE   VR60                                                             
         B     VSFMISS                                                          
*                                                                               
VR51     CLI   ESLFTPO,EDIOS4Q     OS/400 (FOR AS/400)                          
         BE    VR52                                                             
         CLI   ESLFTPO,EDIOS2Q     OS/2 (FOR PC)                                
         BNE   VSFIVAL                                                          
*                                                                               
VR52     MVC   EDIFTPO,ESLFTPO                                                  
         LA    R2,ESLFTPLH         FTP LU NAME                                  
         CLI   ESLFTPLH+5,0                                                     
         BE    VSFMISS                                                          
         CLI   ESLFTPLH+5,8        LUID MUST BE EXACTLY 8 CHARACTERS            
         BNE   VSFIVAL                                                          
*                                                                               
         MVC   EDIFTPL,ESLFTPL                                                  
         OC    EDIFTPL,SPACES                                                   
         LA    R2,ESLFTPUH         FTP APPC USERID                              
         CLI   ESLFTPUH+5,0                                                     
         BE    VSFMISS                                                          
*                                                                               
         MVC   EDIFTPU,ESLFTPU                                                  
         OC    EDIFTPU,SPACES                                                   
         LA    R2,ESLFTPPH         FTP APPC PASSWORD                            
         CLI   ESLFTPPH+5,0                                                     
         BE    VSFMISS                                                          
*                                                                               
         MVC   EDIFTPP,ESLFTPP                                                  
         OC    EDIFTPP,SPACES                                                   
         LA    R2,ESLFTPSH         FTP APPC SERVER CLASS                        
         CLI   ESLFTPSH+5,0                                                     
         BE    VSFMISS                                                          
*                                                                               
         TM    ESLFTPSH+4,X'04'    MUST BE ALPHA                                
         BZ    VSFNCHAR                                                         
         CLI   ESLFTPS,C'U'        RESERVED FOR UK                              
         BE    VSFIVAL                                                          
*                                                                               
         MVC   EDIFTPS,ESLFTPS                                                  
         LA    R2,ESLFTPRH         INCLUDE RECORD COUNT?                        
         CLI   ESLFTPRH+5,0                                                     
         BE    VR55                OPTIONAL FIELD                               
         CLI   ESLFTPR,C'N'                                                     
         BE    VR55                NO IS DEFAULT                                
         CLI   ESLFTPR,C'Y'                                                     
         BNE   VSFIVAL                                                          
         OI    EDIFFLGS,EDIFTPRC   YES                                          
                                                                                
VR55     LA    R2,ESLFTPCH         DO CHARACTER SET CONVERSION?                 
         CLI   ESLFTPCH+5,0                                                     
         BE    VR60                OPTIONAL FIELD                               
         CLI   ESLFTPC,C'N'                                                     
         BE    VR60                NO IS DEFAULT                                
         CLI   ESLFTPC,C'Y'                                                     
         BNE   VSFIVAL             MUST BE Y/N/BLANK                            
         CLI   ESLFTPO,EDIOS2Q     ONLY VALID FOR OS/2 TRANSFERS                
         BNE   VSFIVAL                                                          
         OI    EDIFFLGS,EDIFTPCO                                                
*                                                                               
VR60     DS    0H                                                               
*&&DO                                                                           
*ADVANTIS IS NO LONGER USED, COMMENT OUT THIS CODE ON 7/23/02                   
         LA    R2,ESLADNAH         ADVANTIS ACCOUNT                             
         CLI   ESLADNAH+5,0                                                     
         BNE   VR61                                                             
         CLI   ESLMETR,EDIADVNQ    REQUIRED FOR ADVANTIS                        
         BE    VSFMISS                                                          
         B     VR70                                                             
*                                                                               
VR61     MVC   EDIADNA,ESLADNA                                                  
         OC    EDIADNA,SPACES                                                   
         LA    R2,ESLADNUH         ADVANTIS USERID                              
         CLI   ESLADNUH+5,0                                                     
         BE    VSFMISS                                                          
*                                                                               
         MVC   EDIADNU,ESLADNU                                                  
         OC    EDIADNU,SPACES                                                   
         LA    R2,ESLADNCH         ADVANTIS CLASS                               
         CLI   ESLADNCH+5,0                                                     
         BE    VSFMISS                                                          
         MVC   EDIADNC,ESLADNC                                                  
         OC    EDIADNC,SPACES                                                   
*&&                                                                             
*                                                                               
VR70     LA    R2,ESLCOLLH         COLUMBINE LUID                               
         CLI   ESLCOLLH+5,0                                                     
         BNE   VR71                                                             
         CLI   ESLMETR,EDICOLQ     REQUIRED FOR COLUMBINE                       
         BNE   VR80                                                             
         B     VSFMISS                                                          
*                                                                               
VR71     CLI   ESLCOLLH+5,8        LUID MUST BE EXACTLY 8 CHARACTERS            
         BNE   VSFIVAL                                                          
*                                                                               
         MVC   EDICOLL,ESLCOLL                                                  
         OC    EDICOLL,SPACES                                                   
         LA    R2,ESLCOLUH         COLUMBINE APPC USERID                        
         CLI   ESLCOLUH+5,0                                                     
         BE    VSFMISS                                                          
*                                                                               
         MVC   EDICOLU,ESLCOLU                                                  
         OC    EDICOLU,SPACES                                                   
         LA    R2,ESLCOLPH         COLUMBINE APPC PASSWORD                      
         CLI   ESLCOLPH+5,0                                                     
         BE    VSFMISS                                                          
*                                                                               
         MVC   EDICOLP,ESLCOLP                                                  
         OC    EDICOLP,SPACES                                                   
                                                                                
***********************************************************************         
* BDE-FTP                                                                       
***********************************************************************         
VR80     LA    R2,ESLBDCNH         BDE-FTP COMMON NAME                          
         CLI   ESLBDCNH+5,0                                                     
         BNE   VR81                                                             
         CLI   ESLMETR,EDIBDFQ     REQUIRED FOR BDE-FTP                         
         BE    VSFMISS                                                          
         CLI   ESLMETS,EDIPDFQ     REQUIRED FOR PDF                             
         BE    VSFMISS                                                          
         B     VR100               MQ TAG ID WHEN PDF                           
*                                                                               
VR81     MVC   EDIBDECN,ESLBDCN                                                 
         CLI   ESLMETS,EDIPDFQ     REQUIRED FOR BDE-FTP                         
         BNE   VR82                                                             
         CLI   ESLBDCNH+5,16                                                    
         BNE   VSFIVAL             MUST BE 16 LONG                              
*                                                                               
VR82     LA    R2,ESLBDBIH         BDE-FTP BINARY DATA?                         
         MVI   EDIBDEBI,C'N'       'N' AS DEFAULT                               
         CLI   ESLBDBIH+5,0                                                     
         BE    VR83                                                             
         CLI   ESLBDBI,C'N'                                                     
         BE    VR83                                                             
         CLI   ESLBDBI,C'Y'                                                     
         BNE   VSFIVAL             MUST BE EITHER 'Y' OR 'N' OR BLANK           
         MVC   EDIBDEBI,ESLBDBI                                                 
*                                  CLEAR OS/CONVERT/CODE-PAGE                   
         XC    EDIBDEOP,EDIBDEOP                                                
         XC    EDIBDECA,EDIBDECA                                                
         XC    EDIBDECP,EDIBDECP                                                
*                                                                               
VR83     DS    0H                                                               
         LA    R2,ESLBDENH         BDE-FTP ENCRYPTION                           
         MVI   EDIBDEEN,C'N'       'NONE' AS DEFAULT                            
         CLI   ESLBDENH+5,0                                                     
         BE    VR85                                                             
         CLI   ESLBDEN,EDIBNOQ     NONE                                         
         BE    VR85                                                             
         CLI   ESLBDEN,EDIBBFQ     BLOWFISH                                     
         BE    VR84                                                             
         CLI   ESLBDEN,EDIB3DQ     3DES                                         
         BNE   VSFIVAL                                                          
VR84     MVC   EDIBDEEN,ESLBDEN                                                 
*                                                                               
VR85     LA    R2,ESLBDCMH         BDE-FTP COMPRESS FIRST                       
         MVI   EDIBDECM,C'N'       'N' AS DEFAULT                               
         CLI   ESLBDCMH+5,0                                                     
         BE    VR87                                                             
         CLI   ESLBDCM,C'N'                                                     
         BE    VR87                                                             
         CLI   ESLBDCM,C'Y'                                                     
         BNE   VSFIVAL             MUST BE EITHER 'Y' OR 'N' OR BLANK           
         MVC   EDIBDECM,ESLBDCM                                                 
*                                                                               
VR87     LA    R2,ESLBDSFH         BDE-FTP DELETE SENT FILE                     
         MVI   EDIBDESF,C'N'       'N' AS DEFAULT                               
         CLI   ESLBDSFH+5,0                                                     
         BE    VR87X                                                            
         CLI   ESLBDSF,C'N'                                                     
         BE    VR87X                                                            
         CLI   ESLBDSF,C'Y'                                                     
         BNE   VSFIVAL             MUST BE EITHER 'Y' OR 'N' OR BLANK           
         MVC   EDIBDESF,ESLBDSF                                                 
VR87X    DS    0H                                                               
*                                                                               
         CLI   ESLBDBI,C'Y'        BINARY=Y?                                    
         BNE   VR88                NO - CONTINUE                                
         B     VR99                YES - SKIP OS/CONVERT/CODE-PAGE              
*                                                                               
VR88     LA    R2,ESLBDOPH         BDE-FTP OPSYS                                
         CLI   ESLBDOPH+5,0        REQUIRED                                     
         BE    VSFMISS                                                          
*                                                                               
         CLI   ESLBDOP,EDIBMVSQ    MVS                                          
         BE    VR88X                                                            
         CLI   ESLBDOP,EDIBUIXQ    UNIX                                         
         BE    VR88X                                                            
         CLI   ESLBDOP,EDIBWINQ    WIN                                          
         BE    VR88X                                                            
         CLI   ESLBDOP,EDIBAS4Q    OS/400                                       
         BE    VR88X                                                            
         CLI   ESLBDOP,EDIBOS2Q    OS/2 (PC)                                    
         BNE   VSFIVAL                                                          
*                                                                               
VR88X    MVC   EDIBDEOP,ESLBDOP                                                 
*                                                                               
VR89     LA    R2,ESLBDCAH         BDE-FTP CONVERT TO ASCII                     
         MVI   EDIBDECA,C'N'       'N' AS DEFAULT                               
         CLI   ESLBDCAH+5,0                                                     
         BE    VR93                                                             
         CLI   ESLBDCA,C'N'                                                     
         BE    VR93                                                             
         CLI   ESLBDCA,C'Y'                                                     
         BNE   VSFIVAL             MUST BE EITHER 'Y' OR 'N' OR BLANK           
*                                                                               
         MVC   EDIBDECA,ESLBDCA                                                 
*                                                                               
VR93     LA    R2,ESLBDCPH         BDE-FTP CODE PAGE                            
         CLI   ESLBDCPH+5,0                                                     
         BE    VR99                                                             
         LA    RE,CODEPAGE                                                      
VR97     CLI   0(RE),0                                                          
         BE    VSFIVAL                                                          
*                                                                               
         OC    ESLBDCP,SPACES                                                   
         CLC   ESLBDCP,0(RE)                                                    
         BE    *+12                                                             
         AHI   RE,L'CODEPAGE                                                    
         B     VR97                                                             
*                                                                               
         MVC   EDIBDECP,ESLBDCP                                                 
         B     VR99                                                             
*                                                                               
CODEPAGE DS    0CL10                                                            
******** DC    CL10'IBM-1047'      NOT IN USE, 2/1/05, YYUN                     
         DC    CL10'ISO8859-1'                                                  
         DC    X'00'                                                            
*                                                                               
VR99     MVC   EDIBDEFN,ESLBDFN    BDE-FTP FAILURE NOTIFICATION                 
*                                                                               
VR100    EQU   *                                                                
         CLI   ESLNAME,C'A'        SPECIAL RECORD?                              
         BL    VR150               YES - CHECK PWD                              
*                                                                               
         CLC   OLDECNN,ESLAECN     ECN ACCOUNT# CHANGE?                         
         BNE   VR150                                                            
         CLC   OLDADVN,ESLADVN     ADV EASYLINK# CHANGE?                        
         BNE   VR150                                                            
         CLC   OLDREPN,ESLREPN     REP EASYLINK# CHANGE?                        
         BNE   VR150                                                            
         CLC   OLDACCN,ESLACCN     EASYLINK ACCOUNT# CHANGE?                    
         BNE   VR150                                                            
         CLC   OLDXRRN,ESLAERN     ER ACCOUNT# CHANGE?                          
         BNE   VR150                                                            
*                                                                               
         CLI   OLDMETHS,EDIEASYQ   OLD SENDING METHOD = EZ?                     
         BE    VR110                                                            
         CLI   ESLMETS,EDIEASYQ    NEW SENDING METHOD = EZ?                     
         BNE   VR120               BOTH OLD & NEW METH != EZ                    
         B     VR150               OLD METH!=EZ & NEW METH=EZ - CHK PWD         
*                                                                               
VR110    CLI   ESLMETS,EDIEASYQ    NEW SENDING METHOD = EZ?                     
         BE    VR200               OLD METH=EZ & NEW METH=EZ - SKIP PWD         
         B     VR150               OLD METH=EZ & NEW METH!=EZ - CHK PWD         
*                                                                               
VR120    CLI   OLDMETHS,EDIEDIQ    OLD SENDING METHOD = EZ-EDI?                 
         BE    VR130                                                            
         CLI   ESLMETS,EDIEDIQ     NEW SENDING METHOD = EDI?                    
         BNE   VR200               BOTH OLD & NEW METH != EDI-SKIP PWD          
         B     VR150               OLD METH!=EDI & NEW METH=EDI-CHK PWD         
*                                                                               
VR130    CLI   ESLMETS,EDIEDIQ     NEW SENDING METHOD = EZ-EDI?                 
         BE    VR200               OLD METH=EDI & NEW METH=EDI-SKIP PWD         
*                                                                               
VR150    BAS   RE,CHKPWD           CHECK THE PASSWORD                           
*                                                                               
VR200    GOTO1 ADDELEM             ADD EDICT ELEMENT                            
*                                                                               
         B     DR                  REDISPLAY RECORD                             
         DROP  R5                                                               
         EJECT                                                                  
*********************************************************************           
* CHECK PASSWORD                                                                
*********************************************************************           
CHKPWD   NTR1                                                                   
         GOTO1 GETFACT,DMCB,0      GET THE FACPAK INFO                          
         L     RF,0(R1)                                                         
         USING FACTSD,RF                                                        
         ZIC   RE,FASYSID          FACIDTAB INDEX                               
         DROP  RF                                                               
*                                  CHECK IF TEST FACPAK                         
         MHI   RE,L'FACIDTAB       X LENGTH OF FACIDTAB ENTRY                   
         LA    RF,FACIDTAB(RE)     POINT TO TARGET ENTRY                        
*                                                                               
         USING FACITABD,RF                                                      
         TM    FACIFL,FACITST      IS IT A TEST FACPAK                          
         BO    CPOK                SKIP CHECKING PASSWORD                       
         DROP  RF                                                               
*                                                                               
CP10     CLI   ESLNAME,C'A'        SPECIAL RECORD?                              
         BL    CP50                YES - CHECK FOR OTHER PWD                    
*                                                                               
         LA    R2,ESLPWDH                                                       
         CLI   5(R2),0             PASSWORD REQUIRED FOR UPDATE ACTIONS         
         BNE   CP30                                                             
         MVI   GERROR1,MISSING                                                  
         B     CPBAD                                                            
*                                                                               
CP30     CLI   5(R2),5             MUST BE 5 CHAR                               
         BNE   *+14                                                             
         CLC   =C'WHINE',ESLPWD                                                 
         BE    CPOK                                                             
         MVI   GERROR1,INVALID                                                  
         B     CPBAD                                                            
*                                                                               
CP50     CLI   ACTNUM,ACTADD       ACTION ADD?                                  
         BE    CPOK                IT IS OKAY TO ADD W/O PASSWORD               
         LA    R2,ESLPWDH                                                       
         CLI   5(R2),0             PASSWORD REQUIRED FOR UPDATE ACTIONS         
         BNE   CP60                                                             
         MVI   GERROR1,MISSING                                                  
         B     CPBAD                                                            
*                                                                               
CP60     CLI   5(R2),8             MUST BE 8 CHAR                               
         BNE   CP70                                                             
         CLC   =C'TZIHDDNY',ESLPWD                                              
         BE    CPOK                                                             
         CLC   =C'AWILDDNY',ESLPWD                                              
         BE    CPOK                                                             
         CLC   =C'RCRIDDNY',ESLPWD                                              
         BE    CPOK                                                             
         CLC   =C'AHYDDDNY',ESLPWD                                              
         BE    CPOK                                                             
CP70     MVI   GERROR1,INVALID                                                  
         B     CPBAD                                                            
*                                                                               
CPBAD    XC    ESLPWD,ESLPWD       CLEAR THE PASSWORD                           
         OI    ESLPWDH+6,X'80'                                                  
         B     VSFMERR                                                          
*                                                                               
CPOK     XC    ESLPWD,ESLPWD       CLEAR THE PASSWORD                           
         OI    ESLPWDH+6,X'80'                                                  
CPX      B     XIT                                                              
         EJECT                                                                  
**********************************************************************          
* DISPLAY RECORD                                                                
**********************************************************************          
DR       L     R6,AIO                                                           
         TWAXC ESLMETSH                                                         
         MVI   ELCODE,EDILNKEQ     EDICT ELEMENT                                
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                THIS ELEMENT IS REQUIRED                     
*                                                                               
         USING EDILNKD,R6                                                       
         MVC   ESLUIDO,EDIOVUID    ALTERNATE USERID                             
         OI    ESLUIDOH+6,X'80'                                                 
         MVC   ESLMETR,EDIMETHR    METHOD OF RECEIVING                          
         OI    ESLMETRH+6,X'80'                                                 
         MVC   ESLMETS,EDIMETHS    METHOD OF SENDING                            
         OI    ESLMETSH+6,X'80'                                                 
         MVC   ESLADVN,EDIADVNO    EASYLINK ADV NUMBER                          
         OI    ESLADVNH+6,X'80'                                                 
         MVC   ESLREPN,EDIREPNO    EASYLINK REP NUMBER                          
         OI    ESLREPNH+6,X'80'                                                 
         MVC   ESLACCN,EDIEACC     EASYLINK ACCOUNT #                           
         OI    ESLACCNH+6,X'80'                                                 
         MVC   ESLNJEC,EDINJEC     NJE DATA                                     
         OI    ESLNJECH+6,X'80'                                                 
         MVC   ESLNJEN,EDINJEN                                                  
         OI    ESLNJENH+6,X'80'                                                 
         MVC   ESLNJEU,EDINJEU                                                  
         OI    ESLNJEUH+6,X'80'                                                 
         MVC   ESLFTPO,EDIFTPO     FTP DATA                                     
         OI    ESLFTPOH+6,X'80'                                                 
         MVC   ESLFTPL,EDIFTPL                                                  
         OI    ESLFTPLH+6,X'80'                                                 
         MVC   ESLFTPU,EDIFTPU                                                  
         OI    ESLFTPUH+6,X'80'                                                 
         MVC   ESLFTPP,EDIFTPP                                                  
         OI    ESLFTPPH+6,X'80'                                                 
         MVC   ESLFTPS,EDIFTPS                                                  
         OI    ESLFTPSH+6,X'80'                                                 
         MVI   ESLFTPR,C'N'                                                     
         OI    ESLP360H+6,X'80'                                                 
         MVI   ESLP360,C'N'                                                     
         TM    EDIOPTS,EDIPM360     What's PM360?                               
         BZ    *+8                                                              
         MVI   ESLP360,C'Y'        Yep                                          
         MVC   ESLAERN,SPACES                                                   
         CLI   EDIMETHS,EDIEXRRQ   ER NUMBER                                    
         BNE   DR12                                                             
         MVC   ESLAERN,EDIXRRN                                                  
         OI    ESLAERNH+6,X'80'                                                 
*                                                                               
DR12     TM    EDIFFLGS,EDIFTPRC                                                
         BZ    *+8                                                              
         MVI   ESLFTPR,C'Y'                                                     
         OI    ESLFTPRH+6,X'80'                                                 
         MVI   ESLFTPC,C'N'                                                     
         TM    EDIFFLGS,EDIFTPCO                                                
         BZ    *+8                                                              
         MVI   ESLFTPC,C'Y'                                                     
         OI    ESLFTPCH+6,X'80'                                                 
*&&DO                                                                           
*ADVANTIS IS NO LONGER USED, COMMENT OUT THIS CODE ON 7/23/02                   
         MVC   ESLADNA,EDIADNA     ADVANTIS DATA                                
         OI    ESLADNAH+6,X'80'                                                 
         MVC   ESLADNU,EDIADNU                                                  
         OI    ESLADNUH+6,X'80'                                                 
         MVC   ESLADNC,EDIADNC                                                  
         OI    ESLADNCH+6,X'80'                                                 
*&&                                                                             
         MVC   ESLCOLL,EDICOLL     COLUMBINE DATA                               
         OI    ESLCOLLH+6,X'80'                                                 
         MVC   ESLCOLU,EDICOLU                                                  
         OI    ESLCOLUH+6,X'80'                                                 
         MVC   ESLCOLP,EDICOLP                                                  
         OI    ESLCOLPH+6,X'80'                                                 
         MVC   ESLBDCN,EDIBDECN    BDE-FTP DATA                                 
         OI    ESLBDCNH+6,X'80'                                                 
         MVC   ESLBDBI,EDIBDEBI                                                 
         OI    ESLBDBIH+6,X'80'                                                 
         MVC   ESLBDOP,EDIBDEOP                                                 
         OI    ESLBDOPH+6,X'80'                                                 
         MVC   ESLBDEN,EDIBDEEN                                                 
         OI    ESLBDENH+6,X'80'                                                 
         MVC   ESLBDCM,EDIBDECM                                                 
         OI    ESLBDCMH+6,X'80'                                                 
         MVC   ESLBDSF,EDIBDESF                                                 
         OI    ESLBDSFH+6,X'80'                                                 
         MVC   ESLBDCA,EDIBDECA                                                 
         OI    ESLBDCAH+6,X'80'                                                 
         MVC   ESLBDCP,EDIBDECP                                                 
         OI    ESLBDCPH+6,X'80'                                                 
         MVC   ESLBDFN,EDIBDEFN                                                 
         OI    ESLBDFNH+6,X'80'                                                 
         MVC   ESLAECN,SPACES                                                   
         CLI   EDIMETHS,EDIECNQ    ECN?                                         
         BNE   DR14                                                             
         MVC   ESLAECN,EDIECNN                                                  
         OI    ESLAECNH+6,X'80'                                                 
*                                                                               
DR14     CLI   ACTNUM,ACTDEL       LET GENCON FORCE CONFIRM OF DELETE           
         BE    DRX                                                              
*                                                                               
         L     R6,AIO              POINT TO EDICT RECORD                        
         MVI   ELCODE,X'F1'        ACTIVITY ELEMENT                             
         BAS   RE,GETEL                                                         
         BNE   DRX                 NONE PRESENT                                 
*                                                                               
         USING ACTVD,R6                                                         
         GOTO1 DATCON,DMCB,(3,ACTVCHDT),(11,LUPDATE)                            
         XC    DMCB,DMCB                                                        
         GOTO1 GETTXT,DMCB,28,0,(C'I',0),(11,LUPDATE)                           
         OI    GENSTAT2,USMYOK                                                  
         DROP  R6                                                               
*                                                                               
DRX      B     XIT                                                              
         EJECT                                                                  
*********************************************************************           
* DISPLAY KEY                                                                   
*********************************************************************           
DK       L     R4,AIO              SELECTED RECORD                              
         USING EDIKEYD,R4                                                       
*                                                                               
         MVC   ESLNAME,EDINAME     TRANSMIT NAME                                
         OI    ESLNAMEH+6,X'80'                                                 
*                                                                               
         MVI   DDSUSER,C'N'        ASSUME NOT A DDS USER                        
         MVC   ESLUNUM,=C'*****'                                                
         OI    ESLUNUMH+6,X'80'                                                 
         XC    ESLDEST,ESLDEST                                                  
         OI    ESLDESTH+6,X'80'                                                 
*                                                                               
         XC    KEY,KEY             BUILD ID RECORD KEY                          
         USING CTIREC,RF                                                        
         LA    RF,KEY                                                           
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKID,SPACES                                                    
         MVC   CTIKID(L'EDINAME),EDINAME                                        
         DROP  R4,RF                                                            
*                                                                               
         MVC   AIO,AIO2                                                         
         GOTO1 HIGH                LOOK FOR ID RECORD                           
         L     R6,AIO                                                           
         CLC   KEYSAVE(25),0(R6)                                                
         BNE   DKX                 NOT A DDS USER                               
*                                                                               
         MVI   DDSUSER,C'Y'        THIS IS A DDS USERID                         
         MVI   ELCODE,X'02'        FIND PASSIVE POINTER ELEMENT                 
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         EDIT  (B2,2(R6)),(5,ESLUNUM),ALIGN=LEFT                                
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'30'        FIND DESTINATION DETAIL ELEMENT              
         BAS   RE,GETEL                                                         
         BNE   *+10                                                             
         MVC   ESLDEST,2(R6)       DESTINATION NAME                             
*                                                                               
DKX      MVC   AIO,AIO1            RESTORE AIO                                  
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ONLINE LIST                                                                   
***********************************************************************         
LR       LA    R4,KEY                                                           
         USING EDIKEYD,R4                                                       
         OC    KEY,KEY             FIRST TIME THROUGH?                          
         BNZ   LR10                NO                                           
*                                                                               
         MVI   EDIKSYS,EDIKSYSQ    SYSTEM                                       
         MVI   EDITYPE,EDITYPEQ    RECORD TYPE                                  
         MVC   EDINAME,ESLNAME     START USERID                                 
*                                                                               
LR10     GOTO1 HIGH                FIRST RECORD                                 
         L     R4,AIO                                                           
         B     LR30                                                             
*                                                                               
LR20     GOTO1 SEQ                 NEXT RECORD                                  
*                                                                               
LR30     CLI   0(R4),EDIKSYSQ      EDICT RECORD?                                
         BNE   LRX                                                              
         CLI   1(R4),EDITYPEQ                                                   
         BNE   LRX                 NO MORE RECORDS TO LIST                      
*                                                                               
         LR    R6,R4                                                            
         MVI   ELCODE,EDILNKEQ     EDICT ELEMENT                                
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                THIS ELEMENT IS REQUIRED                     
*                                                                               
         USING EDILNKD,R6                                                       
         CLI   EALAORR,C'A'        LIST ADV EASYLINK ONLY?                      
         BNE   *+18                                                             
         OC    EDIADVNO,EDIADVNO   ANY ADV EASYLINK NUMBER?                     
         BZ    LR20                NO -- SKIP THIS RECORD                       
         B     LR40                                                             
         CLI   EALAORR,C'R'        LIST REP EASYLINK ONLY?                      
         BNE   LR40                                                             
         OC    EDIREPNO,EDIREPNO   ANY REP EASYLINK NUMBER?                     
         BZ    LR20                NO -- SKIP THIS RECORD                       
*                                                                               
LR40     CLI   EALMETRH+5,0        ANY RECEIVING METHOD GIVEN?                  
         BE    *+14                                                             
         CLC   EALMETR,EDIMETHR    YES -- MATCH ON RECEIVING METHOD?            
         BNE   LR20                NO -- SKIP THIS RECORD                       
*                                                                               
         CLI   EALMETSH+5,0        ANY SENDING METHOD GIVEN?                    
         BE    *+14                                                             
         CLC   EALMETS,EDIMETHS    YES -- MATCH ON SENDING METHOD?              
         BNE   LR20                NO -- SKIP THIS RECORD                       
*                                                                               
         MVC   SAVEKEY,EDIKEY      SAVE THIS KEY                                
         MVC   LISTAR,SPACES       CLEAR LIST LINE                              
*                                                                               
         MVC   LSTNAME,EDINAME     NAME                                         
         MVC   LSTMETHS,EDIMETHS   METHOD OF SENDING TRANSMISSIONS              
         MVC   LSTMETHR,EDIMETHR   METHOD OF RECEVING TRANSMISSIONS             
*                                                                               
         CLI   EDIMETHS,EDIEASYQ   SENDING METHOD IS EASYLINK?                  
         BNE   LR44                                                             
         OC    EDIADVNO,EDIADVNO   EASYLINK MAILBOX NUMBER                      
         BZ    *+18                                                             
         MVI   LSTAORR,C'A'                                                     
         MVC   LSTMBOX,EDIADVNO                                                 
         B     LR50                                                             
         OC    EDIREPNO,EDIREPNO                                                
         BZ    LR50                                                             
         MVI   LSTAORR,C'R'                                                     
         MVC   LSTMBOX,EDIREPNO                                                 
         B     LR50                                                             
*                                                                               
LR44     CLI   EDIMETHS,EDIECNQ    SENDING METHOD IS ECN?                       
         BNE   LR45                                                             
         MVC   LSTECNN,EDIECNN                                                  
*                                                                               
LR45     CLI   EDIMETHS,EDIEXRRQ   SENDING METHOD IS ER ?                       
         BNE   LR46                                                             
         MVC   LSTXRRN,EDIXRRN                                                  
*                                                                               
LR46     CLI   EDIMETHR,C'F'       RECEIVING METHOD IS FTP?                     
         BNE   LR47                                                             
         MVC   LSTFLUID,EDIFTPL                                                 
         B     LR50                                                             
*                                                                               
LR47     CLI   EDIMETHR,C'C'       RECEIVING METHOD IS COLUMBINE?               
         BNE   *+10                                                             
         MVC   LSTCLUID,EDICOLL                                                 
         DROP  R6                                                               
*                                                                               
LR50     MVC   LSTUNUM,=C'*****'   ASSUME NOT A DDS USER                        
         XC    KEY,KEY             BUILD ID RECORD KEY                          
         USING CTIREC,RF                                                        
         LA    RF,KEY                                                           
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKID,SPACES                                                    
         MVC   CTIKID(L'EDINAME),EDINAME                                        
         DROP  R4,RF                                                            
*                                                                               
         MVC   AIO,AIO2                                                         
         GOTO1 HIGH                LOOK FOR ID RECORD                           
         L     R6,AIO                                                           
         CLC   KEYSAVE(25),0(R6)                                                
         BNE   LR60                                                             
*                                                                               
         MVI   ELCODE,X'02'        FIND PASSIVE POINTER ELEMENT                 
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         EDIT  (B2,2(R6)),(5,LSTUNUM),ALIGN=LEFT NUMERIC USERID                 
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'30'        FIND DESTINATION DETAIL ELEMENT              
         BAS   RE,GETEL                                                         
         BNE   *+10                                                             
         MVC   LSTDEST,2(R6)       DESTINATION NAME                             
*                                                                               
LR60     MVC   AIO,AIO1            RESTORE AIO                                  
         XC    KEY,KEY                                                          
         MVC   KEY(25),SAVEKEY     RESTORE EDICT KEY                            
         GOTO1 HIGH                RE-READ THE EDICT RECORD                     
         CLC   KEYSAVE(25),0(R4)                                                
         BE    *+6                                                              
         DC    H'0'                WE FOUND IT BEFORE                           
*                                                                               
         GOTO1 LISTMON             SEND RECORD TO SCREEN                        
         B     LR20                NEXT RECORD                                  
*                                                                               
LRX      B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* PRINT REPORT                                                                  
***********************************************************************         
PR       EQU   *                                                                
         CLI   OFFLINE,C'Y'        OFFLINE? (ONLY ALLOW OFFLINE PRINT)          
         BNE   PRX                 NO                                           
*                                                                               
         LA    R1,HEDSPECS         SET UP HEADHOOK AND SPECS                    
         ST    R1,SPECS                                                         
         LA    R1,HOOK                                                          
         ST    R1,HEADHOOK                                                      
*                                                                               
         XC    LUPDATE,LUPDATE     DATE FILTER                                  
         CLI   ERLDATEH+5,0        START DATE?                                  
         BE    PR2                 NO                                           
*                                                                               
         ZIC   R2,ERLDATEH+5        INPUT LENGTH                                
         GOTO1 PERVAL,DMCB,((R2),ERLDATE),(X'40',PERBUF)                        
         TM    4(R1),X'01'+X'02'   VALID DATE?                                  
         BNZ   PR2                 NO                                           
*                                                                               
         MVC   LUPDATE(3),PERBUF+29                                             
*                                                                               
PR2      MVI   SORTFLAG,C'S'       SET FLAG FOR SENDING METHOD                  
         CLI   ERLSORTH+5,0        IS SORT METHOD GIVEN?                        
         BE    PR4                 NO                                           
         XC    TESTMETH,TESTMETH   USE TO TEST FOR BREAK ON METHOD              
         MVC   SORR,=C'SEND='      PAGE BREAK HEADING                           
*                                                                               
PR4      CLI   ERLSORT,C'S'        IS IT SORT BY SENDING METHOD?                
         BE    PR5                 YES                                          
         CLI   ERLSORT,C'B'        IS IT BOTH METHODS?                          
         BE    PR5                 YES                                          
*                                                                               
         MVI   SORTFLAG,C'R'       SET FLAG FOR RECEIVING METHOD                
         CLI   ERLSORTH+5,0        IS SORT METHOD GIVEN?                        
         BE    PR5                 NO                                           
*                                                                               
         MVC   SORR,=C'RECV='      PAGE BREAK HEADING                           
*                                                                               
PR5      XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING EDIKEYD,R4                                                       
*                                                                               
         CLI   ERLNAMEH+5,0        IS NAME GIVEN?                               
         BNE   *+10                YES                                          
         XC    ERLNAME,ERLNAME                                                  
*                                                                               
         MVI   EDIKSYS,EDIKSYSQ    SYSTEM                                       
         MVI   EDITYPE,EDITYPEQ    RECORD TYPE                                  
*                                                                               
         CLI   ERLNAME,C'='        IS IT NOT A START ADDR FILTER?               
         BNE   *+14                NO                                           
*                                                                               
         MVC   EDINAME,ERLNAME+1   YES, DROP C'=' IN NAME                       
         B     *+10                                                             
*                                                                               
         MVC   EDINAME,ERLNAME     START USERID                                 
*                                                                               
         OC    EDINAME,SPACES                                                   
*                                                                               
         GOTO1 SORTER,DMCB,SORTCARD,RECCARD  INITIATE OFFLINE SORTER            
         B     PR10                                                             
*                                                                               
SORTCARD DC    CL80'SORT FIELDS=(5,17,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=V,LENGTH=1050'                                  
*                                                                               
PR10     GOTO1 HIGH                FIRST RECORD                                 
         L     R4,AIO                                                           
         B     PR30                                                             
*                                                                               
PR20     GOTO1 SEQ                 NEXT RECORD                                  
*                                                                               
PR30     CLI   0(R4),EDIKSYSQ      EDICT RECORD?                                
         BNE   PR100                                                            
         CLI   1(R4),EDITYPEQ                                                   
         BNE   PR100               NO MORE RECORDS FOR REPORT                   
*                                                                               
         CLI   ERLNAMEH+5,0        IS NAME GIVEN?                               
         BE    PR40                NO                                           
         CLI   ERLNAME,C'='        IS IT NOT START ADDR?                        
         BNE   PR40                NO, ITS A START ADDR                         
*                                                                               
         ZIC   R5,ERLNAMEH+5       LENGTH OF INPUTTED NAME                      
         SH    R5,=H'2'            EXCLUDE LENGTH OF C'='                       
         EX    R5,*+8                                                           
         B     *+10                                                             
         CLC   ERLNAME+1(0),EDINAME  IS THERE A MATCH ON NAME?                  
         BNE   PR20                  NO                                         
*                                                                               
PR40     DS    0H                                                               
         OC    LUPDATE,LUPDATE     DATE FILTER PRESENT?                         
         BZ    PR45                NO                                           
*                                                                               
         LR    R6,R4               POINT TO EDICT RECORD                        
         MVI   ELCODE,X'F1'        ACTIVITY ELEMENT                             
         BAS   RE,GETEL                                                         
         BNE   PR20                NONE PRESENT SKIP IT                         
*                                                                               
         USING ACTVD,R6                                                         
         CLC   ACTVCHDT,LUPDATE    UPDATED BEFORE FILTER DATE?                  
         BL    PR20                YES - SKIP IT                                
*                                                                               
PR45     LR    R6,R4               POINT TO EDICT RECORD                        
         MVI   ELCODE,EDILNKEQ     EDICT ELEMENT                                
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                THIS ELEMENT IS REQUIRED                     
*                                                                               
         USING EDILNKD,R6                                                       
         CLI   ERLAORR,C'A'        LIST ADV EASYLINK ONLY?                      
         BNE   PR46                                                             
         OC    EDIADVNO,EDIADVNO   ANY ADV EASYLINK NUMBER?                     
         BZ    PR20                NO -- SKIP THIS RECORD                       
         B     PR50                                                             
*                                                                               
PR46     CLI   ERLAORR,C'R'        LIST REP EASYLINK ONLY?                      
         BNE   PR50                                                             
         OC    EDIREPNO,EDIREPNO   ANY REP EASYLINK NUMBER?                     
         BZ    PR20                NO -- SKIP THIS RECORD                       
*                                                                               
PR50     CLI   ERLSORT,C'E'        SORT BY EASYLINK NUMBER?                     
         BNE   PR55                                                             
         MVC   SORTELN,EDIADVNO                                                 
         OC    SORTELN,SORTELN                                                  
         BNZ   PR55                                                             
         MVC   SORTELN,EDIREPNO                                                 
*                                                                               
PR55     CLI   ERLMETRH+5,0        ANY RECEIVING METHOD GIVEN?                  
         BE    PR60                                                             
         CLC   ERLMETR,EDIMETHR    YES -- MATCH ON RECEIVING METHOD?            
         BNE   PR20                NO -- SKIP THIS RECORD                       
*                                                                               
PR60     CLI   ERLMETSH+5,0        ANY SENDING METHOD GIVEN?                    
         BE    PR70                                                             
         CLC   ERLMETS,EDIMETHS    YES -- MATCH ON SENDING METHOD?              
         BNE   PR20                NO -- SKIP THIS RECORD                       
*                                                                               
PR70     XC    SORTMETH,SORTMETH   ASSUME NO OPTIONAL SORT KEY                  
         CLI   SORTFLAG,C'S'       IS IT SORT BY SENDING METHOD?                
         BNE   PR80                NO                                           
*                                                                               
         MVC   SORTMETH,EDIMETHS   PRIMARY SORT KEY=SENDING METHOD              
         B     PR90                                                             
*                                                                               
PR80     CLI   ERLSORT,C'R'        IS IT SORT BY RECV METNOD?                   
         BE    PR85                YES                                          
         CLI   ERLSORT,C'B'        IS IT BOTH METHODS?                          
         BNE   PR90                NO OPTIONAL SORT KEY                         
*                                                                               
PR85     MVC   SORTMETH,EDIMETHR   PRIMARY SORT KEY=RECV METHOD                 
*                                                                               
PR90     MVC   SORTNAME,EDINAME    SECONDARY SORT KEY (DEFAULT)                 
*                                                                               
         LA    RF,SORTIO           A(RECORD) IN SORT RECORD                     
         L     R1,=F'1000'         LENGTH OF SORTIO                             
         L     RE,AIO              A(EDICT RECORD)                              
         MOVE  ((RF),(R1)),(RE)    MOVE 1000 BYTES                              
*                                                                               
         ZICM  R5,EDIRECLN,2       LENGTH OF RECORD TO BE SORTED                
*                                                                               
         LA    R5,L'SORTMETH+L'SORTNAME+L'SRTRECLN+2(R5)                        
*                                                                               
         STCM  R5,3,SRTRECLN               LENGTH OF SORT RECORD                
         XC    SRTRECLN+2(2),SRTRECLN+2    2 BYTES OF NULLS                     
         GOTO1 SORTER,DMCB,=C'PUT',SRTREC  SEND RECORD TO SORTER                
         B     PR20                        NEXT RECORD TO BE SORTED             
*                                                                               
PR100    GOTO1 SORTER,DMCB,=C'GET' RETRIEVE SORTED RECORDS                      
         LA    R5,1                INITIALIZE PRINT LINE COUNTER                
         OC    4(4,R1),4(R1)       ANYMORE RECORD?                              
         BZ    PR250               YES                                          
*                                                                               
         L     R4,4(R1)                                                         
         LA    R4,SORTIO-SRTREC(R4)  POINT TO IO(EDICT RECORD)                  
*                                                                               
         MVC   SVUNUM,=C'*****'    ASSUME NOT A DDS USER                        
         MVC   SVDEST,=CL33'*****'                                              
*                                                                               
         XC    KEY,KEY             BUILD ID RECORD KEY                          
         USING CTIREC,RF                                                        
         LA    RF,KEY                                                           
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKID,SPACES                                                    
         MVC   CTIKID(L'EDINAME),EDINAME                                        
         DROP  RF                                                               
*                                                                               
         MVC   AIO,AIO2                                                         
         GOTO1 HIGH                LOOK FOR ID RECORD                           
         L     R6,AIO                                                           
         CLC   KEYSAVE(25),0(R6)                                                
         BNE   PR110                                                            
*                                                                               
         MVI   ELCODE,X'02'        FIND PASSIVE POINTER ELEMENT                 
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         EDIT  (B2,2(R6)),(5,SVUNUM),ALIGN=LEFT NUMERIC USERID                  
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'30'        FIND DESTINATION DETAIL ELEMENT              
         BAS   RE,GETEL                                                         
         BNE   *+10                                                             
         MVC   SVDEST,2(R6)        DESTINATION NAME                             
*                                                                               
         MVC   AIO,AIO1            RESTORE AIO                                  
*                                                                               
PR110    LR    R6,R4               POINT TO EDICT RECORD                        
         USING EDILNKD,R6                                                       
         MVI   ELCODE,EDILNKEQ     EDICT ELEMENT                                
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                THIS ELEMENT IS REQUIRED                     
*                                                                               
         CLI   ERLSORTH+5,0        IS SORT METHOD GIVEN?                        
         BE    PR115               NO                                           
*                                                                               
         CLI   TESTMETH,0          IS IT FIRST TIME?                            
         BE    PR114               YES                                          
         CLI   SORTFLAG,C'S'       IS IT SORT ON SENDING METHOD?                
         BNE   PR111               NO                                           
         CLC   TESTMETH,EDIMETHS   IS THERE A BREAK ON SEND?                    
         BE    PR115               NO                                           
*                                                                               
         MVC   METHOD,EDIMETHS     PAGE BREAK HEADING                           
         B     PR112               YES                                          
*                                                                               
PR111    CLI   ERLSORTH,0          IS SORT METHOD GIVEN?                        
         BE    PR115               NO, SKIP BREAK ON TRANS METHOD               
         CLC   TESTMETH,EDIMETHR   IS THERE A BREAK ON RECEIVE?                 
         BE    PR115               NO                                           
*                                                                               
         MVC   METHOD,EDIMETHR     PAGE BREAK HEADING                           
*                                                                               
PR112    MVI   FORCEHED,C'Y'       YES                                          
*                                                                               
PR114    CLI   SORTFLAG,C'S'       IS IT SORT ON SEND?                          
         BNE   *+20                NO                                           
         MVC   TESTMETH,EDIMETHS   UPDATE SENDING METHOD                        
         MVC   METHOD,EDIMETHS     PAGE HEADING                                 
         B     PR115                                                            
*                                                                               
         MVC   TESTMETH,EDIMETHR   UPDATE RECEIVING METHOD                      
         MVC   METHOD,EDIMETHR     PAGE HEADING                                 
*                                                                               
PR115    MVI   ALLOWLIN,X'5'       KEEP ENTIRE RECORD ON SAME PAGE              
         MVC   SVMETHR,EDIMETHR    SAVE RECEIVING METHOD                        
*                                                                               
         MVC   PRNAME,EDINAME      NAME                                         
         MVC   PRMETHSH,=C'SEND:'  SEND HEADING                                 
         MVC   PRMETHS,EDIMETHS    METHOD OF SENDING TRANSMISSIONS              
*                                                                               
         CLI   EDIMETHS,EDIEASYQ   IS SEND METHOD EASYLINK?                     
         BE    PR120               YES                                          
         CLI   EDIMETHR,EDIEASYQ   IS RECV METHOD EASYLINK?                     
         BNE   PR130               NO                                           
*                                                                               
PR120    MVC   PREZLNKH,=C'EASYLINK:'                                           
*                                                                               
         MVC   PRADVNOH,=C'ADV#'                                                
         MVC   PRREPNOH,=C'REP#'                                                
         MVC   PRELACCH,=C'ACCT:'                                               
*                                                                               
         OC    EDIADVNO,EDIADVNO   EASYLINK MAILBOX NUMBER                      
         BZ    *+14                                                             
         MVI   PRADV,C'A'                                                       
         MVC   PRADVNO,EDIADVNO                                                 
*                                                                               
         OC    EDIREPNO,EDIREPNO                                                
         BZ    *+14                                                             
         MVI   PRREP,C'R'                                                       
         MVC   PRREPNO,EDIREPNO                                                 
*                                                                               
         OC    EDIEACC,EDIEACC                                                  
         BZ    *+10                                                             
         MVC   PRELACC,EDIEACC                                                  
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)     1ST PRINT LINE OF RECORD                     
         LA    R5,1(R5)            INCREMENT PRINT LINE COUNTER                 
         MVC   PRUNUM,SVUNUM       NUMERIC USER NUMBER                          
         MVC   PRMETHRH,=C'RECV:'                                               
         MVC   PRMETHR,EDIMETHR    RECEIVING METHOD                             
*                                                                               
PR130    CLI   EDIMETHS,EDIFTPQ    IS SEND METHOD FTP?                          
         BE    PR140               YES                                          
         CLI   EDIMETHR,EDIFTPQ    IS RECV METHOD FTP?                          
         BNE   PR150               NO                                           
*                                                                               
PR140    MVC   PRFTPH,=C'FTP:'                                                  
         MVC   PRFTPOH,=C'OPSYS'                                                
         MVC   PRFTPLH,=C'LUID'                                                 
         MVC   PRFTPUH,=C'USERID'                                               
         MVC   PRFTPPH,=C'PASSWORD'                                             
         MVC   PRFTPSH,=C'SRVR CLASS'                                           
*                                                                               
         MVC   PRFTPO,EDIFTPO      FTP OPSYS                                    
         MVC   PRFTPL,EDIFTPL      FTP REMOTE LU NAME                           
         MVC   PRFTPU,EDIFTPU      FTP APPC USERID                              
         MVC   PRFTPP,EDIFTPP      FTP APPC PASSWORD                            
         MVC   PRFTPS,EDIFTPS      FTP APPC SERVER CLASS                        
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
         LA    R5,1(R5)            INCREMENT PRINT LINE COUNTER                 
*                                                                               
         CH    R5,=H'2'            IS 2ND PRINT LINE NEXT?                      
         BNE   PR145               NO, 3RD PRINT LINE IS NEXT                   
*                                                                               
         MVC   PRUNUM,SVUNUM       NUMERIC USERID                               
         MVC   PRMETHRH,=C'RECV:'                                               
         MVC   PRMETHR,EDIMETHR    RECEIVING METHOD                             
         B     PR150                                                            
*                                                                               
PR145    MVC   PRDEST,SVDEST       DESTINATION NAME                             
*                                                                               
PR150    CLI   EDIMETHS,EDINJEQ    IS SEND METHOD NJE?                          
         BE    PR160               YES                                          
         CLI   EDIMETHR,EDINJEQ    IS RECV METHOD NJE?                          
         BNE   PR170               NO                                           
*                                                                               
PR160    MVC   PRNJEH,=C'NJE:'                                                  
         MVC   PRNJECH,=C'CLASS'                                                
         MVC   PRNJENH,=C'NODE'                                                 
         MVC   PRNJEUH,=C'USERID'                                               
*                                                                               
         MVC   PRNJEC,EDINJEC      NJE CLASS                                    
         MVC   PRNJEN,EDINJEN      NJE NODE                                     
         MVC   PRNJEU,EDINJEU      NJE USERID                                   
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
         LA    R5,1(R5)            INCREMENT PRINT LINE COUNTER                 
*                                                                               
         CH    R5,=H'2'            IS 2ND PRINT LINE NEXT?                      
         BNE   PR165               NO, 3RD PRINT LINE IS NEXT                   
*                                                                               
         MVC   PRUNUM,SVUNUM       NUMERIC USERID                               
         MVC   PRMETHRH,=C'RECV:'                                               
         MVC   PRMETHR,EDIMETHR    RECEIVING METHOD                             
         B     PR170                                                            
*                                                                               
PR165    MVC   PRDEST,SVDEST       DESTINATION NAME                             
*                                                                               
PR170    CH    R5,=H'1'            IS IT FIRST PRINT LINE?                      
         BNE   PR220               NO                                           
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)     1ST PRINT LINE                               
*                                                                               
         MVC   PRUNUM,SVUNUM       NUMERIC USERID                               
         MVC   PRMETHRH,=C'RECV:'                                               
         MVC   PRMETHR,SVMETHR     RECEIVING METHOD                             
         GOTO1 SPOOL,DMCB,(R8)     2ND PRINT LINE                               
*                                                                               
         MVC   PRDEST,SVDEST       DESTINATION NAME                             
         GOTO1 SPOOL,DMCB,(R8)     3RD PRINT LINE                               
         B     PR240                                                            
*                                                                               
PR220    CH    R5,=H'2'            IS IT 2ND PRINT LINE?                        
         BNE   PR230               NO                                           
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)     2ND PRINT LINE                               
*                                                                               
         MVC   PRDEST,SVDEST       DESTINATION NAME                             
         GOTO1 SPOOL,DMCB,(R8)     3RD PRINT LINE                               
         B     PR240                                                            
*                                                                               
PR230    CH    R5,=H'3'            IS IT 3RD PRINT LINE?                        
         BNE   PR240               NO                                           
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)     3RD PRINT LINE                               
*                                                                               
*              CODE TO PRINT THE DATE OF LAST MODIFICATION                      
*                   ASSUME 3 LINES PRINTED AT THIS POINT                        
PR240    DS    0H                                                               
         LR    R6,R4               POINT TO EDICT RECORD                        
         MVI   ELCODE,X'F1'        ACTIVITY ELEMENT                             
         BAS   RE,GETEL                                                         
         BNE   PR244               NONE PRESENT                                 
*                                                                               
         USING ACTVD,R6                                                         
         MVC   PCHDTHD,=C'LAST UPDATE: '                                        
         GOTO1 DATCON,DMCB,(3,ACTVCHDT),(11,PCHDATE)                            
         GOTO1 SPOOL,DMCB,(R8)                                                  
         DROP  R6                                                               
         B     PR248                                                            
*                                                                               
PR244    DS    0H                                                               
         MVC   PCHDTHD,=C'LAST UPDATE: '                                        
         MVC   PCHDATE(10),=C'NOT AVAIL.'                                       
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     PR248                                                            
*                                                                               
PR248    OC    ABOX,ABOX           TEST IF WE HAVE BOXES                        
         BZ    PR100               NO                                           
*                                                                               
         ZIC   R5,MAXLINES         MAX LINES ON PAGE                            
         ZIC   R1,ALLOWLIN                                                      
         SR    R5,R1                                                            
         SH    R5,=H'3'                                                         
         ZIC   R1,LINE             CURRENT LINE NUMBER                          
         CR    R1,R5               LOOK FOR LAST RECORD ON PAGE                 
         BH    PR100               SKIP LAST MID LINE ON PAGE                   
*                                                                               
         L     R5,ABOX             ADDRESS OF BOX DSECT                         
         USING BOXD,R5                                                          
         ZIC   R1,LINE             CURRENT LINE NUMBER                          
         LA    R1,BOXROWS-1(R1)    INDEX INTO BOXROW ARRAY                      
         MVI   0(R1),C'M'          DYNAMICALLY CHANGE CENTER LINE               
         MVI   BOXINIT,0                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         DROP  R5                                                               
*                                                                               
         B     PR100               NEXT RECORD                                  
*                                                                               
PR250    GOTO1 SORTER,DMCB,=C'END' TERMINATE SORTER                             
         CLI   SORTFLAG,C'S'       IS IT 1ST TIME THROUGH?                      
         BNE   PRX                 NO                                           
*                                                                               
         CLI   ERLSORT,C'R'        IS SORT BY RECV METHOD GIVEN?                
         BE    PR255               YES                                          
         CLI   ERLSORT,C'B'        IS IT BOTH METHODS?                          
         BE    PR255               YES                                          
*                                                                               
         CLI   ERLSORT,C'S'        WAS REPORT SORTED BY SEND METHOD?            
         BE    PRX                 YES, NOT DEFAULT SORT KEY                    
*                                                                               
PR255    MVI   SORTFLAG,C'R'       SET FLAG FOR 2ND TIME THROUGH                
         CLI   ERLSORTH+5,0        IS SORT METHOD GIVEN?                        
         BE    *+10                NO                                           
         MVC   SORR,=C'RECV='      PAGE BREAK HEADING                           
*                                                                               
         MVI   FORCEHED,C'Y'       EJECT A PAGE FOR 2ND REPORT                  
         B     PR5                                                              
*                                                                               
PRX      B     XIT                                                              
         EJECT                                                                  
HOOK     NTR1                                                                   
         MVC   H5(5),SORR          BREAK ON SEND OR RECEIVING                   
         MVC   H5+5(1),METHOD      TRANSMISSION METHOD                          
*                                                                               
         MVI   H3,0                SKIP A LINE                                  
         MVI   H6+1,0              SKIP A LINE                                  
         MVI   H8+1,0              SKIP A LINE                                  
         MVI   H11+1,0             SKIP A LINE                                  
         MVC   H9+5(23),=C'USERID/CONTROL ID#/NAME'                             
         MVC   H9+35(12),=C'TRANSMISSION'                                       
         MVC   H10+38(6),=C'METHOD'                                             
         MVC   H9+81(17),=C'TRANSMISSION DATA'                                  
*                                                                               
         OC    ABOX,ABOX           TEST IF WE HAVE BOXES                        
         BZ    HOOKX               NO                                           
*                                                                               
         L     R4,ABOX                                                          
         USING BOXD,R4                                                          
         MVC   BOXROWS,SPACES                                                   
         MVI   BOXROWS+6,C'T'      TOP OF BOX                                   
         MVI   BOXROWS+11,C'M'                                                  
         MVC   BOXCOLS,SPACES                                                   
         MVI   BOXCOLS,C'L'        LEFTMOST COLUMN                              
         MVI   BOXCOLS+34,C'C'     MIDDLE COLUMN                                
         MVI   BOXCOLS+47,C'C'     MIDDLE COLUMN                                
         MVI   BOXCOLS+131,C'R'    RIGHTMOST COLUMN                             
         MVI   BOXWT,1                                                          
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXINIT,0                                                        
         MVI   BOXOFF,0                                                         
         DROP  R4                                                               
*                                                                               
HOOKX    B     XIT                                                              
         EJECT                                                                  
*                                                                               
HEDSPECS SSPEC H1,1,REQUESTOR                                                   
         SSPEC H1,60,C'EDICT RECORD REPORT'                                     
         SSPEC H2,60,C'-------------------'                                     
         SSPEC H1,95,AGYNAME                                                    
         SSPEC H2,95,AGYADD                                                     
         SSPEC H4,95,RUN                                                        
         SSPEC H5,95,REPORT                                                     
         SSPEC H5,112,PAGE                                                      
         DC    X'00'                                                            
         EJECT                                                                  
VSFMISS  MVI   GERROR1,MISSING     MISSING INPUT                                
         J     VSFMERR                                                          
*                                                                               
VSFNOTF  MVI   GERROR1,NOTFOUND    NO DATA FOUND                                
         J     VSFMERR                                                          
*                                                                               
VSFIVAL  MVI   GERROR1,INVALID     INVALID INPUT                                
         J     VSFMERR                                                          
*                                                                               
VSFNOT#  MVI   GERROR1,NOTNUM      NOT NUMRIC                                   
         J     VSFMERR                                                          
*                                                                               
VSFNCHAR MVI   GERROR1,NOTALPHA                                                 
         J     VSFMERR                                                          
*                                                                               
VSFMERR  GOTO1 SFMERR                                                           
*                                  SHORT DESP OF ERROR MSGS                     
EMBOXMIS EQU   59        (NOT USED YET) MISSING AN EASYLINK MAILBOX #           
***********************************************************************         
         GETEL R6,DATADISP,ELCODE                                               
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE FACIDTAB                                                       
         EJECT                                                                  
       ++INCLUDE DDBIGBOX                                                       
         EJECT                                                                  
       ++INCLUDE DDACTIVD                                                       
         EJECT                                                                  
       ++INCLUDE DDFH                                                           
         EJECT                                                                  
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
       ++INCLUDE CTGENFILE                                                      
         EJECT                                                                  
       ++INCLUDE FAFACTS                                                        
         EJECT                                                                  
       ++INCLUDE FACIDTABD                                                      
         EJECT                                                                  
         PRINT ON                                                               
       ++INCLUDE CTGENEDICT                                                     
         EJECT                                                                  
       ++INCLUDE CTSFMFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE CTSFMD4D                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE CTSFMD5D                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE CTSFMD7D                                                       
         EJECT                                                                  
       ++INCLUDE CTSFMWORKD                                                     
         EJECT                                                                  
***********************************************************************         
* START OF SAVED STORAGE (6144)                                                 
***********************************************************************         
         ORG   SYSSPARE                                                         
RELO     DS    A                   RELOCATION FACTOR                            
TESTMETH DS    CL1                 TO TEST FOR BREAK ON TRANS METHOD            
METHOD   DS    CL1                 PAGE HEADING                                 
LUPDATE  DS    CL11                BUFFER FOR DAT OF LAST UPDATE                
PERBUF   DS    CL60                OUTPUT AREA FOR PERVAL                       
SORR     DS    CL5                 SEND OR RECEIVE PAGE HEADING                 
SAVEKEY  DS    XL25                CTFILE KEY                                   
SVUNUM   DS    CL5                 SAVE NUMERIC USERID                          
SVDEST   DS    CL33                SAVE DESTINATION NAME                        
SVMETHR  DS    CL1                 SAVE METHOD OF TRANSMISSION                  
DDSUSER  DS    C                   'Y' OR 'N' -- DDS USER                       
SORTFLAG DS    C                   FLAG FOR GENERATING 2 REPORTS                
OLDMETHS DS    C                   SAVED METHOD OF SENDING                      
OLDADVN  DS    CL(L'EDIADVNO)      SAVED ADV EASYLINK#                          
OLDREPN  DS    CL(L'EDIREPNO)      SAVED REP EASYLINK#                          
OLDACCN  DS    CL(L'EDIEACC)       SAVED EASYLINK ACCOUNT#                      
OLDECNN  DS    CL(L'EDIECNN)       SAVED ECN ACCOUNT#                           
OLDXRRN  DS    CL(L'EDIXRRN)       SAVED ER  ACCOUNT#                           
OLDLNQ   EQU   *-OLDMETHS                                                       
*                                                                               
SRTREC   DS    0X                  SORT RECORD                                  
SRTRECLN DS    XL2                 LENGTH OF SORT RECORD                        
         DS    XL2                 2 BYTES OF NULLS                             
SRTKEY   DS    0X                  SORT KEYS...                                 
SORTMETH DS    C                      METHOD OF TRANSMISSION                    
SORTELN  DS    CL8                 EASYLINK#                                    
SORTNAME DS    CL8                    NAME                                      
SORTIO   DS    1000X               I/O AREA                                     
                                                                                
***********************************************************************         
* ONLINE LIST LINE                                                              
***********************************************************************         
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
LSTNAME  DS    CL8                 ALPHA USERID                                 
         DS    C                                                                
LSTMETHS DS    C                   METHOD OF SENDING TRANSMISSIONS              
         DS    C                                                                
LSTMETHR DS    C                   METHOD OF RECEIVING TRANSMISSIONS            
         DS    C                                                                
LSTUNUM  DS    CL5                 NUMERIC USERID                               
         DS    C                                                                
LSTDEST  DS    CL33                DESTINATION NAME                             
         DS    C                                                                
LSTPARMS DS    CL20                                                             
         ORG   LSTPARMS                                                         
LSTAORR  DS    C                   'A' OR 'R' FOR ADV/REP MAILBOX #             
LSTMBOX  DS    CL8                 EASYLINK MAILBOX NUMBER                      
         ORG   LSTPARMS                                                         
LSTFLUID DS    CL8                 FTP LUID                                     
         ORG   LSTPARMS                                                         
LSTCLUID DS    CL8                 COLUMBINE LUID                               
         ORG   LSTPARMS                                                         
LSTECNN  DS    CL8                 ECN ID NUMBER                                
         ORG   LSTPARMS                                                         
LSTXRRN  DS    CL8                 ER  ID NUMBER                                
         EJECT                                                                  
***********************************************************************         
* PRINT LINE                                                                    
***********************************************************************         
SPOOLD   DSECT                                                                  
         ORG   P                                                                
         DS    C                                                                
PRNAME   DS    CL8                 ALPHA USERID                                 
         DS    CL26                                                             
PRMETHSH DS    CL5                 'SEND:'                                      
         DS    C                                                                
PRMETHS  DS    C                   METHOD OF SENDING TRANSMISSIONS              
         DS    CL6                                                              
PREZLNKH DS    CL9                 'EASYLINK:'                                  
         DS    CL6                                                              
PRADVNOH DS    CL4                 'ADV#'                                       
         DS    C                                                                
PRADV    DS    C                   'A' OR 'R' FOR ADV/REP MAILBOX #             
PRADVNO  DS    CL8                 ADV EASYLINK MAILBOX NUMBER                  
         DS    CL3                                                              
PRREPNOH DS    CL4                 'REP# '                                      
         DS    C                                                                
PRREP    DS    C                   'A' OR 'R' FOR ADV/REP MAILBOX #             
PRREPNO  DS    CL8                 REP EASYLINK MAILBOX NUMBER                  
         DS    CL3                                                              
PRELACCH DS    CL5                 'ACCT:'                                      
         DS    C                                                                
PRELACC  DS    CL10                                                             
         DS    CL30                                                             
*                                                                               
         ORG   P                                                                
         DS    C                                                                
PRUNUM   DS    CL5                 NUMERIC USERID                               
         DS    CL29                                                             
PRMETHRH DS    CL5                 'RECV:'                                      
         DS    C                                                                
PRMETHR  DS    C                   METHOD OF RECEIVING TRANSMISSIONS            
         DS    CL6                                                              
PRFTPH   DS    CL4                 'FTP:'                                       
         DS    CL2                                                              
PRFTPOH  DS    CL5                 'OPSYS'                                      
         DS    C                                                                
PRFTPO   DS    C                   FTP OPERATING SYSTEM                         
         DS    CL2                                                              
PRFTPLH  DS    CL4                 'LUID'                                       
         DS    C                                                                
PRFTPL   DS    CL8                 FTP REMOTE LU NAME                           
         DS    CL2                                                              
PRFTPUH  DS    CL6                 'USERID'                                     
         DS    C                                                                
PRFTPU   DS    CL10                FTP APPC USERID                              
         DS    CL2                                                              
PRFTPPH  DS    CL8                 'PASSWORD'                                   
         DS    C                                                                
PRFTPP   DS    CL10                FTP APPC PASSWORD                            
         DS    CL2                                                              
PRFTPSH  DS    CL10                'SRVR CLASS'                                 
         DS    C                                                                
PRFTPS   DS    C                   FTP APPC SERVER CLASS                        
*                                                                               
         ORG   P                                                                
         DS    C                                                                
PRDEST   DS    CL33                DESTINATION NAME                             
         DS    CL14                                                             
PRNJEH   DS    CL4                 'NJE:'                                       
         DS    CL2                                                              
PRNJECH  DS    CL5                 'CLASS'                                      
         DS    C                                                                
PRNJEC   DS    C                   NJE CLASS                                    
         DS    CL2                                                              
PRNJENH  DS    CL4                 'NJE NODE'                                   
         DS    C                                                                
PRNJEN   DS    CL8                 NJE NODE                                     
         DS    CL2                                                              
PRNJEUH  DS    CL6                 'USERID'                                     
         DS    C                                                                
PRNJEU   DS    CL8                 NJE USERID                                   
         DS    CL39                                                             
*                                                                               
         ORG   P                                                                
         DS    C                                                                
PCHDTHD  DS    CL13                'DATE OF LAST CHANGE: '                      
PCHDATE  DS    CL10                LAST CHANGE DATE                             
         DS    CL100                                                            
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'152CTSFM1B   08/26/19'                                      
         END                                                                    
