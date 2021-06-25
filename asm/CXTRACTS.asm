*          DATA SET CXTRACTS   AT LEVEL 086 AS OF 05/01/02                      
*PHASE CXTRACTA                                                                 
*INCLUDE DMUTLCT                                                                
*INCLUDE DMDMGRL                                                                
*INCLUDE CXSAGC                                                                 
*INCLUDE CXSUIC                                                                 
*INCLUDE CXSPUC                                                                 
*INCLUDE CXSUPC                                                                 
*INCLUDE CXSPPC                                                                 
*INCLUDE CXSPEC                                                                 
*INCLUDE CXSAAC                                                                 
*INCLUDE CXSALC                                                                 
*INCLUDE CXSFAC                                                                 
*INCLUDE CXSFGC                                                                 
*INCLUDE CXSOAC                                                                 
*INCLUDE CXSOGC                                                                 
*INCLUDE CXOLSC                                                                 
*INCLUDE CXOLEC                                                                 
*INCLUDE CXAPNC                                                                 
*INCLUDE CXAPMC                                                                 
*INCLUDE CXPAMC                                                                 
*INCLUDE CXSPWC                                                                 
*INCLUDE CXSAGX                                                                 
*INCLUDE CXSUIX                                                                 
*INCLUDE CXSPPX                                                                 
*INCLUDE CXSUPX                                                                 
*INCLUDE CXSPUX                                                                 
*INCLUDE CXSPEX                                                                 
*INCLUDE CXSAAX                                                                 
*INCLUDE CXSALX                                                                 
*INCLUDE CXSFAX                                                                 
*INCLUDE CXSFGX                                                                 
*INCLUDE CXSOAX                                                                 
*INCLUDE CXSOGX                                                                 
*INCLUDE CXOLSX                                                                 
*INCLUDE CXOLEX                                                                 
*INCLUDE CXAPNX                                                                 
*INCLUDE CXAPMX                                                                 
*INCLUDE CXPAMX                                                                 
*INCLUDE MXCNVX                                                                 
*INCLUDE DICTATE                                                                
*INCLUDE FAGETTXT                                                               
*INCLUDE DATCON                                                                 
*INCLUDE HEXOUT                                                                 
*INCLUDE GETIDS                                                                 
*INCLUDE CRYPT                                                                  
*                                                                               
         TITLE 'CXTRACT - EXTRACT CONTROL SYSTEM FILE DATA'                     
**********************************************************                      
*                                                        *                      
* CONTROL SYSTEM EXTRACT CONTROL MODULE                  *                      
*                                                        *                      
* CONTROL IS PASSED FROM DXTRACT WITH PARAMETERS:        *                      
* R1=A(EXTRACT CONTROL DATA BLOCK - SEE DSECT DXBLOCKD)  *                      
*                                                        *                      
* MODULE ENTERED WITH ONE OF FOLLOWING MODES IN DXMODE:  *                      
*   DXOPENQ  - OPEN SYSTEM FILES                         *                      
*   DXCLOSEQ - CLOSE SYSTEM FILES                        *                      
*   DXLOADQ  - EXTRACT FROM FILES IN LOAD MODE           *                      
*   DXUPDTQ  - EXTRACT FROM FILES IN UPDATE MODE         *                      
*                                                        *                      
* FOR DXLOADQ AND DXUPDTQ MODES,                         *                      
* DXBLOCKD CONTAINS DXSTPTR WHICH IS:                    *                      
* A(CURRENT ENTRY IN SUB SYSTEM EXTRACT DRIVER TABLE -   *                      
*     SEE DSECT SYSTABD)                                 *                      
*                                                        *                      
*                                                        *                      
* RETURN CC .NE. IF ERROR CONDITION ELSE CC .EQ. FOR OK  *                      
*                                                        *                      
**********************************************************                      
         SPACE 1                                                                
CXTRACT  CSECT                                                                  
         ENTRY CXSSPAT                                                          
         ENTRY CXSAGUT                                                          
         PRINT NOGEN                                                            
         NMOD1 WORKX-WORKD,**CXTRA*,RA,R9,R8,RR=RE                              
*                                                                               
         ENTRY SSB                 FOR DATAMGR                                  
*                                                                               
         USING WORKD,RC            RC=A(GLOBAL W/S)                             
         ST    RE,RELO                                                          
         L     R7,0(R1)            R7=A(EXTRACT CONTROL DATA BLOCK)             
         USING DXBLOCKD,R7                                                      
         L     RF,DXSTPTR          RF=A(EXTRACT SYSTEM TABLE ENTRY)             
         USING SXDTABD,RF                                                       
         MVC   AGENCYID,SXDTAGY                                                 
         MVC   AGYBIN,SXDTAGB                                                   
         MVC   TYPESAVE,SXDTTYP                                                 
         MVC   PLATFORM,SXDTPLFM                                                
         DROP  RF                                                               
         B     MAIN                                                             
         EJECT                                                                  
***********************************************************************         
* MAIN CONTROL CODE                                                   *         
***********************************************************************         
         SPACE 1                                                                
MAIN     EQU   *                                                                
         BAS   RE,GENINIT          GENERAL INITIALISATION                       
*                                                                               
MOPEN    CLI   DXMODE,DXOPENQ                                                   
         BNE   MCLOSE                                                           
         BAS   RE,PROCOPEN         OPEN SYSTEM FILES                            
         BNE   MERR                  EXIT IF ERROR                              
         B     MXIT                                                             
*                                                                               
MCLOSE   CLI   DXMODE,DXCLOSEQ                                                  
         BNE   MLOAD                                                            
         BAS   RE,PROCCLOS         CLOSE SYSTEM FILES                           
         BNE   MERR                  EXIT IF ERROR                              
         B     MXIT                                                             
*                                                                               
MLOAD    CLI   DXMODE,DXLOADQ                                                   
         BNE   MUPDT                                                            
         BAS   RE,PROCLOAD         EXTRACT FROM FILES IN LOAD MODE              
         BNE   MERR                  EXIT IF ERROR                              
         B     MXIT                                                             
*                                                                               
MUPDT    CLI   DXMODE,DXUPDTQ                                                   
         BNE   MERR                                                             
         BAS   RE,PROCUPDT         EXTRACT FROM FILES IN UPDATE MODE            
         BNE   MERR                  EXIT IF ERROR                              
         B     MXIT                                                             
*                                                                               
MERR     MVI   RETCODE,0                                                        
         B     MXIT                                                             
*                                                                               
MXIT     XMOD1 1                                                                
         EJECT                                                                  
***********************************************************************         
* GENERAL INITIALISATION                                              *         
***********************************************************************         
         SPACE 1                                                                
GENINIT  NTR1  ,                                                                
         L     R4,DXSTPTR          RF=A(EXTRACT SYSTEM TABLE ENTRY)             
         USING SXDTABD,R4                                                       
         XC    AENCKEY,AENCKEY                                                  
         CLI   SXDTLEV,1           LEVEL 1                                      
         BH    GINI010                                                          
         CLI   SXDTVER,0           VERSION 0                                    
         BNH   GINI020                                                          
*                                  TEST FOR PASSWORD ENCRYPTION                 
GINI010  CLC   SXDTEKEY,SPACES                                                  
         BE    GINI020                                                          
         LA    RF,SXDTEKEY                                                      
         STCM  RF,15,AENCKEY                                                    
*                                                                               
GINI020  CLI   SXDTSUB,X'01'                                                    
         BE    GINI030                                                          
         CLI   SXDTSUB,X'02'                                                    
         BE    GINI040                                                          
         CLI   SXDTSUB,X'03'                                                    
         BE    GINI050                                                          
         CLI   SXDTSUB,X'04'                                                    
         BE    GINI060                                                          
         CLI   SXDTSUB,X'05'                                                    
         BE    GINI060                                                          
         DC    H'0'                                                             
*                                                                               
GINI030  LA    RE,SECTAB                                                        
         STCM  RE,15,ASUBTAB                                                    
         B     GINI100                                                          
GINI040  LA    RE,SECTAB                                                        
         STCM  RE,15,ASUBTAB                                                    
         B     GINI100                                                          
GINI050  LA    RE,PRETAB                                                        
         STCM  RE,15,ASUBTAB                                                    
         B     GINI100                                                          
GINI060  LA    RE,TEMTAB                                                        
         STCM  RE,15,ASUBTAB                                                    
         B     GINI100                                                          
GINI070  LA    RE,SECTAB                                                        
         STCM  RE,15,ASUBTAB                                                    
         B     GINI100                                                          
*                                                                               
GINI100  EQU   *                                                                
         XIT1                                                                   
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* DXMODE = DXOPENQ - OPEN SYSTEM FILES AND INITIALISE TABLES          *         
***********************************************************************         
         SPACE 1                                                                
PROCOPEN NTR1  ,                                                                
*                                  SET UTL SENUM                                
         L     RE,=V(UTL)                                                       
         MVC   4(1,RE),DXSENUM                                                  
*                                  OPEN SYSTEM DISK FILES                       
         GOTO1 VDATAMGR,DMCB,DMOPEN,CONTROL,CTFILES,IOL                         
*                                  INITIALISE MASTER SECURITY AGENCY            
         BAS   RE,INSAGTAB           ALPHA ID TABLE                             
*                                  INITIALISE SYSTEM PROGRAM ACCESS             
         BAS   RE,INSPATAB           TABLE FROM SECURITY RECORDS                
*                                                                               
         BAS   RE,INAGUTAB         INITIALISE AGENCY USERID ACCESS TBL          
*                                                                               
         B     POPEOK                                                           
*                                                                               
POPENO   B     NO                                                               
POPEOK   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* DXMODE = DXCLOSEQ - CLOSE SYSTEM FILES                             *          
***********************************************************************         
         SPACE 1                                                                
PROCCLOS NTR1  ,                                                                
*                                  SET UTL SENUM                                
         L     RE,=V(UTL)                                                       
         MVC   4(1,RE),DXSENUM                                                  
*                                                                               
         GOTO1 VDATAMGR,DMCB,DMCLSE,=C'FILES',,IOL                              
         B     PCLOOK                                                           
*                                                                               
PCLONO   B     NO                                                               
PCLOOK   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* PROCESS CONTROL FILE SECURITY FILE DATA IN LOAD MODE                *         
***********************************************************************         
         SPACE 1                                                                
PROCLOAD NTR1  ,                                                                
         MVC   TYPECODE,TYPESAVE                                                
         BAS   RE,GETTYP           SET UP RECORD TYPE TABLE DATA                
         L     RF,TYPEALOD         CALL EXTRACT LOAD ROUTINE                    
         A     RF,RELO                                                          
         BASR  RE,RF                                                            
         BNE   PLODNO              ERROR EXIT                                   
         B     PLODOK              EXIT OK                                      
*                                                                               
PLODNO   B     NO                                                               
PLODOK   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* LOAD ALL SQL SECURITY SUB SYSTEM RECORD DATA                        *         
***********************************************************************         
         SPACE 1                                                                
LOADSEC  NTR1                                                                   
         BAS   RE,LOADSAG          AGENCY                                       
         BNE   LSECNO                                                           
         BAS   RE,LOADSUI          USER ID                                      
         BNE   LSECNO                                                           
         BAS   RE,LOADSPE          PERSON PASSWORD                              
         BNE   LSECNO                                                           
         BAS   RE,LOADSUP          USERID PROGRAM                               
         BNE   LSECNO                                                           
         BAS   RE,LOADSPP          PERSON PROGRAM                               
         BNE   LSECNO                                                           
         BAS   RE,LOADSPU          PERSON USER ID XREF                          
         BNE   LSECNO                                                           
         BAS   RE,LOADSAA          AGENCY AUTHORISATION LEVELS                  
         BNE   LSECNO                                                           
         BAS   RE,LOADSAL          ACCESS GROUP AUTHORISATION LEVELS            
         BNE   LSECNO                                                           
         BAS   RE,LOADSFA          FIELD CONTROLS AT AGENCY LEVEL               
         BNE   LSECNO                                                           
         BAS   RE,LOADSFG          FIELD CONTROLS AT GROUP LEVEL                
         BNE   LSECNO                                                           
         BAS   RE,LOADSOA          OPTION CONTROLS AT AGENCY LEVEL              
         BNE   LSECNO                                                           
         BAS   RE,LOADSOG          OPTION CONTROLS AT GROUP LEVEL               
         BNE   LSECNO                                                           
         B     LSECOK                                                           
*                                                                               
LSECNO   B     NO                                                               
LSECOK   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* LOAD ALL TEMPO SUB SYSTEM RECORD DATA                               *         
***********************************************************************         
         SPACE 1                                                                
LOADTEM  NTR1                                                                   
         BAS   RE,LOADOLS          OFFICE LIST                                  
         BNE   LTEMNO                                                           
         BAS   RE,LOADOLE          OFFICE LIST ENTRY                            
         BNE   LTEMNO                                                           
         BAS   RE,LOADAPN          APROVER GROUP NAME                           
         BNE   LTEMNO                                                           
         BAS   RE,LOADAPM          APPROVER GROUP MANAGER                       
         BNE   LTEMNO                                                           
         BAS   RE,LOADPAM          PERSON APPROVER MANAGER                      
         BNE   LTEMNO                                                           
         B     LTEMOK                                                           
*                                                                               
LTEMNO   B     NO                                                               
LTEMOK   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* LOAD PRESTO SUB SYSTEM RECORD DATA                                  *         
***********************************************************************         
         SPACE 1                                                                
LOADPRE  NTR1                                                                   
         BAS   RE,LOADOLS          OFFICE LIST                                  
         BNE   LPRENO                                                           
         BAS   RE,LOADOLE          OFFICE LIST ENTRY                            
         BNE   LPRENO                                                           
         B     LPREOK                                                           
*                                                                               
LPRENO   B     NO                                                               
LPREOK   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* LOAD AGENCY RECORD DATA                                             *         
***********************************************************************         
         SPACE 1                                                                
LOADSAG  NTR1                                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST AGENCY RECORD          
         USING CT5REC,R2                                                        
         XC    CT5KEY,CT5KEY                                                    
         MVI   CT5KTYP,CT5KTYPQ                                                 
         L     R2,DXARECB                                                       
         GOTO1 VDATAMGR,DMCB,(X'00',DMRDHI),CTFILE,IOKEY,(R2)                   
*                                                                               
LSAG010  TM    8(R1),X'80'         ALL DONE IF EOF                              
         BO    LSAGOK                                                           
         TM    8(R1),X'7D'         DIE IF DISK ERROR                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                  ALL DONE IF RECORD TYPE CHANGES              
         CLI   CT5KTYP,CT5KTYPQ                                                 
         BNE   LSAGOK                                                           
         GOTO1 CHKSAGY,CT5KALPH    CHECK IN SECURITY AGENCY TABLE               
         BE    LSAG020                                                          
*                                                                               
LSAG014  MVC   IOKEY(L'CT5KEY),0(R2)                                            
         GOTO1 VDATAMGR,DMCB,(X'00',DMRSEQ),CTFILE,IOKEY,(R2)                   
         B     LSAG010                                                          
*                                                                               
LSAG020  EQU   *                                                                
         GOTO1 CXLOAD,DMCB,VCXSAGC,VCXSAGX,INITSAG,0                            
         BNE   LSAGNO                                                           
         OC    MAXIOS,MAXIOS       EXIT IF MAXIOS EXCEEDED                      
         BZ    LSAGOK                                                           
         B     LSAG010                                                          
*                                                                               
LSAGNO   B     NO                                                               
LSAGOK   B     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LOAD USER ID RECORD DATA                                            *         
***********************************************************************         
         SPACE 1                                                                
LOADSUI  NTR1                                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST USER ID RECORD         
         USING CTIREC,R2                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKNUM,=XL2'FFFF'                                               
         L     R2,DXARECB                                                       
         GOTO1 VDATAMGR,DMCB,(X'00',DMRDHI),CTFILE,IOKEY,(R2)                   
*                                                                               
LSUI010  TM    8(R1),X'80'         ALL DONE IF EOF                              
         BO    LSUIOK                                                           
         TM    8(R1),X'7D'         DIE IF DISK ERROR                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                  ALL DONE IF RECORD TYPE CHANGES              
         CLI   CTIKTYP,CTIKTYPQ                                                 
         BNE   LSUIOK                                                           
         OC    CTIKID(8),CTIKID                                                 
         BNZ   *+6                                                              
         DC    H'00'                                                            
*                                                                               
         BAS   RE,GETUAGY                                                       
         BNE   LSUI014                                                          
         GOTO1 CHKSAGY,USERAGY                                                  
         BE    LSUI020                                                          
         B     LSUI014                                                          
*                                                                               
LSUI014  MVC   IOKEY(L'CTIKEY),0(R2)                                            
         GOTO1 VDATAMGR,DMCB,(X'00',DMRSEQ),CTFILE,IOKEY,(R2)                   
         B     LSUI010                                                          
*                                                                               
LSUI020  EQU   *                                                                
         GOTO1 CXLOAD,DMCB,VCXSUIC,VCXSUIX,INITSUI,0                            
         BNE   LSUINO                                                           
         OC    MAXIOS,MAXIOS       EXIT IF MAXIOS EXCEEDED                      
         BZ    LSUIOK                                                           
         B     LSUI010                                                          
*                                                                               
LSUINO   B     NO                                                               
LSUIOK   B     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LOAD PERSON PASSWORD RECORD DATA                                    *         
***********************************************************************         
         SPACE 1                                                                
LOADSPE  NTR1                                                                   
         MVC   MAXIOS,DXMAXREC                                                  
         XC    PIDSAVE,PIDSAVE                                                  
*                                  GET TODAYS DATE                              
         MVC   TODAYC,FFILL                                                     
         XC    TODAYC,DXDATENC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST PERSON RECORD          
         USING SAPEREC,R2                                                       
         XC    SAPEKEY,SAPEKEY                                                  
         MVI   SAPETYP,SAPETYPQ                                                 
         MVI   SAPESUB,SAPESUBQ                                                 
         MVC   SAPEAGY,AGENCYID                                                 
*                                                                               
         L     R2,DXARECB                                                       
         GOTO1 VDATAMGR,DMCB,(X'00',DMRDHI),CTFILE,IOKEY,(R2)                   
*                                                                               
LSPE010  TM    8(R1),X'80'         ALL DONE IF EOF                              
         BO    LSPEOK                                                           
         TM    8(R1),X'7D'         DIE IF DISK ERROR                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                  ALL DONE IF RECORD TYPE CHANGES              
         CLC   SAPEREC(SAPEPID-SAPEKEY),IOKEY                                   
         BNE   LSPEOK                                                           
*                                                                               
         CLC   SAPEPID,PIDSAVE                                                  
         BNE   LSPE012                                                          
         MVC   IOKEY(L'SAPEKEY),0(R2)                                           
         GOTO1 VDATAMGR,DMCB,(X'00',DMRSEQ),CTFILE,IOKEY,(R2)                   
         B     LSPE010                                                          
*                                                                               
LSPE012  MVC   PIDSAVE,SAPEPID                                                  
         CLC   SAPEDEF,TODAYC                                                   
         BNL   LSPE020                                                          
         LA    R2,IOKEY                                                         
         MVC   SAPEPID,PIDSAVE                                                  
         MVC   SAPEDEF,TODAYC                                                   
*                                                                               
         L     R2,DXARECB                                                       
         GOTO1 VDATAMGR,DMCB,(X'00',DMRDHI),CTFILE,IOKEY,(R2)                   
*                                                                               
LSPE020  EQU   *                                                                
         GOTO1 CXLOAD,DMCB,VCXSPEC,VCXSPEX,INITSPE,0                            
         BNE   LSPENO                                                           
         OC    MAXIOS,MAXIOS       EXIT IF MAXIOS EXCEEDED                      
         BZ    LSPEOK                                                           
         B     LSPE010                                                          
*                                                                               
LSPENO   B     NO                                                               
LSPEOK   B     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LOAD USER ID PROGRAM RECORD DATA                                    *         
***********************************************************************         
         SPACE 1                                                                
LOADSUP  NTR1                                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST USER ID RECORD         
         USING CTIREC,R2                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKNUM,=XL2'FFFF'                                               
         L     R2,DXARECB                                                       
         GOTO1 VDATAMGR,DMCB,(X'00',DMRDHI),CTFILE,IOKEY,(R2)                   
*                                                                               
LSUP010  TM    8(R1),X'80'         ALL DONE IF EOF                              
         BO    LSUPOK                                                           
         TM    8(R1),X'7D'         DIE IF DISK ERROR                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                  ALL DONE IF RECORD TYPE CHANGES              
         CLI   CTIKTYP,CTIKTYPQ                                                 
         BNE   LSUPOK                                                           
         OC    CTIKID(8),CTIKID                                                 
         BNZ   *+6                                                              
         DC    H'00'                                                            
*                                  READ NEW SECURITY/UNLOCKED ONLY              
         BAS   RE,GETUAGY                                                       
         BNE   LSUP014                                                          
         GOTO1 CHKSAGY,USERAGY                                                  
         BE    LSUP020                                                          
         B     LSUP014                                                          
*                                                                               
LSUP014  MVC   IOKEY(L'CTIKEY),0(R2)                                            
         GOTO1 VDATAMGR,DMCB,(X'00',DMRSEQ),CTFILE,IOKEY,(R2)                   
         B     LSUP010                                                          
*                                                                               
LSUP020  EQU   *                                                                
         GOTO1 CXLOAD,DMCB,VCXSUPC,VCXSUPX,INITSUP,0                            
         BNE   LSUPNO                                                           
         OC    MAXIOS,MAXIOS       EXIT IF MAXIOS EXCEEDED                      
         BZ    LSUPOK                                                           
         B     LSUP010                                                          
*                                                                               
LSUPNO   B     NO                                                               
LSUPOK   B     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LOAD PERSON PROGRAM RECORD DATA                                     *         
***********************************************************************         
         SPACE 1                                                                
LOADSPP  NTR1                                                                   
         MVC   MAXIOS,DXMAXREC                                                  
         LA    R2,IOKEY            SET KEY TO READ FIRST                        
         USING SA0REC,R2             PASSWORD NUMBER RECORD                     
         XC    SA0KEY,SA0KEY                                                    
         MVI   SA0KTYP,SA0KTYPQ                                                 
         MVC   SA0KAGY,AGENCYID                                                 
         L     R2,DXARECB                                                       
         GOTO1 VDATAMGR,DMCB,(X'00',DMRDHI),CTFILE,IOKEY,(R2)                   
*                                                                               
LSPP010  TM    8(R1),X'80'         ALL DONE IF EOF                              
         BO    LSPPOK                                                           
         TM    8(R1),X'7D'         DIE IF DISK ERROR                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                  ALL DONE IF RECORD TYPE CHANGES              
         CLC   SA0KEY(SA0KEYS-SA0KEY),IOKEY                                     
         BNE   LSPPOK                                                           
*                                    OR IF PASSWORD CODE PASSIVES               
         OC    SA0KCODE(SA0KNUM-SA0KCODE),SA0KCODE                              
         BNZ   LSPPOK                                                           
*                                  READ NEW SECURITY/UNLOCKED ONLY              
         CLI   SA0STAT,X'20'                                                    
         BE    LSPP012                                                          
         CLI   SA0STAT,X'40'                                                    
         BNE   LSPP012                                                          
         B     LSPP020                                                          
*                                                                               
LSPP012  MVC   IOKEY(L'SA0KEY),0(R2)                                            
         GOTO1 VDATAMGR,DMCB,(X'00',DMRSEQ),CTFILE,IOKEY,(R2)                   
         B     LSPP010                                                          
*                                                                               
LSPP020  EQU   *                                                                
         GOTO1 CXLOAD,DMCB,VCXSPPC,VCXSPPX,INITSPP,0                            
         BNE   LSPPNO                                                           
         OC    MAXIOS,MAXIOS       EXIT IF MAXIOS EXCEEDED                      
         BZ    LSPPOK                                                           
         B     LSPP010                                                          
*                                                                               
LSPPNO   B     NO                                                               
LSPPOK   B     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LOAD PERSON USERID XREF RECORDS                                     *         
***********************************************************************         
         SPACE 1                                                                
LOADSPU  NTR1                                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST PERSON                 
         USING SA0REC,R2             PASSWORD NUMBER RECORD                     
         XC    SA0KEY,SA0KEY                                                    
         MVI   SA0KTYP,SA0KTYPQ                                                 
         MVC   SA0KAGY,AGENCYID                                                 
         L     R2,DXARECB                                                       
         GOTO1 VDATAMGR,DMCB,(X'00',DMRDHI),CTFILE,IOKEY,(R2)                   
*                                                                               
LSPU010  TM    8(R1),X'80'         ALL DONE IF EOF                              
         BO    LSPUOK                                                           
         TM    8(R1),X'7D'         DIE IF DISK ERROR                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                  ALL DONE IF RECORD TYPE CHANGES              
         CLC   SA0KEY(SA0KEYS-SA0KEY),IOKEY                                     
         BNE   LSPUOK                                                           
*                                    OR IF PASSWORD CODE PASSIVES               
         OC    SA0KCODE(SA0KNUM-SA0KCODE),SA0KCODE                              
         BNZ   LSPUOK                                                           
*                                  READ NEW SECURITY/UNLOCKED ONLY              
         CLI   SA0STAT,X'20'                                                    
         BE    LSPU012                                                          
         CLI   SA0STAT,X'40'                                                    
         BNE   LSPU012                                                          
         B     LSPU020                                                          
*                                                                               
LSPU012  MVC   IOKEY(L'SA0KEY),0(R2)                                            
         GOTO1 VDATAMGR,DMCB,(X'00',DMRSEQ),CTFILE,IOKEY,(R2)                   
         B     LSPU010                                                          
*                                                                               
LSPU020  EQU   *                                                                
         GOTO1 CXLOAD,DMCB,VCXSPUC,VCXSPUX,INITSPU,0                            
         BNE   LSPUNO                                                           
         OC    MAXIOS,MAXIOS       EXIT IF MAXIOS EXCEEDED                      
         BZ    LSPUOK                                                           
         B     LSPU010                                                          
*                                                                               
LSPUNO   B     NO                                                               
LSPUOK   B     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LOAD AGENCY ACCESS AUTHORISATION LEVELS                             *         
***********************************************************************         
         SPACE 1                                                                
LOADSAA  NTR1                                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST ACCESS RECORD          
         USING SAASREC,R2                                                       
         XC    SAASKEY,SAASKEY                                                  
         MVI   SAASTYP,SAASTYPQ                                                 
         MVI   SAASSUB,SAASSUBQ                                                 
         MVC   SAASAGY,AGENCYID                                                 
         L     R2,DXARECB                                                       
         GOTO1 VDATAMGR,DMCB,(X'00',DMRDHI),CTFILE,IOKEY,(R2)                   
*                                                                               
LSAA010  TM    8(R1),X'80'         ALL DONE IF EOF                              
         BO    LSAAOK                                                           
         TM    8(R1),X'7D'         DIE IF DISK ERROR                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                  ALL DONE IF RECORD TYPE CHANGES              
         CLC   SAASKEY(SAASOVPG-SAASKEY),IOKEY                                  
         BNE   LSAAOK                                                           
*                                  READ AGENCY LEVEL ONLY                       
         OC    SAASUID,SAASUID                                                  
         BNZ   LSAA012                                                          
         OC    SAASAGN,SAASAGN                                                  
         BZ    LSAA020                                                          
*                                                                               
LSAA012  MVC   IOKEY(L'SAASKEY),0(R2)                                           
         GOTO1 VDATAMGR,DMCB,(X'00',DMRSEQ),CTFILE,IOKEY,(R2)                   
         B     LSAA010                                                          
*                                                                               
LSAA020  EQU   *                                                                
         GOTO1 CXLOAD,DMCB,VCXSAAC,VCXSAAX,INITSAA,0                            
         BNE   LSAANO                                                           
         OC    MAXIOS,MAXIOS       EXIT IF MAXIOS EXCEEDED                      
         BZ    LSAAOK                                                           
         B     LSAA010                                                          
*                                                                               
LSAANO   B     NO                                                               
LSAAOK   B     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LOAD ACCESS GROUP AUTHORISATION LEVELS                              *         
***********************************************************************         
         SPACE 1                                                                
LOADSAL  NTR1                                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST ACCESS RECORD          
         USING SAASREC,R2                                                       
         XC    SAASKEY,SAASKEY                                                  
         MVI   SAASTYP,SAASTYPQ                                                 
         MVI   SAASSUB,SAASSUBQ                                                 
         MVC   SAASAGY,AGENCYID                                                 
         L     R2,DXARECB                                                       
         GOTO1 VDATAMGR,DMCB,(X'00',DMRDHI),CTFILE,IOKEY,(R2)                   
*                                                                               
LSAL010  TM    8(R1),X'80'         ALL DONE IF EOF                              
         BO    LSALOK                                                           
         TM    8(R1),X'7D'         DIE IF DISK ERROR                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                  ALL DONE IF RECORD TYPE CHANGES              
         CLC   SAASKEY(SAASOVPG-SAASKEY),IOKEY                                  
         BNE   LSALOK                                                           
*                                  READ ACCESS GROUP LEVEL ONLY                 
         OC    SAASUID,SAASUID                                                  
         BNZ   LSAL012                                                          
         OC    SAASAGN,SAASAGN                                                  
         BNZ   LSAL020                                                          
*                                                                               
LSAL012  MVC   IOKEY(L'SAASKEY),0(R2)                                           
         GOTO1 VDATAMGR,DMCB,(X'00',DMRSEQ),CTFILE,IOKEY,(R2)                   
         B     LSAL010                                                          
*                                                                               
LSAL020  EQU   *                                                                
         GOTO1 CXLOAD,DMCB,VCXSALC,VCXSALX,INITSAL,0                            
         BNE   LSALNO                                                           
         OC    MAXIOS,MAXIOS       EXIT IF MAXIOS EXCEEDED                      
         BZ    LSALOK                                                           
         B     LSAL010                                                          
*                                                                               
LSALNO   B     NO                                                               
LSALOK   B     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LOAD FIELD CONTROL AGENCY LEVEL                                     *         
***********************************************************************         
         SPACE 1                                                                
LOADSFA  NTR1                                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST FCONTROL REC.          
         USING SAFCREC,R2                                                       
         XC    SAFCKEY,SAFCKEY                                                  
         MVI   SAFCTYP,SAFCTYPQ                                                 
         MVI   SAFCSUB,SAFCSUBQ                                                 
         MVC   SAFCAGY,AGENCYID                                                 
         L     R2,DXARECB                                                       
         GOTO1 VDATAMGR,DMCB,(X'00',DMRDHI),CTFILE,IOKEY,(R2)                   
*                                                                               
LSFA010  TM    8(R1),X'80'         ALL DONE IF EOF                              
         BO    LSFAOK                                                           
         TM    8(R1),X'7D'         DIE IF DISK ERROR                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                  ALL DONE IF RECORD TYPE CHANGES              
         CLC   SAFCKEY(SAFCOVPG-SAFCKEY),IOKEY                                  
         BNE   LSFAOK                                                           
*                                  READ AGENCY LEVEL ONLY                       
         OC    SAFCUID,SAFCUID                                                  
         BNZ   LSFA012                                                          
         OC    SAFCAGN,SAFCAGN                                                  
         BZ    LSFA020                                                          
*                                                                               
LSFA012  MVC   IOKEY(L'SAFCKEY),0(R2)                                           
         GOTO1 VDATAMGR,DMCB,(X'00',DMRSEQ),CTFILE,IOKEY,(R2)                   
         B     LSFA010                                                          
*                                                                               
LSFA020  EQU   *                                                                
         GOTO1 CXLOAD,DMCB,VCXSFAC,VCXSFAX,INITSFA,0                            
         BNE   LSFANO                                                           
         OC    MAXIOS,MAXIOS       EXIT IF MAXIOS EXCEEDED                      
         BZ    LSFAOK                                                           
         B     LSFA010                                                          
*                                                                               
LSFANO   B     NO                                                               
LSFAOK   B     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LOAD FIELD CONTROL ACCESS GROUP LEVEL RECORDS                       *         
***********************************************************************         
         SPACE 1                                                                
LOADSFG  NTR1                                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST ACCESS RECORD          
         USING SAFCREC,R2                                                       
         XC    SAFCKEY,SAFCKEY                                                  
         MVI   SAFCTYP,SAFCTYPQ                                                 
         MVI   SAFCSUB,SAFCSUBQ                                                 
         MVC   SAFCAGY,AGENCYID                                                 
         L     R2,DXARECB                                                       
         GOTO1 VDATAMGR,DMCB,(X'00',DMRDHI),CTFILE,IOKEY,(R2)                   
*                                                                               
LSFG010  TM    8(R1),X'80'         ALL DONE IF EOF                              
         BO    LSFGOK                                                           
         TM    8(R1),X'7D'         DIE IF DISK ERROR                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                  ALL DONE IF RECORD TYPE CHANGES              
         CLC   SAFCKEY(SAFCOVPG-SAFCKEY),IOKEY                                  
         BNE   LSFGOK                                                           
*                                  READ ACCESS GROUP LEVEL ONLY                 
         OC    SAFCUID,SAFCUID                                                  
         BNZ   LSFG012                                                          
         OC    SAFCAGN,SAFCAGN                                                  
         BNZ   LSFG020                                                          
*                                                                               
LSFG012  MVC   IOKEY(L'SAFCKEY),0(R2)                                           
         GOTO1 VDATAMGR,DMCB,(X'00',DMRSEQ),CTFILE,IOKEY,(R2)                   
         B     LSFG010                                                          
*                                                                               
LSFG020  EQU   *                                                                
         GOTO1 CXLOAD,DMCB,VCXSFGC,VCXSFGX,INITSFG,0                            
         BNE   LSFGNO                                                           
         OC    MAXIOS,MAXIOS       EXIT IF MAXIOS EXCEEDED                      
         BZ    LSFGOK                                                           
         B     LSFG010                                                          
*                                                                               
LSFGNO   B     NO                                                               
LSFGOK   B     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LOAD OPTION CONTROL AGENCY LEVEL                                    *         
***********************************************************************         
         SPACE 1                                                                
LOADSOA  NTR1                                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST FCONTROL REC.          
         USING SAOCREC,R2                                                       
         XC    SAOCKEY,SAOCKEY                                                  
         MVI   SAOCTYP,SAOCTYPQ                                                 
         MVI   SAOCSUB,SAOCSUBQ                                                 
         MVC   SAOCAGY,AGENCYID                                                 
         L     R2,DXARECB                                                       
         GOTO1 VDATAMGR,DMCB,(X'00',DMRDHI),CTFILE,IOKEY,(R2)                   
*                                                                               
LSOA010  TM    8(R1),X'80'         ALL DONE IF EOF                              
         BO    LSOAOK                                                           
         TM    8(R1),X'7D'         DIE IF DISK ERROR                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                  ALL DONE IF RECORD TYPE CHANGES              
         CLC   SAOCKEY(SAOCOVPG-SAOCKEY),IOKEY                                  
         BNE   LSOAOK                                                           
*                                  READ AGENCY LEVEL ONLY                       
         OC    SAOCUID,SAOCUID                                                  
         BNZ   LSOA012                                                          
         OC    SAOCAGN,SAOCAGN                                                  
         BZ    LSOA020                                                          
*                                                                               
LSOA012  MVC   IOKEY(L'SAOCKEY),0(R2)                                           
         GOTO1 VDATAMGR,DMCB,(X'00',DMRSEQ),CTFILE,IOKEY,(R2)                   
         B     LSOA010                                                          
*                                                                               
LSOA020  EQU   *                                                                
         GOTO1 CXLOAD,DMCB,VCXSOAC,VCXSOAX,INITSOA,0                            
         BNE   LSOANO                                                           
         OC    MAXIOS,MAXIOS       EXIT IF MAXIOS EXCEEDED                      
         BZ    LSOAOK                                                           
         B     LSOA010                                                          
*                                                                               
LSOANO   B     NO                                                               
LSOAOK   B     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LOAD OPTION CONTROL ACCESS GROUP LEVEL RECORDS                      *         
***********************************************************************         
         SPACE 1                                                                
LOADSOG  NTR1                                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST ACCESS RECORD          
         USING SAOCREC,R2                                                       
         XC    SAOCKEY,SAOCKEY                                                  
         MVI   SAOCTYP,SAOCTYPQ                                                 
         MVI   SAOCSUB,SAOCSUBQ                                                 
         MVC   SAOCAGY,AGENCYID                                                 
         L     R2,DXARECB                                                       
         GOTO1 VDATAMGR,DMCB,(X'00',DMRDHI),CTFILE,IOKEY,(R2)                   
*                                                                               
LSOG010  TM    8(R1),X'80'         ALL DONE IF EOF                              
         BO    LSOGOK                                                           
         TM    8(R1),X'7D'         DIE IF DISK ERROR                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                  ALL DONE IF RECORD TYPE CHANGES              
         CLC   SAOCKEY(SAOCOVPG-SAOCKEY),IOKEY                                  
         BNE   LSOGOK                                                           
*                                  READ ACCESS GROUP LEVEL ONLY                 
         OC    SAOCUID,SAOCUID                                                  
         BNZ   LSOG012                                                          
         OC    SAOCAGN,SAOCAGN                                                  
         BNZ   LSOG020                                                          
*                                                                               
LSOG012  MVC   IOKEY(L'SAOCKEY),0(R2)                                           
         GOTO1 VDATAMGR,DMCB,(X'00',DMRSEQ),CTFILE,IOKEY,(R2)                   
         B     LSOG010                                                          
*                                                                               
LSOG020  EQU   *                                                                
         GOTO1 CXLOAD,DMCB,VCXSOGC,VCXSOGX,INITSOG,0                            
         BNE   LSOGNO                                                           
         OC    MAXIOS,MAXIOS       EXIT IF MAXIOS EXCEEDED                      
         BZ    LSOGOK                                                           
         B     LSOG010                                                          
*                                                                               
LSOGNO   B     NO                                                               
LSOGOK   B     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LOAD ACCOUNT SYSTEM OFFICE LIST DATA FROM USER PROFILE RECORDS      *         
***********************************************************************         
         SPACE 1                                                                
LOADOLS  NTR1                                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST AGENCY RECORD          
         USING CTUREC,R2                                                        
         XC    CTUKEY,CTUKEY                                                    
         MVI   CTUKTYP,CTUKTYPQ                                                 
         MVI   CTUKSYS,C'A'        ACCOUNT SYSTEM                               
         L     R2,DXARECB                                                       
         GOTO1 VDATAMGR,DMCB,(X'00',DMRDHI),CTFILE,IOKEY,(R2)                   
*                                                                               
LOLS010  TM    8(R1),X'80'         ALL DONE IF EOF                              
         BO    LOLSOK                                                           
         TM    8(R1),X'7D'         DIE IF DISK ERROR                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                  ALL DONE IF RECORD TYPE CHANGES              
         CLC   CTUKEY(CTUKPROG-CTUKEY),IOKEY                                    
         BNE   LOLSOK                                                           
         CLI   CTUKPROG,X'00'                                                   
         BE    LOLS012                                                          
         CLI   CTUKPROG,C'$'                                                    
         BE    LOLS014                                                          
         B     LOLS020                                                          
*                                                                               
LOLS012  CLI   CTUKPROG+1,C'$'                                                  
         BE    LOLS014                                                          
         B     LOLS020                                                          
*                                                                               
LOLS014  CLC   CTUKAGY,AGENCYID                                                 
         BNE   LOLS020                                                          
         B     LOLS030                                                          
*                                                                               
LOLS020  MVC   IOKEY(L'CTUKEY),0(R2)                                            
         GOTO1 VDATAMGR,DMCB,(X'00',DMRSEQ),CTFILE,IOKEY,(R2)                   
         B     LOLS010                                                          
*                                                                               
LOLS030  EQU   *                                                                
         GOTO1 CXLOAD,DMCB,VCXOLSC,VCXOLSX,INITOLS,0                            
         BNE   LOLSNO                                                           
         OC    MAXIOS,MAXIOS       EXIT IF MAXIOS EXCEEDED                      
         BZ    LOLSOK                                                           
         B     LOLS010                                                          
*                                                                               
LOLSNO   B     NO                                                               
LOLSOK   B     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LOAD ACCOUNT SYSTEM OFFICE LIST ENTRY DATA FROM USER PROFILE RECORDS*         
***********************************************************************         
         SPACE 1                                                                
LOADOLE  NTR1                                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST AGENCY RECORD          
         USING CTUREC,R2                                                        
         XC    CTUKEY,CTUKEY                                                    
         MVI   CTUKTYP,CTUKTYPQ                                                 
         MVI   CTUKSYS,C'A'        ACCOUNT SYSTEM                               
         L     R2,DXARECB                                                       
         GOTO1 VDATAMGR,DMCB,(X'00',DMRDHI),CTFILE,IOKEY,(R2)                   
*                                                                               
LOLE010  TM    8(R1),X'80'         ALL DONE IF EOF                              
         BO    LOLEOK                                                           
         TM    8(R1),X'7D'         DIE IF DISK ERROR                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                  ALL DONE IF RECORD TYPE CHANGES              
         CLC   CTUKEY(CTUKPROG-CTUKEY),IOKEY                                    
         BNE   LOLEOK                                                           
         CLI   CTUKPROG,X'00'                                                   
         BE    LOLE012                                                          
         CLI   CTUKPROG,C'$'                                                    
         BE    LOLE014                                                          
         B     LOLE020                                                          
*                                                                               
LOLE012  CLI   CTUKPROG+1,C'$'                                                  
         BE    LOLE014                                                          
         B     LOLE020                                                          
*                                                                               
LOLE014  CLC   CTUKAGY,AGENCYID                                                 
         BNE   LOLE020                                                          
         B     LOLE030                                                          
*                                                                               
LOLE020  MVC   IOKEY(L'CTUKEY),0(R2)                                            
         GOTO1 VDATAMGR,DMCB,(X'00',DMRSEQ),CTFILE,IOKEY,(R2)                   
         B     LOLE010                                                          
*                                                                               
LOLE030  EQU   *                                                                
         GOTO1 CXLOAD,DMCB,VCXOLEC,VCXOLEX,INITOLE,0                            
         BNE   LOLENO                                                           
         OC    MAXIOS,MAXIOS       EXIT IF MAXIOS EXCEEDED                      
         BZ    LOLEOK                                                           
         B     LOLE010                                                          
*                                                                               
LOLENO   B     NO                                                               
LOLEOK   B     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LOAD TIME SHEET APPROVER GROUP NAME DATA                            *         
***********************************************************************         
         SPACE 1                                                                
LOADAPN  NTR1                                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST ACCESS RECORD          
         USING SAAPREC,R2                                                       
         XC    SAAPKEY,SAAPKEY                                                  
         MVI   SAAPTYP,SAAPTYPQ                                                 
         MVI   SAAPSUB,SAAPSUBQ                                                 
         MVC   SAAPAGY,AGENCYID                                                 
         MVI   SAAPAGR+L'SAAPAGR-1,X'01'                                        
         L     R2,DXARECB                                                       
         GOTO1 VDATAMGR,DMCB,(X'00',DMRDHI),CTFILE,IOKEY,(R2)                   
*                                                                               
LAPN010  TM    8(R1),X'80'         ALL DONE IF EOF                              
         BO    LAPNOK                                                           
         TM    8(R1),X'7D'         DIE IF DISK ERROR                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                  ALL DONE IF RECORD KEY CHANGES               
         CLC   SAAPKEY(SAAPAGR-SAAPKEY),IOKEY                                   
         BNE   LAPNOK                                                           
         B     LAPN020                                                          
*                                                                               
LAPN012  MVC   IOKEY(L'SAASKEY),0(R2)                                           
         GOTO1 VDATAMGR,DMCB,(X'00',DMRSEQ),CTFILE,IOKEY,(R2)                   
         B     LAPN010                                                          
*                                                                               
LAPN020  EQU   *                                                                
         GOTO1 CXLOAD,DMCB,VCXAPNC,VCXAPNX,INITAPN,0                            
         BNE   LAPNNO                                                           
         OC    MAXIOS,MAXIOS       EXIT IF MAXIOS EXCEEDED                      
         BZ    LAPNOK                                                           
         B     LAPN010                                                          
*                                                                               
LAPNNO   B     NO                                                               
LAPNOK   B     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LOAD TIME SHEET APPROVER GROUP MANAGER DATE                         *         
***********************************************************************         
         SPACE 1                                                                
LOADAPM  NTR1                                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST ACCESS RECORD          
         USING SAAPREC,R2                                                       
         XC    SAAPKEY,SAAPKEY                                                  
         MVI   SAAPTYP,SAAPTYPQ                                                 
         MVI   SAAPSUB,SAAPSUBQ                                                 
         MVC   SAAPAGY,AGENCYID                                                 
         MVI   SAAPAGR+L'SAAPAGR-1,X'01'                                        
         L     R2,DXARECB                                                       
         GOTO1 VDATAMGR,DMCB,(X'00',DMRDHI),CTFILE,IOKEY,(R2)                   
*                                                                               
LAPM010  TM    8(R1),X'80'         ALL DONE IF EOF                              
         BO    LAPMOK                                                           
         TM    8(R1),X'7D'         DIE IF DISK ERROR                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                  ALL DONE IF RECORD KEY CHANGES               
         CLC   SAAPKEY(SAAPAGR-SAAPKEY),IOKEY                                   
         BNE   LAPMOK                                                           
         B     LAPM020                                                          
*                                                                               
LAPM012  MVC   IOKEY(L'SAASKEY),0(R2)                                           
         GOTO1 VDATAMGR,DMCB,(X'00',DMRSEQ),CTFILE,IOKEY,(R2)                   
         B     LAPM010                                                          
*                                                                               
LAPM020  EQU   *                                                                
         GOTO1 CXLOAD,DMCB,VCXAPMC,VCXAPMX,INITAPM,0                            
         BNE   LAPMNO                                                           
         OC    MAXIOS,MAXIOS       EXIT IF MAXIOS EXCEEDED                      
         BZ    LAPMOK                                                           
         B     LAPM010                                                          
*                                                                               
LAPMNO   B     NO                                                               
LAPMOK   B     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LOAD PERSON TIME SHEET APPROVER GROUP MANAGER RECORD                *         
***********************************************************************         
         SPACE 1                                                                
LOADPAM  NTR1                                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*****    XC    PIDSAVE,PIDSAVE     WANT ALL RECS, SO DON'T NEED THIS            
*                                  DHAB 9/25/97                                 
*                                  GET TODAYS DATE                              
*****    MVC   TODAYC,FFILL                                                     
*****    XC    TODAYC,DXDATENC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST PERSON RECORD          
         USING SAPEREC,R2                                                       
         XC    SAPEKEY,SAPEKEY                                                  
         MVI   SAPETYP,SAPETYPQ                                                 
         MVI   SAPESUB,SAPESUBQ                                                 
         MVC   SAPEAGY,AGENCYID                                                 
*                                                                               
         L     R2,DXARECB                                                       
         GOTO1 VDATAMGR,DMCB,(X'00',DMRDHI),CTFILE,IOKEY,(R2)                   
*                                                                               
LPAM010  TM    8(R1),X'80'         ALL DONE IF EOF                              
         BO    LPAMOK                                                           
         TM    8(R1),X'7D'         DIE IF DISK ERROR                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                  ALL DONE IF RECORD TYPE CHANGES              
         CLC   SAPEREC(SAPEPID-SAPEKEY),IOKEY                                   
         BNE   LPAMOK                                                           
*                                                                               
*****    CLC   SAPEPID,PIDSAVE                                                  
*****    BNE   LPAM012                                                          
*****    MVC   IOKEY(L'SAPEKEY),0(R2)                                           
*****    GOTO1 VDATAMGR,DMCB,(X'00',DMRSEQ),CTFILE,IOKEY,(R2)                   
*****    B     LPAM010                                                          
*                                                                               
*****12  MVC   PIDSAVE,SAPEPID                                                  
*****    CLC   SAPEDEF,TODAYC                                                   
*****    BNL   LPAM020                                                          
*****    LA    R2,IOKEY                                                         
*****    MVC   SAPEPID,PIDSAVE                                                  
*****    MVC   SAPEDEF,TODAYC                                                   
*                                                                               
*****    L     R2,DXARECB                                                       
*****    GOTO1 VDATAMGR,DMCB,(X'00',DMRDHI),CTFILE,IOKEY,(R2)                   
*                                                                               
*****20  EQU   *                                                                
         GOTO1 CXLOAD,DMCB,VCXPAMC,VCXPAMX,INITPAM,0                            
         BNE   LPAMNO                                                           
         OC    MAXIOS,MAXIOS       EXIT IF MAXIOS EXCEEDED                      
         BZ    LPAMOK                                                           
         B     LPAM010                                                          
*                                                                               
LPAMNO   B     NO                                                               
LPAMOK   B     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* PROCESS CONTROL FILE SECURITY DATA IN UPDATE MODE                   *         
***********************************************************************         
         SPACE 1                                                                
PROCUPDT NTR1                                                                   
         L     R6,DXARECB          POINT TO RECOVERY RECORD BUFFER              
         USING RECDS,R6                                                         
         MVC   TYPECODE,TYPESAVE                                                
         BAS   RE,GETTYP           SET TYPE TABLE DATA                          
         CLI   RFILTY,CONFILQ      TEST CONFILE RECORD TYPE                     
         BNE   PUPDOK                ELSE IGNORE RECORD                         
         BAS   RE,PROCKEY          PROCESS RECORD KEY VALUES                    
         BNE   PUPDOK                EITHER IGNORE RECORD                       
         L     RF,TYPEAUPD           ELSE CALL UPDATE PROCESS ROUTINE           
         A     RF,RELO                                                          
         BASR  RE,RF                                                            
         BNE   PUPDNO              EXIT ERROR                                   
         B     PUPDOK              EXIT OK                                      
*                                                                               
PUPDNO   B     NO                                                               
PUPDOK   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* UPDATE ALL RECORD DATA                                              *         
***********************************************************************         
         SPACE 1                                                                
UPDTALL  NTR1                                                                   
         ICM   R3,15,ASUBTAB       SET A(RECORD TYPE DATA TABLE)                
*                                                                               
UALL010  CLI   0(R3),0             EXIT IF END OF TABLE                         
         BE    UALLOK                                                           
         CLC   RECVHDR+L'RECVHDR(1),TRECTYP(R3)  COMPARE RECORD TYPE            
         BE    UALL030                                                          
*                                                                               
UALL020  LA    R3,SUBTABL(R3)      GET NEXT ENTRY                               
         B     UALL010                                                          
*                                                                               
UALL030  EQU   *                   RECORD TYPE MATCH FOUND                      
         L     RF,TAUPDT(R3)       CALL EXTRACT ROUTINE                         
         A     RF,RELO                                                          
         BASR  RE,RF                                                            
         BNE   UALLNO                                                           
         B     UALL020                                                          
*                                                                               
UALLNO   B     NO                                                               
UALLOK   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* UPDATE AGENCY RECORD DATA                                           *         
***********************************************************************         
         SPACE 1                                                                
UPDTSAG  NTR1                                                                   
*                                  SET A(AGENCY ACCESS RECORD) FROM             
         LA    R2,RECVHDR+L'RECVHDR  RECOVERY RECORD BUFFER                     
         USING CT5REC,R2                                                        
         GOTO1 CHKSAGY,CT5KALPH    FILTER ON SECURITY AGENCY ALPHA ID           
         BNE   USAGOK                                                           
*                                  TRANSFER RECORD DATA TO EXTRACT FILE         
         GOTO1 CXUPDT,DMCB,VCXSAGC,VCXSAGX,INITSAG                              
         BNE   USAGNO                                                           
         B     USAGOK                                                           
*                                                                               
USAGNO   B     NO                                                               
USAGOK   B     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* UPDATE USERID RECORD DATA                                           *         
***********************************************************************         
         SPACE 1                                                                
UPDTSUI  NTR1                                                                   
*                                  SET A(USERID RECORD) FROM                    
         LA    R2,RECVHDR+L'RECVHDR  RECOVERY RECORD BUFFER                     
         USING CTIREC,R2                                                        
         OC    CTIKID(8),CTIKID    IGNORE ID # RECORD                           
         BZ    USUIOK                                                           
         MVC   ACTIONSV,DXACTION   SAVE DXACTION                                
         CLI   DXACTION,C'C'       TEST FOR CHANGE ACTION                       
         BNE   USUI100                                                          
         BAS   RE,GETUAGY          IF SO MUST CHECK IF AGENCY CODE              
         BNE   USUIOK                CHANGED WITHIN RECORD                      
         MVC   USERAGY1,USERAGY    SAVE CHANGED AGENCY ID VALUE                 
         L     R2,DXACPYB                                                       
         LA    R2,L'RECVHDR+4(R2)  R2=A(SAVED RECOVERY COPY RECORD)             
         BAS   RE,GETUAGY          GET OLD AGENCY ID VALUE                      
         LA    R2,RECVHDR+L'RECVHDR                                             
*        CLC   USERAGY,AGENCYID    CHECK IF OLD AGENCY WAS THIS ONE             
         GOTO1 CHKSAGY,USERAGY     CHECK IF OLD AGENCY WAS THIS ONE             
         BE    USUI020                                                          
*                                  HERE IF OLD AGENCY IS NOT THIS ONE           
USUI010  EQU   *                                                                
*        CLC   USERAGY1,AGENCYID   CHECK AGENCY ID CHANGED TO THIS ONE          
         GOTO1 CHKSAGY,USERAGY1    CHECK AGENCY ID CHANGED TO THIS ONE          
         BNE   USUIOK                                                           
         MVI   DXACTION,C'A'       IF SO SET RECORD ACTION TO ADDED             
         B     USUI110                                                          
*                                  HERE IF OLD AGENCY IS THIS ONE               
USUI020  EQU   *                                                                
*        CLC   USERAGY1,AGENCYID   CHECK AGENCY ID CHANGED                      
         GOTO1 CHKSAGY,USERAGY1    CHECK AGENCY ID CHANGED                      
         BE    USUI110                                                          
         MVI   DXACTION,C'D'       IF SO SET RECORD ACTION TO DELETED           
         B     USUI110                                                          
*                                                                               
USUI100  EQU   *                                                                
         BAS   RE,GETUAGY          GET USERID AGENCY ALPHA FOR ACTIONS          
         BNE   USUIOK                OTHER THAN 'CHANGED'                       
*        CLC   USERAGY,AGENCYID    CHECK THIS AGENCY ID                         
         GOTO1 CHKSAGY,USERAGY     CHECK THIS AGENCY ID                         
         BNE   USUIOK                                                           
*                                  TRANSFER RECORD DATA TO EXTRACT FILE         
USUI110  EQU   *                                                                
         GOTO1 CXUPDT,DMCB,VCXSUIC,VCXSUIX,INITSUI                              
         BNE   USUINO                                                           
         MVC   DXACTION,ACTIONSV   RESTORE DXACTION                             
         B     USUIOK                                                           
*                                                                               
USUINO   B     NO                                                               
USUIOK   B     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* UPDATE USERID PROGRAM RECORD DATA                                   *         
***********************************************************************         
         SPACE 1                                                                
UPDTSUP  NTR1                                                                   
*                                  SET A(USERID RECORD) FROM                    
         LA    R2,RECVHDR+L'RECVHDR  RECOVERY RECORD BUFFER                     
         USING CTIREC,R2                                                        
         OC    CTIKID(8),CTIKID    IGNORE ID # RECORD                           
         BZ    USUPOK                                                           
         MVC   ACTIONSV,DXACTION   SAVE DXACTION                                
         CLI   DXACTION,C'C'       TEST FOR CHANGE ACTION                       
         BNE   USUP100                                                          
         BAS   RE,GETUAGY          IF SO MUST CHECK IF AGENCY CODE              
         BNE   USUPOK                CHANGED WITHIN RECORD                      
         MVC   USERAGY1,USERAGY    SAVE CHANGED AGENCY ID VALUE                 
         L     R2,DXACPYB                                                       
         LA    R2,L'RECVHDR+4(R2)  R2=A(SAVED RECOVERY COPY RECORD)             
         BAS   RE,GETUAGY          GET OLD AGENCY ID VALUE                      
         LA    R2,RECVHDR+L'RECVHDR                                             
*        CLC   USERAGY,AGENCYID    CHECK IF OLD AGENCY WAS THIS ONE             
         GOTO1 CHKSAGY,USERAGY     CHECK IF OLD AGENCY WAS THIS ONE             
         BE    USUP020                                                          
*                                  HERE IF OLD AGENCY IS NOT THIS ONE           
USUP010  EQU   *                                                                
*        CLC   USERAGY1,AGENCYID   CHECK AGENCY ID CHANGED TO THIS ONE          
         GOTO1 CHKSAGY,USERAGY1    CHECK AGENCY ID CHANGED TO THIS ONE          
         BNE   USUPOK                                                           
         MVI   DXACTION,C'A'       IF SO SET RECORD ACTION TO ADDED             
         B     USUP110                                                          
*                                  HERE IF OLD AGENCY IS THIS ONE               
USUP020  EQU   *                                                                
*        CLC   USERAGY1,AGENCYID   CHECK AGENCY ID CHANGED                      
         GOTO1 CHKSAGY,USERAGY1    CHECK AGENCY ID CHANGED                      
         BE    USUP110                                                          
         MVI   DXACTION,C'D'       IF SO SET RECORD ACTION TO DELETED           
         B     USUP110                                                          
*                                                                               
USUP100  EQU   *                                                                
         BAS   RE,GETUAGY          GET USERID AGENCY ALPHA FOR ACTIONS          
         BNE   USUPOK                OTHER THAN 'CHANGED'                       
*        CLC   USERAGY,AGENCYID    CHECK THIS AGENCY ID                         
         GOTO1 CHKSAGY,USERAGY     CHECK THIS AGENCY ID                         
         BNE   USUPOK                                                           
*                                  TRANSFER RECORD DATA TO EXTRACT FILE         
USUP110  EQU   *                                                                
         GOTO1 CXUPDT,DMCB,VCXSUPC,VCXSUPX,INITSUP                              
         BNE   USUPNO                                                           
         MVC   DXACTION,ACTIONSV   RESTORE DXACTION                             
         B     USUPOK                                                           
*                                                                               
USUPNO   B     NO                                                               
USUPOK   B     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* UPDATE PERSON PROGRAM RECORD DATA                                   *         
***********************************************************************         
         SPACE 1                                                                
UPDTSPP  NTR1                                                                   
*                                  SET A(PERSON PASSWORD RECORD) FROM           
         LA    R2,RECVHDR+L'RECVHDR  RECOVERY RECORD BUFFER                     
         USING SA0REC,R2                                                        
*                                  IGNORE PASSWORD CODE PASSIVES                
         OC    SA0KCODE(SA0KNUM-SA0KCODE),SA0KCODE                              
         BNZ   USPPOK                                                           
*                                  FILTER ON AGENCY ALPHA ID                    
         CLC   SA0KAGY,AGENCYID                                                 
         BNE   USPPOK                                                           
*                                  PROCESS NEW SECURITY/UNLOCKED ONLY           
         CLI   SA0STAT,X'20'                                                    
         BE    USPPOK                                                           
         CLI   SA0STAT,X'40'                                                    
         BNE   USPPOK                                                           
*                                  TRANSFER RECORD DATA TO EXTRACT FILE         
         GOTO1 CXUPDT,DMCB,VCXSPPC,VCXSPPX,INITSPP                              
         BNE   USPPNO                                                           
         B     USPPOK                                                           
*                                                                               
USPPNO   B     NO                                                               
USPPOK   B     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* UPDATE PERSON USERID XREF RECORD DATA                               *         
***********************************************************************         
         SPACE 1                                                                
UPDTSPU  NTR1                                                                   
*                                  SET A(PERSON PASSWORD RECORD) FROM           
         LA    R2,RECVHDR+L'RECVHDR  RECOVERY RECORD BUFFER                     
         USING SA0REC,R2                                                        
*                                  IGNORE PASSWORD CODE PASSIVES                
         OC    SA0KCODE(SA0KNUM-SA0KCODE),SA0KCODE                              
         BNZ   USPUOK                                                           
*                                  FILTER ON AGENCY ALPHA ID                    
         CLC   SA0KAGY,AGENCYID                                                 
         BNE   USPUOK                                                           
*                                  PROCESS NEW SECURITY/UNLOCKED ONLY           
         CLI   SA0STAT,X'20'                                                    
         BE    USPUOK                                                           
         CLI   SA0STAT,X'40'                                                    
         BNE   USPUOK                                                           
*                                  TRANSFER RECORD DATA TO EXTRACT FILE         
         GOTO1 CXUPDT,DMCB,VCXSPUC,VCXSPUX,INITSPU                              
         BNE   USPUNO                                                           
         B     USPUOK                                                           
*                                                                               
USPUNO   B     NO                                                               
USPUOK   B     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* UPDATE PERSON PASSWORD RECORD DATA                                  *         
***********************************************************************         
         SPACE 1                                                                
UPDTSPW  NTR1                                                                   
*                                  SET A(PERSON PASSWORD RECORD) FROM           
         LA    R2,RECVHDR+L'RECVHDR  RECOVERY RECORD BUFFER                     
         USING SA0REC,R2                                                        
*                                  IGNORE PASSWORD NUMBER PASSIVES              
         OC    SA0KCODE(SA0KNUM-SA0KCODE),SA0KCODE                              
         BZ    USPWOK                                                           
*                                  FILTER ON AGENCY ALPHA ID                    
         CLC   SA0KAGY,AGENCYID                                                 
         BNE   USPWOK                                                           
*                                  PROCESS NEW SECURITY/UNLOCKED ONLY           
         CLI   SA0STAT,X'20'                                                    
         BE    USPWOK                                                           
         CLI   SA0STAT,X'40'                                                    
         BNE   USPWOK                                                           
*                                  TRANSFER RECORD DATA TO EXTRACT FILE         
         GOTO1 CXUPDT,DMCB,VCXSPWC,VCXSPEX,INITSPE                              
         BNE   USPWNO                                                           
         B     USPWOK                                                           
*                                                                               
USPWNO   B     NO                                                               
USPWOK   B     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* UPDATE PERSON RECORD DATA                                           *         
***********************************************************************         
         SPACE 1                                                                
UPDTSPE  NTR1                                                                   
*                                  SET A(PERSON RECORD) FROM                    
         LA    R2,RECVHDR+L'RECVHDR  RECOVERY RECORD BUFFER                     
         USING SAPEREC,R2                                                       
*                                  FILTER ON PERSON RECORD SUB TYPE             
         CLI   SAPESUB,SAPESUBQ                                                 
         BNE   USPEOK                                                           
*                                  FILTER ON AGENCY ALPHA ID                    
         CLC   SAPEAGY,AGENCYID                                                 
         BNE   USPEOK                                                           
*                                  GET TODAYS DATE                              
         MVC   TODAYC,FFILL                                                     
         XC    TODAYC,DXDATENC                                                  
*                                  PROCESS CURRENT PERSON RECORD ONLY           
         CLI   DXACTION,C'D'                                                    
         BE    USPE010                                                          
         MVC   IOKEY(L'SAPEKEY),SAPEKEY                                         
         SR    RF,RF                                                            
         ICM   RF,3,TODAYC                                                      
         STCM  RF,3,SAPEDEF-SAPEREC+IOKEY                                       
         LA    R4,IO                                                            
         GOTO1 VDATAMGR,DMCB,(X'08',DMRDHI),CTFILE,IOKEY,(R4)                   
*                                                                               
         TM    8(R1),X'80'         IGNORE IF EOF                                
         BO    USPEOK                                                           
         TM    8(R1),X'7D'         DIE IF DISK ERROR                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                  IGNORE IF NOT CURRENT PIDS                   
         CLC   0(L'SAPEKEY,R4),0(R2)                                            
         BNE   USPEOK                                                           
*                                  TRANSFER RECORD DATA TO EXTRACT FILE         
USPE010  GOTO1 CXUPDT,DMCB,VCXSPEC,VCXSPEX,INITSPE                              
         BNE   USPENO                                                           
         B     USPEOK                                                           
*                                                                               
USPENO   B     NO                                                               
USPEOK   B     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* UPDATE AGENCY ACCESS AUTHORISATION LEVELS RECORD                    *         
***********************************************************************         
         SPACE 1                                                                
UPDTSAA  NTR1                                                                   
*                                  SET A(ACCESS RECORD) FROM                    
         LA    R2,RECVHDR+L'RECVHDR  RECOVERY RECORD BUFFER                     
         USING SAASREC,R2                                                       
*                                  FILTER ON ACCESS RECORD SUB TYPE             
         CLI   SAASSUB,SAASSUBQ                                                 
         BNE   USAAOK                                                           
*                                  FILTER ON AGENCY ALPHA ID                    
         CLC   SAASAGY,AGENCYID                                                 
         BNE   USAAOK                                                           
*                                  READ AGENCY LEVEL ONLY                       
         OC    SAASUID,SAASUID                                                  
         BNZ   USAAOK                                                           
         OC    SAASAGN,SAASAGN                                                  
         BNZ   USAAOK                                                           
*                                  TRANSFER RECORD DATA TO EXTRACT FILE         
         GOTO1 CXUPDT,DMCB,VCXSAAC,VCXSAAX,INITSAA                              
         BNE   USAANO                                                           
         B     USAAOK                                                           
*                                                                               
USAANO   B     NO                                                               
USAAOK   B     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* UPDATE ACCESS GROUP AUTHORISATION LEVELS RECORD                     *         
***********************************************************************         
         SPACE 1                                                                
UPDTSAL  NTR1                                                                   
*                                  SET A(ACCESS RECORD) FROM                    
         LA    R2,RECVHDR+L'RECVHDR  RECOVERY RECORD BUFFER                     
         USING SAASREC,R2                                                       
*                                  FILTER ON ACCESS RECORD SUB TYPE             
         CLI   SAASSUB,SAASSUBQ                                                 
         BNE   USALOK                                                           
*                                  FILTER ON AGENCY ALPHA ID                    
         CLC   SAASAGY,AGENCYID                                                 
         BNE   USALOK                                                           
*                                  READ ACCESS GROUP LEVEL ONLY                 
         OC    SAASUID,SAASUID                                                  
         BNZ   USALOK                                                           
         OC    SAASAGN,SAASAGN                                                  
         BZ    USALOK                                                           
*                                  TRANSFER RECORD DATA TO EXTRACT FILE         
         GOTO1 CXUPDT,DMCB,VCXSALC,VCXSALX,INITSAL                              
         BNE   USALNO                                                           
         B     USALOK                                                           
*                                                                               
USALNO   B     NO                                                               
USALOK   B     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* UPDATE FIELD CONTROL AGENCY LEVEL RECORD                            *         
***********************************************************************         
         SPACE 1                                                                
UPDTSFA  NTR1                                                                   
*                                  SET A(FCONTROL RECORD) FROM                  
         LA    R2,RECVHDR+L'RECVHDR  RECOVERY RECORD BUFFER                     
         USING SAFCREC,R2                                                       
*                                  FILTER ON FCONTROL RECORD SUB TYPE           
         CLI   SAFCSUB,SAFCSUBQ                                                 
         BNE   USFAOK                                                           
*                                  FILTER ON AGENCY ALPHA ID                    
         CLC   SAFCAGY,AGENCYID                                                 
         BNE   USFAOK                                                           
*                                  READ AGENCY LEVEL ONLY                       
         OC    SAFCUID,SAFCUID                                                  
         BNZ   USFAOK                                                           
         OC    SAFCAGN,SAFCAGN                                                  
         BNZ   USFAOK                                                           
*                                  TRANSFER RECORD DATA TO EXTRACT FILE         
         GOTO1 CXUPDT,DMCB,VCXSFAC,VCXSFAX,INITSFA                              
         BNE   USFANO                                                           
         B     USFAOK                                                           
*                                                                               
USFANO   B     NO                                                               
USFAOK   B     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* UPDATE FIELD CONTROL ACCESS GROUP LEVEL RECORDS                     *         
***********************************************************************         
         SPACE 1                                                                
UPDTSFG  NTR1                                                                   
*                                  SET A(FCONTROL RECORD) FROM                  
         LA    R2,RECVHDR+L'RECVHDR  RECOVERY RECORD BUFFER                     
         USING SAFCREC,R2                                                       
*                                  FILTER ON FCONTROL RECORD SUB TYPE           
         CLI   SAFCSUB,SAFCSUBQ                                                 
         BNE   USFGOK                                                           
*                                  FILTER ON AGENCY ALPHA ID                    
         CLC   SAFCAGY,AGENCYID                                                 
         BNE   USFGOK                                                           
*                                  READ ACCESS GROUP LEVEL ONLY                 
         OC    SAFCUID,SAFCUID                                                  
         BNZ   USFGOK                                                           
         OC    SAFCAGN,SAFCAGN                                                  
         BZ    USFGOK                                                           
*                                  TRANSFER RECORD DATA TO EXTRACT FILE         
         GOTO1 CXUPDT,DMCB,VCXSFGC,VCXSFGX,INITSFG                              
         BNE   USFGNO                                                           
         B     USFGOK                                                           
*                                                                               
USFGNO   B     NO                                                               
USFGOK   B     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* UPDATE OPTION CONTROL AGENCY LEVEL RECORD                           *         
***********************************************************************         
         SPACE 1                                                                
UPDTSOA  NTR1                                                                   
*                                  SET A(OCONTROL RECORD) FROM                  
         LA    R2,RECVHDR+L'RECVHDR  RECOVERY RECORD BUFFER                     
         USING SAOCREC,R2                                                       
*                                  FILTER ON OCONTROL RECORD SUB TYPE           
         CLI   SAOCSUB,SAOCSUBQ                                                 
         BNE   USOAOK                                                           
*                                  FILTER ON AGENCY ALPHA ID                    
         CLC   SAOCAGY,AGENCYID                                                 
         BNE   USOAOK                                                           
*                                  READ AGENCY LEVEL ONLY                       
         OC    SAOCUID,SAOCUID                                                  
         BNZ   USOAOK                                                           
         OC    SAOCAGN,SAOCAGN                                                  
         BNZ   USOAOK                                                           
*                                  TRANSFER RECORD DATA TO EXTRACT FILE         
         GOTO1 CXUPDT,DMCB,VCXSOAC,VCXSOAX,INITSOA                              
         BNE   USOANO                                                           
         B     USOAOK                                                           
*                                                                               
USOANO   B     NO                                                               
USOAOK   B     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* UPDATE OPTION CONTROL ACCESS GROUP LEVEL RECORD                     *         
***********************************************************************         
         SPACE 1                                                                
UPDTSOG  NTR1                                                                   
*                                  SET A(FCONTROL RECORD) FROM                  
         LA    R2,RECVHDR+L'RECVHDR  RECOVERY RECORD BUFFER                     
         USING SAOCREC,R2                                                       
*                                  FILTER ON FCONTROL RECORD SUB TYPE           
         CLI   SAOCSUB,SAOCSUBQ                                                 
         BNE   USOGOK                                                           
*                                  FILTER ON AGENCY ALPHA ID                    
         CLC   SAOCAGY,AGENCYID                                                 
         BNE   USOGOK                                                           
*                                  READ ACCESS GROUP LEVEL ONLY                 
         OC    SAOCUID,SAOCUID                                                  
         BNZ   USOGOK                                                           
         OC    SAOCAGN,SAOCAGN                                                  
         BZ    USOGOK                                                           
*                                  TRANSFER RECORD DATA TO EXTRACT FILE         
         GOTO1 CXUPDT,DMCB,VCXSOGC,VCXSOGX,INITSOG                              
         BNE   USOGNO                                                           
         B     USOGOK                                                           
*                                                                               
USOGNO   B     NO                                                               
USOGOK   B     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* UPDATE ACCOUNT SYSTEM OFFICE LIST DATA FROM PROFILE RECORDS         *         
***********************************************************************         
         SPACE 1                                                                
UPDTOLS  NTR1                                                                   
*                                  SET A(PROFILE RECORD) FROM                   
         LA    R2,RECVHDR+L'RECVHDR  RECOVERY RECORD BUFFER                     
         USING CTUREC,R2                                                        
         CLI   CTUKSYS,C'A'                                                     
         BNE   UOLSOK                                                           
         CLI   CTUKPROG,X'00'                                                   
         BE    UOLS010                                                          
         CLI   CTUKPROG,C'$'                                                    
         BE    UOLS020                                                          
         B     UOLSOK                                                           
*                                                                               
UOLS010  CLI   CTUKPROG+1,C'$'                                                  
         BE    UOLS020                                                          
         B     UOLSOK                                                           
*                                                                               
UOLS020  CLC   CTUKAGY,AGENCYID                                                 
         BNE   UOLSOK                                                           
*                                  TRANSFER RECORD DATA TO EXTRACT FILE         
         GOTO1 CXUPDT,DMCB,VCXOLSC,VCXOLSX,INITOLS                              
         BNE   UOLSNO                                                           
         B     UOLSOK                                                           
*                                                                               
UOLSNO   B     NO                                                               
UOLSOK   B     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* UPDATE ACCOUNT SYSTEM OFFICE LIST ENTRY DATA FROM PROFILE RECORDS   *         
***********************************************************************         
         SPACE 1                                                                
UPDTOLE  NTR1                                                                   
*                                  SET A(PROFILE RECORD) FROM                   
         LA    R2,RECVHDR+L'RECVHDR  RECOVERY RECORD BUFFER                     
         USING CTUREC,R2                                                        
         CLI   CTUKSYS,C'A'                                                     
         BNE   UOLEOK                                                           
         CLI   CTUKPROG,X'00'                                                   
         BE    UOLE010                                                          
         CLI   CTUKPROG,C'$'                                                    
         BE    UOLE020                                                          
         B     UOLEOK                                                           
*                                                                               
UOLE010  CLI   CTUKPROG+1,C'$'                                                  
         BE    UOLE020                                                          
         B     UOLEOK                                                           
*                                                                               
UOLE020  CLC   CTUKAGY,AGENCYID                                                 
         BNE   UOLEOK                                                           
*                                  TRANSFER RECORD DATA TO EXTRACT FILE         
         GOTO1 CXUPDT,DMCB,VCXOLEC,VCXOLEX,INITOLE                              
         BNE   UOLENO                                                           
         B     UOLEOK                                                           
*                                                                               
UOLENO   B     NO                                                               
UOLEOK   B     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* UPDATE TIME SHEET APPROVER GROUP NAME RECORD                        *         
***********************************************************************         
         SPACE 1                                                                
UPDTAPN  NTR1                                                                   
*                                  SET A(SECURITY RECORD) FROM                  
         LA    R2,RECVHDR+L'RECVHDR  RECOVERY RECORD BUFFER                     
         USING SAAPREC,R2                                                       
         CLI   SAAPSUB,SAAPSUBQ                                                 
         BNE   UAPNOK                                                           
         CLC   SAAPAGY,AGENCYID                                                 
         BNE   UAPNOK                                                           
         OC    SAAPAGR,SAAPAGR                                                  
         BZ    UAPNOK                                                           
*                                  TRANSFER RECORD DATA TO EXTRACT FILE         
         GOTO1 CXUPDT,DMCB,VCXAPNC,VCXAPNX,INITAPN                              
         BNE   UAPNNO                                                           
         B     UAPNOK                                                           
*                                                                               
UAPNNO   B     NO                                                               
UAPNOK   B     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* UPDATE TIME SHEET APPROVER GROUP MANAGER RECORD                     *         
***********************************************************************         
         SPACE 1                                                                
UPDTAPM  NTR1                                                                   
*                                  SET A(SECURITY RECORD) FROM                  
         LA    R2,RECVHDR+L'RECVHDR  RECOVERY RECORD BUFFER                     
         USING SAAPREC,R2                                                       
         CLI   SAAPSUB,SAAPSUBQ                                                 
         BNE   UAPMOK                                                           
         CLC   SAAPAGY,AGENCYID                                                 
         BNE   UAPMOK                                                           
         OC    SAAPAGR,SAAPAGR                                                  
         BZ    UAPMOK                                                           
*                                  TRANSFER RECORD DATA TO EXTRACT FILE         
         GOTO1 CXUPDT,DMCB,VCXAPMC,VCXAPMX,INITAPM                              
         BNE   UAPMNO                                                           
         B     UAPMOK                                                           
*                                                                               
UAPMNO   B     NO                                                               
UAPMOK   B     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* UPDATE PERSON TIME SHEET APPROVER GROUP MANAGER RECORD              *         
***********************************************************************         
         SPACE 1                                                                
UPDTPAM  NTR1                                                                   
*                                  SET A(PERSON RECORD) FROM                    
         LA    R2,RECVHDR+L'RECVHDR  RECOVERY RECORD BUFFER                     
         USING SAPEREC,R2                                                       
*                                  FILTER ON PERSON RECORD SUB TYPE             
         CLI   SAPESUB,SAPESUBQ                                                 
         BNE   UPAMOK                                                           
*                                  FILTER ON AGENCY ALPHA ID                    
         CLC   SAPEAGY,AGENCYID                                                 
         BNE   UPAMOK                                                           
*                                  TRANSFER RECORD DATA TO EXTRACT FILE         
UPAM010  GOTO1 CXUPDT,DMCB,VCXPAMC,VCXPAMX,INITPAM                              
         BNE   UPAMNO                                                           
         B     UPAMOK                                                           
*                                                                               
UPAMNO   B     NO                                                               
UPAMOK   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* PROCESS KEY VALUES IN RECOVERY RECORD                               *         
***********************************************************************         
         SPACE 1                                                                
         USING RECDS,R6                                                         
PROCKEY  NTR1  ,                                                                
         CLI   TYPEREC,0           TEST RECORD TYPE CODE                        
         BE    PKEY010               IF SET FROM TYPE TABLE                     
         CLC   TYPEREC,RECVHDR+L'RECVHDR                                        
         BNE   PKEYNO              IGNORE RECORD IF NOT THIS TYPE               
*                                                                               
PKEY010  EQU   *                                                                
*                                  TEST FOR DELETED RECORD                      
PKEY100  EQU   *                     USING STANDARD OFFSET DSECT                
         TM    RECVHDR+L'RECVHDR+CTISTAT-CTIKEY,X'80'                           
         BZ    PKEY110                                                          
         CLI   DXACTION,C'C'                                                    
         BNE   PKEYNO              ?? ONCE GOT DELETED ID RECORD WITH           
         CLI   RRECTY,X'02'                                                     
         BNE   PKEYNO                                                           
         L     R4,DXACPYB                                                       
         TM    L'RECVHDR+CTISTAT-CTIKEY+4(R4),X'80'                             
         BNZ   PKEYNO              AVOID DELETED 'CHANGED'                      
         MVI   DXACTION,C'D'                                                    
         B     PKEYOK                                                           
*                                  TEST FOR RESTORED RECORD                     
*                                    USING SAVED RECOVERY COPY RECORD           
PKEY110  CLI   RRECTY,X'02'          WITH CHANGE RECOVERY RECORD TYPE           
         BNE   PKEYOK                                                           
         L     R4,DXACPYB                                                       
         TM    L'RECVHDR+CTISTAT-CTIKEY+4(R4),X'80'                             
         BZ    PKEYOK                                                           
         MVI   DXACTION,C'A'                                                    
         B     PKEYOK                                                           
*                                                                               
PKEYNO   B     NO                                                               
PKEYOK   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* GET RECORD TYPE TABLE VALUES FROM 3 CHARACTER CODE                  *         
***********************************************************************         
         SPACE 1                                                                
GETTYP   NTR1                                                                   
         ICM   RE,15,ASUBTAB                                                    
*                                                                               
GTYP010  CLI   0(RE),0             END OF TABLE                                 
         BNE   *+6                                                              
         DC    H'00'                                                            
         CLC   TYPECODE,TNAME(RE)  COMPARE NAME                                 
         BE    GTYP020                                                          
         LA    RE,SUBTABL(RE)      GET NEXT ENTRY                               
         B     GTYP010                                                          
*                                                                               
GTYP020  EQU   *                   MATCH FOUND                                  
         MVC   TYPEREC,TRECTYP(RE)                                              
         MVC   TYPEMED,TMEDIA(RE)                                               
         MVC   TYPEALOD,TALOAD(RE)                                              
         MVC   TYPEAUPD,TAUPDT(RE)                                              
         B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* INITIALISE ALL EXTRACT RECORDS - R1=RECORD LENGTH                   *         
***********************************************************************         
         SPACE 1                                                                
INITALL  NTR1                                                                   
         L     R3,DXAXREC          R3=A(EXTRACT RECORD AREA)                    
         USING DXHDRD,R3                                                        
         LR    R0,R3                                                            
         LA    RE,*                                                             
         SR    RF,RF                                                            
         ICM   RF,8,SPACES                                                      
         MVCL  R0,RE               SET RECORD TO ALL SPACES                     
         LA    RF,DXHDRHL                                                       
         SLL   RF,16                                                            
         ST    RF,DXHDRLEN         SET MINIMUM RECORD LEN IN HDR                
         MVI   DXHDRRTY-1,MXTRTQ                                                
         MVC   DXHDRRTY,DXACTION                                                
         MVI   DXHDRCDT-1,MXTRTQ                                                
         CLI   DXMODE,DXLOADQ      TEST IF LOAD MODE                            
         BE    IALL100                                                          
*                                  HERE IF UPDATE MODE                          
*                                  FORMAT DATE AND TIME FROM RCVHDR             
         GOTO1 VDATCON,DMCB,(3,RDATE),(0,DXHDRCDT+2)                            
         MVC   DXHDRCDT(2),DXCENT                                               
         MVI   DXHDRCTI-1,MXTRTQ                                                
         ICM   RF,15,RTIME                                                      
         TM    RTIME,X'80'                                                      
         BZ    *+12                                                             
         SLL   RF,1                                                             
         SRL   RF,5                                                             
         XC    DUB,DUB                                                          
         STCM  RF,15,DUB+4                                                      
         OI    DUB+7,X'0C'                                                      
         UNPK  DXHDRCTI(6),DUB+4(4)                                             
         OI    DXHDRCTI+5,X'F0'                                                 
         B     YES                                                              
*                                  HERE IF LOAD MODE                            
IALL100  MVC   DXHDRCDT,DXDATEN    SET TODAYS DATE                              
         MVI   DXHDRCTI-1,MXTRTQ                                                
         MVC   DXHDRCTI,DXTIMEN                                                 
         CLI   DXDAFORM,C'Y'                                                    
         BNE   YES                                                              
*******************************************TEMP**                               
         MVI   DXHDRCDT+00,C''''         **TEMP**                               
         MVC   DXHDRCDT+01(6),DXDATEN+2  **TEMP**                               
         MVC   DXHDRCDT+07(2),=C'  '     **TEMP**                               
         MVC   DXHDRCDT+09(2),DXTIMEN    **TEMP**                               
         MVI   DXHDRCDT+11,C':'          **TEMP**                               
         MVC   DXHDRCDT+12(2),DXTIMEN+2  **TEMP**                               
         MVI   DXHDRCDT+14,C''''         **TEMP**                               
*******************************************TEMP**                               
         B     YES                                                              
         DROP  R3                                                               
         SPACE 1                                                                
***********************************************************************         
* INITIALISE AGENCY EXTRACT RECORD                                    *         
***********************************************************************         
         SPACE 1                                                                
INITSAG  NTR1                                                                   
         LA    R1,CXSAGDL          R1=L'AGENCY RECORD                           
         BAS   RE,INITALL                                                       
         B     YES                                                              
         SPACE 1                                                                
***********************************************************************         
* INITIALISE USER ID EXTRACT RECORD                                   *         
***********************************************************************         
         SPACE 1                                                                
INITSUI  NTR1                                                                   
         LA    R1,CXSUIDL          R1=L'USER ID RECORD                          
         BAS   RE,INITALL                                                       
         B     YES                                                              
         SPACE 1                                                                
***********************************************************************         
* INITIALISE PERSON PASSWORD RECORD                                   *         
***********************************************************************         
         SPACE 1                                                                
INITSPE  NTR1                                                                   
         LA    R1,CXSPEDL          R1=L'PERSON PASSWORD RECORD                  
         BAS   RE,INITALL                                                       
         B     YES                                                              
         SPACE 1                                                                
***********************************************************************         
* INITIALISE USER ID PROGRAM EXTRACT RECORD                           *         
***********************************************************************         
         SPACE 1                                                                
INITSUP  NTR1                                                                   
         LA    R1,CXSUPDL          R1=L'USER ID PROGRAM RECORD                  
         BAS   RE,INITALL                                                       
         B     YES                                                              
         SPACE 1                                                                
***********************************************************************         
* INITIALISE PERSON PROGRAM EXTRACT RECORD                            *         
***********************************************************************         
         SPACE 1                                                                
INITSPP  NTR1                                                                   
         LA    R1,CXSPPDL          R1=L'PERSON PROGRAM RECORD                   
         BAS   RE,INITALL                                                       
         B     YES                                                              
         SPACE 1                                                                
***********************************************************************         
* INITIALISE PERSON USERID XREF RECORD                                *         
***********************************************************************         
         SPACE 1                                                                
INITSPU  NTR1                                                                   
         LA    R1,CXSPUDL          R1=L'PERSON USERID XREF RECORD               
         BAS   RE,INITALL                                                       
         B     YES                                                              
         SPACE 1                                                                
***********************************************************************         
* INITIALISE AGENCY ACCESS AUTHORISATION LEVELS RECORD                *         
***********************************************************************         
         SPACE 1                                                                
INITSAA  NTR1                                                                   
         LA    R1,CXSAADL          R1=L'AGENCY AUTH LEVELS RECORD               
         BAS   RE,INITALL                                                       
         B     YES                                                              
         SPACE 1                                                                
***********************************************************************         
* INITIALISE ACCESS GROUP AUTHORISATION LEVELS RECORD                 *         
***********************************************************************         
         SPACE 1                                                                
INITSAL  NTR1                                                                   
         LA    R1,CXSALDL          R1=L'ACCESS GROUP AUTH LEVELS RECORD         
         BAS   RE,INITALL                                                       
         B     YES                                                              
         SPACE 1                                                                
***********************************************************************         
* INITIALISE FIELD CONTROL AGENCY LEVEL RECORD                        *         
***********************************************************************         
         SPACE 1                                                                
INITSFA  NTR1                                                                   
         LA    R1,CXSFADL          R1=L'FCONTROL AGENCY LEVEL RECORD            
         BAS   RE,INITALL                                                       
         B     YES                                                              
         SPACE 1                                                                
***********************************************************************         
* INITIALISE FIELD CONTROL ACCESS GROUP LEVEL RECORD                  *         
***********************************************************************         
         SPACE 1                                                                
INITSFG  NTR1                                                                   
         LA    R1,CXSFGDL          R1=L'FCONTROL ACCESS GROUP RECORD            
         BAS   RE,INITALL                                                       
         B     YES                                                              
         SPACE 1                                                                
***********************************************************************         
* INITIALISE OPTION CONTROL AGENCY LEVEL RECORD                       *         
***********************************************************************         
         SPACE 1                                                                
INITSOA  NTR1                                                                   
         LA    R1,CXSOADL          R1=L'OCONTROL AGENCY LEVEL RECORD            
         BAS   RE,INITALL                                                       
         B     YES                                                              
         SPACE 1                                                                
***********************************************************************         
* INITIALISE OPTION CONTROL ACCESS GROUP LEVEL RECORD                 *         
***********************************************************************         
         SPACE 1                                                                
INITSOG  NTR1                                                                   
         LA    R1,CXSOGDL          R1=L'OCONTROL ACCESS GROUP RECORD            
         BAS   RE,INITALL                                                       
         B     YES                                                              
         SPACE 1                                                                
***********************************************************************         
* INITIALISE ACCOUNT SYSTEM OFFICE LIST RECORD                        *         
***********************************************************************         
         SPACE 1                                                                
INITOLS  NTR1                                                                   
         LA    R1,CXOLSDL          R1=L'OFFICE LIST RECORD                      
         BAS   RE,INITALL                                                       
         B     YES                                                              
         SPACE 1                                                                
         SPACE 1                                                                
***********************************************************************         
* INITIALISE ACCOUNT SYSTEM OFFICE LIST ENTRY RECORD                  *         
***********************************************************************         
         SPACE 1                                                                
INITOLE  NTR1                                                                   
         LA    R1,CXOLEDL          R1=L'OFFICE LIST ENTRY RECORD                
         BAS   RE,INITALL                                                       
         B     YES                                                              
         SPACE 1                                                                
***********************************************************************         
* INITIALISE APPROVER GROUP NAME RECORD                               *         
***********************************************************************         
         SPACE 1                                                                
INITAPN  NTR1                                                                   
         LA    R1,CXAPNDL          R1=L'APPROVER GROUP NAME RECORD              
         BAS   RE,INITALL                                                       
         B     YES                                                              
         SPACE 1                                                                
***********************************************************************         
* INITIALISE APPROVER GROUP MANAGER RECORD                            *         
***********************************************************************         
         SPACE 1                                                                
INITAPM  NTR1                                                                   
         LA    R1,CXAPMDL          R1=L'APPROVER GROUP MANAGER RECORD           
         BAS   RE,INITALL                                                       
         B     YES                                                              
         SPACE 1                                                                
***********************************************************************         
* INITIALISE PERSON APPROVER GROUP MANAGER RECORD                     *         
***********************************************************************         
         SPACE 1                                                                
INITPAM  NTR1                                                                   
         LA    R1,CXPAMDL          R1=L'PERSON APPROVER MANAGER REC.            
         BAS   RE,INITALL                                                       
         B     YES                                                              
         SPACE 1                                                                
         EJECT                                                                  
***********************************************************************         
* SUBROUTINE TO EXTRACT CONTROL TEMPO RECORDS IN LOAD MODE            *         
* R2 = A(CONTROL SECURITY RECORD BUFFER)                              *         
* P1 = A(EXTRACT ROUTINE)                                             *         
* P2 = A(FORMAT CONVERT ROUTINE)                                      *         
* P3 = A(EXTRACT RECORD INITIALISATION ROUTINE)                       *         
* P4 = A(RECORD FILTER ROUTINE)                                       *         
***********************************************************************         
         SPACE 1                                                                
CXLOAD   NTR1                                                                   
         LM    R3,R6,0(R1)                                                      
         LTR   R6,R6               TEST IF FILTER ROUTINE PASSED                
         BZ    CXLO010                                                          
         GOTO1 (R6)                FILTER RECORD, GET NEXT IF NOT VALID         
         BNE   CXLO050                                                          
*                                                                               
CXLO010  EQU   *                                                                
         GOTO1 (R5)                INITIALISE EXTRACT BUFFER                    
*                                  CALL RECORD EXTRACT ROUTINE                  
         GOTO1 (R3),DMCB,DXAXREC,(R2),AENCKEY,AGYBIN                            
         MVC   BYTE,DMCB+8         SAVE DMCB+8                                  
         NI    DMCB+8,X'FF'-X'01'                                               
         CLI   DMCB+8,X'88'                                                     
         BE    CXLO040             TEST NO CALL BACK                            
         TM    DMCB+8,X'80'                                                     
         BO    CXLO050             TEST NOT TO WRITE THIS RECORD                
         CLI   DXWRITE,C'Y'                                                     
         BNE   CXLO050                                                          
         CLI   PLATFORM,0                                                       
         BE    CXLO020                                                          
*                                  CONVERT RECORD TO SQL BUFFER                 
         GOTO1 (R4),DMCB,DXAXREC,DXASQLB,0                                      
*                                                                               
         GOTO1 DXPUT,DMCB,DXASQLB,(R7)                                          
         B     CXLO030                                                          
*                                                                               
CXLO020  GOTO1 DXPUT,DMCB,DXAXREC,(R7)                                          
*                                                                               
CXLO030  TM    BYTE,X'40'          TEST IF CALL BACK REQUIRED                   
         BO    CXLO010                                                          
*                                                                               
CXLO040  BAS   RE,DECIOC           DECREMENT MAX IO COUNTER                     
         BNE   CXLOOK                                                           
*                                                                               
CXLO050  EQU   *                   READ NEXT RECORD                             
         MVC   IOKEY(L'IOKEY),0(R2)                                             
         TM    BYTE,X'01'                                                       
         BZ    CXLO060             TEST READ SEQUENCE BROKEN                    
         GOTO1 VDATAMGR,DMCB,(X'00',DMRDHI),CTFILE,IOKEY,(R2)                   
         TM    8(R1),X'FD'                                                      
         BZ    *+6                                                              
         DC    H'0'                                                             
CXLO060  GOTO1 VDATAMGR,DMCB,(X'00',DMRSEQ),CTFILE,IOKEY,(R2)                   
         B     CXLOOK                                                           
*                                                                               
CXLONO   B     NO                                                               
CXLOOK   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* SUBROUTINE TO EXTRACT CONTROL TEMPO RECORD DATA IN UPDATE MODE      *         
* R2 = A(CONTROL SECURITY RECORD BUFFER)                              *         
* P1 = A(EXTRACT ROUTINE)                                             *         
* P2 = A(FORMAT CONVERT ROUTINE)                                      *         
* P3 = A(EXTRACT RECORD INITIALISATION ROUTINE)                       *         
***********************************************************************         
         SPACE 1                                                                
CXUPDT   NTR1                                                                   
         LM    R3,R5,0(R1)                                                      
*                                  INITILAISE EXTRACT BUFFER                    
CXUP010  GOTO1 (R5)                                                             
*                                  EXTRACT RECORD DATA                          
         GOTO1 (R3),DMCB,DXAXREC,(R2),AENCKEY,AGYBIN                            
         NI    DMCB+8,X'FF'-X'01'  ??                                           
         CLI   DMCB+8,X'88'                                                     
         BE    CXUPOK              TEST NO CALL BACK                            
         TM    DMCB+8,X'80'                                                     
         BO    CXUPOK              TEST NOT TO WRITE THIS RECORD                
         MVC   BYTE,DMCB+8         SAVE DMCB+8 FOR CALL BACK TEST               
         CLI   DXWRITE,C'Y'                                                     
         BNE   CXUPOK                                                           
         CLI   PLATFORM,0                                                       
         BE    CXUP020                                                          
*                                  CONVERT RECORD TO SQL BUFFER                 
         GOTO1 (R4),DMCB,DXAXREC,DXASQLB,0                                      
*                                                                               
         GOTO1 DXPUT,DMCB,DXASQLB,(R7)                                          
         B     CXUP030                                                          
*                                                                               
CXUP020  GOTO1 DXPUT,DMCB,DXAXREC,(R7)                                          
*                                                                               
CXUP030  TM    BYTE,X'40'          TEST IF CALL BACK REQUIRED                   
         BO    CXUP010                                                          
         B     CXUPOK                                                           
*                                                                               
CXUPNO   B     NO                                                               
CXUPOK   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* DECREMENT MAXIMUM IO COUNT                                          *         
***********************************************************************         
         SPACE 1                                                                
DECIOC   NTR1                                                                   
         ICM   RF,15,MAXIOS                                                     
         BCTR  RF,0                                                             
         STCM  RF,15,MAXIOS                                                     
         BZ    NO                                                               
         B     YES                                                              
         SPACE 1                                                                
***********************************************************************         
* CHECK SEQUENTIAL IO BROKEN, LAST KEY IN IOKEY                       *         
***********************************************************************         
         SPACE 1                                                                
CHKSEQIO NTR1                                                                   
         L     RE,DTFADDR                                                       
         USING ISDTF,RE                                                         
         L     RE,ISPDKEY                                                       
         CLC   IOKEY(20),0(RE)                                                  
         BNE   NO                                                               
         B     YES                                                              
         DROP  RE                                                               
         EJECT                                                                  
***********************************************************************         
* INITIALISE SYSTEM PROGRAM ACCESS TABLE FROM SECURITY RECORDS        *         
***********************************************************************         
         SPACE 1                                                                
INSPATAB NTR1                                                                   
         L     R4,=A(CXSSPAT)                                                   
         USING SPATABD,R4                                                       
*                                                                               
         LA    R2,IOKEY            READ PROGRAM RECORD                          
         USING SAPGREC,R2            TO EXTRACT ACTION CODE LIST                
         XC    SAPGKEY,SAPGKEY                                                  
         MVI   SAPGTYP,SAPGTYPQ                                                 
         MVI   SAPGSUB,SAPGSUBQ                                                 
*                                                                               
ISPA010  MVC   IO(L'SAPGKEY),IOKEY                                              
         GOTO1 VDATAMGR,DMCB,(X'00',DMRDHI),CTFILE,IO,IO                        
         CLI   8(R1),0                                                          
         BE    ISPA020                                                          
         DC    H'00'                                                            
         B     ISPA100                                                          
*                                                                               
ISPA012  MVC   IO(L'SAPGKEY),IOKEY                                              
         GOTO1 VDATAMGR,DMCB,(X'00',DMREAD),CTFILE,IO,IO                        
         CLI   8(R1),0                                                          
         BE    ISPA014                                                          
         DC    H'00'                                                            
*                                                                               
ISPA014  GOTO1 VDATAMGR,DMCB,(X'00',DMRSEQ),CTFILE,IO,IO                        
         CLI   8(R1),0                                                          
         BE    ISPA020                                                          
         DC    H'00'                                                            
         B     ISPA100                                                          
*                                                                               
ISPA020  LA    R2,IO                                                            
         CLC   SAPGKEY(SAPGOVPG-SAPGKEY),IOKEY                                  
         BNE   ISPA100                                                          
         MVC   IOKEY(L'SAPGKEY),SAPGKEY                                         
         MVC   SPATOVPG,SAPGOVPG                                                
         LA    R3,SAPGDATA                                                      
         SR    RF,RF                                                            
         USING SAPGMD,R3                                                        
*                                                                               
ISPA030  CLI   SAPGMEL,0                                                        
         BNE   *+6                                                              
         DC    H'00'                                                            
         CLI   SAPGMEL,SAPGMELQ                                                 
         BE    *+14                                                             
         IC    RF,SAPGMLN                                                       
         AR    R3,RF                                                            
         B     ISPA030                                                          
*                                                                               
         MVC   IOKEYSV,IOKEY       SAVE IOKEY                                   
*&&UK                                                                           
         CLC   SAPGOVPG(2),=XL2'0A0D'                                           
         BE    *+12                                                             
*&&                                                                             
         TM    SAPGMIND,SAPGMIPC                                                
         BZ    ISPA012             IGNORE NON PC PROGRAMS                       
         CLC   SAPGOVPG(2),=XL2'0A0D'                                           
         MVC   SPATPACL,SAPGMACT                                                
         DROP  R3                                                               
*                                                                               
         BAS   RE,INFCLST          INITIALISE FIELD CONTROL LIST                
         MVC   IOKEY,IOKEYSV       RESTORE IOKEY                                
         BAS   RE,INOCLST          INITIALISE OPTION CONTROL LIST               
         MVC   IOKEY,IOKEYSV       RESTORE IOKEY                                
         BAS   RE,INDUMACC         INITIALISE DUMMY ACCESS RECORD BLOCK         
         MVC   IOKEY,IOKEYSV       RESTORE IOKEY                                
*                                                                               
         LA    R4,SPATLEN(R4)                                                   
         C     R4,=A(CXSSPATX)                                                  
         BL    ISPA012             GET NEXT RECORD/READ SEQUENCE BROKEN         
         DC    H'00'                 OR DIE IF TABLE OVERFLOW                   
*                                                                               
ISPA100  LR    RE,R4                                                            
         LA    RF,SPATLEN                                                       
         XCEF                                                                   
         B     ISPAOK                                                           
*                                                                               
ISPANO   B     NO                                                               
ISPAOK   B     YES                                                              
         DROP  R2,R4                                                            
         EJECT                                                                  
***********************************************************************         
* INITIALISE FIELD CONTROL LIST DEFAULT BIT SETTINGS                  *         
* R4=A(TABLE ENTRY)                                                   *         
***********************************************************************         
         SPACE 1                                                                
INFCLST  NTR1                                                                   
         USING SPATABD,R4                                                       
         USING SAFDREC,R2          READ FIELD DEFINITION RECORDS                
         LA    R2,IOKEY              FOR THIS SYSTEM AND PROGRAM                
*                                                                               
         MVI   SAFDSUB,SAFDSUBQ                                                 
         LA    R2,IO                                                            
         XC    SPATFCL,SPATFCL                                                  
*                                                                               
         MVC   IO(L'SAFDKEY),IOKEY                                              
         GOTO1 VDATAMGR,DMCB,(X'00',DMRDHI),CTFILE,IO,IO                        
         CLI   8(R1),0                                                          
         BE    IFCL020                                                          
         DC    H'00'                                                            
*                                                                               
IFCL010  GOTO1 VDATAMGR,DMCB,(X'00',DMRSEQ),CTFILE,IO,IO                        
         CLI   8(R1),0                                                          
         BE    IFCL020                                                          
         DC    H'00'                                                            
*                                                                               
IFCL020  CLC   SAFDKEY(SAFDFCD-SAFDREC),IOKEY                                   
         BNE   IFCL040                                                          
         MVC   IOKEY(L'SAFDKEY),IO                                              
*                                                                               
         LA    R1,SPATFCL                                                       
         SR    R0,R0                                                            
         IC    R0,SAFDFCD                                                       
         LR    RF,R0               R1=A(BYTE OF BIT)                            
         SRL   RF,3                                                             
         AR    R1,RF                                                            
*                                                                               
         LA    RF,X'07'            RF=MASK                                      
         NR    RF,R0                                                            
         IC    RF,MASKS(RF)                                                     
*                                                                               
         EX    RF,*+8              EX A OI                                      
         B     *+8                                                              
         OI    0(R1),0             A OI                                         
         B     IFCL010                                                          
*                                                                               
IFCL040  EQU   *                                                                
         B     IFCLOK                                                           
*                                                                               
IFCLNO   B     NO                                                               
IFCLOK   B     YES                                                              
         DROP  R2,R4                                                            
         EJECT                                                                  
***********************************************************************         
* INITIALISE OPTION CONTROL LIST DEFAULT BIT SETTINGS                 *         
* R4=A(TABLE ENTRY)                                                   *         
***********************************************************************         
         SPACE 1                                                                
INOCLST  NTR1                                                                   
         USING SPATABD,R4                                                       
         USING SAOPREC,R2          READ OPTION DEFINITION RECORDS               
         LA    R2,IOKEY              FOR THIS SYSTEM AND PROGRAM                
*                                                                               
         MVI   SAOPSUB,SAOPSUBQ                                                 
         LA    R2,IO                                                            
         XC    SPATOCL,SPATOCL                                                  
*                                                                               
         MVC   IO(L'SAOPKEY),IOKEY                                              
         GOTO1 VDATAMGR,DMCB,(X'00',DMRDHI),CTFILE,IO,IO                        
         CLI   8(R1),0                                                          
         BE    IOCL020                                                          
         DC    H'00'                                                            
*                                                                               
IOCL010  GOTO1 VDATAMGR,DMCB,(X'00',DMRSEQ),CTFILE,IO,IO                        
         CLI   8(R1),0                                                          
         BE    IOCL020                                                          
         DC    H'00'                                                            
*                                                                               
IOCL020  CLC   SAOPKEY(SAOPOCD-SAOPREC),IOKEY                                   
         BNE   IOCL040                                                          
         MVC   IOKEY(L'SAOPKEY),IO                                              
*                                                                               
         LA    R1,SPATOCL                                                       
         SR    R0,R0                                                            
         IC    R0,SAOPOCD                                                       
         LR    RF,R0               R1=A(BYTE OF BIT)                            
         SRL   RF,3                                                             
         AR    R1,RF                                                            
*                                                                               
         LA    RF,X'07'            RF=MASK                                      
         NR    RF,R0                                                            
         IC    RF,MASKS(RF)                                                     
*                                                                               
         EX    RF,*+8              EX A OI                                      
         B     *+8                                                              
         OI    0(R1),0             A OI                                         
         B     IOCL010                                                          
*                                                                               
IOCL040  EQU   *                                                                
         B     IOCLOK                                                           
*                                                                               
IOCLNO   B     NO                                                               
IOCLOK   B     YES                                                              
         DROP  R2,R4                                                            
         EJECT                                                                  
***********************************************************************         
* INITIALISE SYSTEM PROGRAM ACCESS TABLE DUJMY ACCESS RECORD BLOCK    *         
* R4=A(TABLE ENTRY)                                                   *         
***********************************************************************         
         SPACE 1                                                                
INDUMACC NTR1                                                                   
         USING SPATABD,R4                                                       
         USING SARCREC,R2          SET UP DUMMY ACCESS RECORD                   
         LA    R2,IOKEY                                                         
*                                    FROM RECORD RECORDS                        
         MVI   SARCSUB,SARCSUBQ                                                 
         MVC   SPATAREC(L'SARCKEY),0(R2)                                        
         LA    R2,IO                                                            
*                                                                               
         LA    R5,SPATAREC+SAASDATA-SAASREC                                     
         USING SAMIXD,R5           R4=A(RECORD/ACTIONS ELEMENT)                 
*                                                                               
         MVC   IO(L'SARCKEY),IOKEY                                              
         GOTO1 VDATAMGR,DMCB,(X'00',DMRDHI),CTFILE,IO,IO                        
         CLI   8(R1),0                                                          
         BE    IDUM020                                                          
         DC    H'00'                                                            
*                                                                               
IDUM010  GOTO1 VDATAMGR,DMCB,(X'00',DMRSEQ),CTFILE,IO,IO                        
         CLI   8(R1),0                                                          
         BE    IDUM020                                                          
         DC    H'00'                                                            
*                                                                               
IDUM020  CLC   SARCKEY(SARCRCD-SARCREC),IOKEY                                   
         BNE   IDUM040                                                          
         MVC   IOKEY(L'SARCKEY),IO                                              
*                                                                               
         GOTO1 FINDEL,SARCDELQ     COPY ELEMENT                                 
         USING SARCDD,R3                                                        
         CLI   SARCDATT,0          TEST RECORD IS ATTACHED                      
         BE    IDUM030                                                          
         GOTO1 FINDACTS,DMCB,(SARCDATT,SPATAREC)                                
         DROP  R3                                                               
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   SAMIXD(SAMIXLNQ+L'SAMIXACT),0(R3)                                
         MVC   SAMIXATT,SAMIXRCD                                                
         MVC   SAMIXRCD,SARCRCD                                                 
         AR    R5,RF                                                            
         B     IDUM010                                                          
*                                                                               
IDUM030  GOTO1 FINDEL,SAMIXELQ                                                  
         MVC   SAMIXD(SAMIXLNQ+L'SAMIXACT),0(R3)                                
         AR    R5,RF                                                            
         B     IDUM010                                                          
*                                                                               
IDUM040  MVI   SAMIXEL,0           SET E-O-R                                    
         B     IDUMOK                                                           
*                                                                               
IDUMNO   B     NO                                                               
IDUMOK   B     YES                                                              
         DROP  R2,R4,R5                                                         
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO FIND SAMIXD ELEMENT IN RECORD                            *         
*                                                                     *         
* NTRY: P1=(RECORD TYPE, A(ACCESS RECORD))                            *         
*                                                                     *         
* EXIT: IF FOUND:     CC=EQUAL,       FACTBITT=BIT MASK FOR ACTIONS   *         
*                     R3=A(ELEMENT),  RF=L(ELEMENT)                   *         
*       IF NOT FOUND: CC=NOT EQUAL,   FACTBITT=ZERO                   *         
***********************************************************************         
         SPACE 1                                                                
FINDACTS XR    R3,R3                                                            
         ICM   R3,7,1(R1)                                                       
         LA    R3,SAASDATA-SAASREC(R3)                                          
         USING SAMIXD,R3           R3=A(RECORD/ACTIONS ELEMENT)                 
*                                                                               
         XC    FACTBITT,FACTBITT                                                
         XR    RF,RF                                                            
*                                                                               
FACTS2   CLI   SAMIXEL,0           TEST E-O-R                                   
         BE    FINDACTN                                                         
*                                                                               
         IC    RF,SAMIXLN                                                       
         CLI   SAMIXEL,SAMIXELQ    MATCH ON ELEMENT CODE                        
         BNE   FACTS4                                                           
         CLC   SAMIXRCD,0(R1)      MATCH ON RECORD TYPE                         
         BE    FINDACTY                                                         
         BH    FINDACTN                                                         
*                                                                               
FACTS4   BXH   R3,RF,FACTS2        BUMP R3 TO NEXT ELEMENT                      
*                                                                               
FINDACTY SH    RF,=Y(SAMIXLNQ+1)   COPY TABLE                                   
         EX    RF,*+4                                                           
         MVC   FACTBITT(0),SAMIXACT                                             
         LA    RF,SAMIXLNQ+1(RF)                                                
         CR    RE,RE                                                            
         BR    RE                                                               
*                                                                               
FINDACTN LTR   RE,RE               EXIT WITH CC=NOT EQUAL                       
         BR    RE                                                               
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO FIND AN ELEMENT IN IOAREA                                *         
*                                                                     *         
* NTRY: R1=ELEMENT CODE                                               *         
* EXIT: R3=A(ELEMENT), RF=L(ELEMENT)                                  *         
***********************************************************************         
         SPACE 1                                                                
FINDEL   LA    R3,IO+(SAASDATA-SAASREC)                                         
         XR    RF,RF                                                            
         SPACE 1                                                                
FEL2     CLI   0(R3),0             TEST E-O-R                                   
         BE    FELOVER                                                          
         SPACE 1                                                                
         IC    RF,1(R3)            RF=L(ELEMENT)                                
         CLM   R1,1,0(R3)          MATCH ON ELEMENT CODE                        
         BER   RE                                                               
         SPACE 1                                                                
NEXTEL   BXH   R3,RF,FEL2          BUMP R3 TO NEXT ELEMENT                      
         SPACE 1                                                                
FELOVER  SR    RE,RB                                                            
         DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
* INITIALISE MASTER SECURITY AGENCY ALPHA ID TABLE                    *         
***********************************************************************         
         SPACE 1                                                                
INSAGTAB NTR1                                                                   
         L     R5,DXSTPTR          R5=A(EXTRACT SYSTEM TABLE ENTRY)             
         USING SXDTABD,R5                                                       
         L     R4,=A(CXSSAGT)      R4=A(SECURITY AGENCY ID TABLE)               
         USING SAGTABD,R4                                                       
*                                                                               
ISAG010  C     R5,DXSTEPTR         EXIT AT END OF SYSTEM TABLE                  
         BE    ISAGOK                                                           
         LR    RE,R4               CLEAR SAGTAB ENTRY                           
         LA    RF,SAGTLEN                                                       
         XCEF                                                                   
         CLI   SXDTSUB,X'01'                                                    
         BNE   ISAG110                                                          
         MVC   AGENCYID,SXDTAGY                                                 
         MVC   SAGMID,AGENCYID                                                  
*                                                                               
         LA    R6,SAGAID                                                        
* ??     DROP  R4                                                               
         USING SAGAID,R6                                                        
*                                                                               
         LA    R2,IOKEY            READ USER ID RECORD                          
         USING CT5KEY,R2             TO EXTRACT ASSOCIATED AGENCIES             
         XC    CT5KEY,CT5KEY                                                    
         MVI   CT5KTYP,CT5KTYPQ                                                 
*                                                                               
ISAG020  MVC   IO(L'CT5KEY),IOKEY                                               
         GOTO1 VDATAMGR,DMCB,(X'00',DMRDHI),CTFILE,IO,IO                        
         CLI   8(R1),0                                                          
         BE    ISAG030                                                          
         DC    H'00'                                                            
         B     ISAG100                                                          
*                                                                               
ISAG022  MVC   IO(L'CT5KEY),IOKEY                                               
         GOTO1 VDATAMGR,DMCB,(X'00',DMREAD),CTFILE,IO,IO                        
         CLI   8(R1),0                                                          
         BE    ISAG024                                                          
         DC    H'00'                                                            
*                                                                               
ISAG024  GOTO1 VDATAMGR,DMCB,(X'00',DMRSEQ),CTFILE,IO,IO                        
         CLI   8(R1),0                                                          
         BE    ISAG030                                                          
         DC    H'00'                                                            
         B     ISAG100                                                          
*                                                                               
ISAG030  LA    R2,IO                                                            
         CLI   CT5KTYP,CT5KTYPQ                                                 
         BNE   ISAG100                                                          
         MVC   IOKEY(L'CT5KEY),CT5KEY                                           
*                                  CHECK SECURITY AGENCY ID                     
         BAS   RE,GETSAGY                                                       
         CLC   SECAGY,AGENCYID                                                  
         BNE   ISAG022                                                          
         MVC   SAGAID,CT5KALPH                                                  
         LA    R6,SAGALEN(R6)                                                   
         B     ISAG022                                                          
*                                                                               
ISAG100  LA    R4,SAGTLEN(R4)                                                   
         C     R4,=A(CXSSAGTX)                                                  
         BL    *+6                                                              
         DC    H'0'                                                             
*                                                                               
ISAG110  LA    R5,SXDTABL(R5)      GET NEXT SYSTEM TABLE ENTRY                  
         B     ISAG010                                                          
*                                                                               
ISAGNO   B     NO                                                               
ISAGOK   B     YES                                                              
         DROP  R2,R4                                                            
         EJECT                                                                  
***********************************************************************         
* INITIALISE AGENCY USER ID ACCESS TABLE                              *         
***********************************************************************         
         SPACE 1                                                                
INAGUTAB NTR1                                                                   
         L     R5,DXSTPTR          R5=A(EXTRACT SYSTEM TABLE ENTRY)             
         USING SXDTABD,R5                                                       
         L     R4,=A(CXSAGUT)      R4=A(AGENCY USERID ACCESS TABLE)             
         USING AGUTABD,R4                                                       
*                                                                               
IAGU010  C     R5,DXSTEPTR         EXIT AT END OF SYSTEM TABLE                  
         BE    IAGUOK                                                           
         LR    RE,R4               CLEAR AGUTAB ENTRY                           
         LA    RF,AGUTLEN                                                       
         XCEF                                                                   
         CLI   SXDTSUB,X'01'                                                    
         BNE   IAGU110                                                          
         MVC   AGENCYID,SXDTAGY                                                 
         MVC   AGUAGYID,AGENCYID                                                
         MVC   AGUSAGID,AGENCYID                                                
*                                                                               
         LA    R6,AGUUID                                                        
* ??     DROP  R4                                                               
         USING AGUUID,R6                                                        
*                                                                               
         LA    R2,IOKEY            READ USER ID RECORD                          
         USING CTIKEY,R2             TO EXTRACT ASSOCIATED AGENCIES             
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKNUM,=XL2'FFFF'                                               
*                                                                               
IAGU020  MVC   IO(L'CTIKEY),IOKEY                                               
         GOTO1 VDATAMGR,DMCB,(X'00',DMRDHI),CTFILE,IO,IO                        
         CLI   8(R1),0                                                          
         BE    IAGU030                                                          
         DC    H'00'                                                            
         B     IAGU100                                                          
*                                                                               
IAGU022  MVC   IO(L'CTIKEY),IOKEY                                               
         GOTO1 VDATAMGR,DMCB,(X'00',DMREAD),CTFILE,IO,IO                        
         CLI   8(R1),0                                                          
         BE    IAGU024                                                          
         DC    H'00'                                                            
*                                                                               
IAGU024  GOTO1 VDATAMGR,DMCB,(X'00',DMRSEQ),CTFILE,IO,IO                        
         CLI   8(R1),0                                                          
         BE    IAGU030                                                          
         DC    H'00'                                                            
         B     IAGU100                                                          
*                                                                               
IAGU030  LA    R2,IO                                                            
         CLI   CTIKTYP,CTIKTYPQ                                                 
         BNE   IAGU100                                                          
         MVC   IOKEY(L'CTIKEY),CTIKEY                                           
*                                  CHECK USERID AGENCY ALPHA ID                 
         BAS   RE,GETUAGY                                                       
         BNE   IAGU022                                                          
         GOTO1 CHKSAGY,USERAGY                                                  
         BNE   IAGU022                                                          
*        CLC   USERAGY,AGENCYID                                                 
         MVC   AGUUID,CTIKID                                                    
         MVI   AGUVAL,C'N'                                                      
*                                  GET USERID NUMBER                            
         LA    R3,CTIDATA                                                       
         SR    RF,RF                                                            
*                                                                               
IAGU050  CLI   0(R3),0                                                          
         BNE   *+6                                                              
         DC    H'00'                                                            
         CLI   0(R3),X'02'                                                      
         BE    *+14                                                             
         IC    RF,1(R3)                                                         
         AR    R3,RF                                                            
         B     IAGU050                                                          
         MVC   AGUUIN,2(R3)                                                     
*                                                                               
         LA    R6,AGUULEN(R6)                                                   
         B     IAGU022                                                          
*                                                                               
IAGU100  LA    R4,AGUTLEN(R4)                                                   
         C     R4,=A(CXSAGUTX)                                                  
         BL    *+6                                                              
         DC    H'0'                                                             
*                                                                               
IAGU110  LA    R5,SXDTABL(R5)      GET NEXT SYSTEM TABLE ENTRY                  
         B     IAGU010                                                          
*                                                                               
IAGUNO   B     NO                                                               
IAGUOK   B     YES                                                              
         DROP  R2,R4                                                            
         EJECT                                                                  
***********************************************************************         
* GET MASTER SECURITY AGENCY ALPHA ID FROM AGENCY ALPHA ACCESS RECORD *         
* R2=A(AGENCY ALPHA ACCESS RECORD)                                    *         
* SECAGY=MASTER SECURITY AGENCY ALPHA ID                              *         
***********************************************************************         
         SPACE 1                                                                
         USING CT5REC,R2                                                        
GETSAGY  NTR1                                                                   
         MVC   SECAGY,CT5KALPH                                                  
         LA    R3,CT5DATA                                                       
         SR    RF,RF                                                            
GSAG010  CLI   0(R3),0                                                          
         BE    GAGYNO                                                           
         CLI   0(R3),CTSEAELQ                                                   
         BE    *+14                                                             
         IC    RF,1(R3)                                                         
         AR    R3,RF                                                            
         B     GSAG010                                                          
         USING CTSEAD,R3                                                        
         MVC   SECAGY,CTSEAAID                                                  
         B     GSAGOK                                                           
GSAGNO   B     NO                                                               
GSAGOK   B     YES                                                              
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* GET AGENCY ALPHA ID FROM USERID RECORD                              *         
* R2=A(USER ID RECORD)                                                *         
* USERAGY=AGENCY ALPHA ID                                             *         
***********************************************************************         
         SPACE 1                                                                
         USING CTIREC,R2                                                        
GETUAGY  NTR1                                                                   
         LA    R3,CTIDATA                                                       
         SR    RF,RF                                                            
GAGY010  CLI   0(R3),0                                                          
         BE    GAGYNO                                                           
         CLI   0(R3),X'06'                                                      
         BE    *+14                                                             
         IC    RF,1(R3)                                                         
         AR    R3,RF                                                            
         B     GAGY010                                                          
         MVC   USERAGY,2(R3)                                                    
         B     GAGYOK                                                           
GAGYNO   B     NO                                                               
GAGYOK   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* CHECK AGENCY ALPHA IM MASTER SECURITY AGENCYID LIST                 *         
* R1=A(AGENCY ALPHA ID)                                               *         
* AGENCYID=MASTER SECURITY AGENCY ALPHA ID                            *         
***********************************************************************         
         SPACE 1                                                                
         USING SAGTABD,R4                                                       
CHKSAGY  NTR1                                                                   
         ICM   R4,15,=A(CXSSAGT)   R4=A(SECURITY AGENCY ID TABLE)               
         BNZ   *+6                                                              
         DC    H'00'                                                            
*                                                                               
CSAG010  CLI   SAGMID,0                                                         
         BNE   *+6                   SYSTEM PROGRAM CODE IN RECORD              
         DC    H'00'                                                            
         CLC   SAGMID,AGENCYID                                                  
         BE    *+12                                                             
         LA    R4,SAGTLEN(R4)                                                   
         B     CSAG010                                                          
*                                                                               
         USING SAGAID,R4                                                        
CSAG020  CLI   SAGAID,0                                                         
         BE    CSAGNO                                                           
         CLC   SAGAID,0(R1)                                                     
         BE    CSAGOK                                                           
         LA    R4,SAGALEN(R4)                                                   
         C     R4,=A(CXSSAGTX)                                                  
         BL    CSAG020                                                          
         DC    H'0'                                                             
*                                                                               
CSAGNO   B     NO                                                               
CSAGOK   B     YES                                                              
         DROP  R4                                                               
         SPACE 1                                                                
*                                                                               
YES      SR    RC,RC               RETURN CC EQUAL                              
NO       LTR   RC,RC               RETURN CC NOT EQUAL                          
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
         SPACE 2                                                                
MXTRTQ   EQU   X'5E'               FIELD SEPERATOR CHR                          
         SPACE 1                                                                
SPACES   DC    80C' '                                                           
FFILL    DC    32X'FF'                                                          
MASKS    DC    X'8040201008040201'                                              
*                                                                               
         DS    0D                                                               
SSB      DC    X'0000',X'FF',X'02' FOR DATAMGR (OFFLINE NO RECOVERY)            
         DC    250X'00'                                                         
*                                                                               
* SUB SYTEM CONTROL ROUTINE TABLES                                              
* AL1    TYPE NUMBER                                                            
* XL1    TYPE FLAGS                                                             
* CL3    TYPE NAME                                                              
* AL4    LOAD ROUTINE ADDRESS                                                   
* AL4    UPDATE ROUTINE ADDRESS                                                 
*                                                                               
TACTION  EQU   0                                                                
TRECTYP  EQU   1                                                                
TNAME    EQU   2                                                                
TMEDIA   EQU   5                                                                
TALOAD   EQU   8                                                                
TAUPDT   EQU   12                                                               
SUBTABL  EQU   16                                                               
         SPACE 2                                                                
* SECTAB DEFINES SECURITY SUB SYSTEM ROUTINES                                   
         DS    0D                                                               
SECTAB   DS    0CL16                                                            
         DC    AL1(01),AL1(0)                                                   
         DC    CL3'ALL',C'   ',AL4(LOADSEC),AL4(UPDTALL)                        
         DC    AL1(02),AL1(CT5KTYPQ)                                            
         DC    CL3'AGY',C'   ',AL4(LOADSAG),AL4(UPDTSAG)                        
         DC    AL1(03),AL1(CTIKTYPQ)                                            
         DC    CL3'UID',C'   ',AL4(LOADSUI),AL4(UPDTSUI)                        
         DC    AL1(04),AL1(CTIKTYPQ)                                            
         DC    CL3'UPG',C'   ',AL4(LOADSUP),AL4(UPDTSUP)                        
         DC    AL1(05),AL1(SA0KTYPQ)                                            
         DC    CL3'PPG',C'   ',AL4(LOADSPP),AL4(UPDTSPP)                        
         DC    AL1(06),AL1(SA0KTYPQ)                                            
         DC    CL3'PUX',C'   ',AL4(LOADSPU),AL4(UPDTSPU)                        
         DC    AL1(07),AL1(SAPETYPQ)                                            
         DC    CL3'PEP',C'   ',AL4(LOADSPE),AL4(UPDTSPE)                        
         DC    AL1(08),AL1(SAASTYPQ)                                            
         DC    CL3'AAL',C'   ',AL4(LOADSAA),AL4(UPDTSAA)                        
         DC    AL1(09),AL1(SAASTYPQ)                                            
         DC    CL3'AGL',C'   ',AL4(LOADSAL),AL4(UPDTSAL)                        
         DC    AL1(10),AL1(SAFCTYPQ)                                            
         DC    CL3'FCA',C'   ',AL4(LOADSFA),AL4(UPDTSFA)                        
         DC    AL1(11),AL1(SAFCTYPQ)                                            
         DC    CL3'FCG',C'   ',AL4(LOADSFG),AL4(UPDTSFG)                        
         DC    AL1(12),AL1(SAOCTYPQ)                                            
         DC    CL3'OCA',C'   ',AL4(LOADSOA),AL4(UPDTSOA)                        
         DC    AL1(13),AL1(SAOCTYPQ)                                            
         DC    CL3'OCG',C'   ',AL4(LOADSOG),AL4(UPDTSOG)                        
         DC    AL1(14),AL1(SA0KTYPQ)                                            
         DC    CL3'XXX',C'   ',AL4(0),AL4(UPDTSPW)                              
SECTABX  DC    AL1(00),X'00',CL3'   ',C'   '                                    
         SPACE 2                                                                
* TEMTAB DEFINES TEMPO SUB SYSTEM ROUTINES                                      
         DS    0D                                                               
TEMTAB   DS    0CL16                                                            
         DC    AL1(01),AL1(0)                                                   
         DC    CL3'ALL',C'   ',AL4(LOADTEM),AL4(UPDTALL)                        
         DC    AL1(02),AL1(CTUKTYPQ)                                            
         DC    CL3'OLS',C'   ',AL4(LOADOLS),AL4(UPDTOLS)                        
         DC    AL1(03),AL1(CTUKTYPQ)                                            
         DC    CL3'OLE',C'   ',AL4(LOADOLE),AL4(UPDTOLE)                        
         DC    AL1(04),AL1(SAAPTYPQ)                                            
         DC    CL3'APN',C'   ',AL4(LOADAPN),AL4(UPDTAPN)                        
         DC    AL1(05),AL1(SAAPTYPQ)                                            
         DC    CL3'APM',C'   ',AL4(LOADAPM),AL4(UPDTAPM)                        
         DC    AL1(06),AL1(SAPETYPQ)                                            
         DC    CL3'PAM',C'   ',AL4(LOADPAM),AL4(UPDTPAM)                        
TEMTABX  DC    AL1(00),X'00',CL3'   ',C'   '                                    
         SPACE 2                                                                
* PRETAB DEFINES TEMPO SUB SYSTEM ROUTINES                                      
         DS    0D                                                               
PRETAB   DS    0CL16                                                            
         DC    AL1(01),AL1(0)                                                   
         DC    CL3'ALL',C'   ',AL4(LOADPRE),AL4(UPDTALL)                        
         DC    AL1(02),AL1(CTUKTYPQ)                                            
         DC    CL3'OLS',C'   ',AL4(LOADOLS),AL4(UPDTOLS)                        
         DC    AL1(03),AL1(CTUKTYPQ)                                            
         DC    CL3'OLE',C'   ',AL4(LOADOLE),AL4(UPDTOLE)                        
PRETABX  DC    AL1(00),X'00',CL3'   ',C'   '                                    
         EJECT                                                                  
         SPACE 1                                                                
VDATAMGR DC    V(DATAMGR)                                                       
VDMOD000 DC    V(DMOD000)                                                       
VDADDS   DC    V(DADDS)                                                         
VLOGIO   DC    V(LOGIO)                                                         
VDATCON  DC    V(DATCON)                                                        
VHEXOUT  DC    V(HEXOUT)                                                        
VCXSAGC  DC    V(CXSAGC)                                                        
VCXSUIC  DC    V(CXSUIC)                                                        
VCXSUPC  DC    V(CXSUPC)                                                        
VCXSPPC  DC    V(CXSPPC)                                                        
VCXSPUC  DC    V(CXSPUC)                                                        
VCXSPEC  DC    V(CXSPEC)                                                        
VCXSAAC  DC    V(CXSAAC)                                                        
VCXSALC  DC    V(CXSALC)                                                        
VCXSFAC  DC    V(CXSFAC)                                                        
VCXSFGC  DC    V(CXSFGC)                                                        
VCXSOAC  DC    V(CXSOAC)                                                        
VCXSOGC  DC    V(CXSOGC)                                                        
VCXOLSC  DC    V(CXOLSC)                                                        
VCXOLEC  DC    V(CXOLEC)                                                        
VCXAPNC  DC    V(CXAPNC)                                                        
VCXAPMC  DC    V(CXAPMC)                                                        
VCXPAMC  DC    V(CXPAMC)                                                        
VCXSPWC  DC    V(CXSPWC)                                                        
VCXSAGX  DC    V(CXSAGX)                                                        
VCXSUIX  DC    V(CXSUIX)                                                        
VCXSUPX  DC    V(CXSUPX)                                                        
VCXSPPX  DC    V(CXSPPX)                                                        
VCXSPUX  DC    V(CXSPUX)                                                        
VCXSPEX  DC    V(CXSPEX)                                                        
VCXSAAX  DC    V(CXSAAX)                                                        
VCXSALX  DC    V(CXSALX)                                                        
VCXSFAX  DC    V(CXSFAX)                                                        
VCXSFGX  DC    V(CXSFGX)                                                        
VCXSOAX  DC    V(CXSOAX)                                                        
VCXSOGX  DC    V(CXSOGX)                                                        
VCXOLSX  DC    V(CXOLSX)                                                        
VCXOLEX  DC    V(CXOLEX)                                                        
VCXAPNX  DC    V(CXAPNX)                                                        
VCXAPMX  DC    V(CXAPMX)                                                        
VCXPAMX  DC    V(CXPAMX)                                                        
VMXCNVX  DC    V(MXCNVX)                                                        
*                                                                               
DMOPEN   DC    C'OPEN   '                                                       
DMREAD   DC    C'DMREAD '                                                       
DMRSEQ   DC    C'DMRSEQ '                                                       
DMRDHI   DC    C'DMRDHI '                                                       
DMCLSE   DC    C'DMCLSE '                                                       
DMFAST   DC    C'DMFAST '                                                       
GETREC   DC    C'GETREC '                                                       
DMRFIL   DC    C'RECOVER'                                                       
CONTROL  DC    C'CONTROL'                                                       
CTFILE   DC    C'CTFILE '                                                       
CTFILES  DC    C'NCTFILE NGENDIR NGENFIL NCTRCVR X'                             
MEDFILE  DC    C'MEDFIL '                                                       
MEDFILQ  EQU   X'42'                                                            
CONFILQ  EQU   X'A1'                                                            
MEDDIR   DC    C'MEDDIR '                                                       
DMDA     DC    F'0'                                                             
DTFADDR  DC    F'0'                                                             
ACTIVITY DC    CL1'Y'                                                           
*                                                                               
         EJECT                                                                  
* DXDSECTS                                                                      
       ++INCLUDE DXDSECTS                                                       
         SPACE 1                                                                
* CXRECD                                                                        
       ++INCLUDE CXRECD                                                         
         SPACE 1                                                                
* FAFACTS                                                                       
         PRINT OFF                                                              
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
         SPACE 1                                                                
* DDPERVALD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDPERVALD                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* DMDTFIS                                                                       
       ++INCLUDE DMDTFIS                                                        
         SPACE 1                                                                
* DMGREQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE DMGREQUS                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* CTFILE DSECTS                                                                 
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE SEACSFILE                                                      
         EJECT                                                                  
         PRINT ON                                                               
* LOCAL CXTRACT DSECTS                                                          
         SPACE 1                                                                
       ++INCLUDE CXTRACTD                                                       
         EJECT                                                                  
*                                                                               
* DSECT TO COVER RECOVERY HEADER                                                
*                                                                               
RECDS    DSECT                                                                  
RECLN    DS    XL2                 TOTAL RECORD LENGTH                          
         DS    XL2                 N/D                                          
       ++INCLUDE DMRCVRHDR                                                      
         SPACE 1                                                                
WORKD    DSECT                     ** GLOBAL WORKING STORAGE **                 
*                                                                               
DUB      DS    D                                                                
DMCB     DS    6F                                                               
PARM     DS    6F                                                               
FULL     DS    F                                                                
RELO     DS    A                                                                
AENCKEY  DS    A                   A(PASSWORD ENCRYPTION KEY OR 0)              
ASUBTAB  DS    A                   A(SUB SYSTEM CONTROL TABLE)                  
IOKEY    DS    CL32                                                             
IOKEYSV  DS    CL32                                                             
DMWORK   DS    12D                                                              
BYTE     DS    XL1                                                              
ERROR    DS    XL1                                                              
RETCODE  DS    XL1                                                              
HEADFLAG DS    XL1                                                              
USERAGY  DS    CL2                                                              
USERAGY1 DS    CL2                                                              
ACTIONSV DS    CL1                                                              
AGENCYID DS    CL2                                                              
SECAGY   DS    CL2                                                              
AGYBIN   DS    XL1                                                              
TYPESAVE DS    CL3                                                              
PLATFORM DS    CL1                                                              
*                                                                               
MAXIOS   DS    XL4                 RECORD IO LIMIT COUNT                        
*                                                                               
*                                  RECORD TYPE TABLE VALUES                     
TYPEREC  DS    XL1                 MEDIA RECORD TYPE                            
TYPECODE DS    CL3                 TYPE CODE                                    
TYPEMED  DS    CL3                 TYPE MEDIA CODES                             
TYPEALOD DS    A                   EXTRACT LOAD ROUTINE                         
TYPEAUPD DS    A                   EXTRACT UPDATE ROUTINE                       
*                                                                               
TODAYC   DS    XL2                                                              
PIDSAVE  DS    XL(L'SAPEPID)                                                    
*                                                                               
FACTBITT DS    XL8                 BIT TABLE RETURNED BY FINDACTS               
*                                                                               
WORK     DS    XL256                                                            
IOL      DS    F                   GENERAL IO AREA                              
IO       DS    2048X                                                            
WORKX    DS    0D                                                               
         SPACE 1                                                                
WORKC    CSECT                     ** WORKING STORAGE POOL 1 **                 
         DS    5000D                                                            
         SPACE 1                                                                
CXSSPAT  CSECT                     SYSTEM PROGRAM ACCESS TABLE                  
         DC    (100*(SPATLEN))X'00'                                             
CXSSPATX EQU   *                                                                
         SPACE 1                                                                
CXSAGUT  CSECT                     AGENCY USER ID TABLE                         
         DC    (200*(AGUTLEN))X'00'                                             
CXSAGUTX EQU   *                                                                
         SPACE 1                                                                
CXSSAGT  CSECT                     MASTER SECURITY AGENCY ID TABLE              
         DC    (200*(SAGTLEN))X'00'                                             
CXSSAGTX EQU   *                                                                
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'086CXTRACTS  05/01/02'                                      
         END                                                                    
