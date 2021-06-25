*          DATA SET MXTRACTDB  AT LEVEL 250 AS OF 05/01/02                      
*PHASE MXTRACTD                                                                 
*INCLUDE DMUTLCT                                                                
*INCLUDE DMDMGRL                                                                
*INCLUDE MXAUD2C                                                                
*INCLUDE MXBUY2C                                                                
*INCLUDE MXBYR2C                                                                
*INCLUDE MXCAM2C                                                                
*INCLUDE MXCLI2C                                                                
*INCLUDE MXMED2C                                                                
*INCLUDE MXPRO2C                                                                
*INCLUDE MXSUP2C                                                                
*INCLUDE MXTVA2C                                                                
*INCLUDE MXAGY2C                                                                
*INCLUDE MXFOL2C                                                                
*INCLUDE MXSCH2C                                                                
*INCLUDE MXOUT2C                                                                
*INCLUDE MXOUL2C                                                                
*INCLUDE MXBUR2C                                                                
*INCLUDE MXCNVX                                                                 
*INCLUDE ADDAY                                                                  
*INCLUDE BUYTER                                                                 
*INCLUDE DEMAND                                                                 
*INCLUDE DICTATE                                                                
*INCLUDE EUREKA                                                                 
*INCLUDE FAGETTXT                                                               
*INCLUDE FASWITCH                                                               
*INCLUDE GETAUD                                                                 
*INCLUDE GETDAY                                                                 
*INCLUDE GETDPT                                                                 
*INCLUDE GETPGM                                                                 
*INCLUDE GETRATE                                                                
*INCLUDE GETRAT2                                                                
*INCLUDE GETUNV                                                                 
*INCLUDE GETEQIV                                                                
*INCLUDE TRAVAIL                                                                
*INCLUDE TMUNPK                                                                 
*INCLUDE DATCON                                                                 
*INCLUDE HEXOUT                                                                 
*INCLUDE PRINTER                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE STABLE                                                                 
*INCLUDE STABTAB                                                                
*INCLUDE STABLST                                                                
*INCLUDE BINSRCH2                                                               
*INCLUDE CUREDIT                                                                
*INCLUDE CASHVAL                                                                
*INCLUDE SCANNER                                                                
*INCLUDE UNSCAN                                                                 
*INCLUDE BLDCUR                                                                 
*INCLUDE LOADER                                                                 
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*                                                                               
         TITLE 'MXTRACT - EXTRACT MEDIA SYSTEM FILE DATA'                       
**********************************************************                      
*                                                        *                      
* MEDIA SQL SUB SYSTEM EXTRACT CONTROL MODULE            *                      
*                                                        *                      
* CONTROL IS PASSED FROM DXTRACT WITH PARAMETERS:        *                      
* R1=A(EXTRACT CONTROL DATA BLOCK - SEE DSECT DXBLOCKD)  *                      
*                                                        *                      
* MODULE ENTERED WITH ONE OF FOLLOWING MODES IN DXMODE:  *                      
*   DXOPENQ  - OPEN SYSTEM FILES                         *                      
*   DXCLOSEQ - CLOSE SYSTEM FILES                        *                      
*   DXLOADQ  - EXTRACT FROM FILES IN LOAD MODE           *                      
*   DXUPDTQ  - EXTRACT FROM FILES IN UPDATE MODE         *                      
*   DXSTATQ  - EXTRACT STATISTICS                        *                      
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
MXTRACT  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORKX-WORKD,**MXTR**,RA,R9,RR=RE                                 
*                                                                               
         ENTRY SSB                 FOR DATAMGR                                  
         ENTRY MEDFACS             FOR MEDIA SUBROUTINES                        
         ENTRY MXBUYCM                                                          
         ENTRY MXBUYCCL                                                         
         ENTRY MXBUYCP                                                          
         ENTRY MXBUYCC                                                          
         ENTRY CURTAB              CURRENCY TABLE                               
         ENTRY ADXBLOCK                                                         
*                                                                               
         USING WORKD,RC            RC=A(GLOBAL W/S)                             
         ST    RE,RELO                                                          
         L     R8,0(R1)            R8=A(EXTRACT CONTROL DATA BLOCK)             
         USING DXBLOCKD,R8                                                      
         L     R7,DXSTPTR          R7=A(EXTRACT SYSTEM TABLE ENTRY)             
         USING SXDTABD,R7                                                       
         ST    R8,ADXBLOCK         SAVE ADXBLOCK FOR EXTERNAL REFERENCE         
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
         BNE   MSTAT                                                            
         BAS   RE,PROCUPDT         EXTRACT FROM FILES IN UPDATE MODE            
         BNE   MERR                  EXIT IF ERROR                              
         B     MXIT                                                             
*                                                                               
MSTAT    CLI   DXMODE,DXSTATQ                                                   
         BNE   MERR                                                             
         GOTO1 =A(PROCSTAT),(RC)   EXTRACT STATISTICS                           
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
         XC    CLIFILT,CLIFILT                                                  
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* DXMODE = DXOPENQ - OPEN MEDIA SYSTEM FILES                          *         
***********************************************************************         
         SPACE 1                                                                
PROCOPEN NTR1  ,                                                                
*                                  SET UTL SENUM                                
*                                  INITIALISE SYSTEM PROGRAM ACCESS             
         L     RE,=V(UTL)                                                       
         MVC   4(1,RE),DXSENUM                                                  
*                                  OPEN MEDZ SYSTEM                             
         BAS   RE,OPENMEDZ                                                      
*                                  GET DTF ADDRESS                              
         XC    IOKEY,IOKEY                                                      
         L     R2,DXARECB                                                       
         GOTO1 VDATAMGR,DMCB,DMFAST,MEDDIR,IOKEY,(R2),DMWORK                    
         L     RF,12(R1)                                                        
         LA    RF,0(RF)                                                         
         ST    RF,DTFADDR                                                       
*                                  OPEN SYSTEM DISK FILES                       
         GOTO1 VDMOD000,DMCB,A(DMODOSYS),(DXSENUM,IOL)                          
*                                                                               
*                                  OPEN CONTROL SYSTEM FILES                    
         L     R4,=V(UTL)                                                       
         MVC   BYTE,4(R4)                                                       
         MVI   4(R4),X'0A'                                                      
         GOTO1 VDATAMGR,DMCB,DMOPEN,CONTROL,CTFILES,IOL                         
         MVC   4(1,R4),BYTE                                                     
*                                                                               
         L     RF,=A(CURTAB)                                                    
         MVC   WORK(4),VDATAMGR                                                 
         GOTO1 =V(BLDCUR),DMCB,0,(X'00',(RF)),WORK                              
         L     RF,=A(CURTABX)                                                   
         OC    0(8,RF),0(RF)                                                    
         BZ    *+6                                                              
         DC    H'0'                CURTAB IN CSECTS EXCEEDED                    
*                                                                               
         L     R4,=V(UTL)                                                       
         MVC   BYTE,4(R4)                                                       
         MVI   4(R4),X'0A'                                                      
         GOTO1 VDATAMGR,DMCB,DMCLSE,=C'FILES',,IOL                              
         MVC   4(1,R4),BYTE                                                     
*                                                                               
         OC    VGETEQV,VGETEQV                                                  
         BNZ   POPEOK                                                           
         B     POPEOK                                                           
         MVC   DUB,=CL8'T00A4F  '                                               
         GOTO1 =V(LOADER),DMCB,DUB,0,0                                          
         OC    VGETEQV,4(R1)       R1=A(LOAD PHASE)                             
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         B     POPEOK                                                           
*                                                                               
POPENO   B     NO                                                               
POPEOK   B     YES                                                              
         SPACE 1                                                                
***********************************************************************         
* OPEN MEDZ SYSTEM                                                    *         
***********************************************************************         
         SPACE 1                                                                
OPENMEDZ NTR1                                                                   
         L     R4,=V(UTL)                                                       
         MVC   BYTE,4(R4)                                                       
         MVI   4(R4),X'14'                                                      
         GOTO1 VDATAMGR,DMCB,DMOPEN,=C'MED',MEDZFLS,IOL                         
         MVC   4(1,R4),BYTE                                                     
         B     YES                                                              
MEDZFLS  DC    CL8'NMEDDIR'                                                     
         DC    CL8'NMEDFIL'                                                     
         DC    CL8'NDMNDIR'                                                     
         DC    CL8'NDMNNEW'                                                     
         DC    CL8'NDMNOLD'                                                     
         DC    CL8'NDMODIR'                                                     
         DC    CL8'NDMO1FL'                                                     
         DC    CL8'NDMO2FL'                                                     
         DC    CL8'NDMO3FL'                                                     
         DC    CL8'NDMO4FL'                                                     
         DC    CL8'X      '                                                     
         EJECT                                                                  
***********************************************************************         
* DXMODE = DXCLOSEQ - CLOSE MEDIA SYSTEM FILES                        *         
***********************************************************************         
         SPACE 1                                                                
PROCCLOS NTR1  ,                                                                
*                                  SET UTL SENUM                                
         L     RE,=V(UTL)                                                       
         MVC   4(1,RE),DXSENUM                                                  
*                                                                               
         GOTO1 VDMOD000,DMCB,A(DMODCSYS),(DXSENUM,IOL)                          
         B     PCLOOK                                                           
*                                                                               
PCLONO   B     NO                                                               
PCLOOK   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* PROCESS MEDIA FILE DATA IN LOAD MODE                                *         
***********************************************************************         
         SPACE 1                                                                
PROCLOAD NTR1  ,                                                                
         MVC   MEDAGY,SXDTAGB      SET MEDIA AGENCY CODE FROM SYSTEM            
         NI    MEDAGY,X'F0'          CONTROL TABLE                              
         MVC   TYPECODE,SXDTTYP                                                 
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
* LOAD ALL RECORD DATA                                                *         
***********************************************************************         
         SPACE 1                                                                
LOADALL  NTR1                                                                   
         BAS   RE,LOADAGY          AGENCY                                       
         BNE   LALLNO                                                           
         BAS   RE,LOADAUD          AUDIENCES                                    
         BNE   LALLNO                                                           
         MVI   TYPEMED,0           ALL SUPPLIERS                                
         BAS   RE,LOADSUP                                                       
         BNE   LALLNO                                                           
         BAS   RE,LOADTVA          TV AREAS                                     
         BNE   LALLNO                                                           
         BAS   RE,LOADBYR          BUYERS                                       
         BNE   LALLNO                                                           
         BAS   RE,LOADMED          MEDIA                                        
         BNE   LALLNO                                                           
         BAS   RE,LOADCLI          CLIENTS                                      
         BNE   LALLNO                                                           
         BAS   RE,LOADPRO          PRODUCTS                                     
         BNE   LALLNO                                                           
         BAS   RE,LOADCAM          CAMPAIGNS                                    
         BNE   LALLNO                                                           
         BAS   RE,LOADBUR          BURSTS                                       
         BNE   LALLNO                                                           
         BAS   RE,LOADBUY          BUYS                                         
         BNE   LALLNO                                                           
         BAS   RE,LOADFOL          FOLIOS                                       
         BNE   LALLNO                                                           
         BAS   RE,LOADSCH          OUTLET SCHEMES                               
         BNE   LALLNO                                                           
         BAS   RE,LOADOUT          OUTLETS                                      
         BNE   LALLNO                                                           
         BAS   RE,LOADOUL          OUTLET LINKS                                 
         BNE   LALLNO                                                           
         B     LALLOK                                                           
*                                                                               
LALLNO   B     NO                                                               
LALLOK   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* LOAD NON BUY RECORD DATA                                            *         
***********************************************************************         
         SPACE 1                                                                
LOADNBY  NTR1                                                                   
         BAS   RE,LOADAGY          AGENCY                                       
         BNE   LNBYNO                                                           
         BAS   RE,LOADAUD          AUDIENCES                                    
         BNE   LNBYNO                                                           
         MVI   TYPEMED,0           ALL SUPPLIERS                                
         BAS   RE,LOADSUP                                                       
         BNE   LNBYNO                                                           
         BAS   RE,LOADTVA          TV AREAS                                     
         BNE   LNBYNO                                                           
         BAS   RE,LOADBYR          BUYERS                                       
         BNE   LNBYNO                                                           
         BAS   RE,LOADMED          MEDIA                                        
         BNE   LNBYNO                                                           
         BAS   RE,LOADCLI          CLIENTS                                      
         BNE   LNBYNO                                                           
         BAS   RE,LOADPRO          PRODUCTS                                     
         BNE   LNBYNO                                                           
         BAS   RE,LOADCAM          CAMPAIGNS                                    
         BNE   LNBYNO                                                           
         BAS   RE,LOADBUR          BURSTS                                       
         BNE   LNBYNO                                                           
         BAS   RE,LOADFOL          FOLIOS                                       
         BNE   LNBYNO                                                           
         BAS   RE,LOADSCH          OUTLET SCHEMES                               
         BNE   LNBYNO                                                           
         BAS   RE,LOADOUT          OUTLETS                                      
         BNE   LNBYNO                                                           
         BAS   RE,LOADOUL          OUTLET LINKS                                 
         BNE   LNBYNO                                                           
         BAS   RE,LOADBUR          BURSTS                                       
         BNE   LNBYNO                                                           
         B     LNBYOK                                                           
*                                                                               
LNBYNO   B     NO                                                               
LNBYOK   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* LOAD LEO SPECIAL                                                    *         
***********************************************************************         
         SPACE 1                                                                
LOADLEO  NTR1                                                                   
         BAS   RE,LOADAGY          AGENCY                                       
         BNE   LLEONO                                                           
         BAS   RE,LOADAUD          AUDIENCES                                    
         BNE   LLEONO                                                           
         MVI   TYPEMED,0           ALL SUPPLIERS                                
         BAS   RE,LOADSUP                                                       
         BNE   LLEONO                                                           
         BAS   RE,LOADBYR          BUYERS                                       
         BNE   LLEONO                                                           
         BAS   RE,LOADMED          MEDIA                                        
         BNE   LLEONO                                                           
         BAS   RE,LOADFOL          FOLIOS                                       
         BNE   LLEONO                                                           
         BAS   RE,LOADSCH          OUTLET SCHEMES                               
         BNE   LLEONO                                                           
         BAS   RE,LOADOUT          OUTLETS                                      
         BNE   LLEONO                                                           
         BAS   RE,LOADOUL          OUTLET LINKS                                 
         BNE   LLEONO                                                           
         B     LLEOOK                                                           
*                                                                               
LLEONO   B     NO                                                               
LLEOOK   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* LOAD FOR POST PEEL - JUST CAMPAIGN AND BURST RECORD TYPES           *         
***********************************************************************         
         SPACE 1                                                                
LOADPEE  NTR1                                                                   
         BAS   RE,LOADCAM          CAMPIAGNS                                    
         BNE   LPEENO                                                           
         BAS   RE,LOADBUR          BURSTS                                       
         BNE   LPEENO                                                           
         B     LPEEOK                                                           
*                                                                               
LPEENO   B     NO                                                               
LPEEOK   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* LOAD FOR MEDZ STATIC RECORD DATA                                    *         
***********************************************************************         
         SPACE 1                                                                
LOADMEZ  NTR1                                                                   
         BAS   RE,LOADAUD          AUDIENCES                                    
         BNE   LMEZNO                                                           
         MVI   TYPEMED,0           ALL SUPPLIERS                                
         BAS   RE,LOADSUP                                                       
         BNE   LMEZNO                                                           
         BAS   RE,LOADTVA          TV AREAS                                     
         BNE   LMEZNO                                                           
         BAS   RE,LOADFOL          FOLIOS                                       
         BNE   LMEZNO                                                           
         B     LMEZOK                                                           
*                                                                               
LMEZNO   B     NO                                                               
LMEZOK   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* LOAD AGENCY RECORD DATA                                             *         
***********************************************************************         
         SPACE 1                                                                
LOADAGY  NTR1                                                                   
         MVC   MAXIOS,DXMAXREC     MAXIMIMUM RECORD COUNT                       
         MVI   BYTE,0              MEDIA KEY SAVE BYTE                          
*                                                                               
LAGY010  LA    R2,IOKEY            SET KEY TO READ FIRST AGENCY RECORD          
         USING DAGY,R2                                                          
         XC    AGYKEY,AGYKEY                                                    
         MVI   AGYKTYP,AGYKTYPQ                                                 
         MVC   AGYKAGY,MEDAGY      SET AGENCY                                   
         L     R2,DXARECB                                                       
         GOTO1 VDATAMGR,DMCB,(X'00',DMRDHI),MEDDIR,IOKEY,(R2),DMWORK            
*                                                                               
LAGY020  TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         BO    LAGYOK                                                           
         TM    DMCB+8,X'7D'        DIE IF DISK ERROR                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         MVC   BYTE,AGYKAGY                                                     
         NI    BYTE,X'F0'                                                       
         CLC   BYTE,AGYKAGY        ALL DONE IF AGENCY CHANGES                   
         BNE   LAGYOK                                                           
         CLI   AGYKTYP,AGYKTYPQ    READ MEDFILE IF SAME RECORD TYPE             
         BNE   LAGYOK                                                           
*                                  TRANSFER RECORD DATA TO EXTRACT FILE         
LAGY030  MVC   MEDADDR,AGYDDA                                                   
         XC    CLIFILT,CLIFILT                                                  
         GOTO1 MEDLOAD,DMCB,VMXAGYC,INITAGY,0                                   
         BNE   LAGYNO                                                           
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         BZ    LAGYOK                                                           
         B     LAGY020                                                          
*                                                                               
LAGYNO   B     NO                                                               
LAGYOK   B     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LOAD AUDIENCE RECORD DATA                                           *         
***********************************************************************         
         SPACE 1                                                                
LOADAUD  NTR1                                                                   
         MVC   MAXIOS,DXMAXREC     MAXIMIMUM RECORD COUNT                       
*                                                                               
LAUD010  L     R3,DXAXREC          CALL V(MXAUDC) FOR NEXT AUDIENCE             
         BAS   RE,INITAUD                                                       
         GOTO1 VMXAUDC,DMCB,(R3),(R2),0                                         
         MVC   BYTE,DMCB+8         SAVE DMCB+8 FOR CALL BACK TEST               
         CLI   DMCB+8,X'88'                                                     
         BE    LAUDOK              EXIT IF NO MORE AUDIENCES                    
         TM    DMCB+8,X'80'                                                     
         BO    LAUD020             TEST NOT TO WRITE THIS RECORD                
         CLI   DXWRITE,C'Y'                                                     
         BNE   LAUD020                                                          
         CLI   SXDTPLFM,0                                                       
         BE    LAUD012                                                          
         L     R4,DXASQLB                                                       
*                                  CONVERT RECORD TO SQL BUFFER                 
         GOTO1 VMXCNVX,DMCB,(R8)                                                
         BE    *+6                                                              
         DC    H'0'                DEAL WITH ERROR STATE HERE??                 
*                                  PUT CONVERTED RECORD TO FILE                 
         GOTO1 DXPUT,DMCB,DXASQLB,(R8)                                          
         B     LAUD020                                                          
*                                  PUT UNCONVERTED RECORD TO FILE               
LAUD012  GOTO1 DXPUT,DMCB,DXAXREC,(R8)                                          
         B     LAUD020                                                          
*                                                                               
LAUD020  MVC   DMCB+8,BYTE         RESTORE DMCB+8                               
         TM    DMCB+8,X'40'        TEST IF CALL BACK REQUIRED                   
         BO    LAUD010                                                          
*                                                                               
LAUDOK   B     YES                                                              
LAUDNO   B     NO                                                               
         EJECT                                                                  
***********************************************************************         
* LOAD BUYER RECORD DATA                                              *         
***********************************************************************         
         SPACE 1                                                                
LOADBYR  NTR1                                                                   
         MVC   MAXIOS,DXMAXREC     MAXIMIMUM RECORD COUNT                       
         MVI   BYTE,0              MEDIA KEY SAVE BYTE                          
*                                                                               
LBYR010  LA    R2,IOKEY            SET KEY TO READ FIRST BUYER RECORD           
         USING DBYR,R2                                                          
         XC    BYRKEY,BYRKEY                                                    
         MVI   BYRKTYP,BYRKTYPQ                                                 
         MVC   BYRKAM,MEDAGY       SET AGENCY                                   
         L     R2,DXARECB                                                       
         GOTO1 VDATAMGR,DMCB,(X'00',DMRDHI),MEDDIR,IOKEY,(R2),DMWORK            
*                                                                               
LBYR020  TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         BO    LBYROK                                                           
         TM    DMCB+8,X'7D'        DIE IF DISK ERROR                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         CLC   BYRKAM,MEDAGY       ALL DONE IF AGENCY CHANGES                   
         BNE   LBYROK                                                           
         CLI   BYRKTYP,BYRKTYPQ    ALL DONE IF RECORD TYPE CHANGES              
         BNE   LBYROK                                                           
*                                  TRANSFER RECORD DATA TO EXTRACT FILE         
LBYR030  MVC   MEDADDR,BYRDDA                                                   
         XC    CLIFILT,CLIFILT                                                  
         GOTO1 MEDLOAD,DMCB,VMXBYRC,INITBYR,0                                   
         BNE   LBYRNO                                                           
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         BZ    LBYROK                                                           
         B     LBYR020                                                          
*                                                                               
LBYRNO   B     NO                                                               
LBYROK   B     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LOAD MEDIA RECORD DATA                                              *         
***********************************************************************         
         SPACE 1                                                                
LOADMED  NTR1                                                                   
         MVC   MAXIOS,DXMAXREC     MAXIMIMUM RECORD COUNT                       
         MVI   BYTE,0              MEDIA KEY SAVE BYTE                          
*                                                                               
LMED010  LA    R2,IOKEY            SET KEY TO READ MEDIA RECORD                 
         USING DMED,R2                                                          
         XC    MEDKEY,MEDKEY                                                    
         MVI   MEDKTYP,MEDKTYPQ                                                 
         MVC   MEDKAM,MEDAGY       SET AGENCY                                   
         OC    MEDKAM,BYTE         SET MEDIA                                    
         L     R2,DXARECB                                                       
         GOTO1 VDATAMGR,DMCB,(X'00',DMRDHI),MEDDIR,IOKEY,(R2),DMWORK            
*                                                                               
LMED020  TM    8(R1),X'80'         ALL DONE IF EOF                              
         BO    LMEDOK                                                           
         TM    8(R1),X'7D'         DIE IF DISK ERROR                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         MVC   BYTE,MEDKAM                                                      
         NI    BYTE,X'F0'                                                       
         CLC   BYTE,MEDAGY         ALL DONE IF AGENCY CHANGES                   
         BNE   LMEDOK                                                           
         CLI   MEDKTYP,MEDKTYPQ    READ MEDFILE IF SAME RECORD TYPE             
         BE    LMED030                                                          
         LA    R2,IOKEY                                                         
         MVC   BYTE,MEDKAM                                                      
         NI    BYTE,X'0F'                                                       
         CLI   BYTE,X'0F'          ALL DONE IF LAST MEDIA                       
         BE    LMEDOK                                                           
         ZIC   RF,BYTE             BUMP TO NEXT MEDIA                           
         LA    RF,1(RF)                                                         
         STC   RF,BYTE                                                          
         B     LMED010                                                          
*                                  TRANSFER RECORD DATA TO EXTRACT FILE         
LMED030  MVC   MEDADDR,MEDDDA                                                   
         XC    CLIFILT,CLIFILT                                                  
         GOTO1 MEDLOAD,DMCB,VMXMEDC,INITMED,0                                   
         BNE   LMEDNO                                                           
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         BZ    LMEDOK                                                           
         B     LMED020                                                          
*                                                                               
LMEDNO   B     NO                                                               
LMEDOK   B     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LOAD CLIENT RECORD DATA                                             *         
***********************************************************************         
         SPACE 1                                                                
LOADCLI  NTR1                                                                   
         MVC   MAXIOS,DXMAXREC     MAXIMIMUM RECORD COUNT                       
         MVI   BYTE,0              MEDIA KEY SAVE BYTE                          
*                                                                               
LCLI010  LA    R2,IOKEY            SET KEY TO READ FIRST CLIENT RECORD          
         USING DCLI,R2                                                          
         XC    CLIKEY,CLIKEY                                                    
         MVI   CLIKTYP,CLIKTYPQ                                                 
         MVC   CLIKAM,MEDAGY       SET AGENCY                                   
         OC    CLIKAM,BYTE         SET MEDIA                                    
         L     R2,DXARECB                                                       
         GOTO1 VDATAMGR,DMCB,(X'00',DMRDHI),MEDDIR,IOKEY,(R2),DMWORK            
*                                                                               
LCLI020  TM    8(R1),X'80'         ALL DONE IF EOF                              
         BO    LCLIOK                                                           
         TM    8(R1),X'7D'         DIE IF DISK ERROR                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         MVC   BYTE,CLIKAM                                                      
         NI    BYTE,X'F0'                                                       
         CLC   BYTE,MEDAGY         ALL DONE IF AGENCY CHANGES                   
         BNE   LCLIOK                                                           
         CLI   CLIKTYP,CLIKTYPQ    READ MEDFILE IF SAME RECORD TYPE             
         BE    LCLI030                                                          
         CLI   BYTE,X'0F'          ALL DONE IF LAST MEDIA                       
         BE    LCLIOK                                                           
         LA    R2,IOKEY                                                         
         MVC   BYTE,CLIKAM                                                      
         NI    BYTE,X'0F'                                                       
         CLI   BYTE,X'0F'          ALL DONE IF LAST MEDIA                       
         BE    LCLIOK                                                           
         ZIC   RF,BYTE             BUMP TO NEXT MEDIA                           
         LA    RF,1(RF)                                                         
         STC   RF,BYTE                                                          
         B     LCLI010                                                          
*                                  TRANSFER RECORD DATA TO EXTRACT FILE         
LCLI030  MVC   MEDADDR,CLIDDA                                                   
         MVC   CLIFILT,CLIKCLI                                                  
         GOTO1 MEDLOAD,DMCB,VMXCLIC,INITCLI,0                                   
         BNE   LCLINO                                                           
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         BZ    LCLIOK                                                           
         B     LCLI020                                                          
*                                                                               
LCLINO   B     NO                                                               
LCLIOK   B     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LOAD PRODUCT RECORD DATA                                            *         
***********************************************************************         
         SPACE 1                                                                
LOADPRO  NTR1                                                                   
         MVC   MAXIOS,DXMAXREC     MAXIMIMUM RECORD COUNT                       
         MVI   BYTE,0              MEDIA KEY SAVE BYTE                          
*                                                                               
LPRO010  LA    R2,IOKEY            SET KEY TO READ FIRST PRODUCT RECORD         
         USING DPRO,R2                                                          
         XC    PROKEY,PROKEY                                                    
         MVI   PROKTYP,PROKTYPQ                                                 
         MVC   PROKAM,MEDAGY       SET AGENCY                                   
         OC    PROKAM,BYTE         SET MEDIA                                    
         L     R2,DXARECB                                                       
         GOTO1 VDATAMGR,DMCB,(X'00',DMRDHI),MEDDIR,IOKEY,(R2),DMWORK            
*                                                                               
LPRO020  TM    8(R1),X'80'         ALL DONE IF EOF                              
         BO    LPROOK                                                           
         TM    8(R1),X'7D'         DIE IF DISK ERROR                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         MVC   BYTE,PROKAM                                                      
         NI    BYTE,X'F0'                                                       
         CLC   BYTE,MEDAGY         ALL DONE IF AGENCY CHANGES                   
         BNE   LPROOK                                                           
         CLI   PROKTYP,PROKTYPQ    READ MEDFILE IF SAME RECORD TYPE             
         BE    LPRO030                                                          
         LA    R2,IOKEY                                                         
         MVC   BYTE,PROKAM                                                      
         NI    BYTE,X'0F'                                                       
         CLI   BYTE,X'0F'          ALL DONE IF LAST MEDIA                       
         BE    LPROOK                                                           
         ZIC   RF,BYTE             BUMP TO NEXT MEDIA                           
         LA    RF,1(RF)                                                         
         STC   RF,BYTE                                                          
         B     LPRO010                                                          
*                                  TRANSFER RECORD DATA TO EXTRACT FILE         
LPRO030  MVC   MEDADDR,PRODDA                                                   
         MVC   CLIFILT,PROKCLI                                                  
         GOTO1 MEDLOAD,DMCB,VMXPROC,INITPRO,0                                   
         BNE   LPRONO                                                           
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         BZ    LPROOK                                                           
         B     LPRO020                                                          
*                                                                               
LPRONO   B     NO                                                               
LPROOK   B     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LOAD CAMPAIGN RECORD DATA                                           *         
***********************************************************************         
         SPACE 1                                                                
LOADCAM  NTR1                                                                   
         MVC   MAXIOS,DXMAXREC     MAXIMIMUM RECORD COUNT                       
         MVI   BYTE,0              MEDIA KEY SAVE BYTE                          
*                                                                               
LCAM010  LA    R2,IOKEY            SET KEY TO READ FIRST CAMPAIGN REC.          
         USING DCAMP,R2                                                         
         XC    CAMKEY,CAMKEY                                                    
         MVI   CAMKTYP,CAMKTYPQ                                                 
         MVC   CAMKAM,MEDAGY       SET AGENCY                                   
         OC    CAMKAM,BYTE         SET MEDIA                                    
         L     R2,DXARECB                                                       
         GOTO1 VDATAMGR,DMCB,(X'00',DMRDHI),MEDDIR,IOKEY,(R2),DMWORK            
*                                                                               
LCAM020  TM    8(R1),X'80'         ALL DONE IF EOF                              
         BO    LCAMOK                                                           
         TM    8(R1),X'7D'         DIE IF DISK ERROR                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         MVC   BYTE,CAMKAM                                                      
         NI    BYTE,X'F0'                                                       
         CLC   BYTE,MEDAGY         ALL DONE IF AGENCY CHANGES                   
         BNE   LCAMOK                                                           
         CLI   CAMKTYP,CAMKTYPQ    READ MEDFILE IF SAME RECORD TYPE             
         BE    LCAM030                                                          
         LA    R2,IOKEY                                                         
         MVC   BYTE,CAMKAM                                                      
         NI    BYTE,X'0F'                                                       
         CLI   BYTE,X'0F'          ALL DONE IF LAST MEDIA                       
         BE    LCAMOK                                                           
         ZIC   RF,BYTE             BUMP TO NEXT MEDIA                           
         LA    RF,1(RF)                                                         
         STC   RF,BYTE                                                          
         B     LCAM010                                                          
*                                  TRANSFER RECORD DATA TO EXTRACT FILE         
LCAM030  MVC   MEDADDR,CAMDDA                                                   
         MVC   CLIFILT,CAMKCLI                                                  
         GOTO1 MEDLOAD,DMCB,VMXCAMC,INITCAM,0                                   
         BNE   LCAMNO                                                           
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         BZ    LCAMOK                                                           
         B     LCAM020                                                          
*                                                                               
LCAMNO   B     NO                                                               
LCAMOK   B     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LOAD BURST RECORD DATA                                              *         
***********************************************************************         
         SPACE 1                                                                
LOADBUR  NTR1                                                                   
         MVC   MAXIOS,DXMAXREC     MAXIMIMUM RECORD COUNT                       
         MVI   BYTE,0              MEDIA KEY SAVE BYTE                          
*                                                                               
LBUR010  LA    R2,IOKEY            SET KEY TO READ FIRST BURST REC.             
         USING BURKEY,R2                                                        
         XC    BURKEY,BURKEY                                                    
         MVI   BURKTYP,BURKTYPQ                                                 
         MVC   BURKAM,MEDAGY       SET AGENCY                                   
         OC    BURKAM,BYTE         SET MEDIA                                    
         L     R2,DXARECB                                                       
         GOTO1 VDATAMGR,DMCB,(X'00',DMRDHI),MEDDIR,IOKEY,(R2),DMWORK            
*                                                                               
LBUR020  TM    8(R1),X'80'         ALL DONE IF EOF                              
         BO    LBUROK                                                           
         TM    8(R1),X'7D'         DIE IF DISK ERROR                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         MVC   BYTE,BURKAM                                                      
         NI    BYTE,X'F0'                                                       
         CLC   BYTE,MEDAGY         ALL DONE IF AGENCY CHANGES                   
         BNE   LBUROK                                                           
         CLI   BURKTYP,BURKTYPQ    READ MEDFILE IF SAME RECORD TYPE             
         BE    LBUR030                                                          
         LA    R2,IOKEY                                                         
         MVC   BYTE,BURKAM                                                      
         NI    BYTE,X'0F'                                                       
         CLI   BYTE,X'0F'          ALL DONE IF LAST MEDIA                       
         BE    LBUROK                                                           
         ZIC   RF,BYTE             BUMP TO NEXT MEDIA                           
         LA    RF,1(RF)                                                         
         STC   RF,BYTE                                                          
         B     LBUR010                                                          
*                                  TRANSFER RECORD DATA TO EXTRACT FILE         
LBUR030  MVC   MEDADDR,BURDDA                                                   
         MVC   CLIFILT,BURKCLI                                                  
         GOTO1 MEDLOAD,DMCB,VMXBURC,INITBUR,0                                   
         BNE   LBURNO                                                           
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         BZ    LBUROK                                                           
         B     LBUR020                                                          
*                                                                               
LBURNO   B     NO                                                               
LBUROK   B     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LOAD BUY RECORD DATA                                                *         
***********************************************************************         
         SPACE 1                                                                
LOADBUY  NTR1                                                                   
         MVC   MAXIOS,DXMAXREC     MAXIMIMUM RECORD COUNT                       
         MVI   BYTE,0              MEDIA KEY SAVE BYTE                          
*                                                                               
LBUY010  LA    R2,IOKEY            SET KEY TO READ FIRST BUY RECORD             
         USING DBUY,R2                                                          
         XC    BUYKEY,BUYKEY                                                    
         MVI   BUYKTYP,BUYKTYPQ                                                 
         MVC   BUYKAM,MEDAGY       SET AGENCY                                   
         OC    BUYKAM,BYTE         SET MEDIA                                    
         L     R2,DXARECB                                                       
         GOTO1 VDATAMGR,DMCB,(X'00',DMRDHI),MEDDIR,IOKEY,(R2),DMWORK            
*                                                                               
LBUY020  TM    8(R1),X'80'         ALL DONE IF EOF                              
         BO    LBUYOK                                                           
         TM    8(R1),X'7D'         DIE IF DISK ERROR                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         MVC   BYTE,BUYKAM                                                      
         NI    BYTE,X'F0'                                                       
         CLC   BYTE,MEDAGY         ALL DONE IF AGENCY CHANGES                   
         BNE   LBUYOK                                                           
         CLI   BUYKTYP,BUYKTYPQ    READ MEDFILE IF SAME RECORD TYPE             
         BE    LBUY030                                                          
         LA    R2,IOKEY                                                         
         MVC   BYTE,BUYKAM                                                      
         NI    BYTE,X'0F'                                                       
         CLI   BYTE,X'0F'          ALL DONE IF LAST MEDIA                       
         BE    LBUYOK                                                           
         ZIC   RF,BYTE             BUMP TO NEXT MEDIA                           
         LA    RF,1(RF)                                                         
         STC   RF,BYTE                                                          
         B     LBUY010                                                          
*                                  TRANSFER RECORD DATA TO EXTRACT FILE         
LBUY030  MVC   MEDADDR,BUYDDA                                                   
         MVC   CLIFILT,BUYKCLI                                                  
         MVC   IOKEYSV(L'MEDKEY),0(R2)   SAVE BUY RECORD KEY                    
         GOTO1 MEDLOAD,DMCB,VMXBUYC,INITBUY,FILTBUY                             
         BNE   LBUYNO                                                           
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         BZ    LBUYOK                                                           
         L     RF,DXAXREC                                                       
         B     LBUY020                                                          
*                                                                               
LBUYNO   B     NO                                                               
LBUYOK   B     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LOAD SUPPLIER RECORD DATA                                           *         
***********************************************************************         
         SPACE 1                                                                
LOADSUP  NTR1                                                                   
         MVC   MAXIOS,DXMAXREC     MAXIMIMUM RECORD COUNT                       
         MVI   BYTE,0              AGENCY KEY SAVE BYTE                         
*                                                                               
LSUP010  LA    R2,IOKEY            SET KEY TO READ SUPPLIER RECORDS             
         USING DSTA,R2                                                          
         XC    STAKEY,STAKEY                                                    
         MVI   STAKTYP,STAKTYPQ                                                 
         MVC   STAKAM,MEDAGY       SET AGENCY                                   
         CLI   BYTE,0              TV HAS STANDARD RECORDS                      
         BNE   *+8                                                              
         MVI   STAKAM,0                                                         
         OC    STAKAM,BYTE         SET MEDIA CODE                               
         L     R2,DXARECB                                                       
         GOTO1 VDATAMGR,DMCB,(X'00',DMRDHI),MEDDIR,IOKEY,(R2),DMWORK            
*                                                                               
LSUP020  TM    8(R1),X'80'         ALL DONE IF EOF                              
         BO    LSUPOK                                                           
         TM    8(R1),X'7D'         DIE IF DISK ERROR                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         CLI   STAKAM,X'00'        TV STATIONS ARE COMMON                       
         BE    LSUP025                                                          
         MVC   BYTE,STAKAM                                                      
         NI    BYTE,X'F0'                                                       
         CLC   BYTE,MEDAGY         ALL DONE IF AGENCY CHANGES                   
         BNE   LSUPOK                                                           
LSUP025  CLI   STAKTYP,STAKTYPQ    READ MEDFILE IF SAME RECORD TYPE             
         BE    LSUP030                                                          
         LA    R2,IOKEY                                                         
         MVC   BYTE,STAKAM                                                      
         NI    BYTE,X'0F'                                                       
         CLI   BYTE,X'0F'          ALL DONE IF LAST MEDIA                       
         BE    LSUPOK                                                           
         CLI   BYTE,X'00'          ALL DONE IF TVS ONLY                         
         BNE   *+12                                                             
         CLI   TYPEMED,C'0'                                                     
         BE    LSUPOK                                                           
         ZIC   RF,BYTE             BUMP TO NEXT MEDIA                           
         LA    RF,1(RF)                                                         
         STC   RF,BYTE                                                          
         B     LSUP010                                                          
*                                  TRANSFER RECORD DATA TO EXTRACT FILE         
LSUP030  MVC   MEDADDR,STADDA                                                   
         XC    CLIFILT,CLIFILT                                                  
         GOTO1 MEDLOAD,DMCB,VMXSUPC,INITSUP,0                                   
         BNE   LSUPNO                                                           
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         BZ    LSUPOK                                                           
         B     LSUP020                                                          
*                                                                               
LSUPNO   B     NO                                                               
LSUPOK   B     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LOAD TV AREA RECORD DATA                                            *         
***********************************************************************         
         SPACE 1                                                                
LOADTVA  NTR1                                                                   
         MVC   MAXIOS,DXMAXREC     MAXIMIMUM RECORD COUNT                       
*                                                                               
LTVA010  L     R3,DXAXREC          CALL V(MXTVAC) FOR NEXT TVAREA               
         BAS   RE,INITTVA                                                       
         GOTO1 VMXTVAC,DMCB,(R3),(R2),0                                         
         MVC   BYTE,DMCB+8         SAVE DMCB+8 FOR CALL BACK TEST               
         CLI   DMCB+8,X'88'                                                     
         BE    LTVAOK              EXIT IF NO MORE TVAIENCES                    
         TM    DMCB+8,X'80'                                                     
         BO    LTVA020             TEST NOT TO WRITE THIS RECORD                
         CLI   DXWRITE,C'Y'                                                     
         BNE   LTVA020                                                          
         CLI   SXDTPLFM,0                                                       
         BE    LTVA012                                                          
         L     R4,DXASQLB                                                       
*                                  CONVERT RECORD TO SQL BUFFER                 
         GOTO1 VMXCNVX,DMCB,(R8)                                                
         BE    *+6                                                              
         DC    H'0'                DEAL WITH ERROR STATE HERE??                 
*                                                                               
*                                  PUT CONVERTED RECORD TO FILE                 
         GOTO1 DXPUT,DMCB,DXASQLB,(R8)                                          
         B     LTVA020                                                          
*                                  PUT UNCONVERTED RECORD TO FILE               
LTVA012  GOTO1 DXPUT,DMCB,DXAXREC,(R8)                                          
         B     LTVA020                                                          
*                                                                               
LTVA020  MVC   DMCB+8,BYTE         RESTORE DMCB+8                               
         TM    DMCB+8,X'40'        TEST IF CALL BACK REQUIRED                   
         BO    LTVA010                                                          
*                                                                               
LTVAOK   B     YES                                                              
LTVANO   B     NO                                                               
         EJECT                                                                  
***********************************************************************         
* LOAD FOLIO RECORD DATA                                              *         
***********************************************************************         
         SPACE 1                                                                
LOADFOL  NTR1                                                                   
         MVC   MAXIOS,DXMAXREC     MAXIMIMUM RECORD COUNT                       
         MVI   BYTE,0              MEDIA KEY SAVE BYTE                          
*                                                                               
LFOL010  LA    R2,IOKEY            SET KEY TO READ FOLIO RECORDS                
         USING DFOL,R2                                                          
         XC    FOLKEY,FOLKEY                                                    
         MVI   FOLKTYP,FOLKTYPQ                                                 
         MVC   FOLKAM,MEDAGY       SET AGENCY                                   
         CLI   BYTE,0              TV HAS STANDARD RECORDS                      
         BNE   *+8                                                              
         MVI   FOLKAM,0                                                         
         OC    FOLKAM,BYTE         SET MEDIA CODE                               
         L     R2,DXARECB                                                       
         GOTO1 VDATAMGR,DMCB,(X'00',DMRDHI),MEDDIR,IOKEY,(R2),DMWORK            
*                                                                               
LFOL020  TM    8(R1),X'80'         ALL DONE IF EOF                              
         BO    LFOLOK                                                           
         TM    8(R1),X'7D'         DIE IF DISK ERROR                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         CLI   FOLKAM,X'00'        TV STATIONS ARE COMMON                       
         BE    LFOL025                                                          
         MVC   BYTE,FOLKAM                                                      
         NI    BYTE,X'F0'                                                       
         CLC   BYTE,MEDAGY         ALL DONE IF AGENCY CHANGES                   
         BNE   LFOLOK                                                           
LFOL025  CLI   FOLKTYP,FOLKTYPQ    READ MEDFILE IF SAME RECORD TYPE             
         BE    LFOL030                                                          
         LA    R2,IOKEY                                                         
         MVC   BYTE,FOLKAM                                                      
         NI    BYTE,X'0F'                                                       
         CLI   BYTE,X'0F'          ALL DONE IF LAST MEDIA                       
         BE    LFOLOK                                                           
         CLI   BYTE,X'00'          ALL DONE IF TVS ONLY                         
         BNE   *+12                                                             
         CLI   TYPEMED,C'0'                                                     
         BE    LFOLOK                                                           
         ZIC   RF,BYTE             BUMP TO NEXT MEDIA                           
         LA    RF,1(RF)                                                         
         STC   RF,BYTE                                                          
         B     LFOL010                                                          
*                                  TRANSFER RECORD DATA TO EXTRACT FILE         
LFOL030  MVC   MEDADDR,FOLDDA                                                   
         XC    CLIFILT,CLIFILT                                                  
         GOTO1 MEDLOAD,DMCB,VMXFOLC,INITFOL,0                                   
         BNE   LFOLNO                                                           
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         BZ    LFOLOK                                                           
         B     LFOL020                                                          
*                                                                               
LFOLNO   B     NO                                                               
LFOLOK   B     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LOAD OUTLET SCHEME DATA                                             *         
***********************************************************************         
         SPACE 1                                                                
LOADSCH  NTR1                                                                   
         MVC   MAXIOS,DXMAXREC     MAXIMIMUM RECORD COUNT                       
         MVI   BYTE,0              MEDIA KEY SAVE BYTE                          
*                                                                               
LSCH010  LA    R2,IOKEY            SET KEY TO READ FIRST SCHEME RECORD          
         USING DOSC,R2                                                          
         XC    OSCKEY,OSCKEY                                                    
         MVI   OSCKTYP,OSCKTYPQ                                                 
         MVC   OSCKAM,MEDAGY       SET AGENCY                                   
         L     R2,DXARECB                                                       
         GOTO1 VDATAMGR,DMCB,(X'00',DMRDHI),MEDDIR,IOKEY,(R2),DMWORK            
*                                                                               
LSCH020  TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         BO    LSCHOK                                                           
         TM    DMCB+8,X'7D'        DIE IF DISK ERROR                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         CLC   OSCKAM,MEDAGY       ALL DONE IF AGENCY CHANGES                   
         BNE   LSCHOK                                                           
         CLI   OSCKTYP,OSCKTYPQ    ALL DONE IF RECORD TYPE CHANGES              
         BNE   LSCHOK                                                           
         CLI   OSCKSUB,OSCKSSHQ    CHECK SCHEME SUB TYPE                        
         BE    LSCH030                                                          
*                                  SKIP RECORD IF NOT SCHEME SUB TYPE           
LSCH022  MVC   IOKEY(L'MEDKEY),0(R2)                                            
         BAS   RE,CHKSEQIO                                                      
         BE    LSCH024                                                          
         GOTO1 VDATAMGR,DMCB,(X'00',DMRDHI),MEDDIR,IOKEY,(R2),DMWORK            
         TM    8(R1),X'FD'                                                      
         BZ    *+6                                                              
         DC    H'0'                                                             
LSCH024  GOTO1 VDATAMGR,DMCB,(X'00',DMRSEQ),MEDDIR,IOKEY,(R2),DMWORK            
         B     LSCH020                                                          
*                                  TRANSFER RECORD DATA TO EXTRACT FILE         
LSCH030  MVC   MEDADDR,OSCDDA                                                   
         XC    CLIFILT,CLIFILT                                                  
         GOTO1 MEDLOAD,DMCB,VMXSCHC,INITSCH,0                                   
         BNE   LSCHNO                                                           
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         BZ    LSCHOK                                                           
         B     LSCH020                                                          
*                                                                               
LSCHNO   B     NO                                                               
LSCHOK   B     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LOAD OUTLET DATA                                                    *         
***********************************************************************         
         SPACE 1                                                                
LOADOUT  NTR1                                                                   
         MVC   MAXIOS,DXMAXREC     MAXIMIMUM RECORD COUNT                       
         MVI   BYTE,0              MEDIA KEY SAVE BYTE                          
*                                                                               
LOUT010  LA    R2,IOKEY            SET KEY TO READ FIRST SCHEME RECORD          
         USING DOSC,R2                                                          
         XC    OSCKEY,OSCKEY                                                    
         MVI   OSCKTYP,OSCKTYPQ                                                 
         MVC   OSCKAM,MEDAGY       SET AGENCY                                   
         L     R2,DXARECB                                                       
         GOTO1 VDATAMGR,DMCB,(X'00',DMRDHI),MEDDIR,IOKEY,(R2),DMWORK            
*                                                                               
LOUT020  TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         BO    LOUTOK                                                           
         TM    DMCB+8,X'7D'        DIE IF DISK ERROR                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         CLC   OSCKAM,MEDAGY       ALL DONE IF AGENCY CHANGES                   
         BNE   LOUTOK                                                           
         CLI   OSCKTYP,OSCKTYPQ    ALL DONE IF RECORD TYPE CHANGES              
         BNE   LOUTOK                                                           
         CLI   OSCKSUB,OSCKSOTQ    CHECK OUTLET SUB TYPE                        
         BE    LOUT030                                                          
*                                  SKIP RECORD IF NOT OUTLET SUB TYPE           
LOUT022  MVC   IOKEY(L'MEDKEY),0(R2)                                            
         BAS   RE,CHKSEQIO                                                      
         BE    LOUT024                                                          
         GOTO1 VDATAMGR,DMCB,(X'00',DMRDHI),MEDDIR,IOKEY,(R2),DMWORK            
         TM    8(R1),X'FD'                                                      
         BZ    *+6                                                              
         DC    H'0'                                                             
LOUT024  GOTO1 VDATAMGR,DMCB,(X'00',DMRSEQ),MEDDIR,IOKEY,(R2),DMWORK            
         B     LOUT020                                                          
*                                  TRANSFER RECORD DATA TO EXTRACT FILE         
LOUT030  MVC   MEDADDR,OSCDDA                                                   
         XC    CLIFILT,CLIFILT                                                  
         GOTO1 MEDLOAD,DMCB,VMXOUTC,INITOUT,0                                   
         BNE   LOUTNO                                                           
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         BZ    LOUTOK                                                           
         B     LOUT020                                                          
*                                                                               
LOUTNO   B     NO                                                               
LOUTOK   B     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LOAD OUTLET LINK DATA                                               *         
***********************************************************************         
         SPACE 1                                                                
LOADOUL  NTR1                                                                   
         MVC   MAXIOS,DXMAXREC     MAXIMIMUM RECORD COUNT                       
         MVI   BYTE,0              MEDIA KEY SAVE BYTE                          
*                                                                               
LOUL010  LA    R2,IOKEY            SET KEY TO READ FIRST SCHEME RECORD          
         USING DOSC,R2                                                          
         XC    OSCKEY,OSCKEY                                                    
         MVI   OSCKTYP,OSCKTYPQ                                                 
         MVC   OSCKAM,MEDAGY       SET AGENCY                                   
         L     R2,DXARECB                                                       
         GOTO1 VDATAMGR,DMCB,(X'00',DMRDHI),MEDDIR,IOKEY,(R2),DMWORK            
*                                                                               
LOUL020  TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         BO    LOULOK                                                           
         TM    DMCB+8,X'7D'        DIE IF DISK ERROR                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         CLC   OSCKAM,MEDAGY       ALL DONE IF AGENCY CHANGES                   
         BNE   LOULOK                                                           
         CLI   OSCKTYP,OSCKTYPQ    ALL DONE IF RECORD TYPE CHANGES              
         BNE   LOULOK                                                           
         CLI   OSCKSUB,OSCKSLGQ    CHECK OUTLET SUB TYPE                        
         BE    LOUL030                                                          
*                                  SKIP RECORD IF NOT OUTLET LINK TYPE          
LOUL022  MVC   IOKEY(L'MEDKEY),0(R2)                                            
         BAS   RE,CHKSEQIO                                                      
         BE    LOUL024                                                          
         GOTO1 VDATAMGR,DMCB,(X'00',DMRDHI),MEDDIR,IOKEY,(R2),DMWORK            
         TM    8(R1),X'FD'                                                      
         BZ    *+6                                                              
         DC    H'0'                                                             
LOUL024  GOTO1 VDATAMGR,DMCB,(X'00',DMRSEQ),MEDDIR,IOKEY,(R2),DMWORK            
         B     LOUL020                                                          
*                                  TRANSFER RECORD DATA TO EXTRACT FILE         
LOUL030  MVC   MEDADDR,OSCDDA                                                   
         XC    CLIFILT,CLIFILT                                                  
         GOTO1 MEDLOAD,DMCB,VMXOULC,INITOUL,0                                   
         BNE   LOULNO                                                           
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         BZ    LOULOK                                                           
         B     LOUL020                                                          
*                                                                               
LOULNO   B     NO                                                               
LOULOK   B     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LOAD LATEST PROGAM NAME DATA INTO BUY TV TABLE - SPOT MATCHED BUYS  *         
***********************************************************************         
         SPACE 1                                                                
LOADPGN  NTR1                                                                   
         MVI   PGNFLAG,C'M'                                                     
         GOTO1 =A(EXTPGN),(RC)     EXTRACT LATEST BUY TV PROGRAM NAMES          
         BE    LPGNOK                EXIT IF ERROR                              
*                                                                               
LPGNNO   B     NO                                                               
LPGNOK   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* LOAD LATEST PROGAM NAME DATA INTO BUY TV TABLE - ALL BUYS           *         
***********************************************************************         
         SPACE 1                                                                
LOADPGA  NTR1                                                                   
         MVI   PGNFLAG,C'A'                                                     
         GOTO1 =A(EXTPGN),(RC)     EXTRACT LATEST BUY TV PROGRAM NAMES          
         BE    LPGAOK                EXIT IF ERROR                              
*                                                                               
LPGANO   B     NO                                                               
LPGAOK   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* PROCESS MEDIA FILE DATA IN UPDATE MODE READ RECOVERY FILES          *         
***********************************************************************         
         SPACE 1                                                                
PROCUPDT NTR1                                                                   
         L     R6,DXARECB          POINT TO RECOVERY RECORD BUFFER              
         USING RECDS,R6                                                         
         MVC   MEDAGY,SXDTAGB      SET MEDIA AGENCY CODE FROM                   
         NI    MEDAGY,X'F0'          SYSTEM CONTROL TABLE                       
         MVC   TYPECODE,SXDTTYP                                                 
         BAS   RE,GETTYP           SET TYPE TABLE DATA                          
         CLI   RFILTY,MEDFILQ      TEST MEDFILE RECORD TYPE                     
         BNE   PUPDOK                ELSE IGNORE RECORD                         
         BAS   RE,PROCKEY          PROCESS RECORD KEY VALUES                    
         BNE   PUPDOK                EITHER IGNORE RECORD                       
         ICM   RF,15,TYPEAUPD        ELSE CALL UPDATE PROCESS ROUTINE           
         BZ    PUPDOK                                                           
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
         LA    RE,TYPTAB           SET A(RECORD TYPE DATA TABLE)                
*                                                                               
UALL010  CLI   0(RE),0             EXIT IF END OF TABLE                         
         BE    UALLOK                                                           
         CLC   RECVHDR+L'RECVHDR+1(1),TRECTYP(RE)  COMPARE RECORD TYPE          
         BE    UALL020                                                          
         LA    RE,L'TYPTAB(RE)     GET NEXT ENTRY                               
         B     UALL010                                                          
*                                                                               
UALL020  EQU   *                   RECORD TYPE MATCH FOUND                      
         ICM   RF,15,TAUPDT(RE)    CALL EXTRACT ROUTINE                         
         BZ    UALLOK              UNLESS NOT WRITTEN                           
         A     RF,RELO                                                          
         BASR  RE,RF                                                            
         BNE   UALLNO                                                           
         B     UALLOK                                                           
*                                                                               
UALLNO   B     NO                                                               
UALLOK   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* UPDATE AGENCY RECORD DATA                                           *         
***********************************************************************         
         SPACE 1                                                                
UPDTAGY  NTR1                                                                   
*                                  SET A(AGENCY RECORD) FROM                    
         LA    R2,RECVHDR+L'RECVHDR  RECOVERY RECORD BUFFER                     
         USING DAGY,R2                                                          
         BAS   RE,INITAGY          INITIALISE EXTRACT BUFFER                    
*                                  TRANSFER RECORD DATA TO EXTRACT FILE         
         XC    CLIFILT,CLIFILT                                                  
         GOTO1 MEDUPDT,DMCB,VMXAGYC,INITAGY,0                                   
         BNE   UAGYNO                                                           
         B     UAGYOK                                                           
*                                                                               
UAGYNO   B     NO                                                               
UAGYOK   B     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* UPDATE CLIENT RECORD DATA                                           *         
***********************************************************************         
         SPACE 1                                                                
UPDTCLI  NTR1                                                                   
*                                  SET A(CLIENT RECORD) FROM                    
         LA    R2,RECVHDR+L'RECVHDR  RECOVERY RECORD BUFFER                     
         USING DCLI,R2                                                          
         BAS   RE,INITCLI          INITIALISE EXTRACT BUFFER                    
         MVC   CLIFILT,CLIKCLI                                                  
*                                  TRANSFER RECORD DATA TO EXTRACT FILE         
         GOTO1 MEDUPDT,DMCB,VMXCLIC,INITCLI,0                                   
         BNE   UCLINO                                                           
         B     UCLIOK                                                           
*                                                                               
UCLINO   B     NO                                                               
UCLIOK   B     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* UPDATE PRODUCT RECORD DATA                                          *         
***********************************************************************         
         SPACE 1                                                                
UPDTPRO  NTR1                                                                   
*                                  SET A(PRODUCT RECORD) FROM                   
         LA    R2,RECVHDR+L'RECVHDR  RECOVERY RECORD BUFFER                     
         USING DPRO,R2                                                          
         BAS   RE,INITPRO          INITIALISE EXTRACT BUFFER                    
         MVC   CLIFILT,PROKCLI                                                  
*                                  TRANSFER RECORD DATA TO EXTRACT FILE         
         GOTO1 MEDUPDT,DMCB,VMXPROC,INITPRO,0                                   
         BNE   UPRONO                                                           
         B     UPROOK                                                           
*                                                                               
UPRONO   B     NO                                                               
UPROOK   B     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* UPDATE CAMPAIGN RECORD DATA                                         *         
***********************************************************************         
         SPACE 1                                                                
UPDTCAM  NTR1                                                                   
*                                  SET A(CAMPAIGN RECORD) FROM                  
         LA    R2,RECVHDR+L'RECVHDR  RECOVERY RECORD BUFFER                     
         USING DCAMP,R2                                                         
         BAS   RE,INITCAM          INITIALISE EXTRACT BUFFER                    
         MVC   CLIFILT,CAMKCLI                                                  
*                                  TRANSFER RECORD DATA TO EXTRACT FILE         
         GOTO1 MEDUPDT,DMCB,VMXCAMC,INITCAM,0                                   
         BNE   UCAMNO                                                           
         B     UCAMOK                                                           
*                                                                               
UCAMNO   B     NO                                                               
UCAMOK   B     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* UPDATE BURST RECORD DATA                                            *         
***********************************************************************         
         SPACE 1                                                                
UPDTBUR  NTR1                                                                   
*                                  SET A(BURST) FROM                            
         LA    R2,RECVHDR+L'RECVHDR  RECOVERY RECORD BUFFER                     
         USING BURKEY,R2                                                        
         BAS   RE,INITBUR          INITIALISE EXTRACT BUFFER                    
         MVC   CLIFILT,BURKCLI                                                  
*                                  TRANSFER RECORD DATA TO EXTRACT FILE         
         GOTO1 MEDUPDT,DMCB,VMXBURC,INITBUR,0                                   
         BNE   UBURNO                                                           
         B     UBUROK                                                           
*                                                                               
UBURNO   B     NO                                                               
UBUROK   B     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* UPDATE BUY RECORD DATA                                              *         
***********************************************************************         
         SPACE 1                                                                
UPDTBUY  EQU   *                                                                
*                                  SET A(BUY RECORD) FROM                       
         LA    R2,RECVHDR+L'RECVHDR  RECOVERY RECORD BUFFER                     
         USING DBUY,R2                                                          
         BAS   RE,INITBUY          INITIALISE EXTRACT BUFFER                    
         MVC   CLIFILT,BUYKCLI                                                  
*                                  TRANSFER RECORD DATA TO EXTRACT FILE         
         GOTO1 MEDUPDT,DMCB,VMXBUYC,INITBUY,FILTBUY                             
         BNE   UBUYNO                                                           
         B     UBUYOK                                                           
*                                                                               
UBUYNO   B     NO                                                               
UBUYOK   B     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* UPDATE BUYER RECORD DATA                                            *         
***********************************************************************         
         SPACE 1                                                                
UPDTBYR  NTR1                                                                   
*                                  SET A(BUYER RECORD) FROM                     
         LA    R2,RECVHDR+L'RECVHDR  RECOVERY RECORD BUFFER                     
         USING DBYR,R2                                                          
         BAS   RE,INITBYR          INITIALISE EXTRACT BUFFER                    
*                                  TRANSFER RECORD DATA TO EXTRACT FILE         
         XC    CLIFILT,CLIFILT                                                  
         GOTO1 MEDUPDT,DMCB,VMXBYRC,INITBYR,0                                   
         BNE   UBYRNO                                                           
         B     UBYROK                                                           
*                                                                               
UBYRNO   B     NO                                                               
UBYROK   B     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* UPDATE MEDIA RECORD DATA                                            *         
***********************************************************************         
         SPACE 1                                                                
UPDTMED  NTR1                                                                   
*                                  SET A(MEDIA RECORD) FROM                     
         LA    R2,RECVHDR+L'RECVHDR  RECOVERY RECORD BUFFER                     
         USING DMED,R2                                                          
         BAS   RE,INITMED          INITIALISE EXTRACT BUFFER                    
*                                  TRANSFER RECORD DATA TO EXTRACT FILE         
         XC    CLIFILT,CLIFILT                                                  
         GOTO1 MEDUPDT,DMCB,VMXMEDC,INITMED,0                                   
         BNE   UMEDNO                                                           
         B     UMEDOK                                                           
*                                                                               
UMEDNO   B     NO                                                               
UMEDOK   B     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* UPDATE SUPPLIER RECORD DATA                                         *         
***********************************************************************         
         SPACE 1                                                                
UPDTSUP  NTR1                                                                   
*                                  SET A(SUPPLIER RECORD) FROM                  
         LA    R2,RECVHDR+L'RECVHDR  RECOVERY RECORD BUFFER                     
         USING DSTA,R2                                                          
         BAS   RE,INITSUP          INITIALISE EXTRACT BUFFER                    
*                                  TRANSFER RECORD DATA TO EXTRACT FILE         
         XC    CLIFILT,CLIFILT                                                  
         GOTO1 MEDUPDT,DMCB,VMXSUPC,INITSUP,0                                   
         BNE   USUPNO                                                           
         B     USUPOK                                                           
*                                                                               
USUPNO   B     NO                                                               
USUPOK   B     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* UPDATE FOLIO RECORD DATA                                            *         
***********************************************************************         
         SPACE 1                                                                
UPDTFOL  NTR1                                                                   
*                                  SET A(FOLIO RECORD) FROM                     
         LA    R2,RECVHDR+L'RECVHDR  RECOVERY RECORD BUFFER                     
         USING DFOL,R2                                                          
         BAS   RE,INITFOL          INITIALISE EXTRACT BUFFER                    
*                                  TRANSFER RECORD DATA TO EXTRACT FILE         
         XC    CLIFILT,CLIFILT                                                  
         GOTO1 MEDUPDT,DMCB,VMXFOLC,INITFOL,0                                   
         BNE   UFOLNO                                                           
         B     UFOLOK                                                           
*                                                                               
UFOLNO   B     NO                                                               
UFOLOK   B     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* UPDATE OUTLET SCHEME DATA                                           *         
***********************************************************************         
         SPACE 1                                                                
UPDTSCH  NTR1                                                                   
*                                  SET A(OUTLET SCHME RECORD) FROM              
         LA    R2,RECVHDR+L'RECVHDR  RECOVERY RECORD BUFFER                     
         USING DOSC,R2                                                          
         BAS   RE,INITSCH          INITIALISE EXTRACT BUFFER                    
*                                  TRANSFER RECORD DATA TO EXTRACT FILE         
         XC    CLIFILT,CLIFILT                                                  
         GOTO1 MEDUPDT,DMCB,VMXSCHC,INITSCH,0                                   
         BNE   USCHNO                                                           
         B     USCHOK                                                           
*                                                                               
USCHNO   B     NO                                                               
USCHOK   B     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* UPDATE OUTLET DATA                                                  *         
***********************************************************************         
         SPACE 1                                                                
UPDTOUT  NTR1                                                                   
*                                  SET A(OUTLET SCHME RECORD) FROM              
         LA    R2,RECVHDR+L'RECVHDR  RECOVERY RECORD BUFFER                     
         USING DOSC,R2                                                          
         BAS   RE,INITOUT          INITIALISE EXTRACT BUFFER                    
*                                  TRANSFER RECORD DATA TO EXTRACT FILE         
         XC    CLIFILT,CLIFILT                                                  
         GOTO1 MEDUPDT,DMCB,VMXOUTC,INITOUT,0                                   
         BNE   UOUTNO                                                           
         B     UOUTOK                                                           
*                                                                               
UOUTNO   B     NO                                                               
UOUTOK   B     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* UPDATE OUTLET LINK DATA                                             *         
***********************************************************************         
         SPACE 1                                                                
UPDTOUL  NTR1                                                                   
*                                  SET A(OUTLET SCHME RECORD) FROM              
         LA    R2,RECVHDR+L'RECVHDR  RECOVERY RECORD BUFFER                     
         USING DOSC,R2                                                          
         BAS   RE,INITOUL          INITIALISE EXTRACT BUFFER                    
*                                  TRANSFER RECORD DATA TO EXTRACT FILE         
         XC    CLIFILT,CLIFILT                                                  
         GOTO1 MEDUPDT,DMCB,VMXOULC,INITOUL,0                                   
         BNE   UOULNO                                                           
         B     UOULOK                                                           
*                                                                               
UOULNO   B     NO                                                               
UOULOK   B     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* PROCESS KEY VALUES IN RECOVERY RECORD                               *         
***********************************************************************         
         SPACE 1                                                                
         USING RECDS,R6                                                         
PROCKEY  NTR1  ,                                                                
         CLI   TYPEREC,0           TEST RECORD TYPE CODE                        
         BE    PKEY010               IF SET FROM TYPE TABLE                     
         CLC   TYPEREC,RECVHDR+L'RECVHDR+1                                      
         BNE   PKEYNO              IGNORE RECORD IF NOT THIS TYPE               
*                                                                               
PKEY010  MVC   BYTE,RECVHDR+L'RECVHDR                                           
         NI    BYTE,X'F0'                                                       
         CLI   MEDAGY,0            TEST MEDIA AGENCY CODE                       
         BE    PKEY020                                                          
         CLC   BYTE,MEDAGY                                                      
         BNE   PKEYNO              IGNORE RECORD IF NOT THIS AGENCY             
*                                                                               
PKEY020  MVC   BYTE,RECVHDR+L'RECVHDR                                           
         NI    BYTE,X'0F'                                                       
         CLI   SXDTFLT1,C' '       TEST MEDIA FILTER IF SET                     
         BE    PKEY100                                                          
         CLC   BYTE,MEDCHAR                                                     
         BE    PKEY100                                                          
         B     PKEYNO                                                           
*                                  TEST FOR DELETED RECORD                      
PKEY100  EQU   *                     USING MEDIA AGENCY RECORD DSECT            
         TM    RECVHDR+L'RECVHDR+AGYSTAT-AGYKEY,X'80'                           
         BZ    PKEY110                                                          
         CLI   DXACTION,C'C'                                                    
         BNE   PKEYNO                                                           
         CLI   RRECTY,X'02'                                                     
         BNE   PKEYNO                                                           
         L     R4,DXACPYB                                                       
         TM    L'RECVHDR+AGYSTAT-AGYKEY+4(R4),X'80'                             
         BNZ   PKEYNO              AVOID DELETED 'CHANGED'                      
*                                  CAN HAPPEN TO STARNGE BUY RECORDS            
         MVI   DXACTION,C'D'                                                    
         B     PKEYOK                                                           
*                                  TEST FOR RESTORED RECORD                     
*                                    USING SAVED RECOVERY COPY RECORD           
PKEY110  CLI   RRECTY,X'02'          WITH CHANGE RECOVERY RECORD TYPE           
         BNE   PKEYOK                                                           
         L     R4,DXACPYB                                                       
         TM    L'RECVHDR+AGYSTAT-AGYKEY+4(R4),X'80'                             
         BZ    PKEYOK                                                           
         MVI   DXACTION,C'A'                                                    
         B     PKEYOK                                                           
*                                                                               
PKEYNO   B     NO                                                               
PKEYOK   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* FILTER BUY RECORD AT R2                                             *         
***********************************************************************         
         SPACE 1                                                                
FILTBUY  NTR1  ,                                                                
         USING DBUY,R2                                                          
         OC    DXFDATEC,DXFDATEC   TEST FROM DATE                               
         BZ    FBUY010                                                          
         CLC   BUYKDAT,DXFDATEC                                                 
         BL    FBUYNO                                                           
*                                                                               
FBUY010  OC    DXTDATEC,DXTDATEC   TEST TO DATE                                 
         BZ    FBUY020                                                          
         CLC   BUYKDAT,DXTDATEC                                                 
         BH    FBUYNO                                                           
*                                                                               
FBUY020  EQU   *                                                                
         CLC   SXDTAGY,=CL2'RA'                                                 
         BNE   FBUYOK                                                           
*        CLC   BUYSER,=XL4'E31D0543'                                            
*        BE    FBUYNO                                                           
         B     FBUYOK                                                           
*                                                                               
FBUYNO   B     NO                                                               
FBUYOK   B     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* FILTER BUY RECORD AT R2 TEST ROUTINE 1                              *         
***********************************************************************         
         SPACE 1                                                                
FILTBUY1 NTR1  ,                                                                
         USING DBUY,R2                                                          
         CLC   SXDTAGY,=CL2'RA'                                                 
         BNE   FBU1OK                                                           
         CLC   BUYSER,=XL4'E31D0543'                                            
         BE    FBU1NO                                                           
         CLC   BUYSER,=XL4'E31D27CF'                                            
         BE    FBU1NO                                                           
         CLC   BUYSER,=XL4'E31D27D0'                                            
         BE    FBU1NO                                                           
         B     FBU1OK                                                           
*                                                                               
FBU1NO   B     NO                                                               
FBU1OK   B     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* GET RECORD TYPE TABLE VALUES FROM 3 CHARACTER CODE                  *         
***********************************************************************         
         SPACE 1                                                                
GETTYP   NTR1                                                                   
         LA    RE,TYPTAB                                                        
*                                                                               
GTYP010  CLI   0(RE),0             END OF TABLE                                 
         BNE   *+6                                                              
         DC    H'00'                                                            
         CLC   TYPECODE,TNAME(RE)  COMPARE NAME                                 
         BE    GTYP020                                                          
         LA    RE,L'TYPTAB(RE)     GET NEXT ENTRY                               
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
*                                                                               
         L     R3,DXAXREC          R3=A(EXTRACT RECORD AREA)                    
         USING DXHDRD,R3                                                        
         LR    R0,R3                                                            
         LA    RE,*                                                             
         SR    RF,RF                                                            
         ICM   RF,8,MXSPACES                                                    
         MVCL  R0,RE               SET RECORD TO ALL SPACES                     
*                                                                               
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
         L     R6,DXARECB          POINT TO RECOVERY RECORD BUFFER              
         USING RECDS,R6                                                         
* ??     GOTO1 VDATCON,DMCB,(3,RDATE),(X'34',DXHDRCDT+2)                        
         GOTO1 VDATCON,DMCB,(3,RDATE),(X'34',DXHDRCDT)                          
* ??     MVC   DXHDRCDT(2),DXCENT                                               
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
         MVC   DXHDRCTI+L'DXHDRCTI+1(2),SXDTAGY                                 
         MVI   DXHDRCTI+L'DXHDRCTI,MXTRTQ                                       
         B     YES                                                              
*                                  HERE IF LOAD MODE                            
IALL100  MVC   DXHDRCDT,DXDATEN    SET TODAYS DATE                              
         MVI   DXHDRCTI-1,MXTRTQ                                                
         MVC   DXHDRCTI,DXTIMEN                                                 
         MVC   DXHDRCTI+L'DXHDRCTI+1(2),SXDTAGY                                 
         MVI   DXHDRCTI+L'DXHDRCTI,MXTRTQ                                       
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
* INITIALISE AUDIENCE EXTRACT RECORD                                  *         
***********************************************************************         
         SPACE 1                                                                
INITAUD  NTR1                                                                   
         LA    R1,MXAUDDL          R1=L'AUDIENCE RECORD                         
         BAS   RE,INITALL                                                       
         B     YES                                                              
         SPACE 1                                                                
***********************************************************************         
* INITIALISE AGENCY EXTRACT RECORD                                    *         
***********************************************************************         
         SPACE 1                                                                
INITAGY  NTR1                                                                   
         LA    R1,MXAGYDL          R1=L'AGENCY RECORD                           
         BAS   RE,INITALL                                                       
         B     YES                                                              
         SPACE 1                                                                
***********************************************************************         
* INITIALISE BUYER EXTRACT RECORD                                     *         
***********************************************************************         
         SPACE 1                                                                
INITBYR  NTR1                                                                   
         LA    R1,MXBYRDL          R1=L'BUYER RECORD                            
         BAS   RE,INITALL                                                       
         B     YES                                                              
         SPACE 1                                                                
***********************************************************************         
* INITIALISE MEDIA EXTRACT RECORD                                     *         
***********************************************************************         
         SPACE 1                                                                
INITMED  NTR1                                                                   
         LA    R1,MXMEDDL          R1=L'MEDIA RECORD                            
         BAS   RE,INITALL                                                       
         B     YES                                                              
         SPACE 1                                                                
***********************************************************************         
* INITIALISE CLIENT EXTRACT RECORD                                    *         
***********************************************************************         
         SPACE 1                                                                
INITCLI  NTR1                                                                   
         LA    R1,MXCLIDL          R1=L'CLIENT RECORD                           
         BAS   RE,INITALL                                                       
         B     YES                                                              
         SPACE 1                                                                
***********************************************************************         
* INITIALISE PRODUCT EXTRACT RECORD                                   *         
***********************************************************************         
         SPACE 1                                                                
INITPRO  NTR1                                                                   
         LA    R1,MXPRODL          R1=L'PRODUCT RECORD                          
         BAS   RE,INITALL                                                       
         B     YES                                                              
         SPACE 1                                                                
***********************************************************************         
* INITIALISE CAMPAIGN EXTRACT RECORD                                  *         
***********************************************************************         
         SPACE 1                                                                
INITCAM  NTR1                                                                   
         LA    R1,MXCAMDL          R1=L'CAMPAIGN RECORD                         
         BAS   RE,INITALL                                                       
         B     YES                                                              
         SPACE 1                                                                
***********************************************************************         
* INITIALISE BUY EXTRACT RECORD                                       *         
***********************************************************************         
         SPACE 1                                                                
INITBUY  NTR1                                                                   
         LA    R1,MXBUYDL          R1=L'BUY RECORD (LONGEST)                    
         CH    R1,=AL2(MXBTVDL)      (FIND LONGEST POSSIBLE)                    
         BH    *+8                                                              
         LA    R1,MXBTVDL                                                       
         CH    R1,=AL2(MXBTV3DL)                                                
         BH    *+8                                                              
         LA    R1,MXBTV3DL                                                      
         CH    R1,=AL2(MXBPRDL)                                                 
         BH    *+8                                                              
         LA    R1,MXBPRDL                                                       
         CH    R1,=AL2(MXBTRDL)                                                 
         BH    *+8                                                              
         LA    R1,MXBTRDL                                                       
         CH    R1,=AL2(MXBOSDL)                                                 
         BH    *+8                                                              
         LA    R1,MXBOSDL                                                       
*                                                                               
IBUY010  BAS   RE,INITALL                                                       
         B     YES                                                              
         SPACE 1                                                                
***********************************************************************         
* INITIALISE SUPPLIER EXTRACT RECORD                                  *         
***********************************************************************         
         SPACE 1                                                                
INITSUP  NTR1                                                                   
         LA    R1,MXSUPNDL         R1=L'SUPPLIER RECORD                         
         BAS   RE,INITALL                                                       
         B     YES                                                              
         SPACE 1                                                                
***********************************************************************         
* INITIALISE TV AREA EXTRACT RECORD                                   *         
***********************************************************************         
         SPACE 1                                                                
INITTVA  NTR1                                                                   
         LA    R1,MXTVADL          R1=L'TVAREA RECORD                           
         BAS   RE,INITALL                                                       
         B     YES                                                              
         SPACE 1                                                                
***********************************************************************         
* INITIALISE FOLIO EXTRACT RECORD                                     *         
***********************************************************************         
         SPACE 1                                                                
INITFOL  NTR1                                                                   
         LA    R1,MXFOLDL          R1=L'FOLIO RECORD                            
         BAS   RE,INITALL                                                       
         B     YES                                                              
         SPACE 1                                                                
***********************************************************************         
* INITIALISE OUTLET SCHEME EXTRACT RECORD                             *         
***********************************************************************         
         SPACE 1                                                                
INITSCH  NTR1                                                                   
         LA    R1,MXSCHDL          R1=L'SCHEME RECORD                           
         BAS   RE,INITALL                                                       
         B     YES                                                              
         SPACE 1                                                                
***********************************************************************         
* INITIALISE OUTLET EXTRACT RECORD                                    *         
***********************************************************************         
         SPACE 1                                                                
INITOUT  NTR1                                                                   
         LA    R1,MXOUTDL          R1=L'OUTLET RECORD                           
         BAS   RE,INITALL                                                       
         B     YES                                                              
         SPACE 1                                                                
***********************************************************************         
* INITIALISE OUTLET LINK EXTRACT RECORD                               *         
***********************************************************************         
         SPACE 1                                                                
INITOUL  NTR1                                                                   
         LA    R1,MXOULDL          R1=L'OUTLET LINK RECORD                      
         BAS   RE,INITALL                                                       
         B     YES                                                              
         SPACE 1                                                                
***********************************************************************         
* INITIALISE BURST EXTRACT RECORD                                     *         
***********************************************************************         
         SPACE 1                                                                
INITBUR  NTR1                                                                   
         LA    R1,MXBURDL          R1=L'BURST RECORD                            
         BAS   RE,INITALL                                                       
         B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* SUBROUTINE TO EXTRACT MEDIA RECORDS IN LOAD MODE                    *         
* R2 = A(MEDIA DIRECTORY RECORD BUFFER)                               *         
* P1 = A(EXTRACT ROUTINE)                                             *         
* P2 = NOT USED                                                       *         
* P3 = A(EXTRACT RECORD INITIALISATION ROUTINE)                       *         
* P5 = A(RECORD FILTER ROUTINE)                                       *         
***********************************************************************         
         SPACE 1                                                                
MEDLOAD  NTR1                                                                   
         LM    R3,R5,0(R1)                                                      
*                                                                               
         BAS   RE,CHKCLI           CHECK FOR CLIENT FILTER                      
         BNE   MLOA060                                                          
*                                                                               
         LTR   R5,R5               TEST IF FILTER ROUTINE PASSED                
         BZ    MLOA010                                                          
         GOTO1 (R5)                FILTER RECORD, GET NEXT IF NOT VALID         
         BNE   MLOA060                                                          
*                                                                               
MLOA010  GOTO1 VDATAMGR,DMCB,(X'00',GETREC),MEDFILE,MEDADDR,(R2),DMWORK         
         CLI   8(R1),0                                                          
         BNE   MLOANO                                                           
*                                                                               
MLOA020  GOTO1 (R4)                INITIALISE EXTRACT BUFFER                    
         SR    RF,RF               SET MEDIA FILTER                             
         ICM   RF,8,TYPEMED                                                     
*                                  CALL RECORD EXTRACT ROUTINE                  
         GOTO1 (R3),DMCB,DXAXREC,(R2),(RF),DXACPYB,(R8)                         
         MVC   BYTE,DMCB+8         SAVE RETURN CODE FOR CALL BACK TEST          
         CLI   DMCB+8,X'88'        TEST RETURN CODE                             
         BE    MLOA050             TEST NO CALL BACK                            
         TM    DMCB+8,X'80'                                                     
         BO    MLOA040             TEST NOT TO WRITE THIS RECORD                
         BAS   RE,CHKBUYST         CHECK FOR BUY SUB-RECORD TYPE                
         BNE   MLOA040                                                          
         CLI   DXWRITE,C'Y'                                                     
         BNE   MLOA050                                                          
         CLI   SXDTPLFM,0          TEST EXTRACT FILE PLATFORM                   
         BE    MLOA030                                                          
*                                  CONVERT RECORD TO SQL BUFFER                 
         GOTO1 VMXCNVX,DMCB,(R8)                                                
         BE    *+6                                                              
         DC    H'0'                DEAL WITH ERROR STATE HERE??                 
*                                                                               
*                                  PUT CONVERTED RECORD TO FILE                 
         GOTO1 DXPUT,DMCB,DXASQLB,(R8)                                          
         B     MLOA040                                                          
*                                  PUT UNCONVERTED RECORD TO FILE               
MLOA030  GOTO1 DXPUT,DMCB,DXAXREC,(R8)                                          
*                                                                               
MLOA040  TM    BYTE,X'40'          TEST CALL BACK REQUIRED                      
         BO    MLOA020                                                          
*                                                                               
MLOA050  BAS   RE,DECIOC           DECREMENT MAX IO COUNTER                     
         BNE   MLOAOK                                                           
*                                                                               
MLOA060  EQU   *                   READ NEXT RECORD                             
         MVC   IOKEY(L'MEDKEY),0(R2)                                            
         BAS   RE,CHKSEQIO                                                      
         BE    MLOA070                                                          
         GOTO1 VDATAMGR,DMCB,(X'00',DMRDHI),MEDDIR,IOKEY,(R2),DMWORK            
         TM    8(R1),X'FD'                                                      
         BZ    *+6                                                              
         DC    H'0'                                                             
MLOA070  GOTO1 VDATAMGR,DMCB,(X'00',DMRSEQ),MEDDIR,IOKEY,(R2),DMWORK            
         B     MLOAOK                                                           
*                                                                               
MLOANO   B     NO                                                               
MLOAOK   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* SUBROUTINE TO EXTRACT MEDIA RECORDS IN UPDATE MODE                  *         
* R2 = A(MEDIA RECORD BUFFER)                                         *         
* P1 = A(EXTRACT ROUTINE)                                             *         
* P2 = NOT USED                                                       *         
* P3 = A(EXTRACT RECORD INITIALISATION ROUTINE)                       *         
* P4 = A(FILTER ROUTINE)                                              *         
***********************************************************************         
         SPACE 1                                                                
MEDUPDT  NTR1                                                                   
         LM    R3,R5,0(R1)                                                      
*                                                                               
         BAS   RE,CHKCLI           CHECK FOR CLIENT FILTER                      
         BNE   MUPDOK                                                           
*                                                                               
         LTR   R5,R5               TEST IF FILTER ROUTINE PASSED                
         BZ    MUPD010                                                          
         GOTO1 (R5)                FILTER RECORD, GET NEXT IF NOT VALID         
         BNE   MUPDOK                                                           
*                                                                               
MUPD010  GOTO1 (R4)                INITIALISE EXTRACT BUFFER                    
         SR    RF,RF               SET MEDIA FILTER                             
         ICM   RF,8,TYPEMED                                                     
*                                  CALL EXTRACT ROUTINE                         
         GOTO1 (R3),DMCB,DXAXREC,(R2),(RF),DXACPYB,(R8)                         
         MVC   BYTE,DMCB+8         SAVE CALL EXTRACT CALL RETURN CODE           
         CLI   DMCB+8,X'88'        TEST RETURN CODE                             
         BE    MUPDOK                                                           
         TM    DMCB+8,X'80'                                                     
         BO    MUPD030                                                          
         CLI   DXWRITE,C'Y'                                                     
         BNE   MUPDOK                                                           
         CLI   SXDTPLFM,0                                                       
         BE    MUPD020                                                          
*                                  CONVERT RECORD TO SQL BUFFER                 
         GOTO1 VMXCNVX,DMCB,(R8)                                                
         BE    *+6                                                              
         DC    H'0'                DEAL WITH ERROR STATE HERE??                 
*                                                                               
*                                  PUT CONVERTED RECORD TO FILE                 
         GOTO1 DXPUT,DMCB,DXASQLB,(R8)                                          
         B     MUPD030                                                          
*                                  PUT UNCONVERTED RECORD TO FILE               
MUPD020  GOTO1 DXPUT,DMCB,DXAXREC,(R8)                                          
*                                                                               
MUPD030  EQU   *                                                                
         TM    BYTE,X'40'          TEST IF EXTRACT CALL BACK REQUIRED           
         BO    MUPD010                                                          
         B     MUPDOK                                                           
*                                                                               
MUPDNO   EQU   *                                                                
         B     NO                                                               
MUPDOK   EQU   *                                                                
         B     YES                                                              
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
         SPACE 1                                                                
***********************************************************************         
* TEST PRINT OUT 1                                                    *         
***********************************************************************         
         SPACE 1                                                                
TSTPRNT1 NTR1                                                                   
         L     R6,=V(CPRINT)                                                    
         USING DPRINT,R6                                                        
         L     RF,DXASQLB                                                       
         MVC   P(132),0(RF)                                                     
         GOTO1 =V(PRINTER)                                                      
         L     RF,DXAXREC                                                       
         MVC   P(132),0(RF)                                                     
         GOTO1 =V(PRINTER)                                                      
         GOTO1 =V(HEXOUT),DMCB,(R2),P,66,=C'TOG'                                
         GOTO1 =V(PRINTER)                                                      
         MVC   P(132),0(R2)                                                     
         GOTO1 =V(PRINTER)                                                      
         MVC   P(5),=C'DMCB:'                                                   
         GOTO1 =V(HEXOUT),DMCB,DMCBTEST,P+7,20,=C'TOG'                          
         GOTO1 =V(PRINTER)                                                      
         GOTO1 =V(PRINTER)                                                      
         XIT1                                                                   
         DROP  R6                                                               
         SPACE 1                                                                
***********************************************************************         
* TEST PRINT OUT 2                                                    *         
***********************************************************************         
         SPACE 1                                                                
TSTPRNT2 NTR1                                                                   
         L     R6,=V(CPRINT)                                                    
         USING DPRINT,R6                                                        
         MVC   P(5),=C'DMCB:'                                                   
         GOTO1 =V(HEXOUT),DMCB,DMCBTEST,P+7,20,=C'TOG'                          
         GOTO1 =V(PRINTER)                                                      
         GOTO1 =V(PRINTER)                                                      
         XIT1                                                                   
         DROP  R6                                                               
         SPACE 1                                                                
***********************************************************************         
* CHECK CLIENT FILTER                                                 *         
***********************************************************************         
         SPACE 1                                                                
CHKCLI   NTR1                                                                   
         OC    CLIFILT,CLIFILT                                                  
         BZ    YES                                                              
*        CLC   SXDTAGY,=CL2'LK'                                                 
*        BE    CHLK                                                             
         CLC   SXDTAGY,=CL2'JW'                                                 
         BE    CHJW                                                             
         B     YES                                                              
*                                  LEO LK CLIENT FILTERS                        
CHLK     EQU   *                                                                
         CLC   CLIFILT,=CL3'PGL'                                                
         BE    YES                                                              
         CLC   CLIFILT,=CL3'RVL'                                                
         BE    YES                                                              
         CLC   CLIFILT,=CL3'RVM'                                                
         BE    YES                                                              
         B     NO                                                               
*                                  JWT JW CLIENT FILTERS                        
CHJW     EQU   *                                                                
         CLC   CLIFILT,=XL3'C32328'                                             
         BE    YES                                                              
         B     NO                                                               
         SPACE 1                                                                
***********************************************************************         
* CHECK BUY SUB-RECORD TYPE                                           *         
***********************************************************************         
         SPACE 1                                                                
CHKBUYST NTR1                                                                   
         ICM   RF,15,DXAXREC                                                    
         CLC   TYPECODE,=CL3'BTV'                                               
         BE    CBSTTV                                                           
         CLC   TYPECODE,=CL3'BTR'                                               
         BE    CBSTTR                                                           
         CLC   TYPECODE,=CL3'BYO'                                               
         BE    CBSTYO                                                           
         CLC   TYPECODE,=CL3'BXR'                                               
         BE    CBSTXR                                                           
         CLC   TYPECODE,=CL3'BBR'                                               
         BE    CBSTBR                                                           
         CLC   TYPECODE,=CL3'BPT'                                               
         BE    CBSTBP                                                           
         CLC   TYPECODE,=CL3'BRC'                                               
         BE    CBSTBRC                                                          
         CLC   TYPECODE,=CL3'BIN'                                               
         BE    CBSTBI                                                           
         CLC   TYPECODE,=CL3'BCO'                                               
         BE    CBSTBC                                                           
         CLC   TYPECODE,=CL3'BPV'                                               
         BE    CBSTBPV                                                          
         CLC   TYPECODE,=CL3'BBV'                                               
         BE    CBSTBBV                                                          
         CLC   TYPECODE,=CL3'BPR'                                               
         BE    CBSTBPR                                                          
         CLC   TYPECODE,=CL3'BOS'                                               
         BE    CBSTBOS                                                          
         B     YES                                                              
         USING MXBTVD,RF                                                        
CBSTTV   CLC   MXBTVTYP,MXBTVDQ                                                 
         BE    YES                                                              
         B     NO                                                               
         DROP  RF                                                               
         USING MXBTRD,RF                                                        
CBSTTR   CLC   MXBTRTYP,MXBTRDQ                                                 
         BE    YES                                                              
         B     NO                                                               
         DROP  RF                                                               
         USING MXBUYD,RF                                                        
CBSTYO   CLC   MXBUYTYP,MXBUYDQ                                                 
         BE    YES                                                              
         B     NO                                                               
         DROP  RF                                                               
         USING MXBXRD,RF                                                        
CBSTXR   CLC   MXBXRTYP,MXBXRDQ                                                 
         BE    YES                                                              
         B     NO                                                               
         DROP  RF                                                               
         USING MXBBRD,RF                                                        
CBSTBR   CLC   MXBBRTYP,MXBBRDQ                                                 
         BE    YES                                                              
         B     NO                                                               
         DROP  RF                                                               
         USING MXBPTD,RF                                                        
CBSTBP   CLC   MXBPTTYP,MXBPTDQ                                                 
         BE    YES                                                              
         B     NO                                                               
         DROP  RF                                                               
         USING MXBRCD,RF                                                        
CBSTBRC  CLC   MXBRCTYP,MXBRCDQ                                                 
         BE    YES                                                              
         B     NO                                                               
         DROP  RF                                                               
         USING MXBIND,RF                                                        
CBSTBI   CLC   MXBINTYP,MXBINDQ                                                 
         BE    YES                                                              
         B     NO                                                               
         DROP  RF                                                               
         USING MXBCOD,RF                                                        
CBSTBC   CLC   MXBCOTYP,MXBCODQ                                                 
         BE    YES                                                              
         B     NO                                                               
         DROP  RF                                                               
         USING MXBPVD,RF                                                        
CBSTBPV  CLC   MXBPVTYP,MXBPVDQ                                                 
         BE    YES                                                              
         B     NO                                                               
         DROP  RF                                                               
         USING MXBBVD,RF                                                        
CBSTBBV  CLC   MXBBVTYP,MXBBVDQ                                                 
         BE    YES                                                              
         B     NO                                                               
         DROP  RF                                                               
         USING MXBPRD,RF                                                        
CBSTBPR  CLC   MXBPRTYP,MXBPRDQ                                                 
         BE    YES                                                              
         B     NO                                                               
         DROP  RF                                                               
         USING MXBOSD,RF                                                        
CBSTBOS  CLC   MXBOSTYP,MXBOSDQ                                                 
         BE    YES                                                              
         B     NO                                                               
         DROP  RF                                                               
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
MXSPACES DC    80C' '                                                           
         SPACE 1                                                                
       ++INCLUDE MXRECID                                                        
         SPACE 1                                                                
       ++INCLUDE DXRECID                                                        
*                                                                               
         DS    0D                                                               
SSB      DC    X'0000',X'FF',X'02' FOR DATAMGR (OFFLINE NO RECOVERY)            
         DC    250X'00'                                                         
         SPACE 2                                                                
         DS    0D                                                               
ADXBLOCK DC    A(0)                                                             
         DS    0D                                                               
         SPACE 2                                                                
* TYPTAB DEFINES PROCESS RECORD TYPES                                           
* AL1    TYPE NUMBER                                                            
* XL1    TYPE FLAGS                                                             
* CL3    TYPE NAME                                                              
* AL4    LOAD ROUTINE ADDRESS                                                   
* AL4    UPDATE ROUTINE ADDRESS                                                 
*                                                                               
         DS    0D                                                               
TYPTAB   DS    0CL16                                                            
         DC    AL1(01),AL1(0)                                                   
         DC    CL3'ALL',C'   ',AL4(LOADALL),AL4(UPDTALL)                        
         DC    AL1(02),AL1(CLIKTYPQ)                                            
         DC    CL3'CLI',C'   ',AL4(LOADCLI),AL4(UPDTCLI)                        
         DC    AL1(03),AL1(PROKTYPQ)                                            
         DC    CL3'PRO',C'   ',AL4(LOADPRO),AL4(UPDTPRO)                        
         DC    AL1(04),AL1(CAMKTYPQ)                                            
         DC    CL3'CAM',C'   ',AL4(LOADCAM),AL4(UPDTCAM)                        
         DC    AL1(05),AL1(BUYKTYPQ)                                            
         DC    CL3'BUY',C'   ',AL4(LOADBUY),AL4(UPDTBUY)                        
         DC    AL1(06),AL1(BYRKTYPQ)                                            
         DC    CL3'BYR',C'   ',AL4(LOADBYR),AL4(UPDTBYR)                        
         DC    AL1(07),AL1(MEDKTYPQ)                                            
         DC    CL3'MED',C'   ',AL4(LOADMED),AL4(UPDTMED)                        
         DC    AL1(08),AL1(STAKTYPQ)                                            
         DC    CL3'SUP',C'   ',AL4(LOADSUP),AL4(UPDTSUP)                        
         DC    AL1(08),AL1(STAKTYPQ)                                            
         DC    CL3'STV',C'0  ',AL4(LOADSUP),AL4(UPDTSUP)                        
         DC    AL1(09),AL1(0)                                                   
         DC    CL3'AUD',C'   ',AL4(LOADAUD),AL4(0)                              
         DC    AL1(10),AL1(AGYKTYPQ)                                            
         DC    CL3'AGY',C'   ',AL4(LOADAGY),AL4(UPDTAGY)                        
         DC    AL1(11),AL1(FOLKTYPQ)                                            
         DC    CL3'FOL',C'   ',AL4(LOADFOL),AL4(UPDTFOL)                        
         DC    AL1(12),AL1(OSCKTYPQ)                                            
         DC    CL3'SCH',C'   ',AL4(LOADSCH),AL4(UPDTSCH)                        
         DC    AL1(13),AL1(OSCKTYPQ)                                            
         DC    CL3'OUT',C'   ',AL4(LOADOUT),AL4(UPDTOUT)                        
         DC    AL1(14),AL1(OSCKTYPQ)                                            
         DC    CL3'OUL',C'   ',AL4(LOADOUL),AL4(UPDTOUL)                        
         DC    AL1(15),AL1(BURKTYPQ)                                            
         DC    CL3'BUR',C'   ',AL4(LOADBUR),AL4(UPDTBUR)                        
         DC    AL1(16),AL1(0)                                                   
         DC    CL3'NBY',C'   ',AL4(LOADNBY),AL4(0)                              
         DC    AL1(17),AL1(BUYKTYPQ)                                            
         DC    CL3'BTR',C'   ',AL4(LOADBUY),AL4(0)                              
         DC    AL1(18),AL1(0)                                                   
         DC    CL3'TVA',C'   ',AL4(LOADTVA),AL4(0)                              
         DC    AL1(19),AL1(0)                                                   
         DC    CL3'LEO',C'   ',AL4(LOADLEO),AL4(0)                              
         DC    AL1(20),AL1(0)                                                   
         DC    CL3'PEE',C'   ',AL4(LOADPEE),AL4(0)                              
         DC    AL1(21),AL1(0)                                                   
         DC    CL3'MEZ',C'   ',AL4(LOADMEZ),AL4(0)                              
         DC    AL1(22),AL1(BUYKTYPQ)                                            
         DC    CL3'BTV',C'   ',AL4(LOADBUY),AL4(0)                              
         DC    AL1(23),AL1(BUYKTYPQ)                                            
         DC    CL3'BYO',C'   ',AL4(LOADBUY),AL4(0)                              
         DC    AL1(24),AL1(0)                                                   
         DC    CL3'PGN',C'   ',AL4(LOADPGN),AL4(0)                              
         DC    AL1(25),AL1(0)                                                   
         DC    CL3'PGA',C'   ',AL4(LOADPGA),AL4(0)                              
         DC    AL1(26),AL1(BUYKTYPQ)                                            
         DC    CL3'BXR',C'   ',AL4(LOADBUY),AL4(0)                              
         DC    AL1(27),AL1(BUYKTYPQ)                                            
         DC    CL3'BBR',C'   ',AL4(LOADBUY),AL4(0)                              
         DC    AL1(28),AL1(BUYKTYPQ)                                            
         DC    CL3'BPT',C'   ',AL4(LOADBUY),AL4(0)                              
         DC    AL1(29),AL1(BUYKTYPQ)                                            
         DC    CL3'BRC',C'   ',AL4(LOADBUY),AL4(0)                              
         DC    AL1(30),AL1(BUYKTYPQ)                                            
         DC    CL3'BIN',C'   ',AL4(LOADBUY),AL4(0)                              
         DC    AL1(31),AL1(BUYKTYPQ)                                            
         DC    CL3'BCO',C'   ',AL4(LOADBUY),AL4(0)                              
         DC    AL1(32),AL1(BUYKTYPQ)                                            
         DC    CL3'BPV',C'   ',AL4(LOADBUY),AL4(0)                              
         DC    AL1(33),AL1(BUYKTYPQ)                                            
         DC    CL3'BBV',C'   ',AL4(LOADBUY),AL4(0)                              
         DC    AL1(34),AL1(BUYKTYPQ)                                            
         DC    CL3'BPR',C'   ',AL4(LOADBUY),AL4(0)                              
         DC    AL1(35),AL1(BUYKTYPQ)                                            
         DC    CL3'BOS',C'   ',AL4(LOADBUY),AL4(0)                              
TYPTABX  DC    AL1(00),X'00',CL3'   ',C'   '                                    
TACTION  EQU   0                                                                
TRECTYP  EQU   1                                                                
TNAME    EQU   2                                                                
TMEDIA   EQU   5                                                                
TALOAD   EQU   8                                                                
TAUPDT   EQU   12                                                               
         SPACE 2                                                                
         EJECT                                                                  
         SPACE 1                                                                
VDMOD000 DC    V(DMOD000)                                                       
VDADDS   DC    V(DADDS)                                                         
VLOGIO   DC    V(LOGIO)                                                         
VDATCON  DC    V(DATCON)                                                        
VADDAY   DC    V(ADDAY)                                                         
VMXAUDC  DC    V(MXAUDC)                                                        
VMXBYRC  DC    V(MXBYRC)                                                        
VMXMEDC  DC    V(MXMEDC)                                                        
VMXCLIC  DC    V(MXCLIC)                                                        
VMXPROC  DC    V(MXPROC)                                                        
VMXCAMC  DC    V(MXCAMC)                                                        
VMXBUYC  DC    V(MXBUYC)                                                        
VMXSUPC  DC    V(MXSUPC)                                                        
VMXTVAC  DC    V(MXTVAC)                                                        
VMXAGYC  DC    V(MXAGYC)                                                        
VMXFOLC  DC    V(MXFOLC)                                                        
VMXSCHC  DC    V(MXSCHC)                                                        
VMXOUTC  DC    V(MXOUTC)                                                        
VMXOULC  DC    V(MXOULC)                                                        
VMXBURC  DC    V(MXBURC)                                                        
VMXCNVX  DC    V(MXCNVX)                                                        
*                                                                               
MEDFACS  DS    0F                  MEDIA SYSTEM FACILITY LIST                   
VDATAMGR DC    V(DATAMGR)                                                       
VCALLOV  DC    A(0)                                                             
VCASHVAL DC    V(CASHVAL)                                                       
VDATVAL  DC    A(0)                                                             
VTMPACK  DC    A(0)                                                             
VTMUNPK  DC    V(TMUNPK)                                                        
VGETDAY  DC    V(GETDAY)                                                        
VGETMSG  DC    A(0)                                                             
VBUDGEP  DC    A(0)                                                             
VBUDGET  DC    A(0)                                                             
VDEMAND  DC    V(DEMAND)                                                        
VGETRATE DC    V(GETRATE)                                                       
VSPROUT  DC    A(0)                                                             
VGETPCNT DC    A(0)                                                             
VSCANNER DC    V(SCANNER)                                                       
VSTABLE  DC    V(STABLE)                                                        
VNETWORK DC    A(0)                                                             
VBUYTER  DC    V(BUYTER)                                                        
VTVDIR   DC    V(TVDIR)                                                         
VTVLIST  DC    V(TVLIST)                                                        
         DC    A(0)                                                             
VMDAYVAL DC    A(0)                                                             
VGETDPT  DC    V(GETDPT)                                                        
VGETEQV  DC    V(GETEQIV)                                                       
VBLDCUR  DC    V(BLDCUR)                                                        
VCUREDIT DC    V(CUREDIT)                                                       
VGETCUR  DC    A(0)                                                             
VBLDMED  DC    A(0)                                                             
VSUGGEST DC    A(0)                                                             
VCOMPOSE DC    A(0)                                                             
VDEFORM  DC    A(0)                                                             
VDEPOSIT DC    A(0)                                                             
VGETCODE DC    A(0)                                                             
VSWITCH  DC    V(FASWITCH)                                                      
VDEMVALS DC    A(0)                                                             
VGETCPT  DC    A(0)                                                             
VTSAR    DC    A(0)                                                             
VMINIBUY DC    A(0)                                                             
VGETAUD  DC    V(GETAUD)                                                        
VGETUNV  DC    V(GETUNV)                                                        
VTRAVAIL DC    V(TRAVAIL)                                                       
VSPCVAL  DC    A(0)                                                             
VDECADE  DC    A(0)                                                             
VGETPGM  DC    V(GETPGM)                                                        
VGETPROD DC    A(0)                                                             
VDICTATE DC    V(DICTATE)                                                       
VMBGENLK DC    A(0)                                                             
VGTVDIR  DC    A(0)             51 GERMAN STABLE'S TABLES (MESTABTABS)          
VGTVLIST DC    A(0)             52 GERMAN STABLE'S SUPERSET LISTAB              
VGETWKS  DC    A(0)                                                             
VBINSRCH DC    A(0)                                                             
VBUYHIST DC    A(0)                                                             
VMEFILT  DC    A(0)             68 MEDLINE FILTER PROCESSOR                     
VDERUNV  DC    A(0)                                                             
VTYPIST  DC    A(0)                                                             
VGETWGTS DC    A(0)                                                             
         DC    A(0)             SPARE                                           
VGETRAT2 DC    V(GETRAT2)                                                       
         DC    5A(0)            SPARE                                           
VMEDCOMF DC    A(COMFACS)       A(COMFACS)                                      
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
MEDDIR   DC    C'MEDDIR '                                                       
MEDDIRZ  DC    C'MEDDIRZ'                                                       
DMDA     DC    F'0'                                                             
DTFADDR  DC    F'0'                                                             
ACTIVITY DC    CL1'Y'                                                           
         SPACE 1                                                                
* COMFACS                                                                       
COMFACS  DS    0D                                                               
       ++INCLUDE DDCOMFACSC                                                     
         SPACE 1                                                                
*                                                                               
         SPACE 1                                                                
         DS    0D                                                               
         DC    CL8'CURTAB  '       CURRENCY TABLE                               
CURTAB   DS    0CL8                                                             
         DC    250XL8'00'                                                       
CURTABX  DC    XL8'00'             FOR OVERFLOW CHECKS                          
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* PROCESS STATISTICS                                                  *         
***********************************************************************         
         SPACE 1                                                                
         DS    0D                                                               
PROCSTAT NMOD1 0,**PSTA**                                                       
         LR    RC,R1                                                            
         MVC   MEDAGY,SXDTAGB      SET MEDIA AGENCY CODE FROM SYSTEM            
         NI    MEDAGY,X'F0'          CONTROL TABLE                              
         BAS   RE,BLDCLIT          BUILD CLIENT TABLE FOR STATS                 
         BNE   PSTANO                EXIT IF ERROR                              
         ZAP   BUYCOUNT,=PL8'0'                                                 
         ZAP   BUYGRTOT,=PL8'0'                                                 
         MVC   MAXIOS,DXMAXREC     MAXIMIMUM RECORD COUNT                       
         MVI   BYTE,0              MEDIA KEY SAVE BYTE                          
*                                                                               
PSTA010  LA    R2,IOKEY            SET KEY TO READ FIRST BUY RECORD             
         USING DBUY,R2                                                          
         XC    BUYKEY,BUYKEY                                                    
         MVI   BUYKTYP,BUYKTYPQ                                                 
         MVC   BUYKAM,MEDAGY       SET AGENCY                                   
         OC    BUYKAM,BYTE         SET MEDIA                                    
         L     R2,DXARECB                                                       
         GOTO1 VDATAMGR,DMCB,(X'00',DMRDHI),MEDDIR,IOKEY,(R2),DMWORK            
*                                                                               
PSTA020  TM    8(R1),X'80'         ALL DONE IF EOF                              
         BO    PSTA100                                                          
         TM    8(R1),X'7D'         DIE IF DISK ERROR                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         MVC   BYTE,BUYKAM                                                      
         NI    BYTE,X'F0'                                                       
         CLC   BYTE,MEDAGY         ALL DONE IF AGENCY CHANGES                   
         BNE   PSTA100                                                          
         CLI   BUYKTYP,BUYKTYPQ    READ MEDFILE IF SAME RECORD TYPE             
         BE    PSTA030                                                          
         LA    R2,IOKEY                                                         
         MVC   BYTE,BUYKAM                                                      
         NI    BYTE,X'0F'                                                       
         CLI   BYTE,X'0F'          ALL DONE IF LAST MEDIA                       
         BE    PSTA100                                                          
         ZIC   RF,BYTE             BUMP TO NEXT MEDIA                           
         LA    RF,1(RF)                                                         
         STC   RF,BYTE                                                          
         B     PSTA010                                                          
*                                  TRANSFER RECORD DATA TO EXTRACT FILE         
PSTA030  MVC   MEDADDR,BUYDDA                                                   
         MVC   CLIFILT,BUYKCLI                                                  
         MVC   IOKEYSV(L'MEDKEY),0(R2)   SAVE BUY RECORD KEY                    
         TM    BUYDSTAT,BUYSDEL    DONT COUNT DELETED                           
         BNZ   PSTA036                                                          
         TM    BUYDSTAT,BUYSDRFT   DONT COUNT DRAFT                             
         BNZ   PSTA036                                                          
         TM    BUYDSTAT,BUYSCANC   DONT COUNT CANCELLED                         
         BNZ   PSTA036                                                          
         OC    DXFDATEC,DXFDATEC   TEST FROM DATE                               
         BZ    PSTA032                                                          
         CLC   BUYKDAT,DXFDATEC                                                 
         BL    PSTA036                                                          
*                                                                               
PSTA032  OC    DXTDATEC,DXTDATEC   TEST TO DATE                                 
         BZ    PSTA034                                                          
         CLC   BUYKDAT,DXTDATEC                                                 
         BH    PSTA036                                                          
*                                                                               
PSTA034  EQU   *                                                                
         CLC   SXDTAGY,=CL2'SS'                                                 
         BE    PSTASS                                                           
         CLC   SXDTAGY,=CL2'ZG'                                                 
         BE    PSTAZG                                                           
         CLC   SXDTAGY,=CL2'BV'                                                 
         BE    PSTABV                                                           
*        CLC   SXDTAGY,=CL2'LK'                                                 
*        BE    PSTALK                                                           
         B     PSTA035                                                          
*                                  LEO LK CLIENT FILTERS                        
PSTALK   EQU   *                                                                
         CLC   CLIFILT,=CL3'PGL'                                                
         BE    PSTA035                                                          
         CLC   CLIFILT,=CL3'RVL'                                                
         BE    PSTA035                                                          
         CLC   CLIFILT,=CL3'RVM'                                                
         BE    PSTA035                                                          
         B     PSTA036                                                          
*                                  SS CLIENT FILTERS                            
PSTASS   EQU   *                                                                
         BAS   RE,GETCLIT                                                       
         BNE   PSTA036                                                          
         CLI   CLIBYAGY,18                                                      
         BE    PSTA035                                                          
         CLI   CLICRAGY,13                                                      
         BNE   PSTA036                                                          
         CLI   CLIBYAGY,1                                                       
         BE    PSTA035                                                          
         B     PSTA036                                                          
*                                  ZG CLIENT FILTERS                            
PSTAZG   EQU   *                                                                
         BAS   RE,GETCLIT                                                       
         BNE   PSTA036                                                          
         CLI   CLIBYAGY,5                                                       
         BE    PSTA035                                                          
         B     PSTA036                                                          
*                                  BV CLIENT FILTERS                            
PSTABV   EQU   *                                                                
         BAS   RE,GETCLIT                                                       
         BNE   PSTA036                                                          
         CLI   CLILACC1,1                                                       
         BE    PSTA035                                                          
         B     PSTA036                                                          
*                                                                               
PSTA035  EQU   *                                                                
         AP    BUYCOUNT,=PL8'1'                                                 
*                                                                               
         BAS   RE,DECIOC           DECREMENT MAX IO COUNTER                     
*                                                                               
         GOTO1 VDATAMGR,DMCB,(X'00',GETREC),MEDFILE,MEDADDR,(R2),DMWORK         
         CLI   8(R1),0                                                          
         BNE   PSTANO                                                           
         USING BUYKEY,R2           R3=A(MEDLINE RECORD)                         
         SPACE 1                                                                
         LA    R5,GETRATEB         R5=A(GETRATE) BLOCK                          
         USING GROSS,R5                                                         
         LA    R1,DMCB                                                          
         XC    16(0,R1),0(R1)      RATES VIA V(GETRATE)                         
         ST    R2,0(R1)                                                         
         ST    R5,4(R1)                                                         
         L     RF,VGETRATE                                                      
         BASR  RE,RF                                                            
         SPACE 1                                                                
         OC    GROSS,GROSS                                                      
         BZ    PSTA036                                                          
         L     R1,GROSS                                                         
         BAS   RE,MONEY                                                         
         XC    DUB,DUB                                                          
         L     R1,GROSS                                                         
         CVD   R1,DUB                                                           
         AP    BUYGRTOT,DUB(8)                                                  
*                                                                               
PSTA036  OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         BZ    PSTANO                                                           
         MVC   IOKEY(L'MEDKEY),0(R2)                                            
         BAS   RE,CHKSEQIO                                                      
         BE    PSTA040                                                          
         GOTO1 VDATAMGR,DMCB,(X'00',DMRDHI),MEDDIR,IOKEY,(R2),DMWORK            
         TM    8(R1),X'FD'                                                      
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
PSTA040  GOTO1 VDATAMGR,DMCB,(X'00',DMRSEQ),MEDDIR,IOKEY,(R2),DMWORK            
*                                                                               
PSTA050  EQU   *                                                                
         L     RF,DXAXREC                                                       
         B     PSTA020                                                          
*                                  TRANSFER RECORD DATA TO EXTRACT FILE         
PSTA100  EQU   *                                                                
         L     R2,DXASQLB                                                       
         USING DXSQLD,R2                                                        
         MVC   DXSQLLEN(2),=AL2(DXSQLDL+35)                                     
         MVC   DXSQLTYP,DXSQLDQ                                                 
         MVI   DXSQLRTY-1,MXTRTQ                                                
         MVI   DXSQLRTY,C'S'                                                    
         MVI   DXSQLLIN-1,MXTRTQ                                                
         MVC   DXSQLLIN(37),=CL37' '                                            
         MVC   DXSQLLIN(10),=CL10'DDS_HCHECK'                                   
         LA    RF,DXSQLLIN+11                                                   
         EDIT  BUYCOUNT,(8,(RF)),ZERO=NOBLANK,FILL=0                            
         LA    RF,DXSQLLIN+19                                                   
         MVI   0(RF),C','                                                       
         LA    RF,1(RF)                                                         
         UNPK  0(16,RF),BUYGRTOT(8)                                             
         OI    15(RF),X'F0'                                                     
         LR    R0,RF                                                            
         LR    R1,RF                                                            
         LA    RE,1(R1)                                                         
         LA    RF,13                                                            
         MVC   0(1,R1),0(RE)                                                    
         LA    R1,1(R1)                                                         
         LA    RE,1(RE)                                                         
         BCT   RF,*-14                                                          
         LR    RF,R0                                                            
         MVI   13(RF),C'.'                                                      
         MVI   DXSQLLIN+36,MXTRTQ                                               
         MVI   DXSQLLIN+37,MXTRTQ                                               
*                                  PUT CONVERTED RECORD TO FILE                 
         GOTO1 DXPUT,DMCB,DXASQLB,(R8)                                          
         B     PSTAOK                                                           
*                                                                               
PSTAOK   SR    RC,RC                                                            
PSTANO   LTR   RC,RC                                                            
         XIT1                                                                   
         SPACE 1                                                                
MONEY    CVD   R1,DUB              CONVERT R1 TO MONEY NNNNNNNN.NN              
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK(10),DUB                                                     
         BR    RE                                                               
         SPACE 1                                                                
PERCENT  CVD   R1,DUB              CONVERT R1 TO PERCENTAGE NNN.NN              
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK(5),DUB                                                      
         BR    RE                                                               
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* BUILD CLIENT INFO TABLE FOR STATS                                   *         
***********************************************************************         
         SPACE 1                                                                
BLDCLIT  NTR1                                                                   
         ICM   R4,15,=A(MXBUYCCL)                                               
         MVI   BYTE,0              MEDIA KEY SAVE BYTE                          
*                                                                               
BCLI010  LA    R2,IOKEY            SET KEY TO READ FIRST CLIENT RECORD          
         USING DCLI,R2                                                          
         XC    CLIKEY,CLIKEY                                                    
         MVI   CLIKTYP,CLIKTYPQ                                                 
         MVC   CLIKAM,MEDAGY       SET AGENCY                                   
         OC    CLIKAM,BYTE         SET MEDIA                                    
         L     R2,DXARECB                                                       
         GOTO1 VDATAMGR,DMCB,(X'00',DMRDHI),MEDDIR,IOKEY,(R2),DMWORK            
*                                                                               
BCLI020  TM    8(R1),X'80'         ALL DONE IF EOF                              
         BO    BCLIOK                                                           
         TM    8(R1),X'7D'         DIE IF DISK ERROR                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         MVC   BYTE,CLIKAM                                                      
         NI    BYTE,X'F0'                                                       
         CLC   BYTE,MEDAGY         ALL DONE IF AGENCY CHANGES                   
         BNE   BCLIOK                                                           
         CLI   CLIKTYP,CLIKTYPQ    READ MEDFILE IF SAME RECORD TYPE             
         BE    BCLI030                                                          
         CLI   BYTE,X'0F'          ALL DONE IF LAST MEDIA                       
         BE    BCLIOK                                                           
         LA    R2,IOKEY                                                         
         MVC   BYTE,CLIKAM                                                      
         NI    BYTE,X'0F'                                                       
         CLI   BYTE,X'0F'          ALL DONE IF LAST MEDIA                       
         BE    BCLIOK                                                           
         ZIC   RF,BYTE             BUMP TO NEXT MEDIA                           
         LA    RF,1(RF)                                                         
         STC   RF,BYTE                                                          
         B     BCLI010                                                          
*                                  TRANSFER RECORD DATA TO EXTRACT FILE         
BCLI030  MVC   MEDADDR,CLIDDA                                                   
         MVC   0(1,R4),CLIKAM                                                   
         MVC   1(3,R4),CLIKCLI                                                  
         MVC   4(1,R4),CLIDCAGY                                                 
         MVC   5(1,R4),CLIDBAGY                                                 
         MVC   6(1,R4),CLIDACC1                                                 
         MVC   7(1,R4),CLIDACC1                                                 
         LA    R4,9(R4)                                                         
         MVC   IOKEY(L'MEDKEY),0(R2)                                            
         GOTO1 VDATAMGR,DMCB,(X'00',DMRSEQ),MEDDIR,IOKEY,(R2),DMWORK            
         B     BCLI020                                                          
*                                                                               
BCLIOK   SR    RC,RC                                                            
BCLINO   LTR   RC,RC                                                            
         XIT1                                                                   
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* GET CLIENT INFO TABLE FOR STATS                                     *         
***********************************************************************         
         SPACE 1                                                                
GETCLIT  NTR1                                                                   
         ICM   R4,15,=A(MXBUYCCL)                                               
         USING DBUY,R2                                                          
*                                                                               
GCLI010  EQU   *                                                                
         CLI   0(R4),0                                                          
         BE    GCLINO                                                           
         CLC   0(1,R4),BUYKAM                                                   
         BNE   GCLI020                                                          
         CLC   1(3,R4),CLIFILT                                                  
         BNE   GCLI020                                                          
         MVC   CLICRAGY,4(R4)                                                   
         MVC   CLIBYAGY,5(R4)                                                   
         MVC   CLILACC1,6(R4)                                                   
         MVC   CLILACC2,7(R4)                                                   
         B     GCLIOK                                                           
*                                                                               
GCLI020  EQU   *                                                                
         LA    R4,9(R4)                                                         
         B     GCLI010                                                          
*                                                                               
GCLIOK   SR    RC,RC                                                            
GCLINO   LTR   RC,RC                                                            
         XIT1                                                                   
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* EXTRACT LATEST PROGRAM NAMES WITH BUY TV TABLE RECORDS              *         
***********************************************************************         
         SPACE 1                                                                
         DS    0D                                                               
EXTPGN   NMOD1 0,**EPGN**                                                       
         LR    RC,R1                                                            
         MVC   MAXIOS,DXMAXREC     MAXIMIMUM RECORD COUNT                       
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST BUY RECORD             
         USING DEMFILED,R2                                                      
         XC    DEMMKEY,DEMMKEY                                                  
         MVI   DEKCNTR,DEKCNTRQ                                                 
         L     R2,DXARECB                                                       
         L     R4,=V(UTL)          SWITCH TO MEDZ FOR READ                      
         MVC   BYTE,4(R4)                                                       
         MVI   4(R4),X'14'                                                      
         GOTO1 VDATAMGR,DMCB,(X'00',DMREAD),MEDDIR,IOKEY,(R2),DMWORK            
         TM    8(R1),X'80'         DIE IF NOT FOUND                             
         BZ    *+6                                                              
         DC    H'0'                                                             
         TM    8(R1),X'7D'         DIE IF DISK ERROR                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         MVC   MEDADDR,DEMMKDA                                                  
         GOTO1 VDATAMGR,DMCB,(X'00',GETREC),MEDFILE,MEDADDR,(R2),DMWORK         
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R3,DEMMFRST                                                      
         USING DEDCND,R3                                                        
*                                                                               
EPGN020  CLI   DEDCNEL,0                                                        
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   DEDCNEL,DEDCNELQ                                                 
         BE    EPGN040                                                          
EPGN030  SR    RF,RF                                                            
         IC    RF,DEDCNLEN                                                      
         AR    R3,RF                                                            
         B     EPGN020                                                          
*                                                                               
EPGN040  CLI   DEDCNTYP,DEDCNTPQ                                                
         BNE   EPGN030                                                          
         MVC   LASTPGDT,DEDCNAGW                                                
         GOTO1 VDATCON,DMCB,(2,LASTPGDT),(0,ADDAYWRK)                           
         GOTO1 VADDAY,DMCB,ADDAYWRK,ADDAYWRK,F'-7'                              
         GOTO1 VDATCON,DMCB,(0,ADDAYWRK),(2,FRSTPGDT)                           
         DROP  R2,R3                                                            
         MVC   4(1,R4),BYTE        SWITCH BACK TO EXTRACT MEDIA SYSTEM          
         MVI   BYTE,0              MEDIA KEY SAVE BYTE                          
*                                                                               
EPGN100  LA    R2,IOKEY            SET KEY TO READ FIRST BUY RECORD             
         USING DBUY,R2                                                          
         XC    BUYKEY,BUYKEY                                                    
         MVI   BUYKTYP,BUYKTYPQ                                                 
         MVC   BUYKAM,MEDAGY       SET AGENCY                                   
         OC    BUYKAM,BYTE         SET MEDIA                                    
         L     R2,DXARECB                                                       
         GOTO1 VDATAMGR,DMCB,(X'00',DMRDHI),MEDDIR,IOKEY,(R2),DMWORK            
*                                                                               
EPGN110  TM    8(R1),X'80'         ALL DONE IF EOF                              
         BO    EPGNOK                                                           
         TM    8(R1),X'7D'         DIE IF DISK ERROR                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         MVC   BYTE,BUYKAM                                                      
         NI    BYTE,X'F0'                                                       
         CLC   BYTE,MEDAGY         ALL DONE IF AGENCY CHANGES                   
         BNE   EPGNOK                                                           
         CLI   BUYKTYP,BUYKTYPQ    READ MEDFILE IF SAME RECORD TYPE             
         BE    EPGN120                                                          
         LA    R2,IOKEY                                                         
         MVC   BYTE,BUYKAM                                                      
         NI    BYTE,X'0F'                                                       
         CLI   BYTE,X'0F'          ALL DONE IF LAST MEDIA                       
         BE    EPGNOK                                                           
         ZIC   RF,BYTE             BUMP TO NEXT MEDIA                           
         LA    RF,1(RF)                                                         
         STC   RF,BYTE                                                          
         B     EPGN100                                                          
*                                  TRANSFER RECORD DATA TO EXTRACT FILE         
EPGN120  MVC   MEDADDR,BUYDDA                                                   
         MVC   CLIFILT,BUYKCLI                                                  
         MVC   IOKEYSV(L'MEDKEY),0(R2)   SAVE BUY RECORD KEY                    
         TM    BUYDSTAT,BUYSDEL    DONT COUNT DELETED                           
         BNZ   EPGN160                                                          
         TM    BUYDSTAT,BUYSDRFT   DONT COUNT DRAFT                             
         BNZ   EPGN160                                                          
         TM    BUYDSTAT,BUYSCANC   DONT COUNT CANCELLED                         
         BNZ   EPGN160                                                          
         CLI   PGNFLAG,C'A'        TEST EXTRACT ALL BUYS                        
         BE    *+12                                                             
         TM    BUYDIND3,BUYSPMCH+BUYSFMCH  ONLY COUNT SPOT MATCHED              
         BZ    EPGN160                                                          
         OC    DXFDATEC,DXFDATEC   TEST FROM DATE                               
         BZ    EPGN122                                                          
         CLC   BUYKDAT,DXFDATEC                                                 
         BL    EPGN160                                                          
         B     EPGN123                                                          
*                                                                               
EPGN122  EQU   *                                                                
         CLI   PGNFLAG,C'A'        TEST EXTRACT ALL BUYS                        
         BE    EPGN123A                                                         
         CLC   BUYKDAT,FRSTPGDT                                                 
         BL    EPGN160                                                          
*                                                                               
EPGN123  EQU   *                   TEST TO DATE                                 
         CLI   PGNFLAG,C'A'        TEST EXTRACT ALL BUYS                        
         BE    EPGN123A                                                         
         CLC   BUYKDAT,LASTPGDT                                                 
         BH    EPGN160                                                          
         B     EPGN124                                                          
*                                                                               
EPGN123A OC    DXTDATEC,DXTDATEC   TEST TO DATE                                 
         BZ    EPGN124                                                          
         CLC   BUYKDAT,DXTDATEC                                                 
         BH    EPGN160                                                          
*                                                                               
EPGN124  EQU   *                                                                
*        CLC   SXDTAGY,=CL2'LK'                                                 
*        BE    EPGN126                                                          
         B     EPGN128                                                          
*                                  LEO LK CLIENT FILTERS                        
EPGN126  EQU   *                                                                
         CLC   CLIFILT,=CL3'PGL'                                                
         BE    EPGN128                                                          
         CLC   CLIFILT,=CL3'RVL'                                                
         BE    EPGN128                                                          
         CLC   CLIFILT,=CL3'RVM'                                                
         BE    EPGN128                                                          
         B     EPGN160                                                          
*                                                                               
EPGN128  EQU   *                                                                
*                                                                               
         BAS   RE,DECIOC           DECREMENT MAX IO COUNTER                     
*                                                                               
         GOTO1 VDATAMGR,DMCB,(X'00',GETREC),MEDFILE,MEDADDR,(R2),DMWORK         
         CLI   8(R1),0                                                          
         BNE   EPGNNO                                                           
*                                                                               
         LA    R1,MXBUYDL          R1=L'BUY RECORD (LONGEST)                    
         CH    R1,=AL2(MXBTVDL)      (FIND LONGEST POSSIBLE)                    
         BH    *+8                                                              
         LA    R1,MXBTVDL                                                       
         CH    R1,=AL2(MXBTV3DL)                                                
         BH    *+8                                                              
         LA    R1,MXBTV3DL                                                      
         CH    R1,=AL2(MXBPTNDL)                                                
         BH    *+8                                                              
         LA    R1,MXBPTNDL                                                      
         CH    R1,=AL2(MXBTRDL)                                                 
         BH    *+8                                                              
         LA    R1,MXBTRDL                                                       
         CH    R1,=AL2(MXBOSDL)                                                 
         BH    *+8                                                              
         LA    R1,MXBOSDL                                                       
*                                                                               
         L     R3,DXAXREC          R3=A(EXTRACT RECORD AREA)                    
         USING DXHDRD,R3                                                        
         LR    R0,R3                                                            
         LA    RE,*                                                             
         SR    RF,RF                                                            
         ICM   RF,8,MXSPACES                                                    
         MVCL  R0,RE               SET RECORD TO ALL SPACES                     
         LA    RF,DXHDRHL                                                       
         SLL   RF,16                                                            
         ST    RF,DXHDRLEN         SET MINIMUM RECORD LEN IN HDR                
         MVI   DXHDRRTY-1,MXTRTQ                                                
         MVC   DXHDRRTY,DXACTION                                                
         MVI   DXHDRCDT-1,MXTRTQ                                                
*                                                                               
         MVC   DXHDRCDT,DXDATEN    SET TODAYS DATE                              
         MVI   DXHDRCTI-1,MXTRTQ                                                
         MVC   DXHDRCTI,DXTIMEN                                                 
         MVC   DXHDRCTI+L'DXHDRCTI+1(2),SXDTAGY                                 
         MVI   DXHDRCTI+L'DXHDRCTI,MXTRTQ                                       
         DROP  R3                                                               
*                                                                               
EPGN129  EQU   *                                                                
         SR    RF,RF               SET MEDIA FILTER                             
         ICM   RF,8,TYPEMED                                                     
*                                  CALL RECORD EXTRACT ROUTINE                  
         GOTO1 VMXBUYC,DMCB,DXAXREC,(R2),(RF),DXACPYB,(R8)                      
         MVC   BYTE,DMCB+8         SAVE RETURN CODE FOR CALL BACK TEST          
         CLI   DMCB+8,X'88'        TEST RETURN CODE                             
         BE    EPGN150             TEST NO CALL BACK                            
         TM    DMCB+8,X'80'                                                     
         BO    EPGN140             TEST NOT TO WRITE THIS RECORD                
         CLI   DXWRITE,C'Y'                                                     
         BNE   EPGN150                                                          
         CLI   SXDTPLFM,0          TEST EXTRACT FILE PLATFORM                   
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                  EXTRACT BUY TV RECORD ONLY                   
         L     RF,DXAXREC                                                       
         USING MXBTVD,RF                                                        
         CLC   MXBTVTYP,=CL5'05206'                                             
         BNE   EPGN140                                                          
*                                  CONVERT RECORD TO SQL BUFFER                 
         GOTO1 VMXCNVX,DMCB,(R8)                                                
         BE    *+6                                                              
         DC    H'0'                DEAL WITH ERROR STATE HERE??                 
*                                                                               
*                                  BUILD CONTROLS TO REPLACE EXISTING           
         L     RF,DXASQLB                                                       
         USING MXBTVD,RF                                                        
         MVI   MXBTVACT,C'K'                                                    
         DROP  RF                                                               
*                                  PUT CONVERTED RECORD TO FILE                 
         GOTO1 DXPUT,DMCB,DXASQLB,(R8)                                          
*                                                                               
         L     RF,DXASQLB                                                       
         USING MXBTVD,RF                                                        
         MVI   MXBTVACT,C'A'                                                    
         DROP  RF                                                               
*                                  PUT CONVERTED RECORD TO FILE                 
         GOTO1 DXPUT,DMCB,DXASQLB,(R8)                                          
         B     EPGN140                                                          
*                                  PUT UNCONVERTED RECORD TO FILE               
EPGN130  GOTO1 DXPUT,DMCB,DXAXREC,(R8)                                          
*                                                                               
EPGN140  TM    BYTE,X'40'          TEST CALL BACK REQUIRED                      
         BO    EPGN129                                                          
*                                                                               
EPGN150  BAS   RE,DECIOC           DECREMENT MAX IO COUNTER                     
         BNE   EPGNNO                                                           
*                                                                               
EPGN160  EQU   *                   READ NEXT RECORD                             
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         BZ    EPGNNO                                                           
         MVC   IOKEY(L'MEDKEY),0(R2)                                            
         BAS   RE,CHKSEQIO                                                      
         BE    EPGN170                                                          
         GOTO1 VDATAMGR,DMCB,(X'00',DMRDHI),MEDDIR,IOKEY,(R2),DMWORK            
         TM    8(R1),X'FD'                                                      
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
EPGN170  GOTO1 VDATAMGR,DMCB,(X'00',DMRSEQ),MEDDIR,IOKEY,(R2),DMWORK            
         B     EPGN110                                                          
*                                                                               
EPGNOK   SR    RC,RC                                                            
EPGNNO   LTR   RC,RC                                                            
         XIT1                                                                   
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
* DXDSECTS                                                                      
       ++INCLUDE DXDSECTS                                                       
         SPACE 1                                                                
* DXSQLD                                                                        
       ++INCLUDE DXSQLD                                                         
         SPACE 1                                                                
* MXREC2D                                                                       
       ++INCLUDE MXREC2D                                                        
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
* DDDPRINT                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* MEFIL STUFF ETC                                                               
         PRINT OFF                                                              
       ++INCLUDE MEFILAGYD                                                      
       ++INCLUDE MEFILMEDD                                                      
       ++INCLUDE MEFILBYRD                                                      
       ++INCLUDE MEFILCLID                                                      
       ++INCLUDE MEFILPROD                                                      
       ++INCLUDE MEFILCAMD                                                      
       ++INCLUDE MEFILBUYD                                                      
       ++INCLUDE MEFILSTAD                                                      
       ++INCLUDE MEFILFOLD                                                      
       ++INCLUDE MEFILOSCD                                                      
       ++INCLUDE MEFILBURD                                                      
       ++INCLUDE MEGETAUDD                                                      
       ++INCLUDE MEDDEQUS                                                       
       ++INCLUDE MEMSGEQUS                                                      
       ++INCLUDE MEGETRATED                                                     
       ++INCLUDE MEDEMFILED                                                     
         PRINT ON                                                               
         EJECT                                                                  
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
DMCBTEST DS    6F                                                               
PARM     DS    6F                                                               
FULL     DS    F                                                                
RELO     DS    A                                                                
MEDADDR  DS    CL4                                                              
IOKEYSV  DS    CL32                                                             
IOKEY    DS    CL32                                                             
DMWORK   DS    12D                                                              
BYTE     DS    XL1                                                              
PGNFLAG  DS    CL1                                                              
ERROR    DS    XL1                                                              
RETCODE  DS    XL1                                                              
HEADFLAG DS    XL1                                                              
FRSTPGDT DS    CL2                 FRST DATE PROGRAM NAME AGB DATA LOAD         
LASTPGDT DS    CL2                 LAST DATE PROGRAM NAME AGB DATA LOAD         
ADDAYWRK DS    CL8                 ADDAY WORK SPACE (YYMMDD) EBCDIC             
*                                                                               
MAXIOS   DS    XL4                 RECORD IO LIMIT COUNT                        
*                                                                               
BUYCOUNT DS    PL8                                                              
BUYGRTOT DS    PL8                                                              
*                                                                               
MEDAGY   DS    XL1                 MEDIA RECORD AGENCY CODE                     
MEDCHAR  DS    CL1                 MEDIA RECORD MEDIA CODE                      
CLIFILT  DS    CL3                 CLIENT CODE FILTER                           
CLICRAGY DS    CL1                 CLIENT CREATIVE AGENCY                       
CLIBYAGY DS    CL1                 CLIENT BUYING AGENCY                         
CLILACC1 DS    CL1                 CLIENT LIMIT ACCESS CODE 1                   
CLILACC2 DS    CL1                 CLIENT LIMIT ACCESS CODE 2                   
*                                                                               
*                                  RECORD TYPE TABLE VALUES                     
TYPEREC  DS    XL1                 MEDIA RECORD TYPE                            
TYPECODE DS    CL3                 TYPE CODE                                    
TYPEMED  DS    CL3                 TYPE MEDIA CODES                             
TYPEALOD DS    A                   EXTRACT LOAD ROUTINE                         
TYPEAUPD DS    A                   EXTRACT UPDATE ROUTINE                       
*                                                                               
WORK     DS    XL256                                                            
GETRATEB DS    XL256               GETRATE BLOCK                                
IOL      DS    F                   GENERAL IO AREA                              
IO       DS    2048X                                                            
WORKX    DS    0D                                                               
         SPACE 1                                                                
WORKC    CSECT                     ** WORKING STORAGE POOL 1 **                 
         DS    10000D                                                           
         SPACE 1                                                                
*                                                                               
         DS    0D                                                               
MXBUYCM  CSECT                     MEDIA SAVE DATA AREA                         
         DC    16XL9'00'                                                        
         DC    8X'FF'                                                           
*                                                                               
         DS    0D                                                               
MXBUYCCL CSECT                     CLIENT SAVE DATA AREA                        
* ??     DC    20000XL12'00'                                                    
         DC    2000XL12'00'                                                     
         DC    8X'FF'                                                           
*                                                                               
         DS    0D                                                               
MXBUYCP  CSECT                     PRODUCT SAVE DATA AREA                       
* ??     DC    40000XL36'00'                                                    
         DC    4000XL36'00'                                                     
         DC    8X'FF'                                                           
*                                                                               
         DS    0D                                                               
MXBUYCC  CSECT                     CAMPAIGN SAVE DATA AREA                      
* ??     DC    200000XL15'00'                                                   
         DC    20000XL15'00'                                                    
         DC    8X'FF'                                                           
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'250MXTRACTDB 05/01/02'                                      
         END                                                                    
