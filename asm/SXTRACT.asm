*          DATA SET SXTRACT    AT LEVEL 006 AS OF 05/01/02                      
*PHASE SXTRACT                                                                  
*INCLUDE DMUTLCT                                                                
*INCLUDE DMDMGRL                                                                
*INCLUDE SXBUYC                                                                 
*INCLUDE SXBUYX                                                                 
*INCLUDE SXCNVX                                                                 
*INCLUDE DICTATE                                                                
*INCLUDE FAGETTXT                                                               
*INCLUDE DATCON                                                                 
*INCLUDE FASWITCH                                                               
*INCLUDE CLPACK                                                                 
*INCLUDE CLUNPK                                                                 
*                                                                               
         TITLE 'SXTRACT - EXTRACT SPOT SYSTEM FILE DATA'                        
**********************************************************                      
*                                                        *                      
* SPOT SQL SUB SYSTEM EXTRACT CONTROL MODULE             *                      
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
                                                                                
SXTRACT  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORKX-WORKD,**SXTR**,RA,R9,RR=RE                                 
         USING WORKD,RC            RC=A(GLOBAL W/S)                             
         ST    RE,RELO                                                          
         L     R8,0(R1)            R8=A(EXTRACT CONTROL DATA BLOCK)             
         USING DXBLOCKD,R8                                                      
         L     R7,DXSTPTR          R7=A(EXTRACT SYSTEM TABLE ENTRY)             
         USING SXDTABD,R7                                                       
         B     MAIN                                                             
         EJECT                                                                  
***********************************************************************         
* MAIN CONTROL CODE                                                   *         
***********************************************************************         
                                                                                
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
                                                                                
GENINIT  NTR1                                                                   
         MVC   MAXIOS,DXMAXREC     MAXIMIMUM RECORD COUNT                       
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* DXMODE = DXOPENQ - OPEN SPOT SYSTEM FILES                           *         
***********************************************************************         
                                                                                
PROCOPEN NTR1                                                                   
         BAS   RE,OPENSPT OPEN SPOT SYSTEM                                      
*                                  GET DTF ADDRESS                              
*        XC    IOKEY,IOKEY         DOES THIS WORK IN US ??                      
*?????   L     R2,DXARECB                                                       
*        GOTO1 VDATAMGR,DMCB,DMFAST,SPTDIR,IOKEY,(R2),DMWORK                    
*        DC    H'00'                                                            
*        L     RF,12(R1)                                                        
*        LA    RF,0(RF)                                                         
*        ST    RF,DTFADDR                                                       
*                                  OPEN SYSTEM DISK FILES                       
         GOTO1 VDMOD000,DMCB,A(DMODOSYS),(DXSENUM,IOL)                          
         B     POPEOK                                                           
*                                                                               
POPENO   B     NO                                                               
POPEOK   B     YES                                                              
                                                                                
***********************************************************************         
* OPEN SPOT SYSTEM                                                              
***********************************************************************         
                                                                                
OPENSPT  NTR1                                                                   
         L     RE,=V(UTL)                                                       
         MVC   4(1,RE),DXSENUM                                                  
         GOTO1 VDATAMGR,DMCB,DMOPEN,=C'SPT',FLIST,IOL                           
***      MVC   4(1,R4),BYTE                                                     
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* DXMODE = DXCLOSEQ - CLOSE SPOT SYSTEM FILES                         *         
***********************************************************************         
                                                                                
PROCCLOS NTR1  ,                                                                
*                                  SET UTL SENUM                                
         L     RE,=V(UTL)          ?????                                        
         MVC   4(1,RE),DXSENUM     ?????                                        
*                                                                               
         GOTO1 VDATAMGR,DMCB,DMCLSE,=C'SPOT'                                    
         GOTO1 VDMOD000,DMCB,A(DMODCSYS),(DXSENUM,IOL) ????                     
         B     PCLOOK                                                           
*                                                                               
PCLONO   B     NO                                                               
PCLOOK   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* PROCESS SPOT FILE DATA IN LOAD MODE                                 *         
***********************************************************************         
                                                                                
PROCLOAD NTR1  ,                                                                
         MVC   SPTAGY,SXDTAGB      SET SPOT AGENCY CODE FROM SYSTEM             
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
                                                                                
LOADALL  NTR1                                                                   
         BAS   RE,LOADTST          SPOT TEST RECORD                             
         BNE   LALLNO                                                           
         BAS   RE,LOADAGY          AGENCY RECORD                                
         BNE   LALLNO                                                           
*&&DO                                                                           
         BAS   RE,LOADCLI          CLIENT RECORDS                               
         BNE   LALLNO                                                           
         BAS   RE,LOADPRD          PRODUCT RECORDS                              
         BNE   LALLNO                                                           
         BAS   RE,LOADEST          ESTIMATE RECORDS                             
         BNE   LALLNO                                                           
*&&                                                                             
         BAS   RE,LOADBUY          BUY RECORDS                                  
         BNE   LALLNO                                                           
         B     LALLOK                                                           
*                                                                               
LALLNO   B     NO                                                               
LALLOK   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* LOAD SPOT TEST RECORD DATA                                          *         
***********************************************************************         
                                                                                
LOADTST  NTR1                                                                   
         B     LTSTOK                                                           
*                                                                               
LTSTNO   B     NO                                                               
LTSTOK   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* LOAD AGENCY RECORD DATA - INCLUDED AS EXAMPLE FROM UK MEDIA SYSTEM  *         
***********************************************************************         
                                                                                
LOADAGY  NTR1                                                                   
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST AGENCY RECORD          
         USING DAGY,R2                                                          
         XC    AGYKEY,AGYKEY                                                    
         MVI   AGYKTYP,X'06'                                                    
         MVC   AGYKAGY,SXDTAGY     SET AGENCY                                   
         MVC   IOKEYSV,IOKEY                                                    
         L     R2,DXARECB                                                       
         GOTO1 VDATAMGR,DMCB,(X'00',DMRDHI),SPTDIR,IOKEY,(R2),DMWORK            
*                                                                               
         CLC   IOKEY(L'AGYKEY),IOKEYSV                                          
         BNE   LAGYNO                                                           
         GOTO1 VDATAMGR,DMCB,(X'00',GETREC),SPTFILE,KEY+14,(R2),DMWORK          
         CLI   8(R1),0                                                          
         BNE   LAGYNO                                                           
                                                                                
*&&DO                                                                           
*                                  TRANSFER RECORD DATA TO EXTRACT FILE         
         MVC   SPTADDR,KEY+14                                                   
         GOTO1 SPTLOAD,DMCB,VMXAGYC,VMXAGYX,INITAGY,0                           
         BE    LAGYYES                                                          
*&&DO                                                                           
*                                                                               
LAGYNO   B     NO                                                               
LAGYYES  B     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* PROCESS SPOT FILE DATA IN UPDATE MODE READ RECOVERY FILES           *         
***********************************************************************         
                                                                                
PROCUPDT NTR1                                                                   
         L     R6,DXARECB          POINT TO RECOVERY RECORD BUFFER              
         USING RECDS,R6                                                         
         MVC   SPTAGY,SXDTAGB      SET MEDIA AGENCY CODE FROM                   
         NI    SPTAGY,X'F0'          SYSTEM CONTROL TABLE                       
         MVC   TYPECODE,SXDTTYP                                                 
         BAS   RE,GETTYP           SET TYPE TABLE DATA                          
         CLI   RFILTY,SPTFILQ      TEST MEDFILE RECORD TYPE                     
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
         L     RF,TAUPDT(RE)       CALL EXTRACT ROUTINE                         
         A     RF,RELO                                                          
         BASR  RE,RF                                                            
         BNE   UALLNO                                                           
         B     UALLOK                                                           
*                                                                               
UALLNO   B     NO                                                               
UALLOK   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* UPDATE SPOT TEST RECORD DATA                                        *         
***********************************************************************         
                                                                                
UPDTTST  NTR1                                                                   
*                                  SET A(SPOT TEST RECORD) FROM                 
         LA    R2,RECVHDR+L'RECVHDR  RECOVERY RECORD BUFFER                     
         BAS   RE,INITTST          INITIALISE EXTRACT BUFFER                    
         B     UTSTOK                                                           
*                                                                               
UTSTNO   B     NO                                                               
UTSTOK   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* UPDATE AGENCY RECORD DATA - INCLUDED AS EXAMPLE FROM UK MEDIA SYSTEM*         
***********************************************************************         
                                                                                
UPDTAGY  NTR1                                                                   
*                                  SET A(AGENCY RECORD) FROM                    
         LA    R2,RECVHDR+L'RECVHDR  RECOVERY RECORD BUFFER                     
         USING DAGY,R2                                                          
         BAS   RE,INITAGY          INITIALISE EXTRACT BUFFER                    
*                                  TRANSFER RECORD DATA TO EXTRACT FILE         
         GOTO1 MEDUPDT,DMCB,VMXAGYC,VMXAGYX,INITAGY                             
         BNE   UAGYNO                                                           
         B     UAGYOK                                                           
*                                                                               
UAGYNO   B     NO                                                               
UAGYOK   B     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* PROCESS KEY VALUES IN RECOVERY RECORD                               *         
***********************************************************************         
                                                                                
         USING RECDS,R6                                                         
PROCKEY  NTR1  ,                                                                
         CLI   TYPEREC,0           TEST RECORD TYPE CODE                        
         BE    PKEY010               IF SET FROM TYPE TABLE                     
         CLC   TYPEREC,RECVHDR+L'RECVHDR+1                                      
         BNE   PKEYNO              IGNORE RECORD IF NOT THIS TYPE               
*                                                                               
PKEY010  MVC   BYTE,RECVHDR+L'RECVHDR                                           
         NI    BYTE,X'F0'                                                       
         CLI   SPTAGY,0            TEST MEDIA AGENCY CODE                       
         BE    PKEY020                                                          
         CLC   BYTE,SPTAGY                                                      
         BNE   PKEYNO              IGNORE RECORD IF NOT THIS AGENCY             
*                                                                               
PKEY020  MVC   BYTE,RECVHDR+L'RECVHDR                                           
         NI    BYTE,X'0F'                                                       
         CLI   SXDTFLT1,0          TEST MEDIA FILTER IF SET                     
         BE    PKEY100                                                          
         CLC   BYTE,MEDCHAR                                                     
         BE    PKEY100                                                          
         B     PKEYNO                                                           
*                                  TEST FOR DELETED RECORD                      
PKEY100  EQU   *                     USING MEDIA AGENCY RECORD DSECT            
         TM    RECVHDR+L'RECVHDR+AGYSTAT-AGYKEY,X'80'                           
         BZ    PKEY110                                                          
         CLI   DXACTION,C'C'                                                    
         BE    *+6                                                              
         DC    H'00'                                                            
         L     R3,DXAXREC                                                       
         USING MXHDRD,R3                                                        
         MVI   MXHDRRTY,C'D'                                                    
         B     PKEYOK                                                           
         DROP  R3                                                               
*                                  TEST FOR RESTORED RECORD                     
*                                    USING SAVED RECOVERY COPY RECORD           
PKEY110  CLI   RRECTY,X'02'          WITH CHANGE RECOVERY RECORD TYPE           
         BNE   PKEYOK                                                           
         L     R4,DXACPYB                                                       
         TM    L'RECVHDR+AGYSTAT-AGYKEY(R4),X'80'                               
         BZ    PKEYOK                                                           
         L     R3,DXAXREC                                                       
         USING MXHDRD,R3                                                        
         MVI   MXHDRRTY,C'R'                                                    
         B     PKEYOK                                                           
*                                                                               
PKEYNO   B     NO                                                               
PKEYOK   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* GET RECORD TYPE TABLE VALUES FROM 3 CHARACTER CODE                  *         
***********************************************************************         
                                                                                
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
                                                                                
INITALL  NTR1                                                                   
         L     R3,DXAXREC          R3=A(EXTRACT RECORD AREA)                    
         USING MXHDRD,R3                                                        
         LR    R0,R3                                                            
         LA    RE,*                                                             
         SR    RF,RF                                                            
         ICM   RF,8,SPACES                                                      
         MVCL  R0,RE               SET RECORD TO ALL SPACES                     
         LA    RF,MXHDRHL                                                       
         SLL   RF,16                                                            
         ST    RF,MXHDRLEN         SET MINIMUM RECORD LEN IN HDR                
         MVI   MXHDRRST-1,MXTRTQ                                                
         MVI   MXHDRRST,C'A'                                                    
         MVI   MXHDRRTY-1,MXTRTQ                                                
         MVC   MXHDRRTY,DXACTION                                                
         MVI   MXHDRCDT-1,MXTRTQ                                                
         CLI   DXMODE,DXLOADQ      TEST IF LOAD MODE                            
         BE    IALL100                                                          
*                                  HERE IF UPDATE MODE                          
*                                  FORMAT DATE AND TIME FROM RCVHDR             
         GOTO1 VDATCON,DMCB,(3,RDATE),(0,MXHDRCDT+2)                            
         MVC   MXHDRCDT(2),DXCENT                                               
         MVI   MXHDRCTI-1,MXTRTQ                                                
         ICM   RF,15,RTIME                                                      
         TM    RTIME,X'80'                                                      
         BZ    *+12                                                             
         SLL   RF,1                                                             
         SRL   RF,5                                                             
         XC    DUB,DUB                                                          
         STCM  RF,15,DUB+4                                                      
         OI    DUB+7,X'0C'                                                      
         UNPK  MXHDRCTI(6),DUB+4(4)                                             
         OI    MXHDRCTI+5,X'F0'                                                 
         B     YES                                                              
*                                  HERE IF LOAD MODE                            
IALL100  MVC   MXHDRCDT,DXDATEN    SET TODAYS DATE                              
         MVI   MXHDRCTI-1,MXTRTQ                                                
         MVC   MXHDRCTI,DXTIMEN                                                 
         B     YES                                                              
         DROP  R3                                                               
                                                                                
***********************************************************************         
* INITIALISE UK MEDIA SYSTEM AGENCY EXTRACT RECORD                    *         
***********************************************************************         
                                                                                
INITAGY  NTR1                                                                   
         LA    R1,MXAGYDL          R1=L'RECORD                                  
         BAS   RE,INITALL                                                       
         B     YES                                                              
                                                                                
***********************************************************************         
* INITIALISE SPOT TEST EXTRACT RECORD                                 *         
***********************************************************************         
                                                                                
INITTST  NTR1                                                                   
         LA    R1,MXAGYDL          R1=L'RECORD                                  
         BAS   RE,INITALL                                                       
         B     YES                                                              
                                                                                
         EJECT                                                                  
***********************************************************************         
* SUBROUTINE TO EXTRACT SPOT RECORDS IN LOAD MODE                     *         
* R2 = A(SPOT DIRECTORY RECORD BUFFER)                                *         
* P1 = A(EXTRACT ROUTINE)                                             *         
* P2 = A(FORMAT CONVERT ROUTINE)                                      *         
* P3 = A(EXTRACT RECORD INITIALISATION ROUTINE)                       *         
* P3 = A(RECORD FILTER ROUTINE)                                       *         
***********************************************************************         
                                                                                
SPTLOAD  NTR1                                                                   
         LM    R3,R6,0(R1)                                                      
         LTR   R6,R6               TEST IF FILTER ROUTINE PASSED                
         BZ    MLOA010                                                          
         GOTO1 (R6)                FILTER RECORD, GET NEXT IF NOT VALID         
         BNE   MLOA040                                                          
*                                                                               
MLOA010  GOTO1 VDATAMGR,DMCB,(X'00',GETREC),MEDFILE,SPTADDR,(R2),DMWORK         
         CLI   8(R1),0                                                          
         BNE   MLOANO                                                           
         GOTO1 (R5)                INITIALISE EXTRACT BUFFER                    
*                                  CALL RECORD EXTRACT ROUTINE                  
         SR    RF,RF               SET MEDIA FILTER                             
         ICM   RF,8,TYPEMED                                                     
         GOTO1 (R3),DMCB,DXAXREC,(R2),(RF)                                      
         TM    DMCB+8,X'80'                                                     
         BO    MLOA040             TEST NOT TO WRITE THIS RECORD                
         CLI   DXWRITE,C'Y'                                                     
         BNE   MLOA040                                                          
         CLI   SXDTPLFM,0                                                       
         BE    MLOA020                                                          
*                                  CONVERT RECORD TO SQL BUFFER                 
         GOTO1 (R4),DMCB,DXAXREC,DXASQLB,0                                      
         L     R1,SXDTXDCB                                                      
         L     R0,DXASQLB                                                       
         PUT   (R1),(R0)                                                        
         B     MLOA030                                                          
*                                                                               
MLOA020  L     R1,SXDTXDCB                                                      
         L     RF,DXAXREC                                                       
         PUT   (R1),(RF)                                                        
*                                                                               
MLOA030  BAS   RE,DECIOC                                                        
         BNE   MLOAOK                                                           
*                                                                               
MLOA040  EQU   *                   READ NEXT RECORD                             
         MVC   IOKEY(L'AGYKEY),0(R2)                                            
         BAS   RE,CHKSEQIO                                                      
         BE    MLOA050                                                          
         GOTO1 VDATAMGR,DMCB,(X'00',DMRDHI),SPTDIR,IOKEY,(R2),DMWORK            
         TM    8(R1),X'FD'                                                      
         BZ    *+6                                                              
         DC    H'0'                                                             
MLOA050  GOTO1 VDATAMGR,DMCB,(X'00',DMRSEQ),SPTDIR,IOKEY,(R2),DMWORK            
         B     MLOAOK                                                           
*                                                                               
MLOANO   B     NO                                                               
MLOAOK   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* SUBROUTINE TO EXTRACT MEDIA RECORDS IN UPDATE MODE                  *         
* R2 = A(MEDIA RECORD BUFFER)                                         *         
* P1 = A(EXTRACT ROUTINE)                                             *         
* P2 = A(FORMAT CONVERT ROUTINE)                                      *         
* P3 = A(EXTRACT RECORD INITIALISATION ROUTINE)                       *         
***********************************************************************         
                                                                                
MEDUPDT  NTR1                                                                   
         LM    R3,R5,0(R1)                                                      
*                                                                               
         SR    RF,RF               SET MEDIA FILTER                             
         ICM   RF,8,TYPEMED                                                     
         GOTO1 (R3),DMCB,DXAXREC,(R2),(RF)                                      
         TM    DMCB+8,X'80'                                                     
         BO    MUPDOK                                                           
         CLI   DXWRITE,C'Y'                                                     
         BNE   MUPDOK                                                           
         CLI   SXDTPLFM,0                                                       
         BE    MUPD010                                                          
*                                  CONVERT RECORD TO SQL BUFFER                 
         GOTO1 (R4),DMCB,DXAXREC,DXASQLB,0                                      
         L     R1,SXDTXDCB                                                      
         L     R0,DXASQLB                                                       
         PUT   (R1),(R0)                                                        
         B     MUPD020                                                          
*                                                                               
MUPD010  L     R1,SXDTXDCB                                                      
         L     RF,DXAXREC                                                       
         PUT   (R1),(RF)                                                        
*                                                                               
MUPD020  BAS   RE,DECIOC                                                        
         BNE   MUPDNO                                                           
         B     MUPDOK                                                           
*                                                                               
MUPDNO   B     NO                                                               
MUPDOK   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* DECREMENT MAXIMUM IO COUNT                                          *         
***********************************************************************         
                                                                                
DECIOC   NTR1                                                                   
         ICM   RF,15,SXDTRNUM                                                   
         LA    RF,1(RF)                                                         
         STCM  RF,15,SXDTRNUM                                                   
         ICM   RF,15,MAXIOS                                                     
         BCTR  RF,0                                                             
         STCM  RF,15,MAXIOS                                                     
         BZ    NO                                                               
         B     YES                                                              
                                                                                
***********************************************************************         
* CHECK SEQUENTIAL IO BROKEN, LAST KEY IN IOKEY                       *         
***********************************************************************         
                                                                                
CHKSEQIO NTR1                                                                   
         L     RE,DTFADDR                                                       
         USING ISDTF,RE                                                         
         L     RE,ISPDKEY                                                       
         CLC   IOKEY(20),0(RE)                                                  
         BNE   NO                                                               
         B     YES                                                              
         DROP  RE                                                               
*                                                                               
YES      SR    RC,RC               RETURN CC EQUAL                              
NO       LTR   RC,RC               RETURN CC NOT EQUAL                          
XIT      XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
                                                                                
MXTRTQ   EQU   X'5E'               FIELD SEPERATOR CHR                          
                                                                                
SPACES   DC    80C' '                                                           
                                                                                
FLIST    DC    CL8' SPTFILE'                                                    
         DC    CL8' SPTDIR '                                                    
         DC    CL8' STAFILE'                                                    
         DC    CL8' CTFILE '                                                    
         DC    CL8'X       '                                                    
                                                                                
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
         DC    AL1(02),AL1(0)                                                   
         DC    CL3'TST',C'   ',AL4(LOADTST),AL4(UPDTTST)                        
         DC    AL1(03),AL1(AGYKTYPQ)                                            
         DC    CL3'AGY',C'   ',AL4(LOADAGY),AL4(UPDTAGY)                        
TYPTABX  DC    AL1(00),X'00',CL3'   ',C'   '                                    
TACTION  EQU   0                                                                
TRECTYP  EQU   1                                                                
TNAME    EQU   2                                                                
TMEDIA   EQU   5                                                                
TALOAD   EQU   8                                                                
TAUPDT   EQU   12                                                               
                                                                                
         EJECT                                                                  
                                                                                
VDATAMGR DC    V(DATAMGR)                                                       
VDMOD000 DC    V(DMOD000)                                                       
VDADDS   DC    V(DADDS)                                                         
VLOGIO   DC    V(LOGIO)                                                         
VDATCON  DC    V(DATCON)                                                        
VMXAGYC  DC    V(MXAGYC)                                                        
VMXAGYX  DC    V(MXAGYX)                                                        
VMXCNVX  DC    V(MXCNVX)                                                        
VCLPACK  DC    V(CLPACK)                                                        
VCLUNPK  DC    V(CLUNPK)                                                        
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
MEDFILE  DC    C'MEDFIL '                                                       
MEDFILQ  EQU   X'42'                                                            
MEDDIR   DC    C'MEDDIR '                                                       
SPTFILQ  EQU   X'21'                                                            
SPTDIR   DC    C'SPTDIR '                                                       
SPTFILE  DC    C'SPTFIL '                                                       
DMDA     DC    F'0'                                                             
DTFADDR  DC    F'0'                                                             
ACTIVITY DC    CL1'Y'                                                           
*                                                                               
         EJECT                                                                  
* DXDSECTS                                                                      
       ++INCLUDE DXDSECTS                                                       
                                                                                
* MXHDRD                                                                        
       ++INCLUDE MXHDRD                                                         
                                                                                
* MXAGYD                                                                        
       ++INCLUDE MXAGYD                                                         
                                                                                
* FAFACTS                                                                       
         PRINT OFF                                                              
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
                                                                                
* DDPERVALD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDPERVALD                                                      
         PRINT ON                                                               
                                                                                
* DMDTFIS                                                                       
       ++INCLUDE DMDTFIS                                                        
                                                                                
* DMGREQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE DMGREQUS                                                       
         PRINT ON                                                               
                                                                                
* MEFIL STUFF ETC - INCLUDED AS EXAMPLE FROM UK MEDIA SYSTEM                    
         PRINT OFF                                                              
DAGY     DSECT                                                                  
       ++INCLUDE MEFILAGY                                                       
         PRINT ON                                                               
         EJECT                                                                  
* DSECT TO COVER RECOVERY HEADER                                                
*                                                                               
RECDS    DSECT                                                                  
RECLN    DS    XL2                 TOTAL RECORD LENGTH                          
         DS    XL2                 N/D                                          
       ++INCLUDE DMRCVRHDR                                                      
                                                                                
WORKD    DSECT                     ** GLOBAL WORKING STORAGE **                 
*                                                                               
DUB      DS    D                                                                
DMCB     DS    6F                                                               
PARM     DS    6F                                                               
FULL     DS    F                                                                
RELO     DS    A                                                                
SPTADDR  DS    CL4                                                              
IOKEY    DS    CL32                                                             
IOKEYSV  DS    CL32                                                             
DMWORK   DS    12D                                                              
BYTE     DS    XL1                                                              
ERROR    DS    XL1                                                              
RETCODE  DS    XL1                                                              
HEADFLAG DS    XL1                                                              
*                                                                               
MAXIOS   DS    XL4                 RECORD IO LIMIT COUNT                        
*                                                                               
MEDAGY   DS    XL1                 MEDIA  RECORD AGENCY CODE                    
SPTAGY   DS    XL1                 SPOT RECORD AGENCY CODE                      
MEDCHAR  DS    CL1                 MEDIA RECORD MEDIA CODE                      
*                                                                               
*                                  RECORD TYPE TABLE VALUES                     
TYPEREC  DS    XL1                 MEDIA RECORD TYPE                            
TYPECODE DS    CL3                 TYPE CODE                                    
TYPEMED  DS    CL3                 TYPE MEDIA CODES                             
TYPEALOD DS    A                   EXTRACT LOAD ROUTINE                         
TYPEAUPD DS    A                   EXTRACT UPDATE ROUTINE                       
*                                                                               
WORK     DS    XL256                                                            
IOL      DS    F                   GENERAL IO AREA                              
IO       DS    2048X                                                            
WORKX    DS    0D                                                               
                                                                                
WORKC    CSECT                     ** WORKING STORAGE POOL 1 **                 
         DS    10000D                                                           
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006SXTRACT   05/01/02'                                      
         END                                                                    
