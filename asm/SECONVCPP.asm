*          DATA SET SECONVCPP  AT LEVEL 184 AS OF 05/01/02                      
*PHASE CONVCPP                                                                  
*INCLUDE DMDMGRL                                                                
*INCLUDE DMUTLCT                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRINTERL                                                               
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE HEXIN                                                                  
*INCLUDE GETFACT                                                                
*INCLUDE CARDS                                                                  
*INCLUDE SORTER                                                                 
*INCLUDE DATCON                                                                 
*INCLUDE DATTIM                                                                 
         TITLE 'SECPAS - SECURITY SYSTEM CREATE NEW PASSWORD RECORDS'           
CONVCPP  CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE WORKX-WORKD,**CCPP**,RA,WORK=A(WORKC),CLEAR=YES                  
         USING WORKD,RC            RC=A(GLOBAL W/S)                             
         L     R9,VCPRINT                                                       
         USING DPRINT,R9                                                        
         MVC   TITLE(26),=C'SECURITY PROGRAM CONVERSION REPORT'                 
         B     MAIN                                                             
         EJECT                                                                  
***********************************************************************         
* MAIN CONTROL CODE                                                   *         
***********************************************************************         
MAIN     BAS   RE,GENINIT          GENERAL INTIALISATION                        
         BAS   RE,VALCARDS         VALIDATE JCL CARD DATA LINE                  
         BNE   MERR                  EXIT IF ERROR                              
*                                                                               
         GOTO1 VSORTER,PARM,SORTCRD1,SORTCRD2,0                                 
*                                                                               
         BAS   RE,BLDPERT          READ CTFILE AND BUILD PERSONID TABLE         
         BNE   MERR                  EXIT IF ERROR                              
*                                                                               
         BAS   RE,READCTFL         READ CTFILE AND PUT RECS TO SORTER           
         BNE   MERR                  EXIT IF ERROR                              
*                                                                               
         BAS   RE,WRITEOUT         WRITE RECORDS FROM SORTER TO OUTPUT          
         BNE   MERR                  EXIT IF ERROR                              
*                                                                               
MERR     MVI   RETCODE,0                                                        
         B     MXIT                                                             
*                                                                               
MXIT     XBASE RC=RETCODE,RL=1                                                  
         EJECT                                                                  
***********************************************************************         
* GENERAL INITIALISATION                                              *         
***********************************************************************         
GENINIT  NTR1  ,                                                                
         MVI   RETCODE,X'FF'                                                    
         MVI   ERROR,0                                                          
         MVI   CONTROLF,0                                                       
*                                                                               
         SR    R0,R0               GET DATE AND TIME                            
         SR    R1,R1                                                            
         TIME  BIN                 R0=DATE,R1=TIME                              
         STM   R0,R1,MVSTIME                                                    
         OI    MVSDATE+3,X'0F'                                                  
*                                  GET TODAYS DATE COMPRESSED                   
         GOTO1 =V(DATCON),DMCB,(X'05',0),(X'02',TODAY)                          
         MVC   TODAYC,FFILL                                                     
         XC    TODAYC,TODAY        CREATE COMPLEMENT                            
*                                  GET CURRENT DATE/TIME INTEGER                
         GOTO1 =V(DATTIM),DMCB,(X'01',DATETIME),0                               
         CLI   4(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   DATETIMC,DATETIME                                                
         XC    DATETIMC,FFILL                                                   
*                                                                               
         ICM   RE,15,=A(PIDTAB)    SET ADDRESSES OF LOCAL TABLES                
         STCM  RE,15,APIDTAB                                                    
         STCM  RE,15,APIDTABP                                                   
         ICM   RF,15,=AL4(22000)                                                
         LA    R0,*                                                             
         L     R1,=F'0'                                                         
         MVCL  RE,R0                                                            
         ICM   RE,15,=A(PIDTABX)                                                
         STCM  RE,15,APIDTABX                                                   
         MVI   PIDTABF,0                                                        
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* VALIDATE JCL CARD DATA LINES AS DEFINED IN CARDTBL                  *         
***********************************************************************         
         SPACE 1                                                                
VALCARDS NTR1                                                                   
*                                                                               
VCLP1    GOTO1 VCARDS,DMCB,P,=C'RE00'                                           
         CLC   =C'/*',P            IF END OF JCL                                
         BE    VCEND                 CHECK REQUIRED CARDS INPUT                 
*                                                                               
         LA    RE,CARDTBL          INITIALISE TABLE POINTER                     
*                                                                               
VCLP2    CLI   0(RE),0             END OF TABLE                                 
         BE    VCERR1              CARD NOT IN TABLE                            
         SR    RF,RF                                                            
         IC    RF,CLENGTH(RE)                                                   
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   P(0),CSTRING(RE)    COMPARE STRING                               
         BE    VCLP2X                                                           
         LA    RE,L'CARDTBL(RE)    GET NEXT ENTRY                               
         B     VCLP2                                                            
*                                                                               
VCLP2X   SR    RF,RF               MATCH FOUND                                  
         IC    RF,CROUTINE(RE)                                                  
         SLL   RF,2                                                             
         B     *+0(RF)             BRANCH TO PROCESSING ROUTINE                 
*                                    FROM JUMP TABLE                            
         B     VDEFAGY                                                          
         B     VSYSTEM                                                          
         B     VPROGRM                                                          
*                                  CARD DATA ERROR CONDITIONS                   
VCEND    B     VCYES                                                            
*                                  CARD DATA ERROR CONDITIONS                   
VCERR1   GOTO1 VPRINTER            INVALID CARD                                 
         MVI   ERROR,1                                                          
         BAS   RE,ERRPRT                                                        
         B     VCNO                                                             
*                                                                               
VCNO     B     NO                  EXIT ERROR CONDITION                         
*                                                                               
VCYES    B     YES                 EXIT OK                                      
         EJECT                                                                  
***********************************************************************         
* ROUTINES TO PROCESS EACH JCL CARD DATA LINE                         *         
***********************************************************************         
         SPACE 1                                                                
VDEFAGY  EQU   *                   AGENCY=                                      
         MVC   DEFAGY,P+7                                                       
         B     VCLP1                                                            
*                                                                               
VSYSTEM  EQU   *                   SYSTEM=                                      
         LA    R3,SYSLST                                                        
         USING SYSLSTD,R3                                                       
         LA    R3,6(R3)            R3=A(SYSTEM LIST)                            
*                                                                               
VSYS2    CLI   0(R3),0             TEST E-O-T                                   
         BE    VCERR1                                                           
         CLI   SYSLNUM,1           IGNORE SERVICE SYSTEM                        
         BE    VSYS3                                                            
         CLC   SYSLNAME(7),P+7                                                  
         BE    VSYS4                                                            
VSYS3    LA    R3,SYSLLEN(R3)      NO - BUMP TO NEXT TABLE ENTRY                
         B     VSYS2                                                            
*                                                                               
VSYS4    MVC   SYSTEM,SYSLNUM      RETURN SYSTEM NUMBERS                        
         B     VCLP1                                                            
*                                                                               
VPROGRM  EQU   *                   PROGRAM=XX                                   
         OC    SYSTEM,SYSTEM                                                    
         BZ    VCERR1                                                           
         GOTO1 VHEXIN,PARM,P+8,PROGRAM,2                                        
         OC    12(4,R1),12(R1)                                                  
         BE    VCERR1                                                           
         B     VCLP1                                                            
         EJECT                                                                  
         EJECT                                                                  
***********************************************************************         
* READ CONTROL FILE RECORDS AND BUILD PERSON TABLE                    *         
***********************************************************************         
BLDPERT  NTR1  ,                                                                
         ZAP   LINE,=P'99'                                                      
         MVC   P,SPACES                                                         
         MVC   P(80),=CL80'SECURITY RECORD REPORT PASS 1'                       
         GOTO1 VPRINTER                                                         
         GOTO1 VPRINTER                                                         
*                                                                               
         MVI   WORK+00,C'N'        OPEN CONTROL FILE                            
         MVC   WORK+01(7),CTFILE                                                
         MVI   WORK+08,C'X'                                                     
         GOTO1 VDATAMGR,DMCB,DMOPEN,CONTROL,WORK,IO                             
*                                                                               
         MVI   SQFLAG,0                                                         
         LA    R2,IO                                                            
         USING SAPEKEY,R2                                                       
         XC    SAPEKEY,SAPEKEY                                                  
         XC    IOKEY,IOKEY                                                      
         GOTO1 VDATAMGR,DMCB,DMRDHI,CTFILE,SAPEKEY,SAPEKEY                      
         B     BPER014                                                          
*                                                                               
BPER010  MVC   SAPEKEY(L'SAPEKEY),IOKEY                                         
         CLI   SQFLAG,0                                                         
         BE    BPER012                                                          
         GOTO1 VDATAMGR,DMCB,DMREAD,CTFILE,SAPEKEY,SAPEKEY                      
         CLI   8(R1),0                                                          
         BE    *+14                                                             
         TM    8(R1),X'80'                                                      
         BNZ   BPER200                                                          
         DC    H'0'                                                             
         MVI   SQFLAG,0                                                         
*                                                                               
BPER012  GOTO1 VDATAMGR,DMCB,DMRSEQ,CTFILE,SAPEKEY,SAPEKEY                      
*                                                                               
BPER014  CLI   8(R1),0                                                          
         BE    *+14                                                             
         TM    8(R1),X'80'                                                      
         BNZ   BPER200                                                          
         DC    H'0'                                                             
         MVC   IOKEY(L'SAPEKEY),SAPEKEY                                         
*                                                                               
         MVC   P,SPACES                                                         
*                                                                               
         CLI   SAPETYP,SAPETYPQ                                                 
         BNE   BPER100                                                          
         CLI   SAPESUB,SAPESUBQ                                                 
         BNE   BPER100                                                          
*                                                                               
         CLC   SAPEAGY,DEFAGY                                                   
         BNE   BPER100                                                          
         MVC   PIDSAVE,SAPEPID                                                  
         BAS   RE,GETPER                                                        
         BAS   RE,LOADPID                                                       
         B     BPER100                                                          
*                                                                               
BPER100  EQU   *                                                                
         B     BPER010                                                          
*                                                                               
BPER200  MVC   P,SPACES                                                         
         GOTO1 VPRINTER                                                         
         MVC   P(13),=C'END OF CTFILE'                                          
         GOTO1 VPRINTER                                                         
         B     BPEROK                                                           
BPERNO   B     NO                                                               
BPEROK   B     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* READ CTFILE RECORDS AND PUT TO SORTER                               *         
***********************************************************************         
READCTFL NTR1  ,                                                                
         ZAP   LINE,=P'99'                                                      
         MVC   P(11),=C'READ CTFILE'                                            
         GOTO1 VPRINTER                                                         
         GOTO1 VPRINTER                                                         
         GOTO1 VDATAMGR,DMCB,DMOPEN,CONTROL,FLISTCTF,IO                         
*                                                                               
         MVI   SQFLAG,0                                                         
         LA    R2,IO                                                            
         USING SA0REC,R2                                                        
         XC    SA0KEY,SA0KEY                                                    
         XC    IOKEY,IOKEY                                                      
         GOTO1 VDATAMGR,DMCB,DMRDHI,CTFILE,SA0KEY,SA0KEY                        
         B     RCTF014                                                          
*                                                                               
RCTF010  MVC   SA0KEY(L'SA0KEY),IOKEY                                           
         CLI   SQFLAG,0                                                         
         BE    RCTF012                                                          
         GOTO1 VDATAMGR,DMCB,DMREAD,CTFILE,SA0KEY,SA0KEY                        
         CLI   8(R1),0                                                          
         BE    *+14                                                             
         TM    8(R1),X'80'                                                      
         BNZ   RCTF200                                                          
         DC    H'0'                                                             
         MVI   SQFLAG,0                                                         
*                                                                               
RCTF012  GOTO1 VDATAMGR,DMCB,DMRSEQ,CTFILE,SA0KEY,SA0KEY                        
*                                                                               
RCTF014  CLI   8(R1),0                                                          
         BE    *+14                                                             
         TM    8(R1),X'80'                                                      
         BNZ   RCTF200                                                          
         DC    H'0'                                                             
         MVC   IOKEY(L'SA0KEY),SA0KEY                                           
*                                                                               
         MVC   P,SPACES                                                         
*                                                                               
         CLI   SA0KTYP,SA0KTYPQ                                                 
         BNE   RCTF016                                                          
         TM    SA0STAT,X'40'                                                    
         BZ    RCTF100                                                          
         CLC   SA0KAGY,DEFAGY                                                   
         BNE   RCTF100                                                          
         OC    SA0KPID(12),SA0KPID                                              
         BNZ   RCTF010                                                          
         OC    SA0KCODE(8),SA0KCODE                                             
         BNZ   RCTF010                                                          
         XC    PERSONID,PERSONID                                                
         XC    PASSWORD,PASSWORD                                                
         MVC   PASSNUM,SA0KNUM                                                  
         LA    R3,SA0DATA                                                       
         B     RCTF020                                                          
*                                                                               
RCTF016  EQU   *                                                                
         DROP  R2                                                               
         USING CT5REC,R2                                                        
         CLI   CT5KTYP,CT5KTYPQ                                                 
         BNE   RCTF100                                                          
         CLC   CT5KALPH,DEFAGY                                                  
         BNE   RCTF100                                                          
         LA    R3,CT5DATA                                                       
         SR    RF,RF                                                            
*                                                                               
RCTF018  CLI   0(R3),0                                                          
         BE    RCTF019                                                          
         CLI   0(R3),CTAADELQ                                                   
         BE    *+14                                                             
         IC    RF,1(R3)                                                         
         AR    R3,RF                                                            
         B     RCTF018                                                          
         OI    CTAADFLG-CTAADD(R3),CTAADPRQ                                     
         B     RCTF100                                                          
RCTF019  EQU   *                                                                
         XC    WORK,WORK                                                        
         LA    R4,WORK                                                          
         USING CTAADD,R4                                                        
         MVI   CTAADEL,CTAADELQ    BUILD PERSON POINTER ELEMENT                 
         MVI   CTAADLEN,CTAADLNQ                                                
         OI    CTAADFLG,CTAADPRQ                                                
*                                                                               
         GOTO1 VHELLO,PARM,(C'P',CTFILE),((R2)),WORK,0                          
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         B     RCTF100                                                          
         DROP  R2                                                               
         USING SA0REC,R2                                                        
*                                                                               
RCTF020  CLI   0(R3),0                                                          
         BE    RCTF040                                                          
         CLI   0(R3),SAPALELQ                                                   
         BE    RCTF024                                                          
         CLI   0(R3),SAPASELQ                                                   
         BE    RCTF030                                                          
RCTF022  SR    RF,RF                                                            
         IC    RF,1(R3)                                                         
         AR    R3,RF                                                            
         B     RCTF020                                                          
*                                                                               
         USING SAPALD,R3                                                        
RCTF024  MVC   PERSONID,SAPALPID                                                
         B     RCTF022                                                          
         DROP  R3                                                               
*                                                                               
         USING SAPASD,R3                                                        
RCTF030  EQU   *                                                                
         MVC   PASSWORD,SAPASDTA                                                
*                                                                               
         LA    RE,PASSWORD                                                      
         SR    R5,R5                                                            
         LA    R0,10                                                            
*                                                                               
RCTF032  CLI   0(RE),C' '                                                       
         BE    RCTF034                                                          
         CLI   0(RE),0                                                          
         BE    RCTF034                                                          
         LA    R5,1(R5)                                                         
         LA    RE,1(RE)                                                         
         BCT   R0,RCTF032                                                       
*                                                                               
RCTF034  EQU   *                                                                
         XC    WORK,WORK                                                        
         LA    R4,WORK                                                          
         USING SAPWHD,R4                                                        
         MVI   SAPWHEL,SAPWHELQ    BUILD PERSON POINTER ELEMENT                 
         MVI   SAPWHLN,SAPWHLNQ                                                 
         MVC   SAPWHDTE,TODAY                                                   
         TIME  BIN                                                              
         LTR   R0,R0                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         STCM  R0,15,SAPWHTIM                                                   
         LR    RE,R5                                                            
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   SAPWHPWD(0),PASSWORD                                             
         SR    RF,RF                                                            
         IC    RF,SAPWHLN                                                       
         AR    RF,RE                                                            
         STC   RF,SAPWHLN                                                       
*                                                                               
         GOTO1 VHELLO,PARM,(C'P',CTFILE),((R2)),WORK,0                          
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         B     RCTF022                                                          
         DROP  R4                                                               
*                                                                               
RCTF040  EQU   *                                                                
         OC    PERSONID,PERSONID                                                
         BNZ   *+6                                                              
         DC    H'0'                                                             
         OC    PASSWORD,PASSWORD                                                
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   P(40),=CL40'PASSWORD CONVERTED:'                                 
         MVC   P+30(8),PERSONID                                                 
         MVC   P+40(10),PASSWORD                                                
         GOTO1 VHEXOUT,PARM,PASSNUM,P+52,2,=C'TOG'                              
         GOTO1 VPRINTER                                                         
         MVC   PIDSAVE,PERSONID                                                 
         TM    SA0STAT,X'20'                                                    
         BNZ   RCTF100                                                          
         BAS   RE,TESTPID                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   PASSNUM,PWNSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   PASSWORD,PWDSAVE                                                 
         BE    *+6                                                              
         DC    H'0'                                                             
         B     RCTF100                                                          
*                                                                               
RCTF100  BAS   RE,PUTSORT                                                       
         B     RCTF010                                                          
*                                                                               
RCTF200  MVC   P,SPACES                                                         
         GOTO1 VPRINTER                                                         
         MVC   P(13),=C'END OF CTFILE'                                          
         GOTO1 VPRINTER                                                         
         B     RCTFOK                                                           
*                                                                               
RCTFNO   B     NO                                                               
*                                                                               
RCTFOK   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* WRITE RECORDS FROM SORTER TO OUTPUT FILE                            *         
***********************************************************************         
WRITEOUT NTR1  ,                                                                
         OPEN  (TAPEOUT,OUTPUT)                                                 
*                                                                               
WOUT010  GOTO1 VSORTER,PARM,SORTGET                                             
         ICM   R2,15,4(R1)                                                      
         BZ    WOUT100                                                          
         CLC   WORK(25),4(R2)       CHECK DUPLICATE KEY                         
         BNE   WOUT020                                                          
         BAS   RE,DUPEKEY           REPORT DUPLICATE KEY & DIFF. DATA           
* ??     DC    H'00'                                                            
         B     WOUT010                                                          
*                                                                               
WOUT020  MVC   WORK(25),4(R2)       SAVE RECORD KEY                             
         PUT   TAPEOUT,(R2)                                                     
         B     WOUT010                                                          
*                                                                               
WOUT100  CLOSE (TAPEOUT)                                                        
         MVC   P,SPACES                                                         
         GOTO1 VPRINTER                                                         
         GOTO1 VPRINTER                                                         
         MVC   P(12),=C'END OF WRITE'                                           
         GOTO1 VPRINTER                                                         
         B     WOUTOK                                                           
*                                                                               
WOUTNO   B     NO                                                               
*                                                                               
WOUTOK   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* REPORT DUPLICATE KEY                                                *         
***********************************************************************         
         SPACE 1                                                                
DUPEKEY  NTR1                                                                   
         MVC   P(24),=C'WARNING: DUPLICATE KEY: '                               
         MVC   P+24(08),11(R2)                                                  
         LA    R2,4(R2)                                                         
         GOTO1 VPRINTER                                                         
         GOTO1 VHEXOUT,PARM,(R2),P,25,=C'TOG'                                   
         GOTO1 VPRINTER                                                         
         XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* PUT RECORD TO SORTER                                                *         
***********************************************************************         
         SPACE 1                                                                
PUTSORT  NTR1                                                                   
         SR    RE,RE                                                            
         ICM   RE,3,IO+X'19'                                                    
         LA    RE,4(RE)                                                         
         SLL   RE,16                                                            
         STCM  RE,15,IOL           SET TAPEOUT RECORD LENGTH                    
         GOTO1 VSORTER,PARM,SORTPUT,IOL                                         
         XIT1  ,                                                                
         SPACE 2                                                                
***********************************************************************         
* PUT RECORD TO SORTER FROM IO2                                       *         
***********************************************************************         
         SPACE 1                                                                
PUTSORT2 NTR1                                                                   
         SR    RE,RE                                                            
         ICM   RE,3,IO2+X'19'                                                   
         LA    RE,4(RE)                                                         
         SLL   RE,16                                                            
         STCM  RE,15,IOL2          SET TAPEOUT RECORD LENGTH                    
         GOTO1 VSORTER,PARM,SORTPUT,IOL2                                        
         XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* LOAD PID TABLE                                                      *         
***********************************************************************         
LOADPID  NTR1  ,                                                                
         ICM   RE,15,APIDTAB                                                    
*                                                                               
LPID010  CLM   RE,15,APIDTABX                                                   
         BL    *+6                                                              
         DC    H'0'                                                             
         CLM   RE,15,APIDTABP                                                   
         BE    LPID020                                                          
         CLC   PIDSAVE,0(RE)                                                    
         BE    LPIDOK                                                           
         LA    RE,21(RE)                                                        
         B     LPID010                                                          
*                                                                               
LPID020  MVC   0(8,RE),PIDSAVE                                                  
         MVC   8(10,RE),PWDSAVE                                                 
         MVC   18(2,RE),PWNSAVE                                                 
         MVI   20(RE),0                                                         
         LA    RE,21(RE)                                                        
         STCM  RE,15,APIDTABP                                                   
         B     LPIDOK                                                           
*                                                                               
LPIDNO   B     NO                                                               
LPIDOK   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* TEST PID TABLE                                                      *         
***********************************************************************         
TESTPID  NTR1  ,                                                                
         ICM   RE,15,APIDTAB                                                    
*                                                                               
TPID010  CLM   RE,15,APIDTABX                                                   
         BL    *+6                                                              
         DC    H'0'                                                             
         CLM   RE,15,APIDTABP                                                   
         BE    TPIDNO                                                           
         CLC   PIDSAVE,0(RE)                                                    
         BE    TPID020                                                          
         LA    RE,21(RE)                                                        
         B     TPID010                                                          
*                                                                               
TPID020  MVC   PWDSAVE,8(RE)                                                    
         MVC   PWNSAVE,18(RE)                                                   
         MVI   20(RE),X'FF'                                                     
         B     TPIDOK                                                           
*                                                                               
TPIDNO   B     NO                                                               
TPIDOK   B     YES                                                              
         EJECT                                                                  
         EJECT                                                                  
***********************************************************************         
* GET PERSON ID DETAILS                                               *         
***********************************************************************         
         USING SAPEKEY,R2                                                       
GETPER   NTR1  ,                                                                
         LA    R3,SAPEDATA                                                      
GPER010  CLI   0(R3),0                                                          
         BE    GPER100                                                          
         CLI   0(R3),SAPERELQ                                                   
         BE    GPER030                                                          
         CLI   0(R3),SAPWDELQ                                                   
         BE    GPER040                                                          
GPER020  SR    RF,RF                                                            
         IC    RF,1(R3)                                                         
         AR    R3,RF                                                            
         B     GPER010                                                          
*                                                                               
         USING SAPERD,R3                                                        
GPER030  MVC   OFFSAVE,SAPEROFF                                                 
         B     GPER020                                                          
*                                                                               
         USING SAPWDD,R3                                                        
GPER040  MVC   PWDSAVE,SAPWDCOD                                                 
         MVC   PWNSAVE,SAPWDNUM                                                 
         B     GPER020                                                          
*                                                                               
GPER100  B     GPEROK                                                           
*                                                                               
GPERNO   B     NO                                                               
GPEROK   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* PRINT ERROR MESSAGE                                                 *         
***********************************************************************         
         SPACE 1                                                                
ERRPRT   NTR1                                                                   
         LA    RE,ERRTAB                                                        
         SR    RF,RF                                                            
         ICM   RF,1,ERROR                                                       
         MH    RF,=H'40'                                                        
         AR    RE,RF                                                            
         MVC   P,SPACES                                                         
         MVC   P+13(10),=C'*** ERROR '                                          
         MVC   P+23(L'ERRMSG0),0(RE)                                            
         GOTO1 VPRINTER                                                         
         MVI   ERROR,0                                                          
         XIT1                                                                   
         SPACE 2                                                                
*                                                                               
YES      SR    RC,RC               RETURN CC EQUAL                              
NO       LTR   RC,RC               RETURN CC NOT EQUAL                          
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
FFILL    DC    32X'FF'                                                          
         SPACE 2                                                                
*                                                                               
* AGENCY DEFAULT EXCEPTIONS                                                     
*                                                                               
AGYTAB   DC    CL2'CI'                                                          
         DC    CL2'D1'                                                          
         DC    CL2'D2'                                                          
         DC    CL2'D4'                                                          
         DC    CL2'D5'                                                          
         DC    CL2'D7'                                                          
         DC    CL2'FE'                                                          
AGYTABX  DC    X'00'                                                            
         SPACE 2                                                                
* CARDTBL DEFINES JOB CONTROL INPUT PARAMETER CARDS                             
* AL1    LENGTH OF CARD NAME                                                    
* AL1    ACTION ROUTINE NUMBER                                                  
* XL1    ACTION FLAGS                                                           
* CL8    CARD NAME                                                              
*                                                                               
CARDTBL  DS    0CL14                                                            
         DC    AL1(07,01),X'00',CL11'AGENCY='                                   
         DC    AL1(07,02),X'00',CL11'SYSTEM='                                   
         DC    AL1(08,03),X'00',CL11'PROGRAM='                                  
CARDTBLX DC    AL1(00)                                                          
CLENGTH  EQU   0                                                                
CROUTINE EQU   1                                                                
CFLAG    EQU   2                                                                
CSTRING  EQU   3                                                                
         SPACE 2                                                                
         EJECT                                                                  
* FASYSLST                                                                      
         PRINT OFF                                                              
       ++INCLUDE FASYSLST                                                       
         PRINT ON                                                               
         SPACE 1                                                                
         EJECT                                                                  
YAUTH    DC    X'000F'                                                          
NAUTH    DC    X'0000'                                                          
XAUTH    DC    X'FFFF'                                                          
MAXLEN   DC    H'999'                                                           
         SPACE 1                                                                
COMFACS  DS    0A                                                               
VDATAMGR DC    V(DATAMGR)                                                       
VCPRINT  DC    V(CPRINT)                                                        
VPRINT   DC    V(PRINT)                                                         
VPRINTER DC    V(PRINTER)                                                       
VHELLO   DC    V(HELLO)                                                         
VHEXOUT  DC    V(HEXOUT)                                                        
VHEXIN   DC    V(HEXIN)                                                         
VGETFACT DC    V(GETFACT)                                                       
VCARDS   DC    V(CARDS)                                                         
VSORTER  DC    V(SORTER)                                                        
*                                                                               
ERRTAB   DS    0H                  ERROR REPORT STRINGS                         
ERRMSG0  DC    CL40'DATASET NOT FOUND IN PAN LIBRARY'                           
ERRMSG1  DC    CL40'INVALID CONTROL CARD INPUT'                                 
ERRMSG2  DC    CL40'INVALID INPUT FILE LINE'                                    
         SPACE 1                                                                
TAPEOUT  DCB   DDNAME=TAPEOUT,DSORG=PS,MACRF=(PM),RECFM=VB,            *        
               BLKSIZE=8200,LRECL=2048,BUFNO=2                                  
         SPACE 1                                                                
SORTCRD1 DC    C'SORT FIELDS=(5,25,A),FORMAT=BI,WORK=1 '                        
SORTCRD2 DC    C'RECORD TYPE=V,LENGTH=(2100,,,,) '                              
SORTPUT  DC    C'PUT'                                                           
SORTGET  DC    C'GET'                                                           
*                                                                               
FLISTCTF DC    CL8'NCTFILE '                                                    
         DC    C'X'                                                             
*                                                                               
FLISTUSE DC    CL8'NCTFILE '                                                    
         DC    CL8'NCTUSER '                                                    
         DC    C'X'                                                             
*                                                                               
DMOPEN   DC    C'OPEN   '                                                       
DMREAD   DC    C'DMREAD '                                                       
DMRSEQ   DC    C'DMRSEQ '                                                       
DMRDHI   DC    C'DMRDHI '                                                       
         SPACE 1                                                                
CONTROL  DC    C'CONTROL'                                                       
ACCOUNT  DC    C'ACC'                                                           
CTFILE   DC    C'CTFILE '                                                       
         EJECT                                                                  
PIDTAB   EQU   *                                                                
         DC    (10000)XL21'00'                                                  
PIDTABX  EQU   *                                                                
         EJECT                                                                  
         EJECT                                                                  
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* SEACSFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE SEACSFILE                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* FASYSLSTD                                                                     
         PRINT OFF                                                              
       ++INCLUDE FASYSLSTD                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* FAFACTS                                                                       
         PRINT OFF                                                              
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
         SPACE 1                                                                
* DDDPRINTL                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDDPRINTL                                                      
         PRINT ON                                                               
         SPACE 1                                                                
WORKD    DSECT                     ** GLOBAL WORKING STORAGE **                 
*                                                                               
DUB      DS    D                                                                
DMCB     DS    6F                                                               
PARM     DS    6F                                                               
ASYSLST  DS    A                                                                
COUNT1   DS    F                                                                
COUNT2   DS    F                                                                
FIRSTFLG DS    XL1                                                              
RETCODE  DS    XL1                                                              
PASSFLAG DS    XL1                                                              
CONTROLF DS    XL1                                                              
SQFLAG   DS    XL1                                                              
LISTALL  EQU   X'01'                                                            
UPDATEY  EQU   X'02'                                                            
DEFAGY   DS    CL2                                                              
SYSTEM   DS    XL1                                                              
PROGRAM  DS    XL1                                                              
MVSPARM  DS    XL1                                                              
ERROR    DS    XL1                 FLAG TO INDIATE INVALID INPUT                
UTYPEOFF DS    XL1                                                              
USERS    DS    XL10                                                             
ACTAB    DS    A                                                                
ACTABX   DS    A                                                                
PERSONID DS    CL8                                                              
PASSWORD DS    CL10                                                             
PASSNUM  DS    XL2                                                              
OFFSAVE  DS    CL(L'SAPEROFF)                                                   
PWDSAVE  DS    CL(L'SAPWDCOD)                                                   
PWNSAVE  DS    CL(L'SAPWDNUM)                                                   
PWDLAST  DS    CL(L'SAPEPID)                                                    
PWDTABF  DS    CL1                                                              
TODAY    DS    XL2                 TODAYS DATE BINARY COMPRESSED                
TODAYC   DS    XL2                 AS ABOVE 2S COMPLEMENTED                     
DATETIME DS    XL4                 DATE/TIME VALUE FROM DATTIM                  
DATETIMC DS    XL4                 DATE/TIME VALUE 1'S COMPLEMENTED             
MVSTIME  DS    F                   IBM TIME BINARY 100THS SECS.                 
MVSDATE  DS    F                   IBM DATE JULIAN                              
LINEBUFF DS    XL80                                                             
PIDSAVE  DS    CL(L'SAPEPID)                                                    
PIDLAST  DS    CL(L'SAPEPID)                                                    
PIDTABF  DS    CL1                                                              
APIDTAB  DS    A                                                                
APIDTABP DS    A                                                                
APIDTABX DS    A                                                                
WORK     DS    XL256                                                            
IOKEY    DS    XL(L'CT5KEY)                                                     
IOL      DS    XL4                                                              
IO       DS    2000X                                                            
IOL2     DS    XL4                                                              
IO2      DS    2000X                                                            
WORKX    DS    0D                                                               
         SPACE 1                                                                
WORKC    CSECT                     ** WORKING STORAGE POOL 1 **                 
         DS    (64*1024)X                                                       
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'184SECONVCPP 05/01/02'                                      
         END                                                                    
