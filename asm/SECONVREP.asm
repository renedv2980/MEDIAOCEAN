*          DATA SET SECONVREP  AT LEVEL 048 AS OF 05/01/02                      
*PHASE SECREP                                                                   
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
         TITLE 'SECMRG - SECURITY SYSTEM REPORT'                                
SECREP   CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE WORKX-WORKD,**SECR**,RA,R8,WORK=A(WORKC),CLEAR=YES               
         USING WORKD,RC            RC=A(GLOBAL W/S)                             
         L     R9,VCPRINT                                                       
         USING DPRINT,R9                                                        
         MVC   TITLE(21),=CL21'SECURITY SYSTEM MERGE'                           
         DC    H'0'                                                             
         B     MAIN                                                             
         EJECT                                                                  
***********************************************************************         
* MAIN CONTROL CODE                                                   *         
***********************************************************************         
MAIN     BAS   RE,GENINIT          GENERAL INTIALISATION                        
         BAS   RE,VALCARDS         VALIDATE JCL CARD DATA LINE                  
         BNE   MERR                  EXIT IF ERROR                              
*                                                                               
         BAS   RE,READCTFL         READ CTFILE AND PUT RECS TO SORTER           
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
         MVI   UKFLAG,X'FF'                                                     
         LH    RF,=Y(IOLA-WORKD)                                                
         LA    RF,WORKD(RF)                                                     
         ST    RF,AIOLA                                                         
         LH    RF,=Y(IOA-WORKD)                                                 
         LA    RF,WORKD(RF)                                                     
         ST    RF,AIOA                                                          
         LH    RF,=Y(IOLF-WORKD)                                                
         LA    RF,WORKD(RF)                                                     
         ST    RF,AIOLF                                                         
         LH    RF,=Y(IOF-WORKD)                                                 
         LA    RF,WORKD(RF)                                                     
         ST    RF,AIOF                                                          
         LH    RF,=Y(IOLO-WORKD)                                                
         LA    RF,WORKD(RF)                                                     
         ST    RF,AIOLO                                                         
         LH    RF,=Y(IOO-WORKD)                                                 
         LA    RF,WORKD(RF)                                                     
         ST    RF,AIOO                                                          
*                                                                               
         ICM   RE,15,=A(PIDTAB)    SET ADDRESSES OF LOCAL TABLES                
         STCM  RE,15,APIDTAB                                                    
         STCM  RE,15,APIDTABP                                                   
         ICM   RF,15,=AL4(20000)                                                
         LA    R0,*                                                             
         L     R1,=F'0'                                                         
         MVCL  RE,R0                                                            
         ICM   RE,15,=A(PIDTABX)                                                
         STCM  RE,15,APIDTABX                                                   
         MVI   PIDTABF,0                                                        
         ICM   RE,15,=A(PWDTAB)    SET ADDRESSES OF LOCAL TABLES                
         STCM  RE,15,APWDTAB                                                    
         STCM  RE,15,APWDTABP                                                   
         ICM   RF,15,=AL4(20000)                                                
         LA    R0,*                                                             
         L     R1,=F'0'                                                         
         MVCL  RE,R0                                                            
         ICM   RE,15,=A(PWDTABX)                                                
         STCM  RE,15,APWDTABX                                                   
         MVI   PWDTABF,0                                                        
         ICM   RE,15,=A(DUPTAB)    SET ADDRESSES OF LOCAL TABLES                
         STCM  RE,15,ADUPTAB                                                    
         STCM  RE,15,ADUPTABP                                                   
         ICM   RF,15,=AL4(20000)                                                
         LA    R0,*                                                             
         L     R1,=F'0'                                                         
         MVCL  RE,R0                                                            
         ICM   RE,15,=A(DUPTABX)                                                
         STCM  RE,15,ADUPTABX                                                   
         MVI   DUPTABF,0                                                        
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
         B     VUKFLAG                                                          
         B     VAGENCY                                                          
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
VDEFAGY  EQU   *                   DEFAULT=                                     
         MVC   DEFAGY,P+8                                                       
         B     VCLP1                                                            
*                                                                               
VSYSTEM  EQU   *                   DEFAULT=                                     
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
*                                                                               
VUKFLAG  EQU   *                   UK=X                                         
         CLI   P+3,C'Y'                                                         
         BE    *+12                                                             
         CLI   P+3,C'N'                                                         
         BNE   VCERR1                                                           
         MVC   UKFLAG,P+3                                                       
         B     VCLP1                                                            
         SPACE 1                                                                
VAGENCY  EQU   *                   AGENCY=                                      
         MVC   AGENCYID,P+7                                                     
         B     VCLP1                                                            
         EJECT                                                                  
***********************************************************************         
* READ CONTROL FILE RECORDS AND CONVERT                               *         
***********************************************************************         
READCTFL NTR1  ,                                                                
         ZAP   LINE,=P'99'                                                      
         MVC   P,SPACES                                                         
         MVC   P(80),=CL80'SECURITY RECORD REPORT'                              
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
         USING SAASKEY,R2                                                       
         XC    SAASKEY,SAASKEY                                                  
         XC    IOKEY,IOKEY                                                      
         GOTO1 VDATAMGR,DMCB,DMRDHI,CTFILE,SAASKEY,SAASKEY                      
         B     CONV15A                                                          
*                                                                               
CONV14   MVC   SAASKEY(L'SAASKEY),IOKEY                                         
         CLI   SQFLAG,0                                                         
         BE    CONV15                                                           
         GOTO1 VDATAMGR,DMCB,DMREAD,CTFILE,SAASKEY,SAASKEY                      
         CLI   8(R1),0                                                          
         BE    *+14                                                             
         TM    8(R1),X'80'                                                      
         BNZ   CONV200                                                          
         DC    H'0'                                                             
         MVI   SQFLAG,0                                                         
*                                                                               
CONV15   GOTO1 VDATAMGR,DMCB,DMRSEQ,CTFILE,SAASKEY,SAASKEY                      
*                                                                               
CONV15A  CLI   8(R1),0                                                          
         BE    *+14                                                             
         TM    8(R1),X'80'                                                      
         BNZ   CONV200                                                          
         DC    H'0'                                                             
         MVC   IOKEY(L'SAASKEY),SAASKEY                                         
*                                                                               
         MVC   P,SPACES                                                         
*                                                                               
         CLI   SAASTYP,SAASTYPQ                                                 
         BNE   CONV100                                                          
         CLC   SAASAGY,=CL2'*A'                                                 
         BE    CONV100                                                          
         OC    AGENCYID,AGENCYID                                                
         BZ    CONV20                                                           
         CLC   SAASAGY,AGENCYID                                                 
         BE    CONV20                                                           
         B     CONV100                                                          
*                                                                               
CONV20   CLI   SAASSUB,SAPESUBQ                                                 
         BE    CONVPE                                                           
         B     CONV100                                                          
*                                                                               
         USING SAPEKEY,R2                                                       
CONVPE   EQU   *                                                                
         CLC   LASTAGY,SAPEAGY                                                  
         BE    CONVPE1                                                          
         XC    LASTPID,LASTPID                                                  
*        GOTO1 VPRINTER                                                         
*        GOTO1 VPRINTER                                                         
         MVC   P(8),=CL8'AGENCY :'                                              
         MVC   P+9(2),SAPEAGY                                                   
*        GOTO1 VPRINTER                                                         
*        GOTO1 VPRINTER                                                         
         MVC   LASTAGY,SAPEAGY                                                  
CONVPE1  MVC   P(8),=CL8'AGENCY :'                                              
         MVC   AGYID,SAPEAGY                                                    
         MVC   P+9(2),SAPEAGY                                                   
         CLC   LASTPID,SAPEPID                                                  
         BE    CVPE100                                                          
         MVC   LASTPID,SAPEPID                                                  
         MVC   P+13(11),=CL11'PERSON ID :'                                      
         MVC   P+25(L'SAPEPID),SAPEPID                                          
         LA    R3,SAPEDATA                                                      
CVPE010  CLI   0(R3),0                                                          
         BE    CVPE100                                                          
         CLI   0(R3),SAPWDELQ                                                   
         BE    CVPE030                                                          
*        CLI   0(R3),SANAMELQ                                                   
*        BE    CVPE040                                                          
CVPE020  SR    RF,RF                                                            
         IC    RF,1(R3)                                                         
         AR    R3,RF                                                            
         B     CVPE010                                                          
*                                                                               
         USING SAPWDEL,R3                                                       
CVPE030  EQU   *                                                                
         MVC   P+38(9),=CL9'PERSON #:'                                          
         GOTO1 VHEXOUT,PARM,SAPWDNUM,P+48,2,=C'TOG'                             
         MVC   P+60(9),=CL9'PASSWORD:'                                          
         MVC   P+70(10),SAPWDCOD                                                
         MVC   PWDNUM,SAPWDNUM                                                  
         MVC   PWDCODE,SAPWDCOD                                                 
         BAS   RE,GETAUTH                                                       
         MVC   P+82(9),=CL9'PNUM PID:'                                          
         MVC   P+92(10),PNUMPID                                                 
         MVC   P+104(9),=CL9'PCOD PID:'                                         
         MVC   P+115(10),PCODPID                                                
         MVI   PFLAG,C'N'                                                       
         CLC   PNUMPID,SAPEPID                                                  
         BE    CVPE032                                                          
         GOTO1 VPRINTER                                                         
         MVI   PFLAG,C'Y'                                                       
         CLC   PNUMPID,=CL8' '                                                  
         BE    CVPE031                                                          
         MVC   P(40),=CL40'<**MISSMATCH PNUMPID**>'                             
         GOTO1 VPRINTER                                                         
         MVC   LASTPID,SAPEPID                                                  
         B     CVPE032                                                          
CVPE031  EQU   *                                                                
         MVC   P(40),=CL40'<**MISSING PNUMPID**>'                               
         GOTO1 VPRINTER                                                         
CVPE032  EQU   *                                                                
         CLC   PCODPID,SAPEPID                                                  
         BE    CVPE036                                                          
         CLI   PFLAG,C'Y'                                                       
         BE    CVPE034                                                          
         GOTO1 VPRINTER                                                         
         MVI   PFLAG,C'Y'                                                       
CVPE034  EQU   *                                                                
         CLC   PCODPID,=CL8' '                                                  
         BE    CVPE035                                                          
         MVC   P(40),=CL40'<**MISSMATCH PCODPID**>'                             
         GOTO1 VPRINTER                                                         
         MVC   LASTPID,SAPEPID                                                  
         B     CVPE036                                                          
CVPE035  EQU   *                                                                
         MVC   P(40),=CL40'<**MISSING PCODPID**>'                               
         GOTO1 VPRINTER                                                         
         MVC   LASTPID,SAPEPID                                                  
CVPE036  EQU   *                                                                
         CLI   PFLAG,C'N'                                                       
         BE    CVPE020                                                          
         GOTO1 VPRINTER                                                         
         B     CVPE020                                                          
*                                                                               
         USING SANAMEL,R3                                                       
CVPE040  EQU   *                                                                
         MVC   P+58(12),=CL12'PERSON NAME:'                                     
         MVI   SAVNAME,C' '                                                     
         MVC   SAVNAME+1(L'SAVNAME-1),SAVNAME                                   
         XC    SAVNAME(2),SAVNAME                                               
         CLI   SANAMLN,SANAMLNQ                                                 
         BL    CVPE046                                                          
         LA    RE,SANAMELN                                                      
         LA    RF,SAVNAME+2                                                     
         SR    R1,R1                                                            
         TM    SANAMIND,SANAMIFN   FIRST NAME PRESENT                           
         BZ    CVPE042                                                          
         IC    R1,0(RE)                                                         
         STC   R1,SAVNAME                                                       
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SAVNAME+2(0),1(RE)    SAVE FIRST NAME                            
         LA    RF,2(R1,RF)                                                      
         LA    RE,2(R1,RE)         BUMP TO NEXT                                 
CVPE042  TM    SANAMIND,SANAMIMN   IF THERE IS A MIDDLE NAME                    
         BZ    CVPE044                                                          
         IC    R1,0(RE)            TAKE LENGTH                                  
         LA    RE,1(R1,RE)         AND BUMP TO NEXT                             
CVPE044  TM    SANAMIND,SANAMILN   LAST NAME PRESENT                            
         BZ    CVPE046                                                          
         IC    R1,0(RE)                                                         
         STC   R1,SAVNAME+1                                                     
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),1(RE)                                                    
CVPE046  MVC   P+71(40),SAVNAME+2                                               
         B     CVPE020                                                          
         DROP  R3                                                               
*                                                                               
CVPE100  EQU   *                                                                
         B     CONV100                                                          
*                                                                               
CONV100  EQU   *                                                                
         B     CONV14                                                           
*                                                                               
CONV200  MVC   P,SPACES                                                         
         GOTO1 VPRINTER                                                         
         MVC   P(13),=C'END OF CTFILE'                                          
         GOTO1 VPRINTER                                                         
         B     RCTFOK                                                           
RCTFNO   B     NO                                                               
RCTFOK   B     YES                                                              
         EJECT                                                                  
         EJECT                                                                  
***********************************************************************         
* GET PASSWORD AUTH RECORD DETAILS                                    *         
***********************************************************************         
GETAUTH  NTR1  ,                                                                
         MVC   PNUMPID,=CL8' '                                                  
         MVC   PCODPID,=CL8' '                                                  
         LA    R4,2000(R2)                                                      
         MVI   SQFLAG,X'FF'                                                     
         USING SA0REC,R4                                                        
         XC    SA0KEY,SA0KEY                                                    
         MVI   SA0KTYP,SA0KTYPQ                                                 
         MVC   SA0KAGY,AGYID                                                    
         MVC   SA0KNUM,PWDNUM                                                   
         GOTO1 VDATAMGR,DMCB,DMREAD,CTFILE,SA0KEY,SA0KEY                        
         CLI   DMCB+8,0                                                         
         BNE   GAUT100                                                          
         LA    R1,SA0DATA          EXTRACT INFO FROM ELEMENTS                   
         SR    R0,R0                                                            
GAUT020  CLI   0(R1),0                                                          
         BE    GAUT100                                                          
         CLI   0(R1),SAPALELQ                                                   
         BE    GAUT040                                                          
GAUT022  IC    R0,1(R1)            DO NEXT ELEMENT                              
         AR    R1,R0                                                            
         B     GAUT020                                                          
*                                                                               
         USING SAPALD,R1                                                        
GAUT040  MVC   PNUMPID,SAPALPID                                                 
         B     GAUT022                                                          
*                                                                               
GAUT100  EQU   *                                                                
         LA    R4,2000(R2)                                                      
         MVI   SQFLAG,X'FF'                                                     
         USING SA0REC,R4                                                        
         XC    SA0KEY,SA0KEY                                                    
         MVI   SA0KTYP,SA0KTYPQ                                                 
         MVC   SA0KAGY,AGYID                                                    
         MVC   SA0KCODE,PWDCODE                                                 
         GOTO1 VDATAMGR,DMCB,DMREAD,CTFILE,SA0KEY,SA0KEY                        
         CLI   DMCB+8,0                                                         
         BNE   GAUTOK                                                           
         LA    R1,SA0DATA          EXTRACT INFO FROM ELEMENTS                   
         SR    R0,R0                                                            
GAUT120  CLI   0(R1),0                                                          
         BE    GAUTOK                                                           
         CLI   0(R1),SAPALELQ                                                   
         BE    GAUT140                                                          
GAUT122  IC    R0,1(R1)            DO NEXT ELEMENT                              
         AR    R1,R0                                                            
         B     GAUT120                                                          
*                                                                               
         USING SAPALD,R1                                                        
GAUT140  MVC   PCODPID,SAPALPID                                                 
         B     GAUT122                                                          
*                                                                               
GAUTNO   B     NO                                                               
*                                                                               
GAUTOK   B     YES                                                              
         DROP  R4                                                               
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
         EJECT                                                                  
* AGENCY DEFAULT EXCEPTIONS                                                     
*                                                                               
AGYTAB   EQU   *                                                                
AGYTABX  DC    X'00'                                                            
         SPACE 2                                                                
* CARDTBL DEFINES JOB CONTROL INPUT PARAMETER CARDS                             
* AL1    LENGTH OF CARD NAME                                                    
* AL1    ACTION ROUTINE NUMBER                                                  
* XL1    ACTION FLAGS                                                           
* CL8    CARD NAME                                                              
*                                                                               
CARDTBL  DS    0CL14                                                            
         DC    AL1(08,01),X'00',CL11'DEFAULT='                                  
         DC    AL1(07,02),X'00',CL11'SYSTEM='                                   
         DC    AL1(08,03),X'00',CL11'PROGRAM='                                  
         DC    AL1(03,04),X'00',CL11'UK='                                       
         DC    AL1(07,05),X'00',CL11'AGENCY='                                   
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
         DC    (2000)XL10'00'                                                   
PIDTABX  EQU   *                                                                
         EJECT                                                                  
PWDTAB   EQU   *                                                                
         DC    (2000)XL10'00'                                                   
PWDTABX  EQU   *                                                                
         EJECT                                                                  
DUPTAB   EQU   *                                                                
         DC    (2000)XL10'00'                                                   
DUPTABX  EQU   *                                                                
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
LISTALL  EQU   X'01'                                                            
UPDATEY  EQU   X'02'                                                            
COUNTRY  DS    CL1                                                              
AGYALPH  DS    CL2                                                              
SQFLAG   DS    XL1                                                              
DEFAGY   DS    CL2                                                              
AGENCYID DS    CL2                                                              
SYSTEM   DS    XL1                                                              
PROGRAM  DS    XL1                                                              
MVSPARM  DS    XL1                                                              
ERROR    DS    XL1                 FLAG TO INDIATE INVALID INPUT                
UTYPEOFF DS    XL1                                                              
UKFLAG   DS    CL1                                                              
PFLAG    DS    CL1                                                              
PIDSAVE  DS    CL(L'SAPEPID)                                                    
PIDLAST  DS    CL(L'SAPEPID)                                                    
PIDTABF  DS    CL1                                                              
OFFSAVE  DS    CL(L'SAPEROFF)                                                   
PWDSAVE  DS    CL(L'SAPWDCOD)                                                   
PWNSAVE  DS    CL(L'SAPWDNUM)                                                   
PWDLAST  DS    CL(L'SAPEPID)                                                    
PWDTABF  DS    CL1                                                              
DUPLAST  DS    CL(L'SA0KCODE)                                                   
DUPTABF  DS    CL1                                                              
USERS    DS    XL10                                                             
PWDNUM   DS    XL2                                                              
PWDCODE  DS    CL10                                                             
PNUMPID  DS    CL8                                                              
PCODPID  DS    CL8                                                              
LASTPID  DS    CL8                                                              
AGYID    DS    CL2                                                              
ACTAB    DS    A                                                                
ACTABX   DS    A                                                                
AIOLA    DS    A                                                                
AIOA     DS    A                                                                
AIOLF    DS    A                                                                
AIOF     DS    A                                                                
AIOLO    DS    A                                                                
AIOO     DS    A                                                                
APIDTAB  DS    A                                                                
APIDTABP DS    A                                                                
APIDTABX DS    A                                                                
APWDTAB  DS    A                                                                
APWDTABP DS    A                                                                
APWDTABX DS    A                                                                
ADUPTAB  DS    A                                                                
ADUPTABP DS    A                                                                
ADUPTABX DS    A                                                                
LASTAGY  DS    CL2                                                              
SAVNAME  DS    XL255                                                            
LINEBUFF DS    XL80                                                             
WORK     DS    XL256                                                            
IOKEY    DS    XL(L'CT5KEY)                                                     
IOL      DS    XL4                                                              
IO       DS    2000X                                                            
IO2      DS    2000X                                                            
IOLA     DS    XL4                                                              
IOA      DS    2000X                                                            
IOLF     DS    XL4                                                              
IOF      DS    2000X                                                            
IOLO     DS    XL4                                                              
IOO      DS    2000X                                                            
WORKX    DS    0D                                                               
         SPACE 1                                                                
WORKC    CSECT                     ** WORKING STORAGE POOL 1 **                 
         DS    (64*1024)X                                                       
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'048SECONVREP 05/01/02'                                      
         END                                                                    
