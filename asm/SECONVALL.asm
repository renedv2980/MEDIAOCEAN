*          DATA SET SECONVALL  AT LEVEL 178 AS OF 03/25/15                      
*PHASE SECALLA                                                                  
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
         TITLE 'SECPGM - SECURITY SYSTEM ACCESS CONTROL RECORDS ADD'            
SECALL   CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE WORKX-WORKD,**SECA**,RA,WORK=A(WORKC),CLEAR=YES                  
         USING WORKD,RC            RC=A(GLOBAL W/S)                             
         L     R9,VCPRINT                                                       
         USING DPRINT,R9                                                        
         MVC   TITLE(34),=C'SECURITY ACCESS CONTROL RECORD ADD'                 
         B     MAIN                                                             
         EJECT                                                                  
***********************************************************************         
* MAIN CONTROL CODE                                                   *         
***********************************************************************         
MAIN     BAS   RE,GENINIT          GENERAL INTIALISATION                        
         BAS   RE,VALCARDS         VALIDATE JCL CARD DATA LINE                  
         BNE   MERR                  EXIT IF ERROR                              
*                                                                               
         BAS   RE,READAGYA         READ DEFAULT AGENCY ACCESS RECORD            
         BNE   MERR                  EXIT IF ERROR                              
*                                                                               
         BAS   RE,READAGYF         READ DEFAULT AGENCY FCON RECORD              
         BNE   MERR                  EXIT IF ERROR                              
*                                                                               
* ??     BAS   RE,READAGYO         READ DEFAULT AGENCY OCON RECORD              
* ??     BNE   MERR                  EXIT IF ERROR                              
*                                                                               
         GOTO1 VSORTER,PARM,SORTCRD1,SORTCRD2,0                                 
*                                                                               
         BAS   RE,READCTFL         READ CTFILE AND PUT RECS TO SORTER           
         BNE   MERR                  EXIT IF ERROR                              
*                                                                               
         BAS   RE,WRITEOUT         WRITE RECORDS FROM SORTER TO OUTPUT          
         BNE   MERR                  EXIT IF ERROR                              
         B     MXIT                                                             
*                                                                               
MERR     MVI   RETCODE,X'FF'                                                    
         B     MXIT                                                             
*                                                                               
MXIT     XBASE RC=RETCODE,RL=1                                                  
         EJECT                                                                  
***********************************************************************         
* GENERAL INITIALISATION                                              *         
***********************************************************************         
GENINIT  NTR1  ,                                                                
         MVI   RETCODE,0                                                        
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
         EJECT                                                                  
***********************************************************************         
* READ DEFAULT AGENCY ACCESS RECORDS                                  *         
***********************************************************************         
READAGYA NTR1  ,                                                                
         ZAP   LINE,=P'99'                                                      
         MVC   P(33),=C'READ AGENCY ACCESS DEFAULT RECORD'                      
         GOTO1 VPRINTER                                                         
         GOTO1 VPRINTER                                                         
         L     R2,AIOA                                                          
         GOTO1 VDATAMGR,DMCB,DMOPEN,CONTROL,FLISTCTF,(R2)                       
*                                                                               
         USING SAASREC,R2                                                       
         XC    SAASKEY,SAASKEY                                                  
         MVI   SAASTYP,SAASTYPQ                                                 
         MVI   SAASSUB,SAASSUBQ                                                 
         MVC   SAASAGY,DEFAGY                                                   
         MVC   SAASOVS,SYSTEM                                                   
         MVC   SAASPGM,PROGRAM                                                  
         MVC   IOKEY,0(R2)                                                      
         GOTO1 VDATAMGR,DMCB,DMREAD,CTFILE,(R2),(R2)                            
*                                                                               
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   P,SPACES                                                         
         GOTO1 VPRINTER                                                         
         MVC   P(11),=C'RECORD READ'                                            
         GOTO1 VPRINTER                                                         
         B     RAGAOK                                                           
*                                                                               
RAGANO   B     NO                                                               
*                                                                               
RAGAOK   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* READ DEFAULT AGENCY FCON RECORDS                                    *         
***********************************************************************         
READAGYF NTR1  ,                                                                
         ZAP   LINE,=P'99'                                                      
         MVC   P(31),=C'READ AGENCY FCON DEFAULT RECORD'                        
         GOTO1 VPRINTER                                                         
         GOTO1 VPRINTER                                                         
         L     R2,AIOF                                                          
         GOTO1 VDATAMGR,DMCB,DMOPEN,CONTROL,FLISTCTF,(R2)                       
*                                                                               
         USING SAFCREC,R2                                                       
         XC    SAFCKEY,SAFCKEY                                                  
         MVI   SAFCTYP,SAFCTYPQ                                                 
         MVI   SAFCSUB,SAFCSUBQ                                                 
         MVC   SAFCAGY,DEFAGY                                                   
         MVC   SAFCOVS,SYSTEM                                                   
         MVC   SAFCPGM,PROGRAM                                                  
         MVC   IOKEY,0(R2)                                                      
         GOTO1 VDATAMGR,DMCB,DMREAD,CTFILE,(R2),(R2)                            
*                                                                               
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   P,SPACES                                                         
         GOTO1 VPRINTER                                                         
         MVC   P(11),=C'RECORD READ'                                            
         GOTO1 VPRINTER                                                         
         B     RAGFOK                                                           
*                                                                               
RAGFNO   B     NO                                                               
*                                                                               
RAGFOK   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* READ DEFAULT AGENCY OCON RECORDS                                    *         
***********************************************************************         
READAGYO NTR1  ,                                                                
         ZAP   LINE,=P'99'                                                      
         MVC   P(31),=C'READ AGENCY OCON DEFAULT RECORD'                        
         GOTO1 VPRINTER                                                         
         GOTO1 VPRINTER                                                         
         L     R2,AIOO                                                          
         GOTO1 VDATAMGR,DMCB,DMOPEN,CONTROL,FLISTCTF,(R2)                       
*                                                                               
         USING SAOCREC,R2                                                       
         XC    SAOCKEY,SAOCKEY                                                  
         MVI   SAOCTYP,SAOCTYPQ                                                 
         MVI   SAOCSUB,SAOCSUBQ                                                 
         MVC   SAOCAGY,DEFAGY                                                   
         MVC   SAOCOVS,SYSTEM                                                   
         MVC   SAOCPGM,PROGRAM                                                  
         MVC   IOKEY,0(R2)                                                      
         GOTO1 VDATAMGR,DMCB,DMREAD,CTFILE,(R2),(R2)                            
*                                                                               
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   P,SPACES                                                         
         GOTO1 VPRINTER                                                         
         MVC   P(11),=C'RECORD READ'                                            
         GOTO1 VPRINTER                                                         
         B     RAGOOK                                                           
*                                                                               
RAGONO   B     NO                                                               
*                                                                               
RAGOOK   B     YES                                                              
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
         LA    R2,IO                                                            
         USING CT5REC,R2                                                        
         XC    CT5KEY,CT5KEY                                                    
         GOTO1 ,DMCB,DMRDHI,CTFILE,IO,IO                                        
*                                                                               
RCTF010  GOTO1 VDATAMGR,DMCB       GET FIRST/NEXT RECORD                        
         LA    R0,DMRSEQ                                                        
         ST    R0,DMCB                                                          
         CLI   8(R1),0                                                          
         BE    RCTF012                                                          
         TM    8(R1),X'80'                                                      
         BNZ   RCTF200                                                          
         DC    H'0'                                                             
*                                                                               
RCTF012  CLI   CT5KTYP,CT5KTYPQ                                                 
         BNE   RCTF100                                                          
         BAS   RE,ADSECURE                                                      
         CLC   CT5KALPH,DEFAGY                                                  
         BE    RCTF100                                                          
*                                                                               
         LA    RF,AGYTAB                                                        
RCTF020  CLI   0(RF),0                                                          
         BE    RCTF030                                                          
         CLC   0(2,RF),CT5KALPH                                                 
         BE    RCTF100                                                          
         LA    RF,2(RF)                                                         
         B     RCTF020                                                          
*                                                                               
RCTF030  EQU   *                                                                
         CLI   UKFLAG,X'FF'                                                     
         BE    RCTF040                                                          
         LA    RF,CT5DATA                                                       
         SR    R0,R0                                                            
         USING CTAGDD,RF                                                        
RCTF032  CLI   CTAGDEL,0                                                        
         BNE   *+6                                                              
         DC    H'00'                                                            
         CLI   CTAGDEL,CTAGDELQ                                                 
         BE    *+14                                                             
         IC    R0,CTAGDLEN                                                      
         AR    RF,R0                                                            
         B     RCTF032                                                          
         CLI   UKFLAG,C'Y'                                                      
         BNE   RCTF034                                                          
         CLI   CTAGDCTY,0                                                       
         BNE   RCTF100                                                          
         B     RCTF040                                                          
RCTF034  CLI   CTAGDCTY,0                                                       
         BE    RCTF100                                                          
         B     RCTF040                                                          
         DROP  RF                                                               
*                                                                               
RCTF040  L     R3,AIOA                                                          
         USING SAASREC,R3                                                       
         MVC   SAASAGY,CT5KALPH                                                 
         DROP  R3                                                               
         BAS   RE,PUTSORTA                                                      
         MVC   P(2),CT5KALPH                                                    
         MVC   P+4(19),=C'ADDED ACCESS RECORD'                                  
         GOTO1 VPRINTER                                                         
*                                                                               
         L     R3,AIOF                                                          
         USING SAFCREC,R3                                                       
         MVC   SAFCAGY,CT5KALPH                                                 
         DROP  R3                                                               
         BAS   RE,PUTSORTF                                                      
         MVC   P(2),CT5KALPH                                                    
         MVC   P+4(21),=C'ADDED FCONTROL RECORD'                                
         GOTO1 VPRINTER                                                         
*                                                                               
* ??     L     R3,AIOO                                                          
* ??     USING SAOCREC,R3                                                       
* ??     MVC   SAOCAGY,CT5KALPH                                                 
* ??     DROP  R3                                                               
* ??     BAS   RE,PUTSORTO                                                      
* ??     MVC   P(2),CT5KALPH                                                    
* ??     MVC   P+4(21),=C'ADDED OCONTROL RECORD'                                
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
* PUT RECORD TO SORTER FROM IOA                                       *         
***********************************************************************         
         SPACE 1                                                                
PUTSORTA NTR1                                                                   
         SR    RE,RE                                                            
         L     RF,AIOA                                                          
         ICM   RE,3,25(RF)                                                      
         LA    RE,4(RE)                                                         
         SLL   RE,16                                                            
         L     R2,AIOLA                                                         
         STCM  RE,15,0(R2)         SET TAPEOUT RECORD LENGTH                    
         GOTO1 VSORTER,PARM,SORTPUT,(R2)                                        
         XIT1  ,                                                                
         SPACE 2                                                                
***********************************************************************         
* PUT RECORD TO SORTER FROM IOF                                       *         
***********************************************************************         
         SPACE 1                                                                
PUTSORTF NTR1                                                                   
         SR    RE,RE                                                            
         L     RF,AIOF                                                          
         ICM   RE,3,25(RF)                                                      
         LA    RE,4(RE)                                                         
         SLL   RE,16                                                            
         L     R2,AIOLF                                                         
         STCM  RE,15,0(R2)         SET TAPEOUT RECORD LENGTH                    
         GOTO1 VSORTER,PARM,SORTPUT,(R2)                                        
         XIT1  ,                                                                
         SPACE 2                                                                
***********************************************************************         
* PUT RECORD TO SORTER FROM IOO                                       *         
***********************************************************************         
         SPACE 1                                                                
PUTSORTO NTR1                                                                   
         SR    RE,RE                                                            
         L     RF,AIOO                                                          
         ICM   RE,3,25(RF)                                                      
         LA    RE,4(RE)                                                         
         SLL   RE,16                                                            
         L     R2,AIOLO                                                         
         STCM  RE,15,0(R2)         SET TAPEOUT RECORD LENGTH                    
         GOTO1 VSORTER,PARM,SORTPUT,(R2)                                        
         XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* ADJUST ACCESS RECORD SYSTEM PROGRAM SECURITY                        *         
***********************************************************************         
         SPACE 1                                                                
ADSECURE NTR1                                                                   
         MVC   P(2),CT5KALPH                                                    
         MVC   P+4(16),=C'SECURITY PRESENT'                                     
         LA    R3,CT5DATA                                                       
         USING CTSYSD,R3                                                        
ASEC010  CLI   CTSYSEL,0                                                        
         BE    ASECX                                                            
         CLI   CTSYSEL,CTSYSELQ                                                 
         BNE   ASEC020                                                          
         CLI   CTSYSNUM,X'06'                                                   
         BNE   ASEC020                                                          
         CLI   CTSYSLEN,X'10'                                                   
         BE    ASEC030                                                          
         CLI   CTSYSLEN,X'18'                                                   
         BE    ASEC040                                                          
ASEC020  SR    R0,R0                                                            
         IC    R0,CTSYSLEN                                                      
         AR    R3,R0                                                            
         B     ASEC010                                                          
         CLI   UKFLAG,C'Y'                                                      
*                                                                               
ASEC030  XC    WORK,WORK                                                        
         ZIC   RF,CTSYSLEN                                                      
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),CTSYSEL                                                  
         MVC   DUB(1),CTSYSNUM                                                  
         GOTO1 VHELLO,PARM,(C'D',CTFILE),(WORK,(R2)),(1,DUB),0                  
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         LA    R3,WORK                                                          
         MVI   CTSYSLEN,X'18'                                                   
         MVC   CTSYSPGM(8),=XL8'0000002900000000'                               
         GOTO1 VHELLO,PARM,(C'P',CTFILE),((R2)),WORK,0                          
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         MVC   P+4(16),=C'SECURITY ADDED  '                                     
         B     ASEC020                                                          
*                                                                               
ASEC040  EQU   *                                                                
         OC    CTSYSPGM(8),=XL8'0000000100000000'                               
         MVC   P+4(16),=C'SECURITY CHANGED'                                     
         B     ASEC020                                                          
*                                                                               
ASECX    GOTO1 VPRINTER                                                         
         XIT1  ,                                                                
         DROP  R3                                                               
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
SORTCRD1 DC    C'SORT FIELDS=(5,25,A),FORMAT=BI '                               
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
DEFAGY   DS    CL2                                                              
SYSTEM   DS    XL1                                                              
PROGRAM  DS    XL1                                                              
MVSPARM  DS    XL1                                                              
ERROR    DS    XL1                 FLAG TO INDIATE INVALID INPUT                
UTYPEOFF DS    XL1                                                              
UKFLAG   DS    CL1                                                              
USERS    DS    XL10                                                             
ACTAB    DS    A                                                                
ACTABX   DS    A                                                                
AIOLA    DS    A                                                                
AIOA     DS    A                                                                
AIOLF    DS    A                                                                
AIOF     DS    A                                                                
AIOLO    DS    A                                                                
AIOO     DS    A                                                                
LINEBUFF DS    XL80                                                             
WORK     DS    XL256                                                            
IOKEY    DS    XL(L'CT5KEY)                                                     
IOL      DS    XL4                                                              
IO       DS    2000X                                                            
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
**PAN#1  DC    CL21'178SECONVALL 03/25/15'                                      
         END                                                                    
