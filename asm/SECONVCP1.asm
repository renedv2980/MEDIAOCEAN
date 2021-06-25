*          DATA SET SECONVCP1  AT LEVEL 023 AS OF 05/22/02                      
*PHASE SECCP1                                                                   
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
         TITLE 'SECCP1 - SECURITY SYSTEM COPY AGENCY TO NEW AGENCY'             
SECCP1   CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE WORKX-WORKD,**SECC**,RA,R8,WORK=A(WORKC),CLEAR=YES               
         USING WORKD,RC            RC=A(GLOBAL W/S)                             
         L     R9,VCPRINT                                                       
         USING DPRINT,R9                                                        
         MVC   TITLE(21),=CL21'SECURITY SYSTEM MERGE'                           
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
* READ CONTROL FILE RECORDS AND BUILD PASSWORD TABLE                  *         
***********************************************************************         
BLDTABLE NTR1  ,                                                                
         ZAP   LINE,=P'99'                                                      
         MVC   P,SPACES                                                         
         MVC   P(80),=CL80'SECURITY RECORD MERGE REPORT PASS 1'                 
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
         USING SA0KEY,R2                                                        
         XC    SA0KEY,SA0KEY                                                    
         XC    IOKEY,IOKEY                                                      
         GOTO1 VDATAMGR,DMCB,DMRDHI,CTFILE,SA0KEY,SA0KEY                        
         B     BTAB020A                                                         
*                                                                               
BTAB010  MVC   SA0KEY(L'SA0KEY),IOKEY                                           
         CLI   SQFLAG,0                                                         
         BE    BTAB020                                                          
         GOTO1 VDATAMGR,DMCB,DMREAD,CTFILE,SA0KEY,SA0KEY                        
         CLI   8(R1),0                                                          
         BE    *+14                                                             
         TM    8(R1),X'80'                                                      
         BNZ   BTAB200                                                          
         DC    H'0'                                                             
         MVI   SQFLAG,0                                                         
*                                                                               
BTAB020  GOTO1 VDATAMGR,DMCB,DMRSEQ,CTFILE,SA0KEY,SA0KEY                        
*                                                                               
BTAB020A CLI   8(R1),0                                                          
         BE    *+14                                                             
         TM    8(R1),X'80'                                                      
         BNZ   BTAB200                                                          
         DC    H'0'                                                             
         MVC   IOKEY(L'SA0KEY),SA0KEY                                           
*                                                                               
         MVC   P,SPACES                                                         
         MVC   P(1),IO                                                          
*                                                                               
         CLI   SA0KTYP,SA0KTYPQ                                                 
         BE    BTABAU                                                           
*                                                                               
         USING SA0KEY,R2                                                        
BTABAU   EQU   *                                                                
         CLC   SA0KAGY,SRCAGY1                                                  
         BE    BTABAU1                                                          
         B     BTAB100                                                          
BTABAU1  EQU   *                                                                
         TM    SA0STAT,X'40'                                                    
         BZ    BTABAU4                                                          
         OC    SA0KEYS(20),SA0KEYS                                              
         BZ    BTABAU2                                                          
         OC    SA0KEYS(12),SA0KEYS                                              
         BZ    BTABAU3                                                          
         DC    H'0'                                                             
BTABAU2  EQU   *                                                                
         B     BTAB100                                                          
BTABAU3  EQU   *                                                                
         ICM   RE,15,ADUPTAB                                                    
*                                                                               
BTABAU4  CLM   RE,15,ADUPTABX                                                   
         BL    *+6                                                              
         DC    H'0'                                                             
         CLM   RE,15,ADUPTABP                                                   
         BE    BTABAU5                                                          
         CLC   SA0KCODE,0(RE)                                                   
         BE    BTAB100                                                          
         LA    RE,L'SA0KCODE(RE)                                                
         B     BTABAU4                                                          
*                                                                               
BTABAU5  MVC   0(L'SA0KCODE,RE),SA0KCODE                                        
         LA    RE,L'SA0KCODE(RE)                                                
         STCM  RE,15,ADUPTABP                                                   
         B     BTAB100                                                          
*                                                                               
         USING SA0KEY,R2                                                        
BTAB100  EQU   *                                                                
         B     BTAB010                                                          
*                                                                               
BTAB200  MVC   P,SPACES                                                         
         GOTO1 VPRINTER                                                         
         MVC   P(13),=C'END OF CTFILE'                                          
         GOTO1 VPRINTER                                                         
         B     BTABOK                                                           
BTABNO   B     NO                                                               
BTABOK   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* READ CONTROL FILE RECORDS AND CONVERT                               *         
***********************************************************************         
READCTFL NTR1  ,                                                                
         ZAP   LINE,=P'99'                                                      
         MVC   P,SPACES                                                         
         MVC   P(80),=CL80'SECURITY RECORD COPY REPORT PASS 1'                  
         GOTO1 VPRINTER                                                         
         GOTO1 VPRINTER                                                         
*                                                                               
         MVI   WORK+00,C'N'        OPEN CONTROL FILE                            
         MVC   WORK+01(7),CTFILE                                                
         MVI   WORK+08,C'X'                                                     
         GOTO1 VDATAMGR,DMCB,DMOPEN,CONTROL,WORK,IO                             
*                                                                               
         OPEN  (TAPEOUT,OUTPUT)                                                 
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
         MVC   P(1),IO                                                          
*                                                                               
         CLI   SAASTYP,SA0KTYPQ                                                 
         BE    CONVAU                                                           
         CLI   SAASTYP,SAASTYPQ                                                 
         BNE   CONV100                                                          
         CLC   SAASAGY,MRGAGY1                                                  
         BE    CONV110                                                          
         CLC   SAASAGY,SRCAGY1                                                  
         BE    CONV20                                                           
         B     CONV100                                                          
*                                                                               
CONV20   CLI   SAASSUB,SAASSUBQ                                                 
         BE    CONVAS                                                           
         CLI   SAASSUB,SAFCSUBQ                                                 
         BE    CONVFC                                                           
         CLI   SAASSUB,SAOCSUBQ                                                 
         BE    CONVOC                                                           
         CLI   SAASSUB,SAOFSUBQ                                                 
         BE    CONVOF                                                           
         CLI   SAASSUB,SADPSUBQ                                                 
         BE    CONVDP                                                           
         CLI   SAASSUB,SAPESUBQ                                                 
         BE    CONVPE                                                           
         CLI   SAASSUB,SAAGSUBQ                                                 
         BE    CONVAG                                                           
         CLI   SAASSUB,SALASUBQ                                                 
         BE    CONVLA                                                           
         CLI   SAASSUB,SALMSUBQ                                                 
         BE    CONVLM                                                           
         CLI   SAASSUB,SAAPSUBQ                                                 
         BE    CONVAP                                                           
         DC    H'0'                                                             
         B     CONV100                                                          
*                                                                               
         USING SAASKEY,R2                                                       
CONVAS   EQU   *                                                                
         OC    SAASUID,SAASUID                                                  
         BZ    *+6                                                              
         DC    H'0'                                                             
         BAS   RE,PUTSORT                                                       
         MVC   P(20),=CL20'ACCESS RECORD : '                                    
         MVC   P+20(2),SAASAGY                                                  
         MVC   P+30(6),=CL6'COPIED'                                             
         MVC   P+38(9),=CL9'SYS/PROG:'                                          
         GOTO1 VHEXOUT,PARM,SAASOVPG,P+48,2,=C'TOG'                             
         MVC   P+54(5),=CL5'UID#:'                                              
         GOTO1 VHEXOUT,PARM,SAASUID,P+60,2,=C'TOG'                              
         MVC   P+66(5),=CL5'GRP#:'                                              
         GOTO1 VHEXOUT,PARM,SAASAGN,P+72,2,=C'TOG'                              
*        GOTO1 VHEXOUT,PARM,(R2),P+44,25,=C'TOG'                                
         GOTO1 VPRINTER                                                         
         MVC   SAASAGY,MRGAGY1                                                  
         B     CONV100                                                          
*                                                                               
         USING SAFCKEY,R2                                                       
CONVFC   EQU   *                                                                
         OC    SAFCUID,SAFCUID                                                  
         BZ    *+6                                                              
         DC    H'0'                                                             
         BAS   RE,PUTSORT                                                       
         MVC   P(20),=CL20'FCONTROL RECORD : '                                  
         MVC   P+20(2),SAFCAGY                                                  
         MVC   P+30(6),=CL6'COPIED'                                             
         MVC   P+38(9),=CL9'SYS/PROG:'                                          
         GOTO1 VHEXOUT,PARM,SAFCOVPG,P+48,2,=C'TOG'                             
         MVC   P+54(5),=CL5'UID#:'                                              
         GOTO1 VHEXOUT,PARM,SAFCUID,P+60,2,=C'TOG'                              
         MVC   P+66(5),=CL5'GRP#:'                                              
         GOTO1 VHEXOUT,PARM,SAFCAGN,P+72,2,=C'TOG'                              
*        GOTO1 VHEXOUT,PARM,(R2),P+44,25,=C'TOG'                                
         GOTO1 VPRINTER                                                         
         MVC   SAFCAGY,MRGAGY1                                                  
         B     CONV100                                                          
*                                                                               
         USING SAOCKEY,R2                                                       
CONVOC   EQU   *                                                                
         OC    SAOCUID,SAOCUID                                                  
         BZ    *+6                                                              
         DC    H'0'                                                             
         BAS   RE,PUTSORT                                                       
         MVC   P(20),=CL20'OCONTROL RECORD : '                                  
         MVC   P+20(2),SAOCAGY                                                  
         MVC   P+30(6),=CL6'COPIED'                                             
         MVC   P+38(9),=CL9'SYS/PROG:'                                          
         GOTO1 VHEXOUT,PARM,SAOCOVPG,P+48,2,=C'TOG'                             
         MVC   P+54(5),=CL5'UID#:'                                              
         GOTO1 VHEXOUT,PARM,SAOCUID,P+60,2,=C'TOG'                              
         MVC   P+66(5),=CL5'GRP#:'                                              
         GOTO1 VHEXOUT,PARM,SAOCAGN,P+72,2,=C'TOG'                              
*        GOTO1 VHEXOUT,PARM,(R2),P+44,25,=C'TOG'                                
         GOTO1 VPRINTER                                                         
         MVC   SAOCAGY,MRGAGY1                                                  
         B     CONV100                                                          
*                                                                               
         USING SAOFKEY,R2                                                       
CONVOF   EQU   *                                                                
         BAS   RE,PUTSORT                                                       
         MVC   P(20),=CL20'OFFICE RECORD : '                                    
         MVC   P+20(2),SAOFAGY                                                  
         MVC   P+30(20),=CL20'COPIED'                                           
         MVC   P+40(L'SAOFOID),SAOFOID                                          
         GOTO1 VPRINTER                                                         
         MVC   P,SPACES                                                         
         MVC   SAOFAGY,MRGAGY1                                                  
         B     CONV100                                                          
*                                                                               
         USING SADPKEY,R2                                                       
CONVDP   EQU   *                                                                
         BAS   RE,PUTSORT                                                       
         MVC   P(20),=CL20'DEPARTMENT RECORD : '                                
         MVC   P+20(2),SADPAGY                                                  
         MVC   P+30(20),=CL20'COPIED'                                           
         MVC   P+40(L'SADPOID),SADPOID                                          
         MVC   P+43(L'SADPDID),SADPDID                                          
         GOTO1 VPRINTER                                                         
         MVC   SADPAGY,MRGAGY1                                                  
         B     CONV100                                                          
*                                                                               
         USING SAPEKEY,R2                                                       
CONVPE   EQU   *                                                                
         BAS   RE,PUTSORT                                                       
         MVC   P(20),=CL20'PERSON RECORD : '                                    
         MVC   P+20(2),SAPEAGY                                                  
         MVC   P(40),=CL40'PERSON ID COPIED:'                                   
         MVC   P+26(L'SAPEPID),SAPEPID                                          
         GOTO1 VPRINTER                                                         
         MVC   SAPEAGY,MRGAGY1                                                  
         B     CONV100                                                          
*                                                                               
         USING SAAGKEY,R2                                                       
CONVAG   EQU   *                                                                
         BAS   RE,PUTSORT                                                       
         MVC   P(20),=CL20'GROUP RECORD : '                                     
         MVC   P+20(2),SAAGAGY                                                  
         MVC   P+26(L'SAAGAGR),SAAGAGR                                          
         GOTO1 VPRINTER                                                         
         MVC   SAAGAGY,MRGAGY1                                                  
         B     CONV100                                                          
*                                                                               
         USING SALAKEY,R2                                                       
CONVLA   EQU   *                                                                
         BAS   RE,PUTSORT                                                       
         MVC   P(20),=CL20'LACCESS RECORD : '                                   
         MVC   P+20(2),SALAAGY                                                  
         GOTO1 VPRINTER                                                         
         MVC   SALAAGY,MRGAGY1                                                  
         B     CONV100                                                          
*                                                                               
         USING SALMKEY,R2                                                       
CONVLM   EQU   *                                                                
         BAS   RE,PUTSORT                                                       
         MVC   P(20),=CL20'LIMIT RECORD : '                                     
         MVC   P+20(2),SALMAGY                                                  
         GOTO1 VHEXOUT,PARM,(R2),P+24,25,=C'TOG'                                
         GOTO1 VPRINTER                                                         
         MVC   SALMAGY,MRGAGY1                                                  
         B     CONV100                                                          
*                                                                               
         USING SAAPKEY,R2                                                       
CONVAP   EQU   *                                                                
         BAS   RE,PUTSORT                                                       
         MVC   P(20),=CL20'APPROVER RECORD : '                                  
         MVC   P+20(2),SAAPAGY                                                  
         GOTO1 VHEXOUT,PARM,(R2),P+24,25,=C'TOG'                                
         GOTO1 VPRINTER                                                         
         MVC   SAAPAGY,MRGAGY1                                                  
         B     CONV100                                                          
*                                                                               
         USING SA0KEY,R2                                                        
CONVAU   EQU   *                                                                
         CLC   SA0KAGY,MRGAGY1                                                  
         BE    CONV110                                                          
         CLC   SA0KAGY,SRCAGY1                                                  
         BE    CONVAU1                                                          
         B     CONV100                                                          
CONVAU1  EQU   *                                                                
         BAS   RE,PUTSORT                                                       
         MVC   P(20),=CL20'AUTH RECORD: '                                       
         MVC   P+20(2),SA0KAGY                                                  
         GOTO1 VHEXOUT,PARM,(R2),P+24,25,=C'TOG'                                
         GOTO1 VPRINTER                                                         
         MVC   SA0KAGY,MRGAGY1                                                  
         B     CONV100                                                          
*                                                                               
         USING SAASKEY,R2                                                       
CONV100  BAS   RE,PUTSORT                                                       
         B     CONV14                                                           
*                                                                               
CONV110  MVC   P(20),=CL20'RECORD DROPPED: '                                    
         GOTO1 VHEXOUT,PARM,(R2),P+24,25,=C'TOG'                                
         GOTO1 VPRINTER                                                         
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
***********************************************************************         
* WRITE RECORDS FROM SORTER TO OUTPUT FILE                            *         
***********************************************************************         
WRITEOUT NTR1  ,                                                                
         OPEN  (TAPEOUT,OUTPUT)                                                 
*                                                                               
WOUT010  GOTO1 VSORTER,PARM,SORTGET                                             
         ICM   R2,15,4(R1)                                                      
         BZ    WOUT100                                                          
         BAS   RE,CHECKPER          CHECK PERSON ID DUPLICATE                   
         BNE   WOUT010                                                          
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
*        MVC   P(24),=CL24'WARNING DUPLICATE KEY '                              
*        GOTO1 VPRINTER                                                         
         LA    R2,4(R2)                                                         
*        GOTO1 VHEXOUT,PARM,(R2),P,25,=C'TOG'                                   
*        GOTO1 VPRINTER                                                         
         USING SAASKEY,R2                                                       
         CLI   SAASTYP,SA0KTYPQ                                                 
         BE    DUPEAU                                                           
         CLI   SAASTYP,SAASTYPQ                                                 
         BNE   DUPEUND                                                          
*                                                                               
DUPESEC  CLC   SAASAGY,SRCAGY1                                                  
         BE    DUPESEC1                                                         
         B     DUPEUND                                                          
*                                                                               
DUPESEC1 CLI   SAASSUB,SAASSUBQ                                                 
         BE    DUPEAS                                                           
         CLI   SAASSUB,SAFCSUBQ                                                 
         BE    DUPEFC                                                           
         CLI   SAASSUB,SAOCSUBQ                                                 
         BE    DUPEOC                                                           
         CLI   SAASSUB,SAOFSUBQ                                                 
         BE    DUPEOF                                                           
         CLI   SAASSUB,SADPSUBQ                                                 
         BE    DUPEDP                                                           
         CLI   SAASSUB,SAPESUBQ                                                 
         BE    DUPEPE                                                           
         CLI   SAASSUB,SAAGSUBQ                                                 
         BE    DUPEAG                                                           
         CLI   SAASSUB,SALASUBQ                                                 
         BE    DUPELA                                                           
         CLI   SAASSUB,SALMSUBQ                                                 
         BE    DUPELM                                                           
         CLI   SAASSUB,SALMSUBQ                                                 
         BE    DUPEAP                                                           
         DC    H'0'                                                             
         B     DUPE100                                                          
*                                                                               
         USING SAASKEY,R2                                                       
DUPEAS   EQU   *                                                                
         MVC   P(20),=CL20'ACCESS RECORD : '                                    
         MVC   P+20(2),SAASAGY                                                  
         GOTO1 VHEXOUT,PARM,(R2),P+24,25,=C'TOG'                                
         GOTO1 VPRINTER                                                         
         B     DUPE100                                                          
*                                                                               
         USING SAFCKEY,R2                                                       
DUPEFC   EQU   *                                                                
         MVC   P(20),=CL20'FCONTROL RECORD : '                                  
         MVC   P+20(2),SAFCAGY                                                  
         GOTO1 VHEXOUT,PARM,(R2),P+24,25,=C'TOG'                                
         GOTO1 VPRINTER                                                         
         B     DUPE100                                                          
*                                                                               
         USING SAOCKEY,R2                                                       
DUPEOC   EQU   *                                                                
         MVC   P(20),=CL20'OCONTROL RECORD : '                                  
         MVC   P+20(2),SAOCAGY                                                  
         GOTO1 VHEXOUT,PARM,(R2),P+24,25,=C'TOG'                                
         GOTO1 VPRINTER                                                         
         B     DUPE100                                                          
*                                                                               
         USING SAOFKEY,R2                                                       
DUPEOF   EQU   *                                                                
         MVC   P(20),=CL20'OFFICE RECORD : '                                    
         MVC   P+20(2),SAOFAGY                                                  
         MVC   P+24(L'SAOFOID),SAOFOID                                          
         GOTO1 VPRINTER                                                         
         B     DUPE100                                                          
*                                                                               
         USING SADPKEY,R2                                                       
DUPEDP   EQU   *                                                                
         MVC   P(20),=CL20'DEPARTMENT RECORD : '                                
         MVC   P+20(2),SADPAGY                                                  
         MVC   P+24(L'SADPOID),SADPOID                                          
         MVC   P+24+L'SADPOID(L'SADPDID),SADPDID                                
         GOTO1 VPRINTER                                                         
         B     DUPE100                                                          
*                                                                               
         USING SAPEKEY,R2                                                       
DUPEPE   EQU   *                                                                
         B     DUPE100             ??                                           
         MVC   P(20),=CL20'PERSON RECORD : '                                    
         MVC   P+20(2),SAPEAGY                                                  
         MVC   P+24(L'SAPEPID),SAPEPID                                          
         MVC   WORK(2),SAPEDEF                                                  
         XC    WORK(2),=XL2'FFFF'                                               
         GOTO1 VHEXOUT,PARM,WORK,P+40,2,=C'TOG'                                 
         GOTO1 VPRINTER                                                         
         B     DUPE100                                                          
*                                                                               
         USING SAAGKEY,R2                                                       
DUPEAG   EQU   *                                                                
         MVC   P(20),=CL20'GROUP RECORD : '                                     
         MVC   P+20(2),SAAGAGY                                                  
         MVC   P+24(L'SAAGAGR),SAAGAGR                                          
         GOTO1 VPRINTER                                                         
         B     DUPE100                                                          
*                                                                               
         USING SALAKEY,R2                                                       
DUPELA   EQU   *                                                                
         MVC   P(20),=CL20'LACCESS RECORD : '                                   
         MVC   P+20(2),SALAAGY                                                  
         MVC   P+24(L'SALAAGR),SALAAGR                                          
         GOTO1 VPRINTER                                                         
         B     DUPE100                                                          
*                                                                               
         USING SALMKEY,R2                                                       
DUPELM   EQU   *                                                                
         MVC   P(20),=CL20'LALIST RECORD : '                                    
         MVC   P+20(2),SALMAGY                                                  
         MVC   P+24(L'SALMLID),SALMLID                                          
         GOTO1 VPRINTER                                                         
         B     DUPE100                                                          
*                                                                               
         USING SAAPKEY,R2                                                       
DUPEAP   EQU   *                                                                
         MVC   P(20),=CL20'APPROVER RECORD : '                                  
         MVC   P+20(2),SAAPAGY                                                  
         MVC   P+24(L'SAAPAGR),SAAPAGR                                          
         GOTO1 VPRINTER                                                         
         B     DUPE100                                                          
*                                                                               
         USING SA0KEY,R2                                                        
DUPEAU   EQU    *                                                               
         CLC   SA0KAGY,SRCAGY1                                                  
         BE    DUPEAU1                                                          
         B     DUPEUND                                                          
DUPEAU1  EQU    *                                                               
         TM    SA0STAT,X'40'                                                    
         BZ    DUPEAU4                                                          
         OC    SA0KEYS(20),SA0KEYS                                              
         BZ    DUPEAU2                                                          
         OC    SA0KEYS(12),SA0KEYS                                              
         BZ    DUPEAU3                                                          
         DC    H'0'                                                             
DUPEAU2  MVC   P(20),=CL20'PASSWORD # RECORD: '                                 
         MVC   P+20(2),SA0KAGY                                                  
         GOTO1 VHEXOUT,PARM,(R2),P+24,25,=C'TOG'                                
         GOTO1 VPRINTER                                                         
         B     DUPE100                                                          
DUPEAU3  MVC   P(20),=CL20'PASSWORD RECORD: '                                   
         MVC   P+20(2),SA0KAGY                                                  
         MVC   P+24(L'SA0KCODE),SA0KCODE                                        
         GOTO1 VPRINTER                                                         
         B     DUPE100                                                          
DUPEAU4  MVC   P(20),=CL20'OLD AUTH RECORD: '                                   
         MVC   P+20(2),SA0KAGY                                                  
         GOTO1 VHEXOUT,PARM,(R2),P+24,25,=C'TOG'                                
         GOTO1 VPRINTER                                                         
         MVC   P(25),0(R2)                                                      
         GOTO1 VPRINTER                                                         
         B     DUPE100                                                          
*                                                                               
DUPEUND  MVC   P(24),=CL24'UNDEFINED DUPLICATE'                                 
         GOTO1 VHEXOUT,PARM,(R2),P+24,25,=C'TOG'                                
         GOTO1 VPRINTER                                                         
         B     DUPEX                                                            
*                                                                               
DUPE100  EQU   *                                                                
         B     DUPEX                                                            
*                                                                               
DUPEX    EQU   *                                                                
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
         LA    RE,L'SAPEPID(RE)                                                 
         B     LPID010                                                          
*                                                                               
LPID020  MVC   0(L'SAPEPID,RE),PIDSAVE                                          
         LA    RE,L'SAPEPID(RE)                                                 
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
*        CLC   PIDLAST,PIDSAVE                                                  
*        BE    TPIDOK                                                           
*        MVC   PIDLAST,PIDSAVE                                                  
*                                                                               
TPID010  CLM   RE,15,APIDTABX                                                   
         BL    *+6                                                              
         DC    H'0'                                                             
         CLM   RE,15,APIDTABP                                                   
         BE    TPIDOK                                                           
         CLC   PIDSAVE,0(RE)                                                    
         BE    TPIDNO                                                           
         LA    RE,L'SAPEPID(RE)                                                 
         B     TPID010                                                          
*                                                                               
TPIDNO   B     NO                                                               
TPIDOK   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* LOAD PWD TABLE                                                      *         
***********************************************************************         
LOADPWD  NTR1  ,                                                                
         ICM   RE,15,APWDTAB                                                    
*                                                                               
LPWD010  CLM   RE,15,APWDTABX                                                   
         BL    *+6                                                              
         DC    H'0'                                                             
         CLM   RE,15,APWDTABP                                                   
         BE    LPWD020                                                          
         CLC   PIDSAVE,0(RE)                                                    
         BE    LPWDOK                                                           
         LA    RE,L'SAPEPID(RE)                                                 
         B     LPWD010                                                          
*                                                                               
LPWD020  MVC   0(L'SAPEPID,RE),PIDSAVE                                          
         LA    RE,L'SAPEPID(RE)                                                 
         STCM  RE,15,APWDTABP                                                   
         B     LPWDOK                                                           
*                                                                               
LPWDNO   B     NO                                                               
LPWDOK   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* TEST PWD TABLE                                                      *         
***********************************************************************         
TESTPWD  NTR1  ,                                                                
         ICM   RE,15,APWDTAB                                                    
*                                                                               
TPWD010  CLM   RE,15,APWDTABX                                                   
         BL    *+6                                                              
         DC    H'0'                                                             
         CLM   RE,15,APWDTABP                                                   
         BE    TPWDOK                                                           
         CLC   PIDSAVE,0(RE)                                                    
         BE    TPWDNO                                                           
         LA    RE,L'SAPEPID(RE)                                                 
         B     TPWD010                                                          
*                                                                               
TPWDNO   B     NO                                                               
TPWDOK   B     YES                                                              
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
* GET PASSWORD DETAILS                                                *         
***********************************************************************         
         USING SA0KEY,R2                                                        
GETPWD   NTR1  ,                                                                
         LA    R3,SA0DATA                                                       
GPWD010  CLI   0(R3),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R3),SAPALELQ                                                   
         BE    GPWD030                                                          
GPWD020  SR    RF,RF                                                            
         IC    RF,1(R3)                                                         
         AR    R3,RF                                                            
         B     GPWD010                                                          
*                                                                               
         USING SAPALD,R3                                                        
GPWD030  MVC   PIDSAVE,SAPALPID                                                 
         B     GPWDOK                                                           
*                                                                               
GPWDNO   B     NO                                                               
GPWDOK   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* UPDATE PERSON ID DETAILS                                            *         
***********************************************************************         
         USING SAPEKEY,R2                                                       
UPDPER   NTR1  ,                                                                
         LA    R3,SAPEDATA                                                      
UPER010  CLI   0(R3),0                                                          
         BE    UPER100                                                          
         CLI   0(R3),SAPWDELQ                                                   
         BE    UPER040                                                          
         CLI   0(R3),SAAGCELQ                                                   
         BE    UPER050                                                          
UPER020  SR    RF,RF                                                            
         IC    RF,1(R3)                                                         
         AR    R3,RF                                                            
         B     UPER010                                                          
*                                                                               
         USING SAPWDD,R3                                                        
UPER040  EQU   *                                                                
         SR    RF,RF                                                            
         ICM   RF,3,SAPWDNUM                                                    
         SH    RF,=H'1000'                                                      
         STCM  RF,3,SAPWDNUM                                                    
         B     UPER020                                                          
*                                                                               
         USING SAAGCD,R3                                                        
UPER050  EQU   *                                                                
         SR    RF,RF                                                            
         ICM   RF,3,SAAGCNUM                                                    
         LA    RF,1000(RF)                                                      
         STCM  RF,3,SAAGCNUM                                                    
         B     UPER020                                                          
*                                                                               
UPER100  B     UPEROK                                                           
*                                                                               
UPERNO   B     NO                                                               
UPEROK   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* UPDATE OFFICE CODE IN PERSON ID RECORD                              *         
***********************************************************************         
         USING SAPEKEY,R2                                                       
UPDPOF   NTR1  ,                                                                
         LA    R3,SAPEDATA                                                      
UPOF010  CLI   0(R3),0                                                          
         BE    UPOF100                                                          
         CLI   0(R3),SAPERELQ                                                   
         BE    UPOF030                                                          
UPOF020  SR    RF,RF                                                            
         IC    RF,1(R3)                                                         
         AR    R3,RF                                                            
         B     UPOF010                                                          
*                                                                               
         USING SAPERD,R3                                                        
UPOF030  EQU   *                                                                
*&&US                                                                           
         CLC   SAPEROFF,=CL2'A '                                                
         BNE   *+14                                                             
         MVC   SAPEROFF,=CL2'BA'                                                
         B     UPOF032                                                          
         CLC   SAPEROFF,=CL2'C '                                                
         BNE   *+14                                                             
         MVC   SAPEROFF,=CL2'BC'                                                
         B     UPOF032                                                          
         CLC   SAPEROFF,=CL2'W '                                                
         BNE   *+14                                                             
         MVC   SAPEROFF,=CL2'BW'                                                
         B     UPOF032                                                          
         CLC   SAPEROFF,=CL2'01'                                                
         BNE   *+14                                                             
         MVC   SAPEROFF,=CL2'B1'                                                
         B     UPOF032                                                          
         CLC   SAPEROFF,=CL2'02'                                                
         BNE   *+14                                                             
         MVC   SAPEROFF,=CL2'B2'                                                
         B     UPOF032                                                          
         CLC   SAPEROFF,=CL2'03'                                                
         BNE   *+14                                                             
         MVC   SAPEROFF,=CL2'B3'                                                
         B     UPOF032                                                          
         CLC   SAPEROFF,=CL2'04'                                                
         BNE   *+14                                                             
         MVC   SAPEROFF,=CL2'B4'                                                
         B     UPOF032                                                          
         CLC   SAPEROFF,=CL2'8 '                                                
         BNE   UPOF032                                                          
         MVC   SAPEROFF,=CL2'B8'                                                
         B     UPOF032                                                          
*&&                                                                             
*&&UK                                                                           
         CLC   SAPEROFF,=CL2'AA'                                                
         BNE   *+14                                                             
         MVC   SAPEROFF,=CL2'9A'                                                
         B     UPOF032                                                          
         CLC   SAPEROFF,=CL2'BB'                                                
         BNE   UPOF032                                                          
         MVC   SAPEROFF,=CL2'9B'                                                
         B     UPOF032                                                          
*&&                                                                             
UPOF032  EQU   *                                                                
         B     UPOF020                                                          
*                                                                               
UPOF100  B     UPOFOK                                                           
*                                                                               
UPOFNO   B     NO                                                               
UPOFOK   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* UPDATE GROUP CODE IN PERSON ID RECORD                               *         
***********************************************************************         
         USING SAPEKEY,R2                                                       
UPDPGR   NTR1  ,                                                                
         LA    R3,SAPEDATA                                                      
UPGR010  CLI   0(R3),0                                                          
         BE    UPGR100                                                          
         CLI   0(R3),SAAGCELQ                                                   
         BE    UPGR030                                                          
UPGR020  SR    RF,RF                                                            
         IC    RF,1(R3)                                                         
         AR    R3,RF                                                            
         B     UPGR010                                                          
*                                                                               
         USING SAAGCD,R3                                                        
UPGR030  EQU   *                                                                
*&&US                                                                           
         CLC   SAAGCCOD,=CL8'SECMGR  '                                          
         BNE   *+14                                                             
         MVC   SAAGCCOD,=CL8'BSECMGR '                                          
         B     UPGR032                                                          
*&&                                                                             
*&&UK                                                                           
         CLC   SAAGCCOD,=CL8'SECMGR  '                                          
         BNE   *+14                                                             
         MVC   SAAGCCOD,=CL8'BSECMGR '                                          
         B     UPGR032                                                          
         CLC   SAAGCCOD,=CL8'01      '                                          
         BNE   *+14                                                             
         MVC   SAAGCCOD,=CL8'01D3    '                                          
         B     UPGR032                                                          
*&&                                                                             
UPGR032  EQU   *                                                                
         B     UPGR020                                                          
*                                                                               
UPGR100  B     UPGROK                                                           
*                                                                               
UPGRNO   B     NO                                                               
UPGROK   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* UPDATE PASSWORD RECORD                                              *         
***********************************************************************         
         USING SA0KEY,R2                                                        
UPDPWD   NTR1  ,                                                                
         LA    R3,SA0DATA                                                       
UPWD010  CLI   0(R3),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R3),SAPASELQ                                                   
         BE    UPWD030                                                          
UPWD020  SR    RF,RF                                                            
         IC    RF,1(R3)                                                         
         AR    R3,RF                                                            
         B     UPWD010                                                          
*                                                                               
         USING SAPASD,R3                                                        
UPWD030  EQU   *                                                                
         CLI   SAPASLEN,X'04'                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         SR    RF,RF                                                            
         SR    RF,RF                                                            
         ICM   RF,3,SAPASDTA                                                    
         SH    RF,=H'1000'                                                      
         STCM  RF,3,SAPASDTA                                                    
         B     UPWDOK                                                           
*                                                                               
UPWDNO   B     NO                                                               
UPWDOK   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* UPDATE MANAGER IDS                                                  *         
***********************************************************************         
         USING SAOFKEY,R2                                                       
UPDMAN   NTR1  ,                                                                
         LA    R3,SAOFDATA                                                      
UMAN010  CLI   0(R3),0                                                          
         BE    UMANOK                                                           
         CLI   0(R3),SAMANELQ                                                   
         BE    UMAN030                                                          
UMAN020  SR    RF,RF                                                            
         IC    RF,1(R3)                                                         
         AR    R3,RF                                                            
         B     UMAN010                                                          
*                                                                               
         USING SAMAND,R3                                                        
UMAN030  EQU   *                                                                
         SR    RF,RF                                                            
         ICM   RF,3,SAMANID                                                     
         SH    RF,=H'1000'                                                      
         STCM  RF,3,SAMANID                                                     
         B     UMAN020                                                          
*                                                                               
UMANNO   B     NO                                                               
UMANOK   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* UPDATE ACCESS GROUP RECORD                                          *         
***********************************************************************         
         USING SAAGKEY,R2                                                       
UPDGRP   NTR1  ,                                                                
         LA    R3,SAAGDATA                                                      
UGRP010  CLI   0(R3),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R3),SAAGNELQ                                                   
         BE    UGRP030                                                          
UGRP020  SR    RF,RF                                                            
         IC    RF,1(R3)                                                         
         AR    R3,RF                                                            
         B     UGRP010                                                          
*                                                                               
         USING SAAGND,R3                                                        
UGRP030  EQU   *                                                                
         SR    RF,RF                                                            
         ICM   RF,3,SAAGNNUM                                                    
         LA    RF,1000(RF)                                                      
         STCM  RF,3,SAAGNNUM                                                    
         B     UGRPOK                                                           
*                                                                               
UGRPNO   B     NO                                                               
UGRPOK   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* CHECK PERSON ID TO BE DROPPED                                       *         
***********************************************************************         
         USING SAPEKEY,R2                                                       
CHECKPER NTR1  ,                                                                
         CLI   SAPETYP,SAPETYPQ                                                 
         BNE   CPEROK                                                           
         CLI   SAPESUB,SAPESUBQ                                                 
         BNE   CPEROK                                                           
         CLC   SAPEAGY,MRGAGY1                                                  
         BNE   CPEROK                                                           
         MVC   PIDSAVE,SAPEPID                                                  
         BAS   RE,TESTPID                                                       
         BE    CPEROK                                                           
         MVC   P,SPACES                                                         
         MVC   P(40),=CL40'PERSON ID DROPPED:'                                  
         MVC   P+32(2),MRGAGY1                                                  
         MVC   P+36(L'SAPEPID),PIDSAVE                                          
         GOTO1 VPRINTER                                                         
         B     CPERNO                                                           
*                                                                               
CPERNO   B     NO                                                               
CPEROK   B     YES                                                              
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
         DS    0D                                                               
IDCTAB   DS    0XL4                                                             
         DC    CL10'OMDBBDO',CL10'OBBDO'                                        
         DC    CL10'OMDBD',CL10'OBD'                                            
         DC    CL10'OMDBH',CL10'OBH'                                            
         DC    CL10'OMDBK',CL10'OBK'                                            
         DC    CL10'OMDBB',CL10'OBB'                                            
         DC    CL10'OMDBC',CL10'OBC'                                            
         DC    CL10'OMDBKOF',CL10'OBKOF'                                        
         DC    CL10'OMDBM',CL10'OBM'                                            
         DC    CL10'OMDBS',CL10'OBS'                                            
         DC    CL10'OMDBSMD',CL10'OBSMD'                                        
         DC    CL10'OMDBBA',CL10'OBBA'                                          
         DC    CL10'OMDBKK',CL10'OBKKD'                                         
         DC    CL10'OMDBKKP',CL10'OBKKPR'                                       
         DC    CL10'OMDBASA',CL10'OBAS'                                         
         DC    CL10'OMDBASE',CL10'OBASE'                                        
         DC    CL10'OMDBT',CL10'OBT'                                            
         DC    CL10'OMDDDB',CL10'ODDB'                                          
         DC    CL10'OMDDD',CL10'ODDBD'                                          
         DC    CL10'OMDDF',CL10'ODDBF'                                          
         DC    CL10'OMDDA',CL10'ODDBA'                                          
         DC    CL10'OMDDJA',CL10'OJARA'                                         
         DC    CL10'OMDALL',CL10'OMDG'                                          
         DC    CL10'OMDBAS',CL10'OMDGBAS'                                       
         DC    CL10'OMDLB',CL10'OMDGLB'                                         
         DC    CL10'OMDTB',CL10'OMDGTB'                                         
         DC    CL10'OMDENG',CL10'OMDGENG'                                       
         DC    CL10'OMDPLAK',CL10'OMDGPLAK'                                     
         DC    CL10'OMDTBWA',CL10'OTWBA'                                        
         DC    CL10'OMDGFMO',CL10'GFMOALL'                                      
         DC    CL10'OMDGHH',CL10'GFMOHH'                                        
         DC    CL10'OMDGFR',CL10'GFMOFR'                                        
         DC    CL10'OMDGPM',CL10'PMG'                                           
         DC    CL10'OMDGMS',CL10'GFMOMS'                                        
         DC    CL10'OMDGHP',CL10'GFMOHP'                                        
         DC    CL10'OMDGINT',CL10'GFMOINT'                                      
         DC    CL10'OMDGST',CL10'GFMOST'                                        
         DC    AL2(0),AL2(0)                                                    
         DS    0D                                                               
SYLTAB   DS    0XL4                                                             
         DC    CL8'OMJOPT',CL8'JOPT'                                            
         DC    CL8'OMDDB',CL8'OTGDDBD'                                          
         DC    CL8'OMTEAH',CL8'OTEAH'                                           
         DC    CL8'OMDDBG',CL8'DDBG'                                            
         DC    CL8'OMTEA',CL8'OTGTEA'                                           
         DC    CL8'OMCON',CL8'OMDCON'                                           
         DC    CL8'OMDALL',CL8'OMDG'                                            
         DC    CL8'OMTEAM',CL8'OTEAM'                                           
         DC    CL8'OMGFMO',CL8'GFMOA'                                           
         DC    AL2(0),AL2(0)                                                    
         EJECT                                                                  
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
*&&UK                                                                           
MRGAGY1  DC    CL2'RK'                                                          
SRCAGY1  DC    CL2'DZ'                                                          
*&&                                                                             
*&&US                                                                           
MRGAGY1  DC    CL2'BS'                                                          
SRCAGY1  DC    CL2'TH'                                                          
*&&                                                                             
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
SYSTEM   DS    XL1                                                              
PROGRAM  DS    XL1                                                              
MVSPARM  DS    XL1                                                              
ERROR    DS    XL1                 FLAG TO INDIATE INVALID INPUT                
UTYPEOFF DS    XL1                                                              
UKFLAG   DS    CL1                                                              
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
**PAN#1  DC    CL21'023SECONVCP1 05/22/02'                                      
         END                                                                    
