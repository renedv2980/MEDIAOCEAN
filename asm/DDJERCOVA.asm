*          DATA SET DDJERCOVA  AT LEVEL 002 AS OF 09/29/11                      
*PHASE JERCOVAA                                                                 
*INCLUDE ENDOFMOD                                                               
*INCLUDE PRINT                                                                  
*INCLUDE PRINTERL                                                               
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE HEXIN                                                                  
*INCLUDE GETFACT                                                                
*INCLUDE CARDS                                                                  
*INCLUDE NUMVAL                                                                 
*INCLUDE TIMBER                                                                 
*INCLUDE DATVAL                                                                 
*INCLUDE DATCON                                                                 
*INCLUDE PERVAL                                                                 
*INCLUDE ACRECTYP                                                               
*INCLUDE ADDAY                                                                  
*INCLUDE CALLOFF                                                                
*INCLUDE DICTATE                                                                
*INCLUDE DMDMGRL                                                                
*INCLUDE FAGETTXT                                                               
*INCLUDE GETDAY                                                                 
         TITLE 'JERCOV - OFFLINE JES JOB RECOVERY'                              
JERCOV   CSECT                                                                  
         ENTRY SSB                                                              
         ENTRY UTL                                                              
         PRINT NOGEN                                                            
         NBASE WORKX-WORKD,**JCOV**,RA,R9,R8,WORK=A(WORKC),CLEAR=YES,  X        
               RR=RE                                                            
         USING WORKD,RC            RC=A(GLOBAL W/S)                             
         ST    RE,RELO                                                          
         MVI   MVSPARM,0                                                        
         L     R1,0(R1)            GET MVS PARMS                                
         SR    RF,RF                                                            
         LH    RF,0(R1)                                                         
         LTR   RF,RF                                                            
         BZ    *+8                                                              
         MVI   MVSPARM,1                                                        
         L     R7,VCPRINT                                                       
         USING DPRINT,R7                                                        
         MVC   TITLE(20),=C'OFFLINE JOB RECOVERY'                               
         ICM   RE,15,=V(ENDOFMOD)  SET STXIT IF END OF MODULE CSECT             
         B     MAIN                ?? FOR ISPF DUMPS ??                         
         BZ    MAIN                                                             
         ST    RB,DUB                                                           
         ST    RE,DUB+4                                                         
         OI    DUB+4,X'80'                                                      
         GOTO1 =V(STXITER),DMCB,DUB                                             
         B     MAIN                                                             
         EJECT                                                                  
***********************************************************************         
* MAIN CONTROL CODE                                                   *         
***********************************************************************         
MAIN     EQU   *                                                                
         BAS   RE,GENINIT          GENERAL INITIALISATION                       
*                                                                               
         BAS   RE,VALCARDS         VALIDATE JCL CARD DATA LINE                  
         BNE   MERR                  EXIT IF ERROR                              
*                                                                               
         MVI   WORK+00,C'N'        OPEN CONTROL FILE                            
         MVC   WORK+01(7),CTFILE                                                
         MVI   WORK+08,C'X'                                                     
         GOTO1 VDATAMGR,DMCB,DMOPEN,CONTROL,WORK,IOL                            
*                                                                               
         BAS   RE,AGYSYS           SET UP AGENCY SYSTEM DATA                    
         BNE   MERR                  EXIT IF ERROR                              
*                                                                               
         BAS   RE,OPENFIL          OPEN SYSTEM FILES                            
         BNE   MERR                  EXIT IF ERROR                              
*                                                                               
         BAS   RE,SYSFLIST         BUILD SYSFLES LIST                           
         BNE   MERR                  EXIT IF ERROR                              
*                                                                               
         BAS   RE,PROCJOB          PROCESS JOB IN RECOVERY FILE                 
         BNE   MERR                  EXIT IF ERROR                              
*                                                                               
         BAS   RE,CLOSEFIL         CLOSE SYSTEM FILES                           
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
GENINIT  NTR1                                                                   
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         TIME                                                                   
         ST    R0,MVSTIME                                                       
         ST    R1,MVSDATE                                                       
         MVC   CENTURY,=CL2'19'                                                 
         GOTO1 VDATCON,DMCB,(5,0),(0,TODAY+2)                                   
         MVC   TODAY(2),CENTURY                                                 
         GOTO1 VDATCON,DMCB,(5,0),(2,TODAYC)                                    
         MVI   INTAPE,C'N'                                                      
         MVI   TDYONLY,C'N'                                                     
         MVI   WRTFLAG,C'Y'                                                     
         MVI   RETCODE,X'FF'                                                    
         MVI   SYSCHAR,C'0'                                                     
         MVI   HDRTYPE,C'L'                                                     
         L     RF,=A(RECBUFF)                                                   
         ST    RF,ARECBUFF                                                      
         L     RF,=A(XREC)                                                      
         ST    RF,AXREC                                                         
         L     RF,=A(TRKBUFF)                                                   
         ST    RF,ATRKBUFF                                                      
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* VALIDATE JCL CARD DATA LINES AS DEFINED IN CARDTBL                  *         
***********************************************************************         
VALCARDS NTR1                                                                   
VCLP1    GOTO1 VCARDS,DMCB,P,=C'RE00'                                           
         CLC   P(2),=C'/*'                                                      
         BE    VCEND               END OF INPUT PARAM CARDS                     
*                                                                               
VCLP1A   CLC   P(6),=C'DDSIO='     DDSIO=XXX... TO SET THE DDSIO                
         BNE   VCLP1B                                                           
         L     RF,=V(DDSIO)        OVERRIDE DATAMGR LOAD MODULE NAME            
         MVC   0(8,RF),P                                                        
         B     VCLP1                                                            
*                                                                               
VCLP1B   CLC   P(7),=C'DSPACE='    DSPACE=X TO SET THE DATA SPACE               
         BNE   VCLP1C                                                           
         L     RF,=A(SSB)                                                       
         MVC   SSODSPAC-SSOOFF(1,RF),P+7                                        
         B     VCLP1                                                            
*                                                                               
VCLP1C   LA    RE,CARDTBL          INITIALISE TABLE POINTER                     
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
         B     VCSYSTEM                                                         
         B     VCAGENCY                                                         
         B     VCINPUT                                                          
         B     VCWRITE                                                          
         B     VCPATCH                                                          
         B     VCDDSIO                                                          
         B     VCMAXREC                                                         
         B     VCJOBNAM                                                         
         B     VCJESID                                                          
*                                  EXIT/ERROR CONDITIONS                        
*                                  CHECK REQUIRED INPUT                         
VCEND    CLI   SYSTEM,0            CHECK SYSTEM ENTERED                         
         BE    VCERR2                                                           
         B     VCOK                                                             
*                                  CARD DATA ERROR CONDITIONS                   
VCERR1   GOTO1 VPRINTER            INVALID CARD                                 
         MVI   ERROR,1                                                          
         BAS   RE,ERRPRT                                                        
         B     VCNO                                                             
*                                                                               
VCERR2   GOTO1 VPRINTER            MISSING SYSTEM DEFINITION                    
         MVI   ERROR,2                                                          
         BAS   RE,ERRPRT                                                        
         B     VCNO                                                             
*                                                                               
VCERR3   GOTO1 VPRINTER            INVALID SYSTEM NAME                          
         MVI   ERROR,3                                                          
         BAS   RE,ERRPRT                                                        
         B     VCNO                                                             
*                                                                               
VCNO     B     NO                  EXIT ERROR CONDITION                         
*                                                                               
VCOK     B     YES                 EXIT OK                                      
         EJECT                                                                  
***********************************************************************         
* ROUTINES TO PROCESS EACH JCL CARD DATA LINE                         *         
***********************************************************************         
VCSYSTEM EQU    *                   SYSTEM=                                     
         LA    R4,P+7                                                           
         SR    RF,RF                                                            
*                                                                               
VCSY010  CLI   0(R4),C' '                                                       
         BE    VCSY012                                                          
         CLI   0(R4),C'='                                                       
         BE    VCSY012                                                          
         LA    RF,1(RF)                                                         
         LA    R4,1(R4)                                                         
         B     VCSY010                                                          
*                                                                               
VCSY012  LTR   RF,RF                                                            
         BZ    VCERR3                                                           
         BCTR  RF,0                                                             
         LA    R3,SYSLST                                                        
         USING SYSLSTD,R3                                                       
         LA    R3,6(R3)            R3=A(SYSTEM LIST)                            
*                                                                               
VCSY020  CLI   0(R3),0             TEST E-O-T                                   
         BE    VCERR3                                                           
         CLI   SYSLNUM,1           IGNORE SERVICE SYSTEM                        
         BE    VCSY022                                                          
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   SYSLNAME(0),P+7                                                  
         BE    VCSY030                                                          
VCSY022  LA    R3,SYSLLEN(R3)      NO - BUMP TO NEXT TABLE ENTRY                
         B     VCSY020                                                          
*                                                                               
VCSY030  MVC   SYSCODE,SYSLRPLT    RETURN SYSTEM CODE                           
         MVC   SYSTEM,SYSLNUM        AND SYSTEM NUMBER                          
*                                                                               
         CLI   0(R4),C' '                                                       
         BE    VCLP1                                                            
         CLI   0(R4),C'='                                                       
         BNE   VCERR1                                                           
*                                                                               
VCSY100  EQU   *                                                                
         LA    R4,1(R4)                                                         
         CLI   0(R4),C' '                                                       
         BE    VCERR1                                                           
*                                                                               
         CLI   0(R4),C'A'                                                       
         BL    VCERR1                                                           
         CLI   0(R4),C'Z'                                                       
         BNH   VCSY120                                                          
         CLI   0(R4),C'0'                                                       
         BL    VCERR1                                                           
         CLI   0(R4),C'9'                                                       
         BH    VCERR1                                                           
VCSY120  MVC   SYSCHAR,0(R4)                                                    
         B     VCLP1                                                            
         EJECT                                                                  
VCAGENCY EQU   *                   AGENCY=                                      
         MVC   AGYID,P+7                                                        
         B     VCLP1                                                            
                                                                                
VCINPUT  EQU   *                   INPUT=                                       
         CLC   P+6(4),=C'TAPE'                                                  
         BNE   VCERR1                                                           
         MVI   INTAPE,C'Y'                                                      
         B     VCLP1                                                            
                                                                                
VCWRITE  EQU   *                   WRITE=                                       
         MVC   WRTFLAG,P+6                                                      
         CLI   WRTFLAG,C'N'        INHIBIT ALL WRITES                           
         BE    VCLP1                                                            
         CLI   WRTFLAG,C'Y'        UPDATE OUTPUT FILE                           
         BE    VCLP1                                                            
         B     VCERR1                                                           
                                                                                
VCPATCH  EQU   *                   PATCH=111111 11                              
         XC    FULL,FULL           GET DISPLACEMENT INTO FULL                   
         GOTO1 VHEXIN,DMCB,P+6,FULL+1,6                                         
         CLC   DMCB+12(4),=F'3'                                                 
         BNE   VCERR1              MUST BE 6 HEX DIGITS                         
         LA    R2,P+71             FIND LENGTH OF PATCH DATA                    
         LH    RE,=H'-1'                                                        
         LA    RF,P+12                                                          
         CLI   0(R2),C' '                                                       
         BNE   *+12                                                             
         BXH   R2,RE,*-8                                                        
         B     VCERR1                                                           
         SR    R2,RF               L'PATCH DATA IN R2                           
         GOTO1 VHEXIN,DMCB,P+13,WORK,(R2)                                       
         ICM   R1,15,DMCB+12       GET L'HEX PATCH DATA IN R1                   
         BZ    VCERR1              ZERO IS NOT ALLOWED                          
         BCTR  R1,0                                                             
         L     RF,FULL             PATCH DISPLACEMENT IN RF                     
         AR    RF,RB               RF = A(AREA TO BE PATCHED)                   
         EX    R1,*+8              MOVE IN THE PATCH DATA                       
         B     VCLP1                                                            
         MVC   0(0,RF),WORK                                                     
                                                                                
VCDDSIO  EQU   *                   DDSIO=                                       
         L     RF,=V(DDSIO)                                                     
         LTR   RF,RF                                                            
         BZ    VCERR1                                                           
         MVC   0(8,RF),P+6                                                      
         B     VCLP1                                                            
                                                                                
VCMAXREC EQU   *                   MAXRECS=                                     
         GOTO1 VNUMVAL,DMCB,P+8,(X'02',0)                                       
         CLI   0(R1),0                                                          
         BNE   VCERR1                                                           
         L     R1,4(R1)                                                         
         C     R1,MAXRECS                                                       
         BH    VCERR1                                                           
         C     R1,=F'1'                                                         
         BL    VCERR1                                                           
         ST    R1,MAXRECS                                                       
         B     VCLP1                                                            
                                                                                
VCJOBNAM EQU  *                   JOBNAME=                                      
         MVC   JOBNAME,P+8                                                      
         B     VCLP1                                                            
                                                                                
VCJESID  EQU  *                   JESID=                                        
         MVC   JESID,P+6                                                        
         B     VCLP1                                                            
*                                                                               
PACKIN   EQU   *                                                                
         SR    R1,R1               GET STRING LENGTH                            
         LA    R0,8                                                             
*                                                                               
PIN010   CLI   0(RF),C' '                                                       
         BE    PINOK                                                            
         CLI   0(RF),C'0'                                                       
         BL    PINNO                                                            
         CLI   0(RF),C'9'                                                       
         BH    PINNO                                                            
         LA    RF,1(RF)                                                         
         LA    R1,1(R1)                                                         
         BCT   R0,PIN010                                                        
         B     PINNO                                                            
*                                                                               
PINNO    SR    R1,R1                                                            
PINOK    LTR   R1,R1                                                            
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* OPEN SYSTEM FILES                                                   *         
***********************************************************************         
OPENFIL  NTR1                                                                   
         MVC   P(13),=C'OPEN SYSTEM: '                                          
         MVC   P+15(7),DMSYS                                                    
         GOTO1 VPRINTER                                                         
*                                                                               
         CLI   INTAPE,C'Y'                                                      
         BNE   OFIL010                                                          
         OPEN  (RCVTAPE,(INPUT))                                                
         B     OFIL020                                                          
*                                                                               
OFIL010  EQU   *                                                                
* ??     GOTO1 VDMOD000,DMODDMCB,A(OPENSYS),AGYSEN                              
*                                                                               
OFIL020  CLI   WRTFLAG,C'Y'                                                     
         BNE   OFILOK                                                           
         OPEN  (EXFILE,OUTPUT)                                                  
         B     OFILOK                                                           
*                                                                               
OFILNO   B     NO                                                               
OFILOK   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* CLOSE SYSTEM FILES                                                  *         
***********************************************************************         
CLOSEFIL NTR1                                                                   
         MVC   P(13),=C'CLOSE FILES  '                                          
         GOTO1 VPRINTER                                                         
*                                                                               
         CLI   INTAPE,C'Y'                                                      
         BNE   CFIL010                                                          
         CLOSE (RCVTAPE)                                                        
         B     CFIL020                                                          
*                                                                               
CFIL010  EQU   *                                                                
* ??     GOTO1 VDMOD000,DMODDMCB,A(CLSESYS),AGYSEN                              
*                                                                               
CFIL020  CLI   WRTFLAG,C'Y'                                                     
         BNE   CFIL030                                                          
         CLOSE (EXFILE)                                                         
         B     CFILOK                                                           
*                                                                               
CFIL030  EQU   *                                                                
         B     CFILOK                                                           
*                                                                               
CFIL040  B     CFILOK                                                           
*                                                                               
CFILNO   B     NO                                                               
CFILOK   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* BUILD SYSFLES LIST                                                  *         
***********************************************************************         
SYSFLIST NTR1                                                                   
         MVC   P(13),=C'BUILD SYSFLES'                                          
         GOTO1 VPRINTER                                                         
*                                                                               
         XC    DMCB+8(4),DMCB+8                                                 
         ZIC   RF,AGYSEN                                                        
         GOTO1 VDATAMGR,DMCB,DMREAD,SYSFLES,(RF)                                
         L     R4,12(R1)           THIS IS A(SYSFLES) ON RETURN                 
         ST    R4,SVSYSFL                                                       
         MVC   SVSYSFLH,0(R4)      SAVE SYSFLES HEADER                          
         MVC   SVSYSFL,AGYSEN      SAVE SYSTEM NUMBER                           
         IC    R0,3(R4)            GET NUMBER OF FILES                          
         LA    R4,4(R4)            POINT TO FIRST FILE                          
*                                                                               
SLIS010  TM    0(R4),X'40'         IS IT THE RECOVERY FILE                      
         BNZ   SLIS020             YES                                          
         LA    R4,8(R4)                                                         
         BCT   R0,SLIS010                                                       
         B     SLISNO              EXIT IF NO RECOVERY FILE                     
*                                                                               
SLIS020  TM    SVSYSFLH+1,X'01'    TEST IF THIS SYSTEM PROT VIA ENQ/DEQ         
         BZ    SLIS030                                                          
         CLI   SVSYSFLH+0,X'0A'    TEST IF THIS IS CONTROL SYSTEM               
         BNE   SLIS030                                                          
         B     SLIS030             ???                                          
         MVC   ENQID,=C'CTRL'                                                   
         MVC   ENQCMND,=C'ENQCTL  '                                             
         GOTO1 VDATAMGR,DMCB,(0,ENQCMND),(C'T',ENQID)                           
         TM    8(R4),X'01'                                                      
         BO    SLIS030             SYSTEM IS ALREADY ENQUEUED                   
         GOTO1 VDATAMGR,DMCB,(0,ENQCMND),(C'E',ENQID)                           
         MVI   ENQFLG,C'E'         SET ENQUEUED SYSTEM                          
*                                                                               
*                                  READ LAST RECORD ON RECOVERY FILE            
*                                                                               
SLIS030  L     R3,ARECBUFF                                                      
         L     R5,4(R4)            R5=A(DTF)                                    
         ST    R5,SVRECDTF                                                      
         USING DTFPHD,R5                                                        
         TM    DTFOPEN,X'80'       EXIT IF READ ONLY FILE                       
         BO    SLISNO                                                           
         CLC   DNEXT,=X'00010000'  TEST NO RECS ON FILE                         
         BE    SLISNO                                                           
         MVC   RECDA,DNEXT         SET LAST REC ADDRESS                         
         MVI   RECDA+3,0                                                        
         XC    DMODDMCB+8(4),DMODDMCB+8                                         
* ??     GOTO1 VDMOD000,DMODDMCB,A(READ),(R3),0,(R5),RECDA                      
         OC    8(2,R1),8(R1)                                                    
         BNZ   SLISERR1            DISK READ ERROR RCVR FILE                    
         MVC   SVBLKSZ,DBLKSZ                                                   
         OC    SVBLKSZ,SVBLKSZ                                                  
         BZ    SLISOK                                                           
         CLC   RECDA(3),0(R3)      CHECK DA IN BLOCK AGREES WITH DNEXT          
         BNE   SLISERR1            AND GET HIGH RECORD NUM IN BLOCK             
         MVC   RECDA+3(1),3(R3)                                                 
         B     SLISOK                                                           
*                                                                               
SLISERR1 MVI   ERROR,4                                                          
         BAS   RE,ERRPRT                                                        
         B     SLISNO                                                           
*                                                                               
SLISNO   B     NO                                                               
SLISOK   B     YES                                                              
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* PROCESS JOB IN RECOVERY FILE                                        *         
***********************************************************************         
PROCJOB  NTR1                                                                   
         MVC   P(24),=C'PROCESS JOB             '                               
         GOTO1 VPRINTER                                                         
         LA    R6,IOL                                                           
         USING RECDS,R6                                                         
*                                                                               
PJOB010  BAS   RE,SEQRCV                                                        
         BNE   PJOBNO                                                           
*                                                                               
         TM    RRECTY,X'80'        IGNORE POINTER COPIES/CHANGES                
         BO    PJOB010                                                          
         CLI   RRECTY,X'01'        IGNORE COPIES                                
         BE    PJOB020                                                          
         CLI   RSIN,X'FF'          START-OF-JOB RECORDS ARE DELETED             
         BNE   PJOB010                                                          
         CLC   0(8,R3),STRJOB                                                   
         BNE   PJOB010                                                          
         CLC   8(8,R3),JOBNAME                                                  
         BNE   PJOB010                                                          
*                                                                               
PJOB020  BAS   RE,SEQRCV                                                        
         BNE   PJOBNO                                                           
         TM    RRECTY,X'80'        IGNORE POINTER COPIES/CHANGES                
         BO    PJOB020                                                          
         CLI   RSIN,X'FF'          END-OF-JOB RECORDS ARE DELETED               
         BNE   PJOB030                                                          
         CLC   0(8,R3),ENDJOB                                                   
         BNE   PJOB020                                                          
         CLC   8(8,R3),JOBNAME                                                  
         BE    PJOBOK                                                           
         DC    H'00'                                                            
*                                                                               
PJOB030  EQU   *                                                                
         BAS   RE,PROCESS                                                       
         B     *+4(RF)                                                          
         B     PJOB100                                                          
         B     PJOB200                                                          
         B     PJOBERR1                                                         
         B     PJOBERR2                                                         
*                                                                               
PJOB100  EQU   *                                                                
         B     PJOB020                                                          
*                                                                               
PJOB200  EQU   *                                                                
         B     PJOB020                                                          
*                                                                               
PJOBERR1 EQU   *                                                                
         B     PJOBNO                                                           
*                                                                               
PJOBERR2 EQU   *                                                                
         B     PJOBNO                                                           
*                                                                               
PJOBNO   B     NO                                                               
PJOBOK   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* PROCESS RECOVERY FILE RECORD                                        *         
***********************************************************************         
PROCESS  NTR1                                                                   
         MVC   TRECTY,RRECTY       DON'T CHANGE IN RECORD                       
         NI    TRECTY,X'7F'        TURN OFF 'POINTER' BIT                       
         CLI   TRECTY,CHANGE                                                    
         BE    RCDELETE            DELETE CHANGES FROM RECOVERY FILE            
*                                                                               
         L     R4,SVSYSFL          SEARCH FOR FILE IN SYSFLES LIST              
         SR    RF,RF                                                            
         IC    RF,3(R4)                                                         
         LA    R4,4(R4)                                                         
RC12A    CLC   RFILTY,3(R4)        MATCH FILE NUMBER                            
         BE    RC12B                                                            
         LA    R4,8(R4)                                                         
         BCT   RF,RC12A                                                         
         B     RCDELETE            ERASE INVALID RCVR REC                       
RC12B    L     RE,4(R4)            GET DTF ADDRESS                              
         L     RF,VDMOD000         SET V(DMCNTL)                                
         LA    R2,DMCB             SET A(DMOD000 DMCB PARAMS)                   
         XC    DMCB(24),DMCB                                                    
         TM    0(R4),X'01'         TEST FILE TYPE                               
         BO    RC16                IS FILE                                      
         B     RC14                DA FILE                                      
                                                                                
***********************************************************************         
* DIRECT ACCESS FILE                                                  *         
***********************************************************************         
RC14     L     R5,ARECBUFF         READ DA FILE RECORD INTO INPUT AREA          
* ??     GOTO1 VDMOD000,DMODDMCB,A(READ),(R5),,4(R4),RVCHR                      
         CLI   DMCB+8,0                                                         
         BE    RC14A                                                            
         TM    DMCB+8,X'90'                                                     
         BZ    RCERR3              DISK READ ERROR DA FILE                      
         CLI   TRECTY,ADD                                                       
         BE    RCDELETE            EOF AND NOTFOUND OK FOR ADD                  
         B     RCERR3                                                           
*                                                                               
RC14A    CLI   TRECTY,COPY         OVERWRITE WITH RECOVERY COPY RECORD          
         BNE   RC14B                                                            
         LA    R0,RECVHDR+L'RECVHDR                                             
* ??     GOTO1 VDMOD000,DMODDMCB,A(WRITE),(R0),,,,                              
         B     RCDELETE                                                         
*                                                                               
RC14B    CLI   TRECTY,ADD          LOGICALLY DELETE DA FILE ADDED REC           
         BNE   RC14C                                                            
         BAS   RE,LDELETE                                                       
* ??     GOTO1 VDMOD000,DMODDMCB,A(WRITE),,,,,                                  
         B     RCDELETE                                                         
*                                                                               
RC14C    B     RCDELETE            ERASE INVALID RCVR REC                       
                                                                                
***********************************************************************         
* INDEX SEQUENTIAL FILE                                               *         
***********************************************************************         
RC16     EQU   *                   READ IS FILE RECORD INTO INPUT AREA          
         LA    RE,RCVREC           KEY IS IN RECOVERY RECORD                    
         ST    RE,DMCB+8                                                        
         L     R5,ARECBUFF         READ DA FILE RECORD INTO INPUT AREA          
* ??     GOTO1 VDMOD000,DMODDMCB,A(RKEY),(R5),,4(R4),RCVREC                     
         CLI   DMCB+8,0                                                         
         BNE   RCERR3              DISK READ ERROR IS FILE                      
         L     RE,DMODDMCB+12                                                   
         LH    RE,46(RE)           GET KEYLEN-1                                 
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R5),RCVREC     COMPARE KEYS                                  
         BE    RC16A                                                            
         CLI   TRECTY,ADD                                                       
         BE    RCDELETE            NOTFOUND OK FOR ADD                          
         B     RCERR3                                                           
*                                                                               
RC16A    CLI   TRECTY,COPY         OVERWRITE WITH RECOVERY COPY RECORD          
         BNE   RC16B                                                            
* ??     GOTO1 VDMOD000,DMODDMCB,A(WKEY),RCVREC,,,,                             
         B     RCDELETE                                                         
*                                                                               
RC16B    CLI   TRECTY,ADD          ERASE IS FILE ADDED RECORD                   
         BNE   RC16C                                                            
* ??     GOTO1 VDMOD000,DMODDMCB,A(EKEY),,,,,                                   
         B     RCDELETE                                                         
*                                                                               
RC16C    B     RCDELETE            ERASE INVALID RCVR REC                       
                                                                                
***********************************************************************         
* SEARCH FILE TABLE TO FIND LOGICAL DELETE FIELD AND SET TO FF'S      *         
***********************************************************************         
LDELETE  EQU   *                                                                
* ??     L     R3,VDMGRFLS         POINT TO DMGRFLES TABLE                      
         LH    R4,0(R3)                                                         
         L     R5,2(R3)                                                         
         LA    R3,6(R3)                                                         
*                                                                               
LDEL2    CLI   0(R3),0             SEARCH FILE TABLE FOR FILE                   
         BE    *+14                                                             
         CLC   RFILTY,8(R3)                                                     
         BE    LDEL4                                                            
         BXLE  R3,R4,LDEL2                                                      
         B     LDEL6                                                            
*                                                                               
LDEL4    SR    R5,R5                                                            
         IC    R5,9(R3)            R5=L'LOGICAL DELETE FIELD                    
         LTR   R5,R5                                                            
         BZ    LDEL6                                                            
         BCTR  R5,0                                                             
         SR    R4,R4                                                            
         IC    R4,10(R3)                                                        
         LA    R4,0(R5,R4)         R4=A(LOGICAL DELETE FIELD)                   
         EX    R5,*+8                                                           
         B     LDELX                                                            
         MVC   0(0,R4),=8X'FF'     SET FIELD TO ALL FF'S                        
*                                                                               
LDEL6    B     RCDELETE            ERASE INVALID RCVR REC                       
*                                                                               
LDELX    BR    RE                                                               
                                                                                
***********************************************************************         
* EXIT ROUTINE WITH RETURN CODE IN RF                                 *         
***********************************************************************         
RCDELETE SR    RF,RF               DELETE THIS RECOVERY RECORD                  
         B     RCXX                                                             
RCIGNORE LA    RF,4                IGNORE THIS RECOVERY RECORD                  
         B     RCXX                                                             
RCERR6   LA    RF,8                ERROR 6 - SIN HAS CHANGED                    
         B     RCXX                                                             
RCERR3   LA    RF,12               ERROR 3 - DISK READ ERROR                    
         B     RCXX                                                             
RCXX     XIT1  REGS=(RF)                                                        
         EJECT                                                                  
***********************************************************************         
* READ SEQUENTIAL RECORD FROM RECOVERY FILE                           *         
***********************************************************************         
SEQRCV   NTR1                                                                   
         CLI   INTAPE,C'Y'                                                      
         BNE   SRCV010                                                          
         LR    R0,R6                                                            
         GET   RCVTAPE,(0)                                                      
         B     SRCVOK                                                           
*                                                                               
EODADRCV B     SRCVNO                                                           
*                                                                               
SRCV010  L     R0,ARECBUFF                                                      
         GOTO1 VDATAMGR,DMCB,(X'28',DMRSEQ),DMRFIL,DMDA,(R0),ATRKBUFF           
         TM    8(R1),X'80'         TEST E-O-F POSTED                            
         BO    SRCVNO                                                           
         CLI   8(R1),0             TEST ERROR POSTED                            
         BE    SRCV020                                                          
*                                                                               
         MVC   P(28),=C'DISK ERROR ON RECOVERY FILE='                           
         MVC   P+28(8),DMRFIL                                                   
         GOTO1 VPRINTER                                                         
         MVC   P(17),=C'DA=XXXXXXXX,DMCB='                                      
         GOTO1 VHEXOUT,DMCB,DMDA,P+34,,=C'TOG'                                  
         GOTO1 (RF),(R1),DMCB,P+17,20,=C'TOG'                                   
         GOTO1 VPRINTER                                                         
         MVC   P(14),=C'RUN CONTINUING'                                         
         BASR  RE,RF                                                            
         BASR  RE,RF                                                            
         LH    R3,DMDA             BUMP TO NEXT TRACK                           
         LA    R3,1(R3)                                                         
         STH   R3,DMDA                                                          
         MVI   DMDA+2,0            SET RECNUM TO ZERO                           
         B     SRCVNO                                                           
*                                  RECOVERY FILE RECORD FOUND OK                
SRCV020  LH    R1,DMCB+18          GET RECORD LENGTH                            
         LA    R1,4(R1)                                                         
         XC    RECLN(4),RECLN                                                   
         STH   R1,RECLN                                                         
         LA    RF,RECVHDR                                                       
         SH    R1,=H'4'                                                         
         LR    RE,R0                                                            
         MOVE  ((RF),(R1)),(RE)    MOVE RECORD TO TAPE BUFFER                   
         B     SRCVOK                                                           
*                                                                               
SRCVNO   B     NO                                                               
SRCVOK   B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* SET AGENCY SYSTEM DATA                                              *         
***********************************************************************         
AGYSYS   NTR1                                                                   
         OC    AGYID,AGYID                                                      
         BZ    ASYS010                                                          
         BAS   RE,GETACC           GET SYSTEM INFO FROM ACCESS RECORD           
         B     ASYS030                                                          
*                                  ELSE FROM INPUT CONTROL CARDS                
ASYS010  LA    RE,SNUMLIST         LOOK-UP LETTER IN LIST                       
ASYS020  CLI   0(RE),0             TEST E-O-L                                   
         BE    ASYSNO                                                           
         CLC   SYSCHAR,0(RE)       MATCH INPUT TO TABLE                         
         BE    *+12                                                             
         LA    RE,1(RE)                                                         
         B     ASYS020                                                          
         LA    RF,SNUMLIST                                                      
         SR    RE,RF               RE=SYSTEM FILE SET NUMBER                    
         STC   RE,SNUMB                                                         
*                                                                               
* READ & PROCESS SYSTEM LIST RECORD                                             
*                                                                               
ASYS030  LA    R3,IOL                                                           
         USING CTWREC,R3           R3=A(RECORD)                                 
         XC    CTWKEY,CTWKEY                                                    
         MVI   CTWKTYP,C'W'                                                     
         MVI   CTWKREC,C'S'                                                     
         GOTO1 VDATAMGR,DMCB,DMREAD,CTFILE,CTWREC,CTWREC                        
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                DIE IF CANT READ RECORD                      
*                                                                               
         LA    R3,CTWDATA          FIND SYSTEM ELEMENT ON RECORD                
         USING SYSELD,R3           R3=A(FIRST ELEMENT)                          
         SR    R0,R0                                                            
ASYS032  CLI   SYSEL,0             TEST E-O-R                                   
         BE    ASYSNO                                                           
         CLI   SYSEL,SYSELQ                                                     
         BNE   ASYS036                                                          
         CLC   SYSSYS,SYSTEM       MATCH SYSTEM NUMBER                          
         BNE   ASYS036                                                          
         OC    AGYSEN,AGYSEN       TEST IF SYSTEM SE# EXTRACTED                 
         BZ    ASYS034                                                          
         CLC   SYSSEN,AGYSEN       MATCH SYSTEM SE NUMBER                       
         BE    ASYS040                                                          
         B     ASYS036                                                          
ASYS034  CLC   SYSNUM,SNUMB        ELSE MATCH SYSTEM RELATIVE NUMBER            
         BE    ASYS050                                                          
ASYS036  IC    R0,SYSLEN                                                        
         AR    R3,R0                                                            
         B     ASYS032                                                          
*                                                                               
ASYS040  LA    RE,SNUMLIST         GET SUB SYSTEM CHARACTER                     
         ZIC   R1,SYSNUM                                                        
         STC   R1,SNUMB                                                         
ASYS042  CLI   0(RE),0             TEST E-O-L                                   
         BE    ASYSNO                                                           
         LA    RE,1(RE)                                                         
         BCT   R1,ASYS042                                                       
         MVC   SYSCHAR,0(RE)                                                    
         B     ASYS060                                                          
*                                                                               
ASYS050  MVC   AGYSEN,SYSSEN       GET SYSTEM SE NUMBER                         
*                                                                               
ASYS060  LA    RE,DMFTAB           GET DATA MANGER INFO FROM TABLE              
ASYS062  CLI   0(RE),0                                                          
         BNE   *+6                                                              
         DC    H'00'                                                            
         CLC   0(1,RE),SYSTEM                                                   
         BE    ASYS070                                                          
ASYS064  LA    RE,1(RE)                                                         
         CLI   0(RE),0                                                          
         BNE   ASYS064                                                          
         LA    RE,1(RE)                                                         
         B     ASYS062                                                          
*                                                                               
ASYS070  MVC   DMSYS,1(RE)                                                      
         MVC   DMFLIST,SPACES                                                   
         LA    RF,DMFLIST                                                       
         LA    RE,9(RE)                                                         
ASYS072  CLI   0(RE),0                                                          
         BE    ASYS080                                                          
         MVC   0(1,RF),0(RE)                                                    
         LA    RF,1(RF)                                                         
         LA    RE,1(RE)                                                         
         B     ASYS072                                                          
*                                                                               
ASYS080  L     RE,=A(UTL)          SET SENUM IN UTL                             
         MVC   4(1,RE),AGYSEN                                                   
         B     ASYSOK                                                           
*                                                                               
ASYSNO   B     NO                                                               
ASYSOK   B     YES                                                              
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* GET AGENCY ACCESS RECORD DATA FROM CONTROL FILE                     *         
***********************************************************************         
GETACC   NTR1                                                                   
         LA    R3,IOL                                                           
         USING CT5REC,R3           R3=A(RECORD)                                 
         XC    CT5KEY,CT5KEY                                                    
         MVI   CT5KTYP,CT5KTYPQ                                                 
         MVC   CT5KALPH,AGYID                                                   
         GOTO1 VDATAMGR,DMCB,DMREAD,CTFILE,CT5REC,CT5REC                        
         CLI   8(R1),0                                                          
         BE    GACC010                                                          
         CLI   8(R1),X'10'                                                      
         BE    GACCNO                                                           
         DC    H'0'                DIE IF IO ERROR                              
*                                                                               
GACC010  LA    R3,CT5DATA          FIND SYSTEM ELEMENT ON RECORD                
         USING CTSYSD,R3           R3=A(FIRST ELEMENT)                          
         SR    R0,R0                                                            
GACC020  CLI   CTSYSEL,0           TEST E-O-R                                   
         BE    GACCNO                                                           
         CLI   CTSYSEL,CTSYSELQ                                                 
         BNE   GACC030                                                          
         CLC   CTSYSNUM,SYSTEM     MATCH SYSTEM NUMBER                          
         BNE   GACC030                                                          
         MVC   AGYSEN,CTSYSSE                                                   
         MVC   MEDAGY,CTSYSAGB                                                  
         NI    MEDAGY,X'F0'                                                     
         B     GACCOK                                                           
GACC030  IC    R0,CTSYSLEN                                                      
         AR    R3,R0                                                            
         B     GACC020                                                          
*                                                                               
GACCNO   B     NO                                                               
GACCOK   B     YES                                                              
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* OPEN MEDZ SYSTEM                                                    *         
***********************************************************************         
OPENMEDZ NTR1                                                                   
         L     R4,=A(UTL)                                                       
         MVC   BYTE,4(R4)                                                       
         MVI   4(R4),X'14'                                                      
         GOTO1 VDATAMGR,DMCB,DMOPEN,=C'MED',=C'NMEDDIR NMEDFIL X',IOL           
         MVC   4(1,R4),BYTE                                                     
         B     YES                                                              
                                                                                
***********************************************************************         
* DECREMENT MAXIMUM IO COUNT                                          *         
***********************************************************************         
DECIOC   NTR1                                                                   
         L     RF,MAXIOS                                                        
         BCTR  RF,0                                                             
         ST    RF,MAXIOS                                                        
         LTR   RF,RF                                                            
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
                                                                                
***********************************************************************         
* PRINT ERROR MESSAGE                                                 *         
***********************************************************************         
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
                                                                                
*                                                                               
YES      SR    RC,RC               RETURN CC EQUAL                              
NO       LTR   RC,RC               RETURN CC NOT EQUAL                          
         XIT1                                                                   
         EJECT                                                                  
MXTRTQ   EQU   X'5E'               FIELD SEPERATOR CHR                          
ADD      EQU   X'03'               RECOVERY RECORD ADD CODE                     
CHANGE   EQU   X'02'               RECOVERY RECORD CHANGE CODE                  
COPY     EQU   X'01'               RECOVERY RECORD COPY CODE                    
         LTORG                                                                  
         EJECT                                                                  
                                                                                
SNUMLIST DC    C' 123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ',X'00'                    
                                                                                
STRJOB   DC    CL8'**SJOB**'                                                    
ENDJOB   DC    CL8'**EJOB**'                                                    
                                                                                
* CARDTBL DEFINES JOB CONTROL INPUT PARAMETER CARDS                             
* AL1    LENGTH OF CARD NAME                                                    
* AL1    ACTION ROUTINE NUMBER                                                  
* XL1    ACTION FLAGS                                                           
* CL8    CARD NAME                                                              
*                                                                               
CARDTBL  DS    0CL14                                                            
         DC    AL1(07,01),X'00',CL11'SYSTEM='                                   
         DC    AL1(07,02),X'00',CL11'AGENCY='                                   
         DC    AL1(06,03),X'00',CL11'INPUT='                                    
         DC    AL1(06,04),X'00',CL11'WRITE='                                    
         DC    AL1(06,05),X'00',CL11'PATCH='                                    
         DC    AL1(06,06),X'00',CL11'DDSIO='                                    
         DC    AL1(08,07),X'00',CL11'MAXRECS='                                  
         DC    AL1(08,08),X'00',CL11'JOBNAME='                                  
         DC    AL1(06,09),X'00',CL11'JESID='                                    
CARDTBLX DC    AL1(00)                                                          
CLENGTH  EQU   0                                                                
CROUTINE EQU   1                                                                
CFLAG    EQU   2                                                                
CSTRING  EQU   3                                                                
                                                                                
* DMFILE TABLE                                                                  
*                                                                               
DMFTAB   DC    X'04',CL8'MEDIA  ',C'NMEDDIR NMEDFIL NMEDRCV X',X'00'            
         DC    X'05',CL8'MPL    ',C'NMPLRCV X',X'00'                            
         DC    X'06',CL8'ACCOUNT',C'NACCRCV X',X'00'                            
         DC    X'0A',CL8'CONTROL',C'NCTRCVR X',X'00'                            
DMFTABX  DC    X'00'                                                            
UTYPENUM EQU   7                                                                
                                                                                
         EJECT                                                                  
* FASYSLST                                                                      
         PRINT OFF                                                              
       ++INCLUDE FASYSLST                                                       
         PRINT ON                                                               
                                                                                
         EJECT                                                                  
                                                                                
COMFACS  DS    0A                                                               
VDATAMGR DC    V(DATAMGR)                                                       
VDMOD000 DC    V(DMOD000)                                                       
VCPRINT  DC    V(CPRINT)                                                        
VPRINT   DC    V(PRINT)                                                         
VPRINTER DC    V(PRINTER)                                                       
VHELLO   DC    V(HELLO)                                                         
VHEXOUT  DC    V(HEXOUT)                                                        
VHEXIN   DC    V(HEXIN)                                                         
VGETFACT DC    V(GETFACT)                                                       
VCARDS   DC    V(CARDS)                                                         
VNUMVAL  DC    V(NUMVAL)                                                        
VTIMBER  DC    V(TIMBER)                                                        
VDATVAL  DC    V(DATVAL)                                                        
VDATCON  DC    V(DATCON)                                                        
VPERVAL  DC    V(PERVAL)                                                        
VRECTYP  DC    V(ACRECTYP)                                                      
VDYNALOC DC    V(DYNALLOC)                                                      
*                                                                               
EXDSNAME DC    CL30'XTR.TEST1                     '                             
EXDDNAME DC    CL8'EXFILE  '                                                    
DMOPEN   DC    C'OPEN   '                                                       
DMREAD   DC    C'DMREAD '                                                       
DMRSEQ   DC    C'DMRSEQ '                                                       
DMRDHI   DC    C'DMRDHI '                                                       
DMCLSE   DC    C'DMCLSE '                                                       
DMFAST   DC    C'DMFAST '                                                       
SYSFLES  DC    C'SYSFLES'                                                       
GETREC   DC    C'GETREC '                                                       
DMRFIL   DC    C'RECOVER'                                                       
CONTROL  DC    C'CONTROL'                                                       
CTFILE   DC    C'CTFILE '                                                       
MEDFILE  DC    C'MEDFIL '                                                       
MEDDIR   DC    C'MEDDIR '                                                       
DMDA     DC    F'0'                                                             
MAXRECS  DC    F'99999'                                                         
MAXIOS   DC    F'0'                                                             
ACTIVITY DC    CL1'Y'                                                           
APALLOC  DC    XL3'0000C8'                                                      
ASALLOC  DC    XL3'000032'                                                      
*                                                                               
ERRTAB   DS    0H                  ERROR REPORT STRINGS                         
ERRMSG0  DC    CL40'DATASET NOT FOUND IN PAN LIBRARY'                           
ERRMSG1  DC    CL40'INVALID CONTROL CARD INPUT'                                 
ERRMSG2  DC    CL40'MISSING SYSTEM DEFINITION'                                  
ERRMSG3  DC    CL40'INVALID SYSTEM NAME      '                                  
ERRMSG4  DC    CL40'ERROR 4                  '                                  
ERRMSG5  DC    CL40'ERROR 5                  '                                  
*                                                                               
* TABLE OF VALID MODES INPUT VIA MODE=CARD                                      
*                                                                               
MODETAB  DS    0CL9                                                             
         DC    C'UPDATE  ',AL1(UPDTQ)                                           
         DC    C'LOAD    ',AL1(LOADQ)                                           
         DC    C'SCAN    ',AL1(SCANQ)                                           
         DC    C'PRTERR  ',AL1(PRTERRQ)                                         
*                                                                               
         DC    C'UPDTPRT ',AL1(UPDTQ+PRTERRQ)                                   
         DC    C'SCANPRT ',AL1(SCANQ+PRTERRQ)                                   
         DC    X'FF'                                                            
                                                                                
*                                                                               
UPDTQ    EQU   X'01'                                                            
LOADQ    EQU   X'02'                                                            
SCANQ    EQU   X'04'                                                            
PRTERRQ  EQU   X'08'                                                            
*                                                                               
RCVTAPE  DCB   DDNAME=RCVTAPE,DSORG=PS,RECFM=VB,LRECL=8200,            *        
               BLKSIZE=0,MACRF=(GM),EODAD=EODADRCV                              
*                                                                               
EXFILE   DCB   DDNAME=EXFILE,DSORG=PS,RECFM=VB,MACRF=(PM),             *        
               LRECL=2048,BLKSIZE=8120                                          
         DS    0D                                                               
SSB      DC    X'0000FF',X'40',4X'00',CL8' ',32X'00',A(0),204X'00'              
UTL      DC    F'0',X'0A',XL3'00',XL56'00'                                      
         EJECT                                                                  
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
                                                                                
* DDSYSELD                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDSYSELD                                                       
         PRINT ON                                                               
                                                                                
* FASYSLSTD                                                                     
         PRINT OFF                                                              
       ++INCLUDE FASYSLSTD                                                      
         PRINT ON                                                               
                                                                                
* FAFACTS                                                                       
         PRINT OFF                                                              
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
                                                                                
* DDDPRINTL                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDDPRINTL                                                      
         PRINT ON                                                               
                                                                                
* DDPERVALD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDPERVALD                                                      
         PRINT ON                                                               
                                                                                
* DMDTFIS                                                                       
       ++INCLUDE DMDTFIS                                                        
                                                                                
* DMDTFPH                                                                       
       ++INCLUDE DMDTFPH                                                        
                                                                                
* DMGREQUS                                                                      
* ??   ++INCLUDE DMGREQUS                                                       
                                                                                
* FASSBOFF                                                                      
       ++INCLUDE FASSBOFF                                                       
         EJECT                                                                  
                                                                                
* DSECT TO COVER RECOVERY HEADER                                                
*                                                                               
RECDS    DSECT                                                                  
RECLN    DS    XL2                 TOTAL RECORD LENGTH                          
         DS    XL2                 N/D                                          
       ++INCLUDE DMRCVRHDR                                                      
RCVREC   DS    2000C               RECOVERY RECORD DATA                         
                                                                                
WORKD    DSECT                     ** GLOBAL WORKING STORAGE **                 
*                                                                               
DUB      DS    D                                                                
DMCB     DS    6F                                                               
DMODDMCB DS    6F                                                               
PARM     DS    6F                                                               
FULL     DS    F                                                                
ASYSLST  DS    A                                                                
RELO     DS    A                                                                
MEDADDR  DS    CL4                                                              
IOKEY    DS    CL32                                                             
DMWORK   DS    12D                                                              
BYTE     DS    XL1                                                              
FIRSTFLG DS    XL1                                                              
ERROR    DS    XL1                                                              
MVSPARM  DS    XL1                                                              
MVSTIME  DS    F                   IBM TIME BINARY 100S SECS                    
MVSDATE  DS    F                   IBM DATE JULIAN                              
CENTURY  DS    CL2                                                              
TODAY    DS    CL8                                                              
TODAYC   DS    XL2                                                              
TIMENOW  DS    CL8                                                              
FTIMEP   DS    XL2                                                              
TTIMEP   DS    XL2                                                              
FTIMEE   DS    CL6                                                              
TTIMEE   DS    CL6                                                              
FDATEE   DS    CL8                                                              
TDATEE   DS    CL8                                                              
FDATEC   DS    XL2                                                              
TDATEC   DS    XL2                                                              
RETCODE  DS    XL1                                                              
PASSFLAG DS    XL1                                                              
CONTROLF DS    XL1                                                              
SYSTEM   DS    XL1                                                              
SYSCODE  DS    CL1                                                              
SYSCHAR  DS    CL1                                                              
SNUMB    DS    XL1                                                              
TESTSYS  DS    CL1                                                              
MODE     DS    XL1                                                              
WRTFLAG  DS    CL1                                                              
HDRTYPE  DS    CL1                                                              
CARDTYPE DS    CL2                                                              
FILETYPE DS    CL1                                                              
FILESUBT DS    CL1                                                              
*                                                                               
RECDA    DS    A                                                                
SVBLKSZ  DS    XL2                                                              
SVRECDTF DS    A                                                                
SVSYSFL  DS    A                                                                
SVSYSFLH DS    XL4                                                              
TRECTY   DS    XL41                                                             
*                                                                               
ENQINFO  DS    0CL13                                                            
ENQFLG   DS    XL1                                                              
ENQID    DS    CL4                                                              
ENQCMND  DS    CL8                                                              
*                                                                               
ACTRY    DS    CL1                                                              
ALANG    DS    CL1                                                              
AGYSEN   DS    XL1                                                              
AGYID    DS    CL3                                                              
*                                                                               
ATRKBUFF DS    A                                                                
ARECBUFF DS    A                                                                
AXREC    DS    A                                                                
*                                                                               
JOBNAME  DS    CL8                                                              
JESID    DS    CL8                                                              
MEDAGY   DS    XL1                                                              
CPYFILT  DS    XL1                                                              
MEDFILT  DS    XL1                                                              
RECTYPE  DS    XL1                                                              
MEDCHAR  DS    CL1                                                              
COMPANY  DS    XL1                                                              
INTAPE   DS    CL1                                                              
TDYONLY  DS    CL1                                                              
*                                                                               
DTFADDR  DS    A                                                                
DMSYS    DS    CL8                                                              
DMFLIST  DS    CL80                                                             
LINEBUFF DS    XL80                                                             
WORK     DS    XL256                                                            
CIOKEY   DS    XL(L'CTIKEY)                                                     
IOL      DS    F                                                                
IO       DS    2048X                                                            
WORKX    DS    0D                                                               
                                                                                
WORKC    CSECT                     ** WORKING STORAGE POOL 1 **                 
         DS    10000D                                                           
                                                                                
RECBUFF  CSECT                                                                  
         DS    2048X                                                            
RECBUFFX EQU   *                                                                
                                                                                
XREC     CSECT                                                                  
         DS    2048X                                                            
XRECX    EQU   *                                                                
                                                                                
TRKBUFF  CSECT                                                                  
         DS    (60000)XL1                                                       
TRKBUFFX EQU   *                                                                
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002DDJERCOVA 09/29/11'                                      
         END                                                                    
