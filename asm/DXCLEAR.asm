*          DATA SET DXCLEAR    AT LEVEL 018 AS OF 05/18/16                      
*PHASE DXCLEARA                                                                 
*INCLUDE DMUTLCT                                                                
*INCLUDE LOGIO                                                                  
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
*INCLUDE ADDAY                                                                  
*INCLUDE PERVAL                                                                 
*INCLUDE DMDMGRL                                                                
*INCLUDE XSORT                                                                  
*INCLUDE GETBOOK                                                                
         TITLE 'DXCLEAR - EXTRACT SYSTEM CLEAR COMMITTED FILES'                 
         PRINT NOGEN                                                            
DXCLEAR  CSECT                                                                  
*                                                                               
         NBASE WORKX-WORKD,**XCLR**,RA,R9,R8,WORK=A(WORKC),CLEAR=YES,  X        
               RR=RE                                                            
*                                                                               
         ENTRY SSB                 FOR DATAMGR                                  
*                                                                               
         LR    R1,RC                                                            
         L     RC,=A(WORKD)                                                     
         USING WORKD,RC            RC=A(GLOBAL W/S)                             
         ST    RE,RELO                                                          
*                                                                               
         MVI   MVSPARM,0                                                        
         L     R1,0(R1)            GET MVS PARMS                                
         SR    RF,RF                                                            
         LH    RF,0(R1)                                                         
         LTR   RF,RF                                                            
         BZ    *+8                                                              
         MVI   MVSPARM,1                                                        
         L     R7,VCPRINT                                                       
         USING DPRINT,R7                                                        
         MVC   TITLE(30),=C'FILE EXTRACT CLEARANCE PROGRAM'                     
         B     MAIN                                                             
         EJECT                                                                  
***********************************************************************         
* MAIN CONTROL CODE.                                                  *         
***********************************************************************         
         SPACE 1                                                                
MAIN     EQU   *                                                                
*                                                                               
         BAS   RE,GENINIT          GENERAL INITIALISATION                       
*                                                                               
         BAS   RE,VALCARDS         VALIDATE JCL CARD DATA LINE                  
         BNE   MERR010               EXIT IF ERROR                              
*                                                                               
*                                  MAIN PROCESS LOOP, RETURN HERE               
*                                    AFTER WAITING EXTRACT TRAWL TIME           
         BAS   RE,OPENCUPD         OPEN CONTROL FILES FOR UPDATE                
*                                                                               
         BAS   RE,SDATIME          SET UP DATE AND TIME VALUES                  
         BNE   MERR020               EXIT IF ERROR                              
*                                                                               
         BAS   RE,BLDXTRT          BUILD XTRANS COMMIT TABLE                    
         BNE   MERR030                                                          
*                                                                               
         BAS   RE,CLRFILES         CLEAR FILES                                  
         BNE   MERR030                                                          
*                                                                               
         BAS   RE,UPDXTRT          UPDATE XTRANS RECORDS                        
         BNE   MERR030                                                          
*                                                                               
MXIT     EQU   *                   EXIT FROM SYSTEM                             
         BAS   RE,CLOSCTFL         CLOSE CONTROL SYSTEM IF STILL OPEN           
*                                                                               
MXBASE   XBASE RC=RETCODE,RL=1     RETURN TO MVS                                
*                                                                               
         EJECT                                                                  
***********************************************************************         
* DXTRACT MAIN PROECESS ERROR RETURNS - ABEND CODE 750+               *         
***********************************************************************         
         SPACE 1                                                                
MERR010  EQU   *                                                                
         BAS   RE,CLOSCTFL                                                      
         ABEND 770,DUMP                                                         
MERR020  EQU   *                                                                
         BAS   RE,CLOSCTFL                                                      
         ABEND 771,DUMP                                                         
MERR030  EQU   *                                                                
         BAS   RE,CLOSCTFL                                                      
         ABEND 772,DUMP                                                         
MERR040  EQU   *                                                                
         BAS   RE,CLOSCTFL                                                      
         ABEND 773,DUMP                                                         
MERR050  EQU   *                                                                
         BAS   RE,CLOSCTFL                                                      
         ABEND 774,DUMP                                                         
MERR060  EQU   *                                                                
         BAS   RE,CLOSCTFL                                                      
         ABEND 775,DUMP                                                         
MERR070  EQU   *                                                                
         BAS   RE,CLOSCTFL                                                      
         ABEND 776,DUMP                                                         
MERR080  EQU   *                                                                
         BAS   RE,CLOSCTFL                                                      
         ABEND 777,DUMP                                                         
MERR090  EQU   *                                                                
         BAS   RE,CLOSCTFL                                                      
         ABEND 778,DUMP                                                         
MERR100  EQU   *                                                                
         BAS   RE,CLOSCTFL                                                      
         ABEND 779,DUMP                                                         
MERR110  EQU   *                                                                
         BAS   RE,CLOSCTFL                                                      
         ABEND 780,DUMP                                                         
         SPACE 1                                                                
***********************************************************************         
*  SAFE ABEND - R0=(ERROR CODE) DEQUEUE AND CLOSE CONTROL SYSTEM FILES*         
***********************************************************************         
         SPACE 1                                                                
SAFEND   EQU   *                                                                
         LR    R2,R0                                                            
         BAS   RE,DEQCTRL                                                       
         BAS   RE,CLOSCTFL         CLOSE CONTROL SYSTEM                         
         DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
* GENERAL INITIALISATION                                              *         
***********************************************************************         
         SPACE 1                                                                
GENINIT  NTR1  ,                                                                
         MVI   CONSOPEN,C'N'       CONTROL SYSTEM CLOSED                        
         MVI   INTAPE,C'N'         RECOVERY NOT FROM TAPE                       
         MVI   CHECKFLG,C'N'                                                    
         MVI   RETCODE,0                                                        
         XC    LIMNUM,LIMNUM                                                    
         XC    AGEDATE,AGEDATE                                                  
         XC    UPTODATE,UPTODATE                                                
         MVI   REMOVE,C'Y'                                                      
         L     RF,=A(RECBUFF)                                                   
         ST    RF,ARECBUFF                                                      
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* VALIDATE JCL CARD DATA LINES AS DEFINED IN CARDTBL                  *         
***********************************************************************         
         SPACE 1                                                                
VALCARDS NTR1                                                                   
*                                                                               
VCLP1    MVC   P+72(8),SPACES      CLEAR LEVEL #'S FROM SYSIN LINES             
         GOTO1 VPRINTER            SYSPRINT SYSIN CARDS                         
VCLP1A   GOTO1 VCARDS,DMCB,P,=C'RE00'                                           
         CLC   =C'/*',P            IF END OF JCL                                
         BE    VCEND                 CHECK REQUIRED CARDS INPUT                 
         CLC   =C'*',P             BUMP AROUND COMMENT CARD                     
         BE    VCLP1A                                                           
*                                                                               
         L     RE,=A(CARDTBL)      INITIALISE TABLE POINTER                     
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
         B     VCTIME                                                           
         B     VCINPUT                                                          
         B     VCDATE                                                           
         B     VCPATCH                                                          
         B     VCDDSIO                                                          
         B     VCPLFORM                                                         
         B     VCSUBSYS                                                         
         B     VCSERVER                                                         
         B     VCDBASE                                                          
         B     VCLIMIT                                                          
         B     VCAGED                                                           
         B     VCREMOVE                                                         
         B     VCUPTO                                                           
         B     VCCHECK                                                          
         B     VCDSPACE                                                         
*                                  EXIT/ERROR CONDITIONS                        
*                                  CHECK REQUIRED INPUT                         
VCEND    EQU   *                                                                
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
         SPACE 1                                                                
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
         L     R3,=A(SYSLST)                                                    
         USING SYSLSTD,R3                                                       
         LA    R3,6(R3)            R3=A(SYSTEM LIST)                            
*                                                                               
VCSY020  CLI   0(R3),0             TEST E-O-T                                   
         BE    VCSY024                                                          
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   SYSLNAME(0),P+7                                                  
         BE    VCSY030                                                          
VCSY022  LA    R3,SYSLLEN(R3)      NO - BUMP TO NEXT TABLE ENTRY                
         B     VCSY020                                                          
*                                                                               
VCSY024  L     R3,=A(SYSLEX)                                                    
         USING SYSLSTD,R3                                                       
         LA    R3,6(R3)            R3=A(EXTENDED SYSTEM LIST)                   
*                                                                               
VCSY026  CLI   0(R3),0             TEST E-O-T                                   
         BE    VCERR3                                                           
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   SYSLNAME(0),P+7                                                  
         BE    VCSY030                                                          
VCSY028  LA    R3,SYSLLEN(R3)      NO - BUMP TO NEXT TABLE ENTRY                
         B     VCSY026                                                          
*                                                                               
VCSY030  MVC   SYSCODE,SYSLRPLT    RETURN SYSTEM CODE                           
         MVC   SYSFILT,SYSLNUM       AND SYSTEM NUMBER                          
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
VCSY120  MVC   SYSCFILT,0(R4)                                                   
         B     VCLP1                                                            
         EJECT                                                                  
VCAGENCY EQU   *                   AGENCY=                                      
         MVC   AGYFILT,P+7                                                      
         B     VCLP1                                                            
         SPACE 1                                                                
VCTIME   EQU   *                   TIME=HHMM-HHMM                               
         LA    R0,9                                                             
         GOTO1 VTIMBER,DMCB,(X'80',(R0)),(X'01',XFTIMEP),P+5                    
         CLI   0(R1),0                                                          
         BNE   VCERR1                                                           
         SR    R1,R1                                                            
         ICM   R1,12,XFTIMEP                                                    
         SRL   R1,4                                                             
         STCM  R1,15,FTIME                                                      
         OI    FTIME+3,X'0C'                                                    
         SR    R1,R1                                                            
         ICM   R1,12,XTTIMEP                                                    
         SRL   R1,4                                                             
         STCM  R1,15,XTTIME                                                     
         OI    XTTIME+3,X'0C'                                                   
         B     VCLP1                                                            
         SPACE 1                                                                
VCINPUT  EQU   *                   INPUT=                                       
         CLC   P+6(4),=C'TAPE'                                                  
         BNE   VCERR1                                                           
         MVI   INTAPE,C'Y'                                                      
         B     VCLP1                                                            
         SPACE 1                                                                
VCDATE   EQU   *                   DATE=START-END                               
         LA    R3,WORK                                                          
         LA    R0,30                                                            
         USING PERVALD,R3                                                       
         GOTO1 VPERVAL,DMCB,((R0),P+5),(0,(R3))                                 
         CLI   4(R1),0                                                          
         BNE   VCERR1                                                           
         MVC   XFDATE,PVALBSTA                                                  
         MVC   XTDATE,PVALBEND                                                  
         B     VCLP1                                                            
         DROP  R3                                                               
         SPACE 1                                                                
         SPACE 1                                                                
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
         SPACE 1                                                                
VCDDSIO  EQU   *                   DDSIO=                                       
         L     RF,=V(DDSIO)                                                     
         LTR   RF,RF                                                            
         BZ    VCERR1                                                           
         MVC   0(8,RF),P+6                                                      
         B     VCLP1                                                            
         SPACE 1                                                                
VCDSPACE EQU   *                   DSPACE=                                      
         L     RF,=A(SSB)                                                       
         MVC   SSODSPAC-SSOOFF(1,RF),P+7                                        
         B     VCLP1                                                            
         SPACE 1                                                                
VCPLFORM EQU   *                   PLATFORM=                                    
         CLC   P+9(6),=C'SYBASE'                                                
         BNE   VCERR1                                                           
         MVI   PLATFORM,X'01'                                                   
         B     VCLP1                                                            
         SPACE 1                                                                
VCSUBSYS EQU  *                    SUBSYS=                                      
         USING SUBLSTD,RE                                                       
         L     RE,=A(SUBLST)       CHECK IN TABLE OF VALID SYSTEMS              
         LA    RE,6(RE)                                                         
*                                                                               
VCSU010  CLI   SUBLNUM,0                                                        
         BE    VCERR1                                                           
         CLC   P+7(L'SUBLNAME),SUBLNAME                                         
         BE    VCSU020                                                          
         LA    RE,SUBLLEN(RE)                                                   
         B     VCSU010                                                          
*                                                                               
VCSU020  MVC   SUBFILT(L'SUBLNUM),SUBLNUM  SET SYSTEM NUMBER FROM LIST          
         MVC   SUBCHAR(L'SUBCHAR),SUBLKEY1  GET SYSTEM KEY CHARACTER            
         MVC   SUBNAME(L'SUBLNAME),SUBLNAME  GET FULL SYSTEM NAME               
         B     VCLP1                                                            
         DROP  RE                                                               
         SPACE 1                                                                
VCSERVER EQU   *                   SERVER=                                      
         MVC   SERVERID,P+7                                                     
         B     VCLP1                                                            
         SPACE 1                                                                
VCAPPKEY EQU   *                   APPLICATION=                                 
         MVC   APPLKEY,P+12                                                     
         B     VCLP1                                                            
         SPACE 1                                                                
VCDBASE  EQU   *                   DATABASE=                                    
         MVC   DATABASE,P+9                                                     
         B     VCLP1                                                            
         SPACE 1                                                                
         SPACE 1                                                                
VCLIMIT  EQU   *                   LIMIT=                                       
         CLC   P+6(3),=CL3'ALL'                                                 
         BNE   *+14                                                             
         MVC   LIMNUM,LIMMAX                                                    
         B     VCLP1                                                            
         GOTO1 VNUMVAL,DMCB,P+6,(X'02',0)                                       
         CLI   0(R1),0                                                          
         BNE   VCERR1                                                           
         L     R1,4(R1)                                                         
         C     R1,=F'1'                                                         
         BL    VCERR1                                                           
         CLM   R1,3,LIMMAX                                                      
         BH    VCERR1                                                           
         STCM  R1,3,LIMNUM                                                      
         B     VCLP1                                                            
         SPACE 1                                                                
VCAGED   EQU   *                   AGE=                                         
         GOTO1 VNUMVAL,DMCB,P+4,(X'02',0)                                       
         CLI   0(R1),0                                                          
         BNE   VCERR1                                                           
         L     R1,4(R1)                                                         
         C     R1,=F'0'                                                         
         BL    VCERR1                                                           
         CLM   R1,3,=AL2(255)                                                   
         BH    VCERR1                                                           
         SR    RF,RF                                                            
         SR    RF,R1                                                            
         ST    RF,FULL                                                          
         GOTO1 VDATCON,DMCB,(X'05',0),(0,ADDATE)                                
         L     RF,FULL                                                          
         GOTO1 VADDAY,DMCB,ADDATE,ADDATE,(RF)                                   
         GOTO1 VDATCON,DMCB,(0,ADDATE),(2,AGEDATE)                              
         B     VCLP1                                                            
         SPACE 1                                                                
VCREMOVE EQU   *                   REMOVE=                                      
         MVC   REMOVE,P+7                                                       
         CLI   REMOVE,C'N'         DONT REMOVE MVS DATA SETS                    
         BE    VCLP1                                                            
         CLI   REMOVE,C'Y'         REMOVE MVS DATA SETS                         
         BE    VCLP1                                                            
         B     VCERR1                                                           
         SPACE 1                                                                
VCUPTO   EQU   *                   UPTO=                                        
         GOTO1 VNUMVAL,DMCB,P+5,(X'02',0)                                       
         CLI   0(R1),0                                                          
         BNE   VCERR1                                                           
         L     R1,4(R1)                                                         
         C     R1,=F'0'                                                         
         BL    VCERR1                                                           
         CLM   R1,3,=AL2(255)                                                   
         BH    VCERR1                                                           
         SR    RF,RF                                                            
         SR    RF,R1                                                            
         ST    RF,FULL                                                          
         GOTO1 VDATCON,DMCB,(X'05',0),(0,ADDATE)                                
         L     RF,FULL                                                          
         GOTO1 VADDAY,DMCB,ADDATE,ADDATE,(RF)                                   
         GOTO1 VDATCON,DMCB,(0,ADDATE),(2,UPTODATE)                             
         B     VCLP1                                                            
         SPACE 1                                                                
VCCHECK  EQU   *                   CHECK=                                       
         MVC   CHECKFLG,P+6                                                     
         CLI   CHECKFLG,C'N'                                                    
         BE    VCLP1                                                            
         CLI   CHECKFLG,C'Y'                                                    
         BE    VCLP1                                                            
         B     VCERR1                                                           
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
* BUILD XTRANS COMMIT FILE NUMBER TABLE                               *         
***********************************************************************         
         SPACE 1                                                                
BLDXTRT  NTR1  ,                                                                
         MVC   P(11),=C'BUILD XTRT '                                            
         GOTO1 VPRINTER                                                         
*                                                                               
         ICM   R5,15,=A(XTRTAB)                                                 
         USING XTRTABD,R5                                                       
         STCM  R5,15,XTRTLAST                                                   
         LA    R2,IOKEY            READ XFILE RECORDS                           
         USING GXTRD,R2                                                         
         XC    GXKEY,GXKEY         CLEAR RECORD KEY                             
         MVI   GXKREC,GXSKRECQ     SET XTRANNS RECORD TYPE                      
         L     R2,ARECBUFF         READ HIGH FOR FIRST RECORD                   
*                                                                               
         GOTO1 VDATAMGR,DMCB,(X'A4',DMRDHI),GENDIR,IOKEY,(R2),DMWORK            
         TM    8(R1),X'FF'-X'80'   ONLY ALLOW EOF ERROR                         
         BZ    *+12                                                             
         LA    R0,1                DATAMANAGER RETURN NOT 0 OR EOF              
         B     SAFEND                                                           
         B     XTRTDATA                                                         
*                                  READ SEQN WITH FLUSH                         
XTRTLOOP MVC   IOKEY(L'GXKEY),IOKEYSV                                           
         L     R2,ARECBUFF         RE-READ FOR LAST RECORD                      
         GOTO1 VDATAMGR,DMCB,(X'A4',DMREAD),GENDIR,IOKEY,(R2),DMWORK            
         TM    8(R1),X'FF'-X'80'   ONLY ALLOW EOF ERROR                         
         BZ    *+12                                                             
         LA    R0,2                DATAMANAGER RETURN NOT 0 OR EOF              
         B     SAFEND                                                           
*                                  READ SEQN WITH FLUSH                         
XTRTSEQN GOTO1 VDATAMGR,DMCB,(X'A4',DMRSEQ),GENDIR,GXKEY,(R2),DMWORK            
         TM    8(R1),X'FF'-X'80'   ONLY ALLOW EOF ERROR                         
         BZ    *+12                                                             
         LA    R0,3                                                             
         B     SAFEND              DATAMANAGER RETURN NOT 0 OR EOF              
*                                  GETREC WITH FLUSH                            
XTRTDATA CLI   8(R1),0                                                          
         BNE   XTRTEND                                                          
         CLI   GXKREC,GXSKRECQ     TEST FOR XFILE RECORD                        
         BNE   XTRTEND                                                          
         MVC   IOKEYSV(L'GXKEY),GXKEY                                           
         OC    GXSKAPID,GXSKAPID                                                
         BNZ   XTRTNEXT                                                         
         OC    AGYFILT,AGYFILT     TEST FOR AGENCY FILTER                       
         BZ    *+14                                                             
         CLC   GXSKAGY,AGYFILT                                                  
         BNE   XTRTNEXT                                                         
         OC    SYSFILT,SYSFILT     TEST FOR SYSTEM FILTER                       
         BZ    *+14                                                             
         CLC   GXSKSYS,SYSFILT                                                  
         BNE   XTRTNEXT                                                         
         OC    SUBFILT,SUBFILT     TEST FOR SUB-SYSTEM FILTER                   
         BZ    *+14                                                             
         CLC   GXSKSUB,SUBFILT                                                  
         BNE   XTRTNEXT                                                         
*                                                                               
         MVC   XTRTEID,GXSKEID                                                  
         MVC   XTRTAGY,GXSKAGY                                                  
         MVC   XTRTSYS,GXSKSYS                                                  
         MVC   XTRTSUB,GXSKSUB                                                  
         MVC   DMDA,GXDDA                                                       
         GOTO1 VDATAMGR,DMCB,(X'A4',GETREC),GENFIL,DMDA,(R2),DMWORK             
         CLI   8(R1),0                                                          
         BE    *+12                                                             
         LA    R0,4                CAN'T FIND RECORD                            
         B     SAFEND                                                           
*                                                                               
XTRT100  LA    R3,GXFIRST(R2)      PROCESS ELEMENT DATA                         
*                                                                               
XTRT110  CLI   0(R3),0                                                          
         BE    XTRT200                                                          
         CLI   0(R3),GXSTELQ       FIND XTRANS ELEMENT                          
         BE    XTRT130                                                          
XTRT120  SR    RF,RF                                                            
         IC    RF,1(R3)                                                         
         AR    R3,RF                                                            
         B     XTRT110                                                          
*                                                                               
         USING GXSTEL,R3                                                        
XTRT130  MVC   XTRTCOFN,GXSTCOFN                                                
         XC    XTRTCLFN,XTRTCLFN                                                
         MVC   XTRTFCOD,GXSTFCOD                                                
         MVC   XTRTMODE,GXSTMODE                                                
         OC    LIMNUM,LIMNUM                                                    
         BZ    XTRT120                                                          
         CLC   GXSTNOFN,LIMNUM                                                  
         BH    XTRT120                                                          
         CLC   GXSTCRFN,GXSTNOFN                                                
         BNH   XTRT120                                                          
         CLC   GXSTCRFN,LIMNUM                                                  
         BH    XTRT132                                                          
         MVC   GXSTNOFN,GXSTCRFN                                                
         B     XTRT134                                                          
XTRT132  MVC   GXSTNOFN,LIMNUM                                                  
XTRT134  MVC   GXSTREFN,GXSTNOFN                                                
         MVC   GXSTCOFN,GXSTNOFN                                                
         B     XTRT120                                                          
*                                                                               
XTRT200  EQU   *                                                                
         OC    LIMNUM,LIMNUM                                                    
         BZ    XTRT210                                                          
*                                  ??                                           
         B     XTRT210                                                          
*                                  ??                                           
         GOTO1 VDATAMGR,DMCB,PUTREC,GENFIL,DMDA,(R2),DMWORK                     
         CLI   8(R1),0                                                          
         BE    *+12                                                             
         LA    R0,11                                                            
         B     SAFEND                                                           
XTRT210  LA    R5,XTRTTBLQ(R5)                                                  
         CLM   R5,15,=A(XTRTABX)                                                
         BL    *+6                                                              
         DC    H'0'                                                             
         B     XTRTNEXT                                                         
*                                                                               
XTRTNEXT EQU   *                                                                
         B     XTRTLOOP                                                         
*                                                                               
XTRTEND  EQU   *                                                                
         STCM  R5,15,XTRTLAST                                                   
         B     XTRTOK                                                           
*                                                                               
XTRTNO   B     NO                  EXIT ERROR                                   
XTRTOK   B     YES                 EXIT OK                                      
         EJECT                                                                  
***********************************************************************         
* CLEAR COMMITTED FILES AND ASSOCIATED CONTROL RECORDS                *         
***********************************************************************         
         SPACE 1                                                                
CLRFILES NTR1  ,                                                                
         MVC   P(11),=C'CLEAR FILES'                                            
         GOTO1 VPRINTER                                                         
*                                                                               
         MVI   ESSFOUND,C'N'                                                    
         LA    R2,IOKEY            READ XFILE RECORDS                           
         USING GXTRD,R2                                                         
         XC    GXKEY,GXKEY         CLEAR RECORD KEY                             
         MVI   GXKREC,GXFKRECQ     SET XAGENCY RECORD TYPE                      
         L     R2,ARECBUFF         READ HIGH FOR FIRST RECORD                   
*                                  READ HIGH WITH FLUSH                         
         BAS   RE,ENQCTRL                                                       
         GOTO1 VDATAMGR,DMCB,(X'A4',DMRDHI),GENDIR,IOKEY,(R2),DMWORK            
         TM    8(R1),X'FF'-X'80'   ONLY ALLOW EOF ERROR                         
         BZ    *+12                                                             
         LA    R0,1                DATAMANAGER RETURN NOT 0 OR EOF              
         B     SAFEND                                                           
         B     CLRFDATA                                                         
*                                  READ SEQN WITH FLUSH                         
CLRFLOOP BAS   RE,ENQCTRL                                                       
         MVC   IOKEY(L'GXKEY),IOKEYSV                                           
         L     R2,ARECBUFF         RE-READ FOR LAST RECORD                      
         GOTO1 VDATAMGR,DMCB,(X'A4',DMREAD),GENDIR,IOKEY,(R2),DMWORK            
         TM    8(R1),X'FF'-X'80'   ONLY ALLOW EOF ERROR                         
         BZ    *+12                                                             
         LA    R0,2                DATAMANAGER RETURN NOT 0 OR EOF              
         B     SAFEND                                                           
         CLI   ESSFOUND,C'Y'                                                    
         BE    CLRFSEQN                                                         
         OI    GXDSTAT,X'80'       SET DELETE FLAG IN DIRECTORY                 
         GOTO1 VDATAMGR,DMCB,(X'00',DMWRT),GENDIR,IOKEY,(R2),DMWORK             
         BE    *+12                                                             
         LA    R0,5                                                             
         B     SAFEND                                                           
*                                                                               
         MVC   P(40),=CL40'DELETE XFILE DIRECTORY RECORD'                       
         GOTO1 VHEXOUT,DMCB,(R2),P+42,32,=C'TOG'                                
         GOTO1 VPRINTER                                                         
         MVC   P(2),SVAGENCY                                                    
         GOTO1 VHEXOUT,DMCB,SVSYSTEM,P+4,1,=C'TOG'                              
         GOTO1 VHEXOUT,DMCB,SVSUBSYS,P+6,1,=C'TOG'                              
         GOTO1 VHEXOUT,DMCB,SVFGNUM,P+10,2,=C'TOG'                              
         GOTO1 VPRINTER                                                         
*                                  READ SEQN WITH FLUSH                         
CLRFSEQN GOTO1 VDATAMGR,DMCB,(X'A4',DMRSEQ),GENDIR,GXKEY,(R2),DMWORK            
         TM    8(R1),X'FF'-X'80'   ONLY ALLOW EOF ERROR                         
         BZ    *+12                                                             
         LA    R0,3                                                             
         B     SAFEND              DATAMANAGER RETURN NOT 0 OR EOF              
         MVI   ESSFOUND,C'N'                                                    
*                                  GETREC WITH FLUSH                            
CLRFDATA CLI   8(R1),0                                                          
         BNE   CLRFEND                                                          
         CLI   GXKREC,GXFKRECQ     TEST FOR XFILE RECORD                        
         BNE   CLRFEND                                                          
         MVC   IOKEYSV(L'GXKEY),GXKEY                                           
         OC    AGYFILT,AGYFILT     TEST FOR AGENCY FILTER                       
         BZ    *+14                                                             
         CLC   GXFKAGY,AGYFILT                                                  
         BNE   CLRFSEQN                                                         
         OC    SYSFILT,SYSFILT     TEST FOR SYSTEM FILTER                       
         BZ    *+14                                                             
         CLC   GXFKSYS,SYSFILT                                                  
         BNE   CLRFSEQN                                                         
         OC    SUBFILT,SUBFILT     TEST FOR SUB-SYSTEM FILTER                   
         BZ    *+14                                                             
         CLC   GXFKSUB,SUBFILT                                                  
         BNE   CLRFSEQN                                                         
         OC    UPTODATE,UPTODATE   TEST FOR UPTO DATE                           
         BZ    CLRF020                                                          
         MVC   TESTDATE,GXFKDAT                                                 
         TM    GXDCTL,GXFTCMP                                                   
         BNZ   CLRF010                                                          
         CLC   TESTDATE,=XL2'7FFF'                                              
         BH    CLRF011                                                          
**********************************************************************          
*TEMPORTARY, COMPLEMENT DATE/TIME FOR ANY ENTRY FROM APR01, 4/12/04             
*LEAVE THIS CODE UNIT MAY                                                       
         CLC   TESTDATE,=XL2'2F7E'     APR01 OR LATER? (RESERVE ORDER)          
         BNH   CLRF010                                                          
**********************************************************************          
         DC    H'0'                                                             
CLRF010  EQU   *                                                                
         CLC   TESTDATE,=XL2'7FFF'                                              
         BNH   *+6                                                              
         DC    H'0'                                                             
         XC    TESTDATE,=XL2'FFFF'                                              
CLRF011  EQU   *                                                                
         CLC   TESTDATE,UPTODATE                                                
         BNL   CLRFSEQN                                                         
*                                                                               
CLRF020  EQU   *                                                                
         MVC   SVAGENCY,GXFKAGY                                                 
         MVC   SVSYSTEM,GXFKSYS                                                 
         MVC   SVSUBSYS,GXFKSUB                                                 
         MVC   SVDATE,GXFKDAT                                                   
         MVC   SVTIME,GXFKTIM                                                   
*                                                                               
         L     R3,=A(SYSLST)                                                    
         USING SYSLSTD,R3                                                       
         LA    R3,6(R3)            R3=A(SYSTEM LIST)                            
*                                                                               
CLRF100  CLI   0(R3),0             TEST E-O-T                                   
         BE    CLRF102                                                          
         CLC   SVSYSTEM,SYSLNUM                                                 
         BE    *+12                                                             
         LA    R3,SYSLLEN(R3)      NO - BUMP TO NEXT TABLE ENTRY                
         B     CLRF100                                                          
         MVC   SVSYSCOD,SYSLRPLT                                                
         B     CLRF106                                                          
*                                                                               
CLRF102  L     R3,=A(SYSLEX)                                                    
         LA    R3,6(R3)            R3=A(EXTENDED SYSTEM LIST)                   
*                                                                               
CLRF104  CLI   0(R3),0             TEST E-O-T                                   
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   SVSYSTEM,SYSLNUM                                                 
         BE    *+12                                                             
         LA    R3,SYSLLEN(R3)      NO - BUMP TO NEXT TABLE ENTRY                
         B     CLRF104                                                          
         MVC   SVSYSCOD,SYSLRPLT                                                
         DROP  R3                                                               
*                                                                               
CLRF106  L     R3,=A(SUBLST)                                                    
         USING SUBLSTD,R3                                                       
         LA    R3,6(R3)            R3=A(EXTRACT SUB-SYSTEM LIST)                
*                                                                               
CLRF110  CLI   0(R3),0             TEST E-O-T                                   
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   SVSUBSYS,SUBLNUM                                                 
         BE    *+12                                                             
         LA    R3,SUBLLEN(R3)      NO - BUMP TO NEXT TABLE ENTRY                
         B     CLRF110                                                          
         MVC   SVSUBCOD,SUBLKEY1                                                
         DROP  R3                                                               
*                                                                               
CLRF200  EQU   *                                                                
         MVC   DMDA,GXDDA                                                       
         GOTO1 VDATAMGR,DMCB,(X'A4',GETREC),GENFIL,DMDA,(R2),DMWORK             
         CLI   8(R1),0                                                          
         BE    *+12                                                             
         LA    R0,4                CAN'T FIND RECORD                            
         B     SAFEND                                                           
         L     R2,ARECBUFF                                                      
*                                                                               
CLRF300  LA    R3,GXFIRST(R2)      GET ELEMENT DATA                             
*                                                                               
CLRF310  CLI   0(R3),0                                                          
         BE    CLRF400             END OF RECORD                                
         CLI   0(R3),GXGNELQ                                                    
         BE    CLRF330                                                          
*                                                                               
CLRF320  SR    RF,RF               GET NEXT ELEMENT                             
         IC    RF,1(R3)                                                         
         AR    R3,RF                                                            
         B     CLRF310                                                          
*                                                                               
         USING GXGNEL,R3                                                        
CLRF330  EQU   *                   PROCESS FILE TRANSMISSION ELEMENT            
         MVC   SVFGNUM,GXGNNUM                                                  
         B     CLRF320                                                          
*                                                                               
CLRF400  LA    R3,GXFIRST(R2)      GET ELEMENT DATA                             
         MVI   ESSFOUND,C'N'                                                    
*                                                                               
CLRF410  CLI   0(R3),0                                                          
         BE    CLRF500             END OF RECORD                                
         CLI   0(R3),GXFTELQ                                                    
         BE    CLRF430                                                          
*                                                                               
CLRF420  SR    RF,RF               GET NEXT ELEMENT                             
         IC    RF,1(R3)                                                         
         AR    R3,RF                                                            
         B     CLRF410                                                          
*                                                                               
         USING GXFTEL,R3                                                        
CLRF430  EQU   *                   PROCESS FILE TRANSMISSION ELEMENT            
         MVC   SVEID,GXFTEID                                                    
         MVC   SVFTNUM,GXFTFNUM                                                 
         MVC   TESTDATE,GXFKDAT                                                 
         TM    GXFCTL,GXFTCMP                                                   
         BNZ   CLRF430A                                                         
         CLC   TESTDATE,=XL2'7FFF'                                              
         BH    CLRF430B                                                         
**********************************************************************          
*TEMPORTARY, COMPLEMENT DATE/TIME FOR ANY ENTRY FROM APR01, 4/12/04             
*LEAVE THIS CODE UNIT MAY                                                       
         CLC   TESTDATE,=XL2'2F7E'     APR01 OR LATER? (RESERVE ORDER)          
         BNH   CLRF430A                                                         
**********************************************************************          
         DC    H'0'                                                             
CLRF430A EQU   *                                                                
         CLC   TESTDATE,=XL2'7FFF'                                              
         BNH   *+6                                                              
         DC    H'0'                                                             
         XC    TESTDATE,=XL2'FFFF'                                              
CLRF430B EQU   *                                                                
         CLC   TESTDATE,AGEDATE                                                 
         BNH   CLRF431                                                          
*                                                                               
         BAS   RE,CHKESS                                                        
         BE    CLRF431A                                                         
         MVI   ESSFOUND,C'Y'                                                    
         MVC   P(13),=CL13'COMMIT FILE# '                                       
         GOTO1 VHEXOUT,DMCB,FULL+2,P+13,2,=C'TOG'                               
         MVC   P+18(12),=CL12'< GEN FILE#'                                      
         GOTO1 VHEXOUT,DMCB,FULL,P+30,2,=C'TOG'                                 
*                                                                               
         GOTO1 VHEXOUT,DMCB,(R2),P+42,32,=C'TOG'                                
         GOTO1 VPRINTER                                                         
         MVC   P(40),=CL40'ESSID:   '                                           
         MVC   P+10(8),SVEID                                                    
         GOTO1 VPRINTER                                                         
         B     CLRF420                                                          
CLRF431  EQU   *                                                                
         MVC   P(40),=CL40'DATE AGED'                                           
         GOTO1 VHEXOUT,DMCB,(R2),P+42,32,=C'TOG'                                
         GOTO1 VPRINTER                                                         
         MVC   P(40),=CL40'ESSID:   '                                           
         MVC   P+10(8),SVEID                                                    
         GOTO1 VPRINTER                                                         
         B     CLRF431B                                                         
CLRF431A EQU   *                                                                
         MVC   P(13),=CL13'COMMIT FILE# '                                       
         GOTO1 VHEXOUT,DMCB,FULL+2,P+13,2,=C'TOG'                               
         MVC   P+18(12),=CL12'>= GEN FILE#'                                     
         GOTO1 VHEXOUT,DMCB,FULL,P+30,2,=C'TOG'                                 
*                                                                               
         GOTO1 VHEXOUT,DMCB,(R2),P+42,32,=C'TOG'                                
         GOTO1 VPRINTER                                                         
         MVC   P(40),=CL40'ESSID:   '                                           
         MVC   P+10(8),SVEID                                                    
         GOTO1 VPRINTER                                                         
CLRF431B EQU   *                                                                
         BAS   RE,UPDESS                                                        
         LA    R2,IOKEY                                                         
         XC    GXKEY,GXKEY                                                      
         MVI   GXKREC,GXSFRECQ                                                  
         MVC   SVEID,GXFTEID                                                    
         MVC   GXSFEID,GXFTEID                                                  
         MVC   GXSFAGY,SVAGENCY                                                 
         MVC   GXSFSYS,SVSYSTEM                                                 
         MVC   GXSFSUB,SVSUBSYS                                                 
         MVC   GXSFFNUM,GXFTFNUM                                                
         GOTO1 VDATAMGR,DMCB,(X'A4',DMREAD),GENDIR,IOKEY,IOKEY,DMWORK           
         TM    8(R1),X'10'                                                      
         BNZ   CLRF432                                                          
         CLI   8(R1),0                                                          
         BE    *+12                                                             
         LA    R0,11                                                            
         B     SAFEND                                                           
         OI    GXDSTAT,X'80'       SET DELETE FLAG IN DIRECTORY                 
         GOTO1 VDATAMGR,DMCB,(X'00',DMWRT),GENDIR,IOKEY,IOKEY,DMWORK            
         BE    *+12                                                             
         LA    R0,12                                                            
         B     SAFEND                                                           
*                                                                               
         MVC   P(40),=CL40'DELETE XFILE SERVER PASSIVE'                         
         GOTO1 VHEXOUT,DMCB,(R2),P+42,32,=C'TOG'                                
         GOTO1 VPRINTER                                                         
*                                                                               
CLRF432  L     R2,ARECBUFF                                                      
         SR    R5,R5                                                            
         IC    R5,1(R3)                                                         
         AR    R5,R3                                                            
         LA    R4,GXFTSLST                                                      
*                                                                               
CLRF434  CR    R4,R5                                                            
         BNL   CLRF450                                                          
         USING GXFTSLST,R4                                                      
         CLI   REMOVE,C'N'                                                      
         BE    CLRF438                                                          
         BAS   RE,BLOGDSN                                                       
*                                  DYNAMICALLY PURGE MVS FILE                   
         MVC   TXTDSN+6(44),LOGDSN    LOAD DYNALLOC BLOCK WITH DS INFO          
         MVC   TXTDD+6(8),=CL8'EXFILE  '                                        
         MVC   TXTUNDD+6(8),=CL8'EXFILE  '                                      
*                                  DYNAMICALLY PURGE MVS FILE                   
         LA    R6,RBLKDELA                                                      
         LA    R1,ARBLKDLA                                                      
         DYNALLOC                                                               
         LTR   RF,RF                                                            
         BZ    CLRF435                                                          
*                                  TRY OLD STYLE LOG DSN                        
         BAS   RE,BOLOGDSN                                                      
*                                  DYNAMICALLY PURGE MVS FILE                   
         MVC   TXTDSN+6(44),LOGDSN    LOAD DYNALLOC BLOCK WITH DS INFO          
         MVC   TXTDD+6(8),=CL8'EXFILE  '                                        
         MVC   TXTUNDD+6(8),=CL8'EXFILE  '                                      
*                                  DYNAMICALLY PURGE MVS FILE                   
         LA    R6,RBLKDELA                                                      
         LA    R1,ARBLKDLA                                                      
         DYNALLOC                                                               
         LTR   RF,RF                                                            
         BNZ   CLRF436                                                          
*                                  DYNAMICALLY DEALLOCATE MVS FILE              
CLRF435  EQU   *                                                                
         LA    R6,RBLKUNA2                                                      
         LA    R1,ARBLKUN2                                                      
         DYNALLOC                                                               
         LTR   RF,RF                                                            
         BNZ   CLRF436                                                          
*                                                                               
         MVC   P(24),=CL24'DELETED LOGFILE:'                                    
         MVC   P+24(44),LOGDSN                                                  
         GOTO1 VPRINTER                                                         
         B     CLRF438                                                          
*                                                                               
CLRF436  MVC   P(24),=CL24'ERROR DELETING LOGFILE:'                             
         MVC   P+24(44),LOGDSN                                                  
         GOTO1 VPRINTER                                                         
*                                                                               
         MVC   P+30(22),=C'DYNALLOC ERROR CODE = '                              
         GOTO1 VHEXOUT,DMCB,4(R6),P+52,2,=C'TOG'                                
         CLC   =F'4',DMCB+16                                                    
         BNE   CLRF520                                                          
         MVC   P+56(15),=C',  INFO CODE = '                                     
         GOTO1 VHEXOUT,DMCB,6(R6),P+71,2,=C'TOG'                                
         GOTO1 VPRINTER                                                         
         B     CLRF438                                                          
*                                                                               
CLRF438  LA    R4,GXFTSLQ(R4)                                                   
         B     CLRF434                                                          
*                                                                               
CLRF450  EQU   *                                                                
         MVC   P(40),=CL40'DELETE XFILE ELEMENT'                                
         GOTO1 VHEXOUT,DMCB,(R2),P+42,32,=C'TOG'                                
         GOTO1 VPRINTER                                                         
         MVC   P(40),=CL40'ELEMENT: '                                           
         GOTO1 VHEXOUT,DMCB,(R3),P+42,32,=C'TOG'                                
         GOTO1 VPRINTER                                                         
         SR    R0,R0               DELETE ELEMENT                               
         ICM   R0,1,0(R3)                                                       
         XC    WORK,WORK                                                        
         MVC   WORK(8),GXFTEID                                                  
         GOTO1 VHELLO,DMCB,(C'D',GENFIL),((R0),(R2)),(8,WORK),0                 
         CLI   DMCB+12,0                                                        
         BE    CLRF410                                                          
         LA    R0,99                                                            
         B     SAFEND                                                           
*                                                                               
CLRF500  EQU   *                                                                
*                                                                               
         CLI   ESSFOUND,C'Y'                                                    
         BE    CLRF600                                                          
         L     R2,ARECBUFF                                                      
         OI    GXFSTAT,X'80'       SET DELETE FLAG IN FILE                      
         GOTO1 VDATAMGR,DMCB,PUTREC,GENFIL,DMDA,(R2),DMWORK                     
         CLI   8(R1),0                                                          
         BE    *+12                                                             
         LA    R0,7                                                             
         B     SAFEND                                                           
*                                                                               
         MVC   P(40),=CL40'DELETE XFILE FILE RECORD'                            
         GOTO1 VHEXOUT,DMCB,(R2),P+42,32,=C'TOG'                                
         GOTO1 VPRINTER                                                         
         LA    R2,IOKEY                                                         
         XC    GXKEY,GXKEY                                                      
         MVI   GXKREC,GXAFRECQ                                                  
         MVC   GXAFAGY,SVAGENCY                                                 
         MVC   GXAFSYS,SVSYSTEM                                                 
         MVC   GXAFSUB,SVSUBSYS                                                 
         MVC   GXAFDAT,SVDATE                                                   
         MVC   GXAFTIM,SVTIME                                                   
         GOTO1 VDATAMGR,DMCB,(X'A4',DMREAD),GENDIR,IOKEY,IOKEY,DMWORK           
         TM    8(R1),X'10'                                                      
         BNZ   CLRF502                                                          
         CLI   8(R1),0                                                          
         BE    *+12                                                             
         LA    R0,8                                                             
         B     SAFEND                                                           
         OI    GXDSTAT,X'80'       SET DELETE FLAG IN DIRECTORY                 
         GOTO1 VDATAMGR,DMCB,(X'00',DMWRT),GENDIR,IOKEY,IOKEY,DMWORK            
         BE    *+12                                                             
         LA    R0,9                                                             
         B     SAFEND                                                           
*                                                                               
         MVC   P(40),=CL40'DELETE XFILE AGENCY PASSIVE'                         
         GOTO1 VHEXOUT,DMCB,(R2),P+42,32,=C'TOG'                                
         GOTO1 VPRINTER                                                         
*                                                                               
CLRF502  EQU   *                                                                
         L     R2,ARECBUFF                                                      
         LA    R3,GXFIRST(R2)      GET ELEMENT DATA                             
*                                                                               
CLRF510  CLI   0(R3),0                                                          
         BE    CLRF600             END OF RECORD                                
         CLI   0(R3),GXFDELQ                                                    
         BE    CLRF540                                                          
*                                                                               
CLRF520  SR    RF,RF               GET NEXT ELEMENT                             
         IC    RF,1(R3)                                                         
         AR    R3,RF                                                            
         B     CLRF510                                                          
*                                                                               
         USING GXFDEL,R3                                                        
CLRF540  EQU   *                   PROCESS FILE DEFINITION ELEMENT              
         CLI   REMOVE,C'N'                                                      
         BE    CLRF520                                                          
*&&US                                                                           
         CLC   =C'DDS.',GXFDDSN     DELETE FILE W/ DDS. ONLY                    
         BNE   CLRF520                                                          
*&&                                                                             
         MVC   TXTDSN+6(44),GXFDDSN   LOAD DYNALLOC BLOCK WITH DS INFO          
         MVC   TXTDD+6(8),=CL8'EXFILE  '                                        
         MVC   TXTUNDD+6(8),=CL8'EXFILE  '                                      
*                                  DYNAMICALLY PURGE MVS FILE                   
         LA    R6,RBLKDELA                                                      
         LA    R1,ARBLKDLA                                                      
         DYNALLOC                                                               
         LTR   RF,RF                                                            
         BNZ   CLRF548                                                          
*                                  DYNAMICALLY DEALLOCATE MVS FILE              
         LA    R6,RBLKUNA2                                                      
         LA    R1,ARBLKUN2                                                      
         DYNALLOC                                                               
         LTR   RF,RF                                                            
         BNZ   CLRF548                                                          
*                                                                               
         MVC   P(24),=CL24'DELETED EXFILE: '                                    
         MVC   P+24(44),GXFDDSN                                                 
         GOTO1 VPRINTER                                                         
         B     CLRF520                                                          
*                                                                               
CLRF548  MVC   P(24),=CL24'ERROR DELETING EXFILE: '                             
         MVC   P+24(44),GXFDDSN                                                 
         GOTO1 VPRINTER                                                         
*                                                                               
         MVC   P+30(22),=C'DYNALLOC ERROR CODE = '                              
         GOTO1 VHEXOUT,DMCB,4(R6),P+52,2,=C'TOG'                                
         CLC   =F'4',DMCB+16                                                    
         BNE   CLRF520                                                          
         MVC   P+56(15),=C',  INFO CODE = '                                     
         GOTO1 VHEXOUT,DMCB,6(R6),P+71,2,=C'TOG'                                
         GOTO1 VPRINTER                                                         
*                                                                               
         B     CLRF520                                                          
*                                                                               
CLRF600  EQU   *                                                                
         B     CLRFNEXT                                                         
*                                                                               
CLRFNEXT EQU   *                                                                
         BAS   RE,DEQCTRL                                                       
         B     CLRFLOOP                                                         
*                                                                               
CLRFEND  EQU   *                                                                
         BAS   RE,DEQCTRL                                                       
         B     CLRFOK                                                           
*                                                                               
CLRFNO   B     NO                  EXIT ERROR                                   
CLRFOK   B     YES                 EXIT OK                                      
         EJECT                                                                  
***********************************************************************         
* UPDATE XTRANS RECORD FROM XTRTAB                                    *         
***********************************************************************         
         SPACE 1                                                                
UPDXTRT  NTR1  ,                                                                
         MVC   P(11),=C'UPDATE XTRT'                                            
         GOTO1 VPRINTER                                                         
*                                                                               
         ICM   R5,15,=A(XTRTAB)                                                 
         USING XTRTABD,R5                                                       
*                                                                               
UPDXNEXT DS    0H                                                               
         CLM   R5,15,XTRTLAST                                                   
         BNL   UPDXOK                                                           
         CLI   XTRTFCOD,C'N'                                                    
         BE    UPDX300                                                          
         CLI   XTRTMODE,C'I'                                                    
         BE    UPDX300                                                          
         OC    XTRTCLFN,XTRTCLFN                                                
         BZ    UPDX300                                                          
*                                                                               
UPDX010  BAS   RE,ENQCTRL                                                       
         LA    R2,IOKEY            READ XTRANS RECORDS                          
         USING GXTRD,R2                                                         
         XC    GXKEY,GXKEY                                                      
         MVI   GXKREC,GXSKRECQ                                                  
         MVC   GXSKEID,XTRTEID                                                  
         MVC   GXSKAGY,XTRTAGY                                                  
         MVC   GXSKSYS,XTRTSYS                                                  
         MVC   GXSKSUB,XTRTSUB                                                  
         L     R2,ARECBUFF         READ HIGH FOR FIRST RECORD                   
*                                                                               
         GOTO1 VDATAMGR,DMCB,(X'A4',DMREAD),GENDIR,IOKEY,(R2),DMWORK            
         TM    8(R1),X'FF'-X'80'   ONLY ALLOW EOF ERROR                         
         BZ    *+12                                                             
         LA    R0,101              DATAMANAGER RETURN NOT 0 OR EOF              
         B     SAFEND                                                           
*                                  GETREC WITH FLUSH                            
         MVC   IOKEYSV(L'GXKEY),GXKEY                                           
         MVC   DMDA,GXDDA                                                       
         GOTO1 VDATAMGR,DMCB,(X'A4',GETREC),GENFIL,DMDA,(R2),DMWORK             
         CLI   8(R1),0                                                          
         BE    *+12                                                             
         LA    R0,102              CAN'T FIND RECORD                            
         B     SAFEND                                                           
*                                                                               
UPDX100  LA    R3,GXFIRST(R2)      PROCESS ELEMENT DATA                         
*                                                                               
UPDX110  CLI   0(R3),0                                                          
         BE    UPDX250                                                          
         CLI   0(R3),GXSTELQ       FIND XTRANS ELEMENT                          
         BE    UPDX130                                                          
UPDX120  SR    RF,RF                                                            
         IC    RF,1(R3)                                                         
         AR    R3,RF                                                            
         B     UPDX110                                                          
         USING GXSTEL,R3                                                        
UPDX130  MVC   HALF,GXSTCOFN       SAVE THE TRANSFER FILE NUMBER                
         DROP  R3                                                               
*                                                                               
*DON'T WRITE BACK IF THE XFILE IS OUTDATED.                                     
*                                                                               
         LA    R2,IOKEY            READ XFILE PASSIVE RECORD                    
         USING GXTRD,R2                                                         
         XC    GXKEY,GXKEY                                                      
         MVI   GXKREC,GXSFRECQ                                                  
         MVC   GXSFEID,XTRTEID                                                  
         MVC   GXSFAGY,XTRTAGY                                                  
         MVC   GXSFSYS,XTRTSYS                                                  
         MVC   GXSFSUB,XTRTSUB                                                  
         MVC   GXSFFNUM,HALF                                                    
         L     R2,=A(RECBUF2)                                                   
*                                                                               
         GOTO1 VDATAMGR,DMCB,(X'08',DMREAD),GENDIR,IOKEY,(R2),DMWORK            
         CLI   8(R1),0                                                          
         BNE   UPDX250             NO UPDATE                                    
*                                  GETREC WITH FLUSH                            
         GOTO1 VDATAMGR,DMCB,(X'08',GETREC),GENFIL,GXDDA,(R2),DMWORK            
         CLI   8(R1),0                                                          
         BNE   UPDX250             NO UPDATE                                    
*                                                                               
         CLC   GXFKDAT(6),XTRTDATE   XFILE DATETIME > LAST DEL DATTIME          
         BNH   UPDX250               YES - NO UPDATE                            
*                                                                               
UPDX200  EQU   *                                                                
         L     R2,ARECBUFF                                                      
         GOTO1 VDATAMGR,DMCB,(X'A4',GETREC),GENFIL,DMDA,(R2),DMWORK             
         CLI   8(R1),0                                                          
         BE    *+12                                                             
         LA    R0,105              CAN'T FIND RECORD                            
         B     SAFEND                                                           
*                                                                               
         LA    R3,GXFIRST(R2)      PROCESS ELEMENT DATA                         
UPDX210  CLI   0(R3),0                                                          
         BE    UPDX250                                                          
         CLI   0(R3),GXSTELQ       FIND XTRANS ELEMENT                          
         BE    UPDX230                                                          
UPDX220  SR    RF,RF                                                            
         IC    RF,1(R3)                                                         
         AR    R3,RF                                                            
         B     UPDX210                                                          
*                                                                               
         USING GXSTEL,R3                                                        
UPDX230  EQU   *                                                                
*                                  PRINT & EMAIL XTRAN REC UPDATE               
         MVC   P(8),XTRTEID                                                     
         MVC   P+10(2),XTRTAGY                                                  
         GOTO1 VHEXOUT,DMCB,XTRTSYS,P+14,1,=C'TOG'                              
         GOTO1 VHEXOUT,DMCB,XTRTSUB,P+16,1,=C'TOG'                              
         GOTO1 VHEXOUT,DMCB,GXSTCRFN,P+24,2,=C'TOG'                             
         GOTO1 VHEXOUT,DMCB,GXSTNOFN,P+30,2,=C'TOG'                             
         GOTO1 VHEXOUT,DMCB,GXSTREFN,P+36,2,=C'TOG'                             
         GOTO1 VHEXOUT,DMCB,GXSTCOFN,P+42,2,=C'TOG'                             
         MVC   P+50(3),=CL3'-->'                                                
         GOTO1 VHEXOUT,DMCB,XTRTCLFN,P+55,2,=C'TOG'                             
         MVC   P+60(10),=CL10'UPDATED!'                                         
         XC    WORK,WORK                                                        
         MVC   WORK+32(70),P                                                    
         MVC   WORK(32),=CL32'AUTONOTE*US-MF_FAC_NOTIFY:XTRAN-'                 
         OC    WORK(102),SPACES                                                 
         GOTO1 VPRINTER                                                         
         GOTO1 VDATAMGR,DMCB,=C'OPMSG',(102,WORK)                               
         GOTO1 VPRINTER                                                         
*                                  UPDATE FILE TRANSFER# ELEMENT                
         MVC   GXSTCOFN,XTRTCLFN                                                
         MVC   GXSTCRFN,XTRTCLFN                                                
         MVC   GXSTNOFN,XTRTCLFN                                                
         MVC   GXSTREFN,XTRTCLFN                                                
         DROP  R3                                                               
*                                                                               
         GOTO1 VDATAMGR,DMCB,PUTREC,GENFIL,DMDA,(R2),DMWORK                     
         CLI   8(R1),0                                                          
         BE    *+12                                                             
         LA    R0,106                                                           
         B     SAFEND                                                           
*                                                                               
UPDX250  BAS   RE,DEQCTRL                                                       
*                                                                               
UPDX300  LA    R5,XTRTTBLQ(R5)                                                  
         CLM   R5,15,=A(XTRTABX)                                                
         BL    *+6                                                              
         DC    H'0'                                                             
         B     UPDXNEXT                                                         
*                                                                               
UPDXNO   B     NO                  EXIT ERROR                                   
UPDXOK   B     YES                 EXIT OK                                      
         EJECT                                                                  
***********************************************************************         
* BUILD OLD STYLE LOG DATA SET NAME                                   *         
***********************************************************************         
         SPACE 1                                                                
         USING GXFTEL,R3                                                        
         USING GXFTSLST,R4                                                      
BOLOGDSN NTR1                                                                   
         MVC   LOGDSN,SPACES                                                    
         LA    R6,LOGDSN                                                        
*&&UK*&& MVC   0(8,R6),=CL8'ESS.XLG.'                                           
*&&US*&& MVC   0(8,R6),=CL8'DDS.XLG.'                                           
         LA    R6,8(R6)                                                         
         MVC   0(8,R6),GXFTEID                                                  
         LA    R6,8(R6)                                                         
         MVI   0(R6),C'.'                                                       
         LA    R6,1(R6)                                                         
         CLI   SVAGENCY,C'A'                                                    
         BL    BODS020                                                          
         CLI   SVAGENCY,C'Z'                                                    
         BH    BODS020                                                          
         CLI   SVAGENCY,C'A'                                                    
         BL    BODS010                                                          
         CLI   SVAGENCY,C'Z'                                                    
         BH    BODS010                                                          
         B     BODS030                                                          
BODS010  CLI   SVAGENCY,C'0'                                                    
         BL    BODS020                                                          
         CLI   SVAGENCY,C'9'                                                    
         BH    BODS020                                                          
         B     BODS030                                                          
BODS020  MVI   0(R6),C'H'                                                       
         LA    R6,1(R6)                                                         
         GOTO1 VHEXOUT,DMCB,SVAGENCY,(R6),2,=C'TOG'                             
         LA    R6,4(R6)                                                         
         B     BODS040                                                          
BODS030  MVC   0(2,R6),SVAGENCY                                                 
         LA    R6,2(R6)                                                         
BODS040  MVC   0(1,R6),SVSYSCOD                                                 
         LA    R6,1(R6)                                                         
         MVC   0(1,R6),SVSUBCOD                                                 
         LA    R6,1(R6)                                                         
         MVI   0(R6),C'.'                                                       
         LA    R6,1(R6)                                                         
         EDIT  SVFGNUM,(6,0(R6)),ZERO=NOBLANK,FILL=0                            
         MVI   0(R6),C'G'                                                       
         LA    R6,6(R6)                                                         
         MVI   0(R6),C'.'                                                       
         LA    R6,1(R6)                                                         
         LA    R1,GXFTSKEY                                                      
         LA    R0,8                                                             
         CLI   0(R1),C' '                                                       
         BE    BODS070                                                          
BODS050  CLI   0(R1),C' '                                                       
         BE    BODS060                                                          
         MVC   0(1,R6),0(R1)                                                    
         LA    R6,1(R6)                                                         
         LA    R1,1(R1)                                                         
         BCT   R0,BODS050                                                       
BODS060  MVI   0(R6),C'.'                                                       
         LA    R6,1(R6)                                                         
BODS070  EDIT  GXFTFNUM,(6,0(R6)),ZERO=NOBLANK,FILL=0                           
         MVI   0(R6),C'T'                                                       
*                                                                               
         B     BODSOK                                                           
*                                                                               
BODSNO   B     NO                                                               
BODSOK   B     YES                                                              
         XIT1                                                                   
         DROP  R3,R4                                                            
         EJECT                                                                  
***********************************************************************         
* BUILD LOG DATA SET NAME                                             *         
***********************************************************************         
         SPACE 1                                                                
         USING GXFTEL,R3                                                        
         USING GXFTSLST,R4                                                      
BLOGDSN  NTR1                                                                   
         MVC   LOGDSN,SPACES                                                    
         LA    R6,LOGDSN                                                        
*&&UK*&& MVC   0(8,R6),=CL8'ESS.XLG.'                                           
*&&US*&& MVC   0(8,R6),=CL8'DDS.XLG.'                                           
         LA    R6,8(R6)                                                         
         MVC   0(8,R6),GXFTEID                                                  
         LA    R6,8(R6)                                                         
         MVI   0(R6),C'.'                                                       
         LA    R6,1(R6)                                                         
         CLI   SVAGENCY,C'A'                                                    
         BL    BDSN020                                                          
         CLI   SVAGENCY,C'Z'                                                    
         BH    BDSN020                                                          
         CLI   SVAGENCY,C'A'                                                    
         BL    BDSN010                                                          
         CLI   SVAGENCY,C'Z'                                                    
         BH    BDSN010                                                          
         B     BDSN030                                                          
BDSN010  CLI   SVAGENCY,C'0'                                                    
         BL    BDSN020                                                          
         CLI   SVAGENCY,C'9'                                                    
         BH    BDSN020                                                          
         B     BDSN030                                                          
BDSN020  MVI   0(R6),C'H'                                                       
         LA    R6,1(R6)                                                         
         GOTO1 VHEXOUT,DMCB,SVAGENCY,(R6),2,=C'TOG'                             
         LA    R6,4(R6)                                                         
         B     BDSN040                                                          
BDSN030  MVC   0(2,R6),SVAGENCY                                                 
         LA    R6,2(R6)                                                         
BDSN040  MVC   0(1,R6),SVSYSCOD                                                 
         LA    R6,1(R6)                                                         
         MVC   0(1,R6),SVSUBCOD                                                 
         LA    R6,1(R6)                                                         
         MVI   0(R6),C'.'                                                       
         LA    R6,1(R6)                                                         
         EDIT  SVFGNUM,(6,0(R6)),ZERO=NOBLANK,FILL=0                            
         MVI   0(R6),C'G'                                                       
         LA    R6,6(R6)                                                         
         MVI   0(R6),C'.'                                                       
         LA    R6,1(R6)                                                         
         LA    R1,GXFTSKEY                                                      
         LA    R0,8                                                             
         CLI   0(R1),C' '                                                       
         BE    BDSNOK                                                           
BDSN050  CLI   0(R1),C' '                                                       
         BE    BDSNOK                                                           
         MVC   0(1,R6),0(R1)                                                    
         LA    R6,1(R6)                                                         
         LA    R1,1(R1)                                                         
         BCT   R0,BDSN050                                                       
*                                                                               
         B     BDSNOK                                                           
*                                                                               
BDSNNO   B     NO                                                               
BDSNOK   B     YES                                                              
         XIT1                                                                   
         DROP  R3,R4                                                            
         EJECT                                                                  
***********************************************************************         
* CHECK THIS EXTRACT FILE HAS BEEN COMMITTED AT ALL DESTIANTIONS      *         
***********************************************************************         
         SPACE 1                                                                
CHKFILE  NTR1  ,                                                                
         L     R2,ARECBUFF         READ HIGH FOR FIRST RECORD                   
*                                                                               
CHKF100  LA    R3,GXFIRST(R2)      PROCESS ELEMENT DATA                         
*                                                                               
CHKF110  CLI   0(R3),0                                                          
         BE    CHKFOK                                                           
         CLI   0(R3),GXFTELQ       FIND SYSTEM DEFINITION ELEMENT               
         BE    CHKF130                                                          
CHKF120  SR    RF,RF                                                            
         IC    RF,1(R3)                                                         
         AR    R3,RF                                                            
         B     CHKF110                                                          
*                                                                               
         USING GXFTEL,R3                                                        
CHKF130  EQU   *                                                                
         ICM   R5,15,=A(XTRTAB)                                                 
         USING XTRTABD,R5                                                       
CHKF132  CLC   XTRTEID,GXFTEID                                                  
         BNE   CHKF134                                                          
         CLC   XTRTAGY,SVAGENCY                                                 
         BNE   CHKF134                                                          
         CLC   XTRTSYS,SVSYSTEM                                                 
         BNE   CHKF134                                                          
         CLC   XTRTSUB,SVSUBSYS                                                 
         BNE   CHKF134                                                          
         OC    LIMNUM,LIMNUM                                                    
         BZ    CHKF133                                                          
         CLC   GXFTFNUM,LIMNUM                                                  
         BNH   CHKF120                                                          
         B     CHKFNO                                                           
CHKF133  CLI   CHECKFLG,C'Y'                                                    
         BNE   CHKF133A                                                         
         CLC   GXFTRCO,=CL6'000000'                                             
         BE    CHKF133A                                                         
         MVC   XTRTCOFN,GXFTFNUM                                                
         SR    RF,RF                                                            
         ICM   RF,3,XTRTCOFN                                                    
         BZ    *+10                                                             
         BCTR  RF,0                                                             
         STCM  RF,3,XTRTCOFN                                                    
         MVC   P(40),=CL40'CHECK ON FILE NOT COMMITTED'                         
         GOTO1 VHEXOUT,DMCB,(R2),P+42,32,=C'TOG'                                
         GOTO1 VPRINTER                                                         
         MVC   P(40),=CL40'ESSID:   '                                           
         MVC   P+10(8),SVEID                                                    
         GOTO1 VPRINTER                                                         
         B     CHKFNO                                                           
CHKF133A CLC   GXFTFNUM,XTRTCOFN                                                
         BNH   CHKF120                                                          
         B     CHKFNO                                                           
CHKF134  LA    R5,XTRTTBLQ(R5)                                                  
         CLM   R5,15,XTRTLAST                                                   
         BL    CHKF132                                                          
         B     CHKF120                                                          
*                                                                               
CHKFNO   B     NO                  EXIT ERROR                                   
CHKFOK   B     YES                 EXIT OK                                      
         EJECT                                                                  
***********************************************************************         
* CHECK THIS EXTRACT FILE HAS BEEN COMMITTED AT GIVEN DESTIANTION ESS *         
***********************************************************************         
         SPACE 1                                                                
CHKESS   NTR1  ,                                                                
         ICM   R5,15,=A(XTRTAB)                                                 
         USING XTRTABD,R5                                                       
CHKE010  CLC   XTRTEID,SVEID                                                    
         BNE   CHKE030                                                          
         CLC   XTRTAGY,SVAGENCY                                                 
         BNE   CHKE030                                                          
         CLC   XTRTSYS,SVSYSTEM                                                 
         BNE   CHKE030                                                          
         CLC   XTRTSUB,SVSUBSYS                                                 
         BNE   CHKE030                                                          
         CLI   XTRTFCOD,C'N'                                                    
         BE    CHKEOK                                                           
         CLI   XTRTMODE,C'I'                                                    
         BE    CHKEOK                                                           
         OC    LIMNUM,LIMNUM                                                    
         BZ    CHKE020                                                          
         CLC   GXFTFNUM,LIMNUM                                                  
         BNH   CHKEOK                                                           
         B     CHKENO                                                           
CHKE020  CLI   CHECKFLG,C'Y'                                                    
         BNE   CHKE022                                                          
         CLC   GXFTRCO,=CL6'000000'                                             
         BE    CHKE022                                                          
         MVC   XTRTCOFN,GXFTFNUM                                                
         SR    RF,RF                                                            
         ICM   RF,3,XTRTCOFN                                                    
         BZ    *+10                                                             
         BCTR  RF,0                                                             
         STCM  RF,3,XTRTCOFN                                                    
         MVC   P(40),=CL40'CHECK ON FILE NOT COMMITTED'                         
         GOTO1 VHEXOUT,DMCB,(R2),P+42,32,=C'TOG'                                
         GOTO1 VPRINTER                                                         
         MVC   P(40),=CL40'ESSID:   '                                           
         MVC   P+10(8),SVEID                                                    
         GOTO1 VPRINTER                                                         
         B     CHKENO                                                           
CHKE022  EQU   *                                                                
         MVC   FULL(2),GXFTFNUM                                                 
         MVC   FULL+2(2),XTRTCOFN                                               
         CLC   GXFTFNUM,XTRTCOFN                                                
         BNH   CHKEOK                                                           
         B     CHKENO                                                           
CHKE030  LA    R5,XTRTTBLQ(R5)                                                  
         CLM   R5,15,XTRTLAST                                                   
         BL    CHKE010                                                          
         B     CHKEOK                                                           
*                                                                               
CHKENO   B     NO                  EXIT ERROR                                   
CHKEOK   B     YES                 EXIT OK                                      
         EJECT                                                                  
***********************************************************************         
* UPDATE LAST CLEARED FILE NUMBER FOR ENTRY IN XTRTAB                 *         
***********************************************************************         
         SPACE 1                                                                
UPDESS   NTR1  ,                                                                
         ICM   R5,15,=A(XTRTAB)                                                 
         USING XTRTABD,R5                                                       
UPDE010  CLC   XTRTEID,SVEID                                                    
         BNE   UPDE030                                                          
         CLC   XTRTAGY,SVAGENCY                                                 
         BNE   UPDE030                                                          
         CLC   XTRTSYS,SVSYSTEM                                                 
         BNE   UPDE030                                                          
         CLC   XTRTSUB,SVSUBSYS                                                 
         BNE   UPDE030                                                          
         OC    XTRTDATE,XTRTDATE   ANY DATE YET?                                
         BZ    UPDE020             NO - GET IT                                  
         CLC   XTRTDATE(6),SVDATE  LATEST DATE & TIME?                          
         BNH   UPDEOK              NO - DONE                                    
UPDE020  MVC   XTRTCLFN,SVFTNUM    YES - UPDATE THESE                           
         MVC   XTRTDATE,SVDATE                                                  
         MVC   XTRTTIME,SVTIME                                                  
         B     UPDEOK                                                           
UPDE030  LA    R5,XTRTTBLQ(R5)                                                  
         CLM   R5,15,XTRTLAST                                                   
         BL    UPDE010                                                          
         B     UPDEOK                                                           
*                                                                               
UPDENO   B     NO                  EXIT ERROR                                   
UPDEOK   B     YES                 EXIT OK                                      
         EJECT                                                                  
***********************************************************************         
* SET UP DATE AND TIME VALUES                                         *         
***********************************************************************         
         SPACE 1                                                                
SDATIME  NTR1  ,                                                                
         SR    R0,R0                                                            
         SR    R1,R1                                                            
*                                  GET THE CURRENT TIME IN BINARY               
TIME010  TIME  BIN                                                              
         LTR   R0,R0                                                            
         BZ    TIME010             BAD RETURN FROM MACRO                        
         ST    R0,TIMENB                                                        
*                                                                               
         SR    R0,R0                                                            
         SR    R1,R1                                                            
*                                  GET THE CURRENT MVS DATE/TIME                
TIME020  TIME                                                                   
         LTR   R0,R0                                                            
         BZ    TIME020             BAD RETURN FROM MACRO                        
         ST    R0,MVSTIME                                                       
         ST    R1,MVSDATE                                                       
*                                                                               
*                                  GET DATE NOW IN VARIOUS FORMATS              
         MVC   CENTURY,=CL2'19'    HARD CODE CENTURY VALUE                      
         GOTO1 VDATCON,DMCB,(5,0),(0,DATEN+2)                                   
         MVC   DATEN(2),CENTURY                                                 
         GOTO1 VDATCON,DMCB,(5,0),(2,DATENC)                                    
         GOTO1 VDATCON,DMCB,(5,0),(3,DATENB)                                    
*                                                                               
         L     R1,MVSTIME                                                       
         STCM  R1,15,TIMEND                                                     
*                                  CONVERT TIME NOW TO EBCDIC                   
         SRL   R1,28                                                            
         STC   R1,TIMEN                                                         
         OI    TIMEN,X'F0'                                                      
         L     R1,MVSTIME                                                       
         SRL   R1,24                                                            
         STC   R1,TIMEN+1                                                       
         OI    TIMEN+1,X'F0'                                                    
         L     R1,MVSTIME                                                       
         SRL   R1,20                                                            
         STC   R1,TIMEN+2                                                       
         OI    TIMEN+2,X'F0'                                                    
         L     R1,MVSTIME                                                       
         SRL   R1,16                                                            
         STC   R1,TIMEN+3                                                       
         OI    TIMEN+3,X'F0'                                                    
         L     R1,MVSTIME                                                       
         SRL   R1,12                                                            
         STC   R1,TIMEN+4                                                       
         OI    TIMEN+4,X'F0'                                                    
         L     R1,MVSTIME                                                       
         SRL   R1,8                                                             
         STC   R1,TIMEN+5                                                       
         OI    TIMEN+5,X'F0'                                                    
         L     R1,MVSTIME                                                       
         SRL   R1,4                                                             
         STC   R1,TIMEN+6                                                       
         OI    TIMEN+6,X'F0'                                                    
         L     R1,MVSTIME                                                       
         STC   R1,TIMEN+7                                                       
         OI    TIMEN+7,X'F0'                                                    
*                                                                               
*                                  CONVERT INPUT PARAMETER CARD                 
*                                    FILTER DATE/TIMES                          
*                                                                               
         GOTO1 VDATCON,DMCB,(3,XFDATE),(0,XFDATEE+2)                            
         GOTO1 VDATCON,DMCB,(3,XTDATE),(0,XTDATEE+2)                            
         MVC   XFDATEE(2),CENTURY                                               
         MVC   XTDATEE(2),CENTURY                                               
         GOTO1 VDATCON,DMCB,(3,XFDATE),(2,XFDATEC)                              
         GOTO1 VDATCON,DMCB,(3,XTDATE),(2,XTDATEC)                              
         GOTO1 VDATCON,DMCB,(3,XFDATE),(1,XFDATEP)                              
         GOTO1 VDATCON,DMCB,(3,XTDATE),(1,XTDATEP)                              
*                                                                               
         LA    R0,9                                                             
         GOTO1 VTIMBER,DMCB,(0,(R0)),(X'01',XFTIMEP),(1,WORK)                   
         CLI   0(R1),0                                                          
         BE    *+6                                                              
         DC    H'00'                                                            
         MVC   XFTIMEE(4),WORK                                                  
         MVC   XFTIMEE+4(2),=CL2'00'                                            
         MVC   XTTIMEE(4),WORK+5                                                
         MVC   XTTIMEE+4(2),=CL2'00'                                            
         MVC   P,SPACES                                                         
         MVC   P(22),=C'DATE:           TIME: '                                 
         MVC   P+6(8),DATEN                                                     
         MVC   P+22(8),TIMEN                                                    
         GOTO1 VPRINTER                                                         
         B     SDATOK                                                           
*                                                                               
SDATNO   B     NO                  EXIT ERROR                                   
SDATOK   B     YES                 EXIT OK                                      
         EJECT                                                                  
         SPACE 1                                                                
***********************************************************************         
* ENQUEUE CONTROL SYSTEM                                              *         
***********************************************************************         
         SPACE 1                                                                
ENQCTRL  NTR1                                                                   
         CLI   ENQCFLAG,C'Y'                                                    
         BE    ECTRLX                                                           
*                                    BUFFER PROBLEM                             
*&&UK*&& MVC   ENQCMND,=C'ENQDEQ  '                                             
*&&US*&& MVC   ENQCMND,=C'ENQCTL  '                                             
         GOTO1 VDATAMGR,DMCBENQ,(0,ENQCMND),(C'T',=C'CTRL')                     
         TM    8(R1),X'01'                                                      
         BO    ECTR010             SYSTEM IS ALREADY ENQUEUED                   
         GOTO1 VDATAMGR,DMCBENQ,(0,ENQCMND),(C'E',=C'CTRL')                     
*                                                                               
ECTR010  MVI   ENQCFLAG,C'Y'                                                    
*                                                                               
ECTRLX   XIT1                                                                   
         SPACE 1                                                                
***********************************************************************         
* DEQUEUE CONTROL SYSTEM                                              *         
***********************************************************************         
         SPACE 1                                                                
DEQCTRL  NTR1                                                                   
         CLI   ENQCFLAG,C'N'                                                    
         BE    DCTRLX                                                           
         GOTO1 VDATAMGR,DMCBENQ,(0,ENQCMND),(C'D',=C'CTRL')                     
         MVI   ENQCFLAG,C'N'                                                    
*                                                                               
DCTRLX   XIT1                                                                   
         SPACE 1                                                                
**********************************************************************          
* OPEN DATA MANAGER CONTROL FILE FOR UPDATE                          *          
**********************************************************************          
         SPACE 1                                                                
OPENCUPD NTR1                                                                   
         CLI   CONSOPEN,C'Y'                                                    
         BE    OPENCUX                                                          
*                                                                               
         GOTO1 VDATAMGR,DMCB,DMOPEN,CONTROL,CTFILEU,IOL,0                       
*                                  GET DTF ADDRESS OF GENDIR                    
         GOTO1 VDATAMGR,DMCB,=C'DTFADR',GENDIR                                  
         ICM   RF,15,12(R1)                                                     
         BNZ   *+6                                                              
         DC    H'00'                                                            
         LA    RF,0(RF)                                                         
         ST    RF,AGENDIR                                                       
*                                  GET DTF ADDRESS OF GENFIL                    
         GOTO1 VDATAMGR,DMCB,=C'DTFADR',GENFIL                                  
         ICM   RF,15,12(R1)                                                     
         BNZ   *+6                                                              
         DC    H'00'                                                            
         LA    RF,0(RF)                                                         
         ST    RF,AGENFIL                                                       
*                                                                               
         MVI   CONSOPEN,C'Y'                                                    
*                                                                               
OPENCUX  XIT1                                                                   
         SPACE 1                                                                
**********************************************************************          
* CLOSE DATA MANAGER CONTROL FILE                                    *          
**********************************************************************          
         SPACE 1                                                                
CLOSCTFL NTR1                                                                   
         CLI   CONSOPEN,C'N'                                                    
         BE    CLOSCTX                                                          
         L     RE,=V(UTL)          SWITCH TO CONTROL SYSTEM                     
         MVI   4(RE),CONTROLQ                                                   
         GOTO1 VDATAMGR,DMCB,DMCLSE,=C'FILES',,IOL                              
         MVI   CONSOPEN,C'N'                                                    
*                                                                               
CLOSCTX  XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* PRINT ERROR MESSAGE                                                 *         
***********************************************************************         
         SPACE 1                                                                
ERRPRT   NTR1                                                                   
         L     RE,=A(ERRTAB)                                                    
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
         SPACE 1                                                                
* FTPFILE DATASETS -- DYNAMIC UNALLOCATION BY DDNAME                            
*                                                                               
         DS    0F                                                               
ARBLKUNA DC    X'80',AL3(RBLKUNA)  R1 POINTS TO THIS BEFORE DYNALLOC            
*                                                                               
RBLKUNA  DC    X'1402000000000000',A(ACATUNT),X'0000000000000000'               
*                                                                               
ACATUNT  DC    X'80',AL3(TXTUNDD)                                               
         SPACE 2                                                                
* DELETE FTPFILE DATASETS -- DYNAMIC ALLOCATION                                 
*                                                                               
         DS    0F                                                               
ARBLKDLA DC    X'80',AL3(RBLKDELA) R1 POINTS TO THIS BEFORE DYNALLOC            
*                                                                               
RBLKDELA DC    X'1401010000000000',A(ADELALLT),X'0000000000000000'              
*                      << IGNORE MIGRATED DATA SETS (SEE S99NOMIG !!)           
*                                                                               
ADELALLT DC    X'00',AL3(TXTDSN)                                                
         DC    X'00',AL3(TXTDISPO)                                              
         DC    X'80',AL3(TXTNDISD)                                              
         SPACE 2                                                                
* FTPFILE DATASETS -- DYNAMIC UNALLOCATION BY DSN                               
*                                                                               
         DS    0F                                                               
ARBLKUN2 DC    X'80',AL3(RBLKUNA2) R1 POINTS TO THIS BEFORE DYNALLOC            
*                                                                               
RBLKUNA2 DC    X'1402000000000000',A(ACATUNT2),X'0000000000000000'              
*                                                                               
ACATUNT2 DC    X'80',AL3(TXTDSN)                                                
         SPACE 3                                                                
TXTDD    DC    AL2(DALDDNAM),X'00010008',CL8' '        DDNAME                   
TXTUNDD  DC    AL2(DUNDDNAM),X'00010008',CL8' '        DDNAME                   
TXTDSN   DC    AL2(DALDSNAM),X'0001002C',CL44' '       DSN=............         
TXTDISP  DC    AL2(DALSTATS),X'0001000104'             DISP=(NEW,.....)         
TXTNDISP DC    AL2(DALNDISP),X'0001000102'                 =(...,CATLG)         
TXTDISPO DC    AL2(DALSTATS),X'0001000101'             DISP=(OLD,...)           
TXTNDISD DC    AL2(DALNDISP),X'0001000104'                 =(...,DEL)           
TXTBLKLN DC    AL2(DALBLKLN),X'00010003',AL3(16500)    SPACE=(16500,...         
TXTPRI   DC    AL2(DALPRIME),X'00010003',AL3(25)            =(..(25,...         
TXTSEC   DC    AL2(DALSECND),X'00010003',AL3(40)            =(...,40),.         
TXTRLSE  DC    AL2(DALRLSE),X'0000'                         =(...,RLSE)         
TXTUNIT  DC    AL2(DALUNIT),X'00010005',C'SYSDA'       UNIT=SYSDA               
         EJECT                                                                  
*                                                                               
CONTROLQ EQU   X'0A'               CONTROL SYSTEM UTL SENUM                     
NOTEND   EQU   X'00'                                                            
END      EQU   X'FF'                                                            
OFF      EQU   X'00'                                                            
ON       EQU   X'FF'                                                            
MXTRTQ   EQU   X'5E'               FIELD SEPERATOR CHR                          
FFILL    DC    32X'FF'                                                          
         EJECT                                                                  
WORKD    DS    0D                  ** GLOBAL WORKING STORAGE **                 
         SPACE 1                                                                
VDATAMGR DC    V(DATAMGR)                                                       
VDMOD000 DC    V(DMOD000)                                                       
VDADDS   DC    V(DADDS)                                                         
VLOGIO   DC    V(LOGIO)                                                         
VCPRINT  DC    V(CPRINT)                                                        
VPRINT   DC    V(PRINT)                                                         
VPRINTER DC    V(PRINTER)                                                       
VHELLO   DC    V(HELLO)                                                         
VHEXOUT  DC    V(HEXOUT)                                                        
VHEXIN   DC    V(HEXIN)                                                         
VCARDS   DC    V(CARDS)                                                         
VNUMVAL  DC    V(NUMVAL)                                                        
VTIMBER  DC    V(TIMBER)                                                        
VDATVAL  DC    V(DATVAL)                                                        
VDATCON  DC    V(DATCON)                                                        
VADDAY   DC    V(ADDAY)                                                         
VPERVAL  DC    V(PERVAL)                                                        
VDYNALOC DC    V(DYNALLOC)                                                      
VXSORT   DC    V(XSORT)                                                         
*                                                                               
         DS    0D                                                               
* ++INCLUDE FASSBOFF                                                            
         PRINT OFF                                                              
       ++INCLUDE FASSBOFF                                                       
         PRINT ON                                                               
         ORG   SSOOFF                                                           
SSB      DC    XL(SSOOFFX-SSOOFF)'00'                                           
         ORG   SSOXTND                                                          
         DC    X'FF'               SET EXTENDED OFFLINE SSB                     
         ORG   SSOSTAT2                                                         
         DC    X'02'               FOR DATAMGR (OFFLINE NO RECOVERY)            
         ORG                                                                    
*                                                                               
EXDSNAME DC    CL44'JOMU.TEST1                    '                             
DMOPEN   DC    C'OPEN   '                                                       
DMREAD   DC    C'DMREAD '                                                       
DMRSEQ   DC    C'DMRSEQ '                                                       
DMRDHI   DC    C'DMRDHI '                                                       
DMADD    DC    C'DMADD  '                                                       
DMWRT    DC    C'DMWRT  '                                                       
DMCLSE   DC    C'DMCLSE '                                                       
DMFAST   DC    C'DMFAST '                                                       
SYSFLES  DC    C'SYSFLES'                                                       
GETREC   DC    C'GETREC '                                                       
PUTREC   DC    C'PUTREC '                                                       
ADDREC   DC    C'ADDREC '                                                       
DMRFIL   DC    C'RECOVER'                                                       
CONTROL  DC    C'CONTROL'                                                       
CTFILE   DC    C'CTFILE '                                                       
GENDIR   DC    C'GENDIR '                                                       
GENFIL   DC    C'GENFIL '                                                       
CTFILEU  DC    C'UCTFILE UGENDIR UGENFIL UCTRCVR X'                             
MEDFILE  DC    C'MEDFIL '                                                       
MEDFILQ  EQU   X'42'                                                            
MEDDIR   DC    C'MEDDIR '                                                       
ACCDIR   DC    C'ACCDIR '                                                       
DMDA     DC    F'0'                                                             
FILEDA   DC    F'0'                                                             
MAXRECS  DC    F'999999'                                                        
COMMAX   DC    PL8'999999'                                                      
LIMMAX   DC    XL2'FFFF'                                                        
ACTIVITY DC    CL1'Y'                                                           
APALLOC  DC    XL3'001388'                                                      
ASALLOC  DC    XL3'001388'                                                      
*                                                                               
*                                                                               
* TABLE OF VALID MODES INPUT VIA MODE=CARD                                      
         SPACE 1                                                                
TTABLIST DC    C'123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ',X'00'                     
         SPACE 1                                                                
SNUMLIST DC    C' 123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ',X'00'                    
         SPACE 2                                                                
         EJECT                                                                  
DUB      DS    D                                                                
DMCB     DS    6F                                                               
DMCBENQ  DS    6F                                                               
*                                                                               
FULL     DS    F                                                                
RELO     DS    A                                                                
HALF     DS    H                                                                
DMWORK   DS    12D                 DATAMANAGER WORK AREA                        
BYTE     DS    XL1                                                              
*                                                                               
ERROR    DS    XL1                 ERROR CODE                                   
FIRSTFLG DS    XL1                 START OF RECOVERY FILE FLAG (N/Y)            
*                                                                               
MVSPARM  DS    XL1                 MVS STEP CONTROL INPUT PARAMETER             
MVSTIME  DS    F                   IBM TIME BINARY 100S SECS                    
MVSDATE  DS    F                   IBM DATE JULIAN                              
*                                                                               
CENTURY  DS    CL2                 CURRENT CENTURY EBCDIC VALUE                 
DATEN    DS    CL8                 DATE NOW EBCDIC                              
DATENC   DS    XL2                 DATE NOW COMPRESSED                          
DATENB   DS    XL3                 DATE NOW BINARY                              
TIMEN    DS    CL8                 TIME NOW EBCDIC (HHMMSSTH)                   
TIMEND   DS    XL4                 TIME NOW DECIMAL UNSIGNED PACKED             
TIMENB   DS    F                   TIME NOW BINARY FULL WORD                    
ADDATE   DS    CL6                 ADDAY DATE WORK SPACE YYMMDD EBCDIC          
*                                                                               
AGYSAVE  DS    CL2                 AGENCY ALPHA SAVE                            
*                                                                               
RETCODE  DS    XL1                 MVC JOB STEP RETUN CODE (X'FF' = OK)         
CONSOPEN DS    CL1                 CONTROL SYSTEM OPEN FLAG (N/Y)               
*                                                                               
*                                  JCL INPUT CONTROL CARD PARAMETERS            
AGYFILT  DS    CL2                 AGENCT ALPHA ID FILTER                       
PLATFORM DS    XL1                 SERVER PLATFORM BYTE (SQL = X'01')           
INTAPE   DS    CL1                 RECOVERY FILE ON TAPE FLAG                   
ENQCFLAG DS    CL1                 ENQUEUED CONTROL SYSTEM FLAG (Y/N)           
ESSID    DS    CL8                 ESS ID FILTER                                
APPLKEY  DS    CL8                 SQL SERVER APPLICATION KEY                   
SERVERID DS    CL16                SQL SERVER ID                                
DATABASE DS    CL16                SQL SERVER DATABASE NAME                     
SVAGENCY DS    CL2                                                              
SVSYSTEM DS    XL1                                                              
SVSUBSYS DS    XL1                                                              
TESTDATE DS    XL2                                                              
SVDATE   DS    XL2                                                              
SVTIME   DS    XL4                                                              
SVSYSCOD DS    CL1                                                              
SVSUBCOD DS    CL1                                                              
SVFGNUM  DS    XL2                                                              
SVFTNUM  DS    XL2                                                              
SVEID    DS    CL8                                                              
ESSFOUND DS    CL1                                                              
CHECKFLG DS    XL1                 CHECK UNCOMMITTED FILE FLAG                  
*                                                                               
FTIME    DS    F                   FILTER FROM TIME                             
AGYALPH  DS    CL2                 AGENCY ALPHA ID                              
FCDATE   DS    XL2                 FILE CREATE DATE BINARY COMPRESSED           
FCTIME   DS    XL4                 FILE CREATE TIME PWOS HHMMSSTH               
XTTIME   DS    F                   FILTER TO TIME                               
XFDATE   DS    F                   FILTER FROM DATE                             
XTDATE   DS    F                   FILTER TO DATE                               
XFTIMEP  DS    XL2                 FILTER FROM TIME PACKED (HHMM)               
XTTIMEP  DS    XL2                 FILTER TO TIME PACKED                        
XFTIMEE  DS    CL6                 FILTER FROM TIME EBCDIC                      
XTTIMEE  DS    CL6                 FILTER TO TIME EBCDIC                        
XFDATEE  DS    CL8                 FILTER FROM DATE EBCDIC                      
XTDATEE  DS    CL8                 FILTER TO DATE EBCDIC                        
XFDATEC  DS    XL2                 FILTER FROM DATE COMPRESSED                  
XTDATEC  DS    XL2                 FILTER TO DATE COMPRESSED                    
XFDATEP  DS    XL3                 FILTER FROM DATE PWOS                        
XTDATEP  DS    XL3                 FILTER TO DATE PWOS                          
*                                                                               
SYSFILT  DS    XL1                 SYSTEM NUMBER FILTER                         
SYSCFILT DS    CL1                 SYSTEM FILE CODE FILTER                      
SYSTEM   DS    XL1                 SYSTEM BINARY CODE                           
SUBFILT  DS    XL1                 SUB SYSTEM BINARY CODE                       
SUBCHAR  DS    CL1                 SUB SYSTEM CHARACTER CODE                    
SUBNAME  DS    XL7                 SUB SYSTEM NAME                              
SYSCODE  DS    CL1                 SYSTEM CHARACTER CODE                        
SYSCHAR  DS    CL1                 SYSTEM FILE CODE CODE                        
SYSLNAM  DS    CL7                 LOGICAL SYSTEM NAME                          
SENUM    DS    XL1                 SYSTEM SE NUMBER                             
AGYAID   DS    CL2                 AGENCY APLHA ID                              
AGYBIN   DS    XL1                 AGENCY BINARY CODE                           
AGYPIN   DS    XL2                 AGENCY PRINCIPLE ID NUMBER                   
RSYSID   DS    XL1                 RECOVERY RECORD SYSTEM ID                    
LIMNUM   DS    XL2                 FILE NUMBER LIMIT VALUE                      
AGEDATE  DS    XL2                 AGED DATE BINARY COMPRESSED                  
UPTODATE DS    XL2                 UPTO DATE BINARY COMPRESSED                  
REMOVE   DS    CL1                 REMOVE MVS DATA SETS=Y                       
*                                                                               
ASYSEXT  DS    A                   A(SYSTEM EXTRACT MODULE)                     
SXDTPTR  DS    A                   A(CURRENT ENTRY IN SXDTAB)                   
SXDTEND  DS    A                   A(END OF SXDTAB)                             
SXDTNXT  DS    A                   A(NEXT SYSTEM GROUP IN SXDTAB)               
ASXDTAB  DS    A                   A(SYSTEM EXTRACT DRIVER TABLE)               
ADXBLOCK DS    A                   A(EXTRACT CONTROL DATA BLOCK)                
ASLLIST  DS    A                   A(SERVER LUID LIST BLOCK)                    
ASLNEXT  DS    A                   A(NEXT FREE SPACE IN SLLIST)                 
ARECBUFF DS    A                   A(GENERAL RECORD BUFFER)                     
ACPYBUFF DS    A                   A(RECOVERY RECORD COPY BUFFER)               
AXREC    DS    A                   A(EXTRACT RECORD BUFFER)                     
ASQLBUFF DS    A                   A(SQL RECORD BUFFER)                         
ATRKBUFF DS    A                   A(DISK IO BUFFER)                            
AGENDIR  DS    A                   A(GENDIR) FROM DTFADR                        
AGENFIL  DS    A                   A(GENFIL) FROM DTFADR                        
*                                                                               
XTRTLAST DS    A                                                                
*                                                                               
ENQCMND  DS    CL8                 SAVE ENQDEQ COMMAND NAME                     
WORK     DS    XL256               GENERAL WORK AREA                            
ELEMENT  DS    XL256               RECORD ELEMENT WORK AREA                     
IOKEYSV  DS    CL32                RECORD KEY BUFFER                            
IOKEY    DS    CL32                RECORD KEY BUFFER                            
IOL      DS    F                   GENERAL IO AREA                              
IO       DS    2048X                                                            
WORKX    DS    0D                                                               
         EJECT                                                                  
         SPACE 1                                                                
LOGDSN   DC    CL44'XLOG.ESSNNNNN.SERVERNM.DATABASE.AAASS.FNNNNN'               
*                                                                               
* INDIRECT ADDRESSABILITY FROM HERE                                             
*                                                                               
         SPACE 1                                                                
ERRTAB   DS    0H                  ERROR REPORT STRINGS                         
ERRMSG0  DC    CL40'DATASET NOT FOUND IN PAN LIBRARY'                           
ERRMSG1  DC    CL40'INVALID CONTROL CARD INPUT'                                 
ERRMSG2  DC    CL40'MISSING SYSTEM DEFINITION'                                  
ERRMSG3  DC    CL40'INVALID SYSTEM NAME      '                                  
ERRMSG4  DC    CL40'ERROR 4                  '                                  
ERRMSG5  DC    CL40'ERROR 5                  '                                  
         SPACE 2                                                                
* FASYSLST                                                                      
         PRINT OFF                                                              
       ++INCLUDE FASYSLST                                                       
         PRINT ON                                                               
* DXSYSLEX                                                                      
         PRINT OFF                                                              
       ++INCLUDE DXSYSLEX                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* DXSUBLST                                                                      
       ++INCLUDE DXSUBLST                                                       
         SPACE 2                                                                
* CARDTBL DEFINES JOB CONTROL INPUT PARAMETER CARDS                             
* AL1    LENGTH OF CARD NAME                                                    
* AL1    ACTION ROUTINE NUMBER                                                  
* XL1    ACTION FLAGS                                                           
* CL8    CARD NAME                                                              
*                                                                               
CARDTBL  DS    0CL14                                                            
         DC    AL1(07,01),X'00',CL11'SYSTEM='                                   
         DC    AL1(07,02),X'00',CL11'AGENCY='                                   
         DC    AL1(05,03),X'00',CL11'TIME='                                     
         DC    AL1(06,04),X'00',CL11'INPUT='                                    
         DC    AL1(05,05),X'00',CL11'DATE='                                     
         DC    AL1(06,06),X'00',CL11'PATCH='                                    
         DC    AL1(06,07),X'00',CL11'DDSIO='                                    
         DC    AL1(09,08),X'00',CL11'PLATFORM='                                 
         DC    AL1(07,09),X'00',CL11'SUBSYS='                                   
         DC    AL1(07,10),X'00',CL11'SERVER='                                   
         DC    AL1(09,11),X'00',CL11'DATABASE='                                 
         DC    AL1(06,12),X'00',CL11'LIMIT='                                    
         DC    AL1(04,13),X'00',CL11'AGE='                                      
         DC    AL1(07,14),X'00',CL11'REMOVE='                                   
         DC    AL1(05,15),X'00',CL11'UPTO='                                     
         DC    AL1(06,16),X'00',CL11'CHECK='                                    
         DC    AL1(07,17),X'00',CL11'DSPACE='                                   
CARDTBLX DC    AL1(00)                                                          
CLENGTH  EQU   0                                                                
CROUTINE EQU   1                                                                
CFLAG    EQU   2                                                                
CSTRING  EQU   3                                                                
         SPACE 1                                                                
*                                                                               
* EXTRACT FILE DCB                                                              
*                                                                               
EXFILE   DCB   DDNAME=EXFILE,DSORG=PS,RECFM=VB,MACRF=(PM),             *        
               LRECL=2048,BLKSIZE=8120                                          
         EJECT                                                                  
WORKC    CSECT                     ** WORKING STORAGE POOL 1 **                 
         DS    10000D                                                           
         SPACE 1                                                                
RECBUFF  CSECT                     RECORD IO AREA                               
         DS    10000X                                                           
RECBUFFX EQU   *                                                                
RECBUF2  CSECT                     RECORD IO AREA 2                             
         DS    10000X                                                           
RECBUF2X EQU   *                                                                
         SPACE 1                                                                
         SPACE 1                                                                
XTRTAB   CSECT                     XTRANS COMMIT TABLE                          
         DC    (2000*(XTRTTBLQ))X'00'                                           
XTRTABX  EQU   *                                                                
         SPACE 1                                                                
*                                                                               
* DSECT FOR XTRANS COMMIT TABLE                                                 
*                                                                               
XTRTABD  DSECT                                                                  
*                                  EXTRACT DESTINATION KEY VALUES               
XTRTEID  DS    CL8                 REMOTE ESS SERVER ID NAME (ESSNNNNN)         
XTRTAGY  DS    CL2                 AGENCY ALPHA CODE                            
XTRTSYS  DS    XL1                 SYSTEM                                       
XTRTSUB  DS    XL1                 SUB SYSTEM                                   
XTRTCOFN DS    XL2                 LAST FILE NUMBER - COMMITTED                 
XTRTCLFN DS    XL2                 LAST FILE NUMBER - CLEARED                   
XTRTFCOD DS    CL1                 TRANSFER FERQUENCY CODE                      
XTRTMODE DS    CL1                 TRANSFER UPDATE MODE                         
XTRTDATE DS    XL2                 DATE                                         
XTRTTIME DS    XL4                 TIME                                         
XTRTTBLQ EQU   *-XTRTABD                                                        
         EJECT                                                                  
* DXSUBLSTD                                                                     
       ++INCLUDE DXSUBLSTD                                                      
         SPACE 1                                                                
* GEGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE GEGENFILE                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* GEGENXTR                                                                      
       ++INCLUDE GEGENXTR                                                       
         SPACE 1                                                                
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
* DDSYSELD                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDSYSELD                                                       
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
* FAUTL                                                                         
         PRINT OFF                                                              
       ++INCLUDE FAUTL                                                          
         PRINT ON                                                               
         SPACE 1                                                                
* DDDPRINTL                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDDPRINTL                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* DDPERVALD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDPERVALD                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* DMDTFIS                                                                       
         PRINT OFF                                                              
       ++INCLUDE DMDTFIS                                                        
         PRINT ON                                                               
         SPACE 1                                                                
* DMGREQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE DMGREQUS                                                       
         PRINT ON                                                               
         IEFZB4D2                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'018DXCLEAR   05/18/16'                                      
         END                                                                    
